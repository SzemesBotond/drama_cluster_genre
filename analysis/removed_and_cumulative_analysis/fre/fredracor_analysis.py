import csv
import logging
import requests
from collections import defaultdict
from itertools import combinations

import networkx as nx
from bs4 import BeautifulSoup

from removed_and_cumulative_analysis.removed_act_analysis_utils import (get_largest_G)
from removed_and_cumulative_analysis.fre.schemas_and_mappings import FREDRACOR_METADATA_PATH
from schemas_and_mappings import (
    BLACKLIST,
    ACT_TAGS,
    POSSIBLE_ACT_TAGS,
    STRICT_ACTS,
    SCENE_TAGS,
)

from dracor_input_handling.dracor_metadata import load_genres_from_metadata

logger = logging.getLogger(__name__)


def load_fredracor_metadata(metadata_filepath, outputs_dir):
    """Return {play_name: genre} for Tragedy/Comedy plays with more than 5 speakers."""

    if metadata_filepath.startswith('http'):
        response = requests.get(
            metadata_filepath,
            headers={'accept': 'text/csv'}
        )
        metadata_filepath = outputs_dir / 'fredracor-metadata.csv'
        with open(metadata_filepath, 'w', encoding='utf-8') as f:
            f.write(response.text)
        logging.info(f'FREDRACOR METADATA: {metadata_filepath} has been downloaded from api.')

    genres = {}
    with open(metadata_filepath) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            if row['normalizedGenre'] in ['Tragedy', 'Comedy'] and int(row['numOfSpeakers']) > 5:
                genres[row['name']] = row['normalizedGenre']
    return genres


def _div_type_count(tag, type_names):
    total = 0
    for name in type_names:
        total += len(tag.find_all('div', {'type': name}, recursive=False))
    return total


def _one_unit_edge_list(unit, lonely_nodes):
    """Extract co-appearance edges between speakers in a single scene/unit."""
    characters = []
    for sp in unit.find_all('sp', {'who': True}):
        for speaker in sp['who'].split(' '):
            characters.append(speaker)
    edges = list(combinations(set(characters), 2))
    if len(characters) == 1:
        lonely_nodes.append(characters[0])
    return edges, lonely_nodes


def _network_from_edges(edge_list, lonely_nodes):
    G = nx.from_edgelist(set(edge_list))
    for node in lonely_nodes:
        if node not in G.nodes:
            G.add_node(node)
    return G


def _edge_list_extractor(act_segments, name):
    """
    Extract co-appearance edges from a list of act soup segments.

    Within each act, looks for SCENE_TAGS sub-divisions; falls back to treating
    the whole act as a single unit. Returns (edge_list, lonely_nodes, scenes_whole).
    scenes_whole is False when the number of recognised scene tags doesn't match
    the total number of child div tags in at least one act.
    """
    scenes_whole = True
    lonely_nodes = []
    edge_list = []

    act_tags_used = ACT_TAGS if STRICT_ACTS else ACT_TAGS + POSSIBLE_ACT_TAGS

    for c, act in enumerate(act_segments, start=1):
        if act is None:
            raise ValueError(f'ACT {c} IS NONE IN {name}')

        n_divs = len(act.find_all('div', recursive=False))
        all_scenes = []
        for scene_tag in SCENE_TAGS:
            found = act.find_all('div', {'type': scene_tag})
            if found:
                all_scenes.extend(found)

        if len(all_scenes) != n_divs:
            scenes_whole = False

        if all_scenes:
            for scene in all_scenes:
                if scene.find('div', {'type': 'scene'}) is not None:
                    logger.warning('Nested scene tags in %s', name)
                else:
                    edges, lonely_nodes = _one_unit_edge_list(scene, lonely_nodes)
                    edge_list += edges
        else:
            if any(act.find(t) is not None for t in act_tags_used):
                raise ValueError(f'Unaccounted div type in {name}!')
            edges, lonely_nodes = _one_unit_edge_list(act, lonely_nodes)
            edge_list += edges

    return edge_list, lonely_nodes, scenes_whole


def _collect_acts(soup):
    act_tags_used = ACT_TAGS if STRICT_ACTS else ACT_TAGS + POSSIBLE_ACT_TAGS
    acts = []
    for tag_type in act_tags_used:
        acts.extend(soup.find_all('div', {'type': tag_type}))
    return acts


def load_fredracor_soups(dracor_tei_generator, genres):
    """
    Parse FreDracor TEI XML files from the generator, keeping only plays that:
      - appear in the genres dict (Comedy/Tragedy, 5+ speakers)
      - are not in BLACKLIST
      - have exactly 5 acts (per STRICT_ACTS setting)
    Returns {play_name: (soup, is_whole)}.
    """
    soups = {}
    for xml_name, xml in dracor_tei_generator:

        if xml_name not in genres or xml_name in BLACKLIST:
            logger.info('Skipped %s (not in genre filter or blacklisted)', xml_name)
            continue

        soup = BeautifulSoup(xml, 'lxml-xml')
        body = soup.body

        all_divs = len(body.find_all('div', recursive=False))
        acts_count = _div_type_count(body, ACT_TAGS)
        acts_and_possible = _div_type_count(body, ACT_TAGS + POSSIBLE_ACT_TAGS)
        is_whole = all_divs == 5

        act_condition = acts_count == 5 if STRICT_ACTS else acts_and_possible == 5

        if act_condition:
            soups[xml_name] = (soup, is_whole)
            logger.info('Loaded %s (%s)', xml_name, genres[xml_name])
        else:
            logger.debug(
                'Skipped %s — acts: %d (strict=%s), all_divs: %d',
                xml_name, acts_count, STRICT_ACTS, all_divs,
            )

    return soups


def _acts_label(depth):
    return f"acts_{'-'.join(str(i) for i in range(1, depth + 1))}"


def build_fredracor_cumulative(soups, genres, acts=None):
    """
    Build whole and cumulative co-appearance networks for each FreDracor play.

    acts is a list of depth strings; each value N means "include the first N acts".
    Defaults to ['1','2','3','4','5'].
    """
    if acts is None:
        acts = ['1', '2', '3', '4', '5']

    depths = [int(n) for n in acts]
    cumulative_G_list = defaultdict(dict)

    for name, (soup, acts_whole) in soups.items():
        cumulative_G_list[name]['genre'] = genres[name]
        cumulative_G_list[name]['title_pretty'] = soup.find('title').get_text(strip=True)

        all_acts = _collect_acts(soup)

        edge_list, lonely_nodes, scenes_whole = _edge_list_extractor(all_acts, name)
        cumulative_G_list[name]['whole'] = _network_from_edges(edge_list, lonely_nodes)
        cumulative_G_list[name]['is_whole'] = acts_whole and scenes_whole

        for depth in depths:
            acts_slice = all_acts[:depth]
            edge_list, lonely_nodes, _ = _edge_list_extractor(acts_slice, name)
            label = _acts_label(depth)
            cumulative_G_list[name][label] = _network_from_edges(edge_list, lonely_nodes)
            logger.info('%s  depth %d (%s): %d edges', name, depth, label, len(set(edge_list)))

    return cumulative_G_list


def write_fredracor_cumulative_csv(cumulative_G_list, filename, acts=None):
    """Write cumulative network metrics to CSV for the requested depth snapshots."""
    if acts is None:
        acts = ['1', '2', '3', '4', '5']

    depths = [int(n) for n in acts]
    metric_names = ['density', 'diameter', 'average_clustering']
    column_names = ['title', 'title_pretty', 'genre', 'is_whole']
    for depth in depths:
        label = _acts_label(depth)
        for metric in metric_names:
            column_names.append(f'{label}_{metric}')

    with open(filename, 'w', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=column_names)
        writer.writeheader()

        for name, drama_data in cumulative_G_list.items():
            row = {
                'title': name,
                'title_pretty': drama_data['title_pretty'],
                'genre': drama_data['genre'],
                'is_whole': drama_data['is_whole'],
            }
            for depth in depths:
                label = _acts_label(depth)
                if label not in drama_data:
                    raise ValueError(f'{label} not found for {name}')
                G = drama_data[label]
                row[f'{label}_density'] = nx.density(G)
                row[f'{label}_diameter'] = nx.diameter(get_largest_G(G, name))
                row[f'{label}_average_clustering'] = nx.average_clustering(G)
            writer.writerow(row)

    logger.info('Cumulative metrics written to %s (%d plays, depths %s)',
                filename, len(cumulative_G_list), ', '.join(acts))



def build_fre_removed_networks(soups):
    """
    Build co-appearance networks for each play:
      - 'whole': all scenes across all acts
      - 'wo1'..'wo5': whole network with act N removed
    Returns a dict keyed by play name.
    """
    all_networks = {}

    # First calculate for whole network
    for name, (soup, _) in soups.items():
        edge_list = []
        lonely_nodes = []

        for act in _collect_acts(soup):
            edges, lonely, _ = _edge_list_extractor([act], name)
            edge_list.extend(edges)
            lonely_nodes.extend(lonely)

        whole = nx.from_edgelist(set(edge_list))
        for node in lonely_nodes:
            if node not in whole.nodes:
                whole.add_node(node)

        all_networks[name] = {
            'whole': whole,
            'title_pretty': soup.find('title').get_text(strip=True),
            'kept_characters': ', '.join(list(whole.nodes())),
            'character_count': len(whole.nodes()),
        }

    for name, (soup, _) in soups.items():
        for n in [1, 2, 3, 4, 5]:
            edge_list = []
            lonely_nodes = []

            for act_n, act in enumerate(_collect_acts(soup)):

                # skip 'nth' act - this is the heart of "removed" act analysis
                if act_n == n:
                    continue

                edges, lonely, _ = _edge_list_extractor([act], name)
                edge_list.extend(edges)
                lonely_nodes.extend(lonely)

            without_n = nx.from_edgelist(set(edge_list))
            for node in lonely_nodes:
                if node not in without_n.nodes:
                    without_n.add_node(node)

            all_networks[name]['wo' + str(n)] = without_n

    return all_networks