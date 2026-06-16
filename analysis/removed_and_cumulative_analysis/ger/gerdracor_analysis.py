import csv
import logging
from pathlib import Path
from collections import defaultdict
from itertools import combinations

import networkx as nx
from bs4 import BeautifulSoup

logger = logging.getLogger(__name__)


def iter_find_bs(tag, tag_names: list):
    for tag_name in tag_names:
        if isinstance(tag_name, str):
            tag = tag.find(tag_name)
        elif isinstance(tag_name, tuple):
            tag = tag.find(tag_name[0], tag_name[1])  # to handle e.g. find('div', {'type': 'scene'})
        if tag is None:
            return None
    return tag

def get_largest_G(input_G, name=None):
    """Extract the largest connected component of a networkx Graph."""
    if len(input_G.nodes) == 0:
        raise ValueError(f'ZERO NODE GRAPH PASSED TO get_largest_G - {input_G}: {name}')
    if nx.is_connected(input_G) is False:
        nodes_in_largest = max(nx.connected_components(input_G), key=len)
        nodes_to_remove = set(input_G.nodes) - nodes_in_largest
        G_copy = input_G.copy()
        G_copy.remove_nodes_from(nodes_to_remove)
        return G_copy
    else:
        return input_G


def one_appearance_unit_edge_list(unit, lonely_nodes):
    """Extract co-appearance edges between speakers in a single scene/unit (BS4 tag)."""
    characters = []

    for sp in unit.find_all('sp', {'who': True}):
        speakers = sp['who'].split(' ')
        for split_sp in speakers:
            characters.append(split_sp)

    edge_list = list(combinations(set(characters), 2))

    if len(characters) == 1:
        lonely_nodes.append(characters[0])

    return edge_list, lonely_nodes


def load_gerdracor_soups(dracor_tei_generator):
    """
    Parse GerDracor TEI XML files, keeping only dramas that have:
      - exactly 5 acts
      - more than 5 cast members
      - genre Tragedy or Comedy
    """
    soups = {}
    for xml_name, xml in dracor_tei_generator:

        # Convert xml text into BeautifulSoup object
        soup = BeautifulSoup(xml, 'lxml-xml')

        # Find acts
        acts = soup.find_all('div', {'type': 'act'})
        if len(acts) == 0:
            logger.warning('No acts found in %s', xml_name)
            continue

        # Find cast members
        list_person = iter_find_bs(soup, ['profileDesc', 'listPerson'])
        if list_person:
            cast_list = list_person.find_all('person')
        else:
            logger.warning('No cast members found in %s', xml_name)
            continue

        # Find genre
        genre_tag = iter_find_bs(soup, ('textClass', 'keywords', ('term', {'type': 'genreTitle'})))
        if genre_tag:
            genre = genre_tag.get_text(strip=True)
        else:
            logger.warning('No genre found in %s', xml_name)
            continue

        if len(acts) == 5 and len(cast_list) > 5 and genre in ['Tragedy', 'Comedy']:
            logger.info('Loaded %s (%s, %d cast members)', xml_name, genre, len(cast_list))
            soups[xml_name] = soup
        else:
            logger.debug(
                'Skipped %s — acts: %d, cast: %d, genre: %s',
                xml_name, len(acts), len(cast_list), genre,
            )

    return soups

def edge_list_extractor(list_of_soup_segments, name=None):
    """
    Extract co-appearance edge list from a list of act soup segments.

    Handles three sub-division types within each act: scene, location, configuration.
    Falls back to treating the whole act as a single unit when none are present.
    Scenes may be nested one level deep (scene-within-scene).
    """
    lonely_nodes = []
    edge_list = []

    for idx, act in enumerate(list_of_soup_segments, start=1):
        if act is None:
            if idx == 1 or idx == len(list_of_soup_segments):
                continue
            else:
                raise ValueError(f'ACT {idx} IS NONE IN {name}')

        configurations_in_act = act.find_all('div', {'type': 'configuration'})
        locations_in_act = act.find_all('div', {'type': 'location'})
        scenes_in_act = act.find_all('div', {'type': 'scene'})

        division_types_present = [
            len(configurations_in_act) > 0,
            len(locations_in_act) > 0,
            len(scenes_in_act) > 0,
        ]
        if sum(division_types_present) > 1:
            raise ValueError(f'UNACCOUNTED STRUCTURE OF DRAMA - {name} ! {division_types_present}')

        if len(scenes_in_act) > 0:
            for scene in scenes_in_act:
                if scene.find('div', {'type': 'scene'}) is not None:
                    # nested scenes — go one level deeper
                    for inner_scene in scene.find_all('div', {'type': 'scene'}):
                        if inner_scene.find('div', {'type': 'scene'}):
                            logger.warning('Scene within scene within scene in %s', name)
                        scene_edges, lonely_nodes = one_appearance_unit_edge_list(inner_scene, lonely_nodes)
                        edge_list += scene_edges
                else:
                    scene_edges, lonely_nodes = one_appearance_unit_edge_list(scene, lonely_nodes)
                    edge_list += scene_edges

        elif len(locations_in_act) > 0:
            for location in locations_in_act:
                loc_edges, lonely_nodes = one_appearance_unit_edge_list(location, lonely_nodes)
                edge_list += loc_edges

        elif len(configurations_in_act) > 0:
            for configuration in configurations_in_act:
                conf_edges, lonely_nodes = one_appearance_unit_edge_list(configuration, lonely_nodes)
                edge_list += conf_edges

        else:
            if act.find('div') is not None:
                raise ValueError(f'Unaccounted div type in {name} !')
            act_edges, lonely_nodes = one_appearance_unit_edge_list(act, lonely_nodes)
            edge_list += act_edges

    return edge_list, lonely_nodes


def _network_from_edges(edge_list, lonely_nodes):
    """Build a networkx graph from an edge list, adding isolated nodes."""
    G = nx.from_edgelist(set(edge_list))
    for node in lonely_nodes:
        if node not in G.nodes:
            G.add_node(node)
    return G


def _acts_label(depth):
    """Return the cumulative label for the first `depth` acts, e.g. 3 → 'acts_1-2-3'."""
    return f"acts_{'-'.join(str(i) for i in range(1, depth + 1))}"


def build_gerdracor_cumulative(soups, acts=None):
    """
    Build whole and cumulative co-appearance networks for each GerDracor play.

    acts is a list of depth strings specifying which cumulative snapshots to compute.
    Each value N means "include the first N acts". Defaults to ['1','2','3','4','5'].
    Because GerDracor acts carry no act-number attribute, snapshots always start
    from act 1 — acts controls how deep each snapshot goes, not which acts to select.
    """
    if acts is None:
        acts = ['1', '2', '3', '4', '5']

    depths = [int(n) for n in acts]
    cumulative_G_list = defaultdict(dict)

    for name, soup in soups.items():
        genre_tag = soup.find('textClass').find('keywords').find('term', {'type': 'genreTitle'})
        cumulative_G_list[name]['genre'] = genre_tag.get_text(strip=True)
        cumulative_G_list[name]['title_pretty'] = soup.find('title').get_text(strip=True)

        all_acts = soup.find_all('div', {'type': 'act'})

        edge_list, lonely_nodes = edge_list_extractor(all_acts, name)
        cumulative_G_list[name]['whole'] = _network_from_edges(edge_list, lonely_nodes)

        for depth in depths:
            acts_slice = all_acts[:depth]
            edge_list, lonely_nodes = edge_list_extractor(acts_slice, name)
            label = _acts_label(depth)
            cumulative_G_list[name][label] = _network_from_edges(edge_list, lonely_nodes)
            logger.info('%s  depth %d (%s): %d edges', name, depth, label, len(set(edge_list)))

    return cumulative_G_list


def write_gerdracor_cumulative_csv(cumulative_G_list, filename, acts=None):
    """Write cumulative network metrics to CSV for the requested depth snapshots."""
    if acts is None:
        acts = ['1', '2', '3', '4', '5']

    depths = [int(n) for n in acts]
    metric_names = ['density', 'diameter', 'average_clustering']
    column_names = ['title', 'title_pretty', 'genre']
    for depth in depths:
        label = _acts_label(depth)
        for metric in metric_names:
            column_names.append(f'{label}_{metric}')

    with open(filename, 'w', newline='', encoding="utf-8") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=column_names)
        writer.writeheader()

        for name, drama_data in cumulative_G_list.items():
            row = {
                'title': name,
                'title_pretty': drama_data['title_pretty'],
                'genre': drama_data['genre'],
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


def write_gerdracor_stats_csv(soups, filename):
    """Write structural stats (acts/scenes/configurations/locations presence) per play to CSV."""
    stats = []

    for name, soup in soups.items():
        has_acts = len(soup.find_all('div', {'type': 'act'})) > 0
        has_prologue = len(soup.find_all('div', {'type': 'prologue'})) > 0
        has_scenes = len(soup.find_all('div', {'type': 'scene'})) > 0
        has_configurations = len(soup.find_all('div', {'type': 'configuration'})) > 0
        has_locations = len(soup.find_all('div', {'type': 'location'})) > 0

        if not any([has_acts, has_scenes, has_configurations, has_locations]):
            raise ValueError(f'CONTENT HOLDER DIVS NOT ACCOUNTED FOR {name}')

        all_acts_have_scenes = False
        if has_acts and has_scenes:
            scene_counts = [
                len(act.find_all('div', {'type': 'scene'}))
                for act in soup.find_all('div', {'type': 'act'})
            ]
            all_acts_have_scenes = all(c > 0 for c in scene_counts)

        stats.append({
            'name': name,
            'prologue': has_prologue,
            'acts': has_acts,
            'scenes': has_scenes,
            'all_acts_have_scenes': all_acts_have_scenes,
            'configurations': has_configurations,
            'locations': has_locations,
        })

    fieldnames = ['name', 'prologue', 'acts', 'scenes', 'all_acts_have_scenes',
                  'configurations', 'locations']
    with open(filename, 'w', newline='', encoding="utf-8") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for row in stats:
            writer.writerow(row)

    logger.info('Structural stats written to %s (%d plays)', filename, len(soups))
