import csv
import logging
from pathlib import Path
from collections import defaultdict
from itertools import combinations

import networkx as nx
from bs4 import BeautifulSoup

from schemas_and_mappings import SHAKESPEAR_GENRES

logger = logging.getLogger(__name__)


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


def load_shakespeare_soups(shake_tei_generator):
    """Parse all Shakespeare TEI XML files in tei_dir into BeautifulSoup objects."""
    soups = {}
    logger.info('Loading SHAKE soups')
    for xml_name, xml in shake_tei_generator:
        soups[xml_name] = BeautifulSoup(xml, 'lxml-xml')
    return soups


def _scene_edge_list(scene):
    """Return co-appearance edges and any lone speaker for a single scene tag."""
    scene_characters = []
    for sp in scene.find_all('sp', {'who': True}):
        for speaker in sp['who'].split(' '):
            scene_characters.append(speaker)
    edges = list(combinations(set(scene_characters), 2))
    lonely = scene_characters[0] if len(scene_characters) == 1 else None
    return edges, lonely


def build_shakespeare_removed_networks(soups):
    """
    Build co-appearance networks for each play:
      - 'whole': all scenes across all acts
      - 'wo1'..'wo5': whole network with act N removed
    Returns a dict keyed by play name.
    """
    shake_networks = {}

    for name, soup in soups.items():
        edge_list = []
        lonely_nodes = []

        for scene in soup.find_all('div', {'type': 'scene'}):
            edges, lonely = _scene_edge_list(scene)
            edge_list += edges
            if lonely:
                lonely_nodes.append(lonely)

        whole = nx.from_edgelist(set(edge_list))
        for node in lonely_nodes:
            if node not in whole.nodes:
                whole.add_node(node)

        shake_networks[name] = {
            'whole': whole,
            'title_pretty': soup.find('title').get_text(strip=True),
            'kept_characters': ', '.join(list(whole.nodes())),
            'character_count': len(whole.nodes()),
        }

    for name, soup in soups.items():
        for n in [1, 2, 3, 4, 5]:
            edge_list = []
            lonely_nodes = []

            for act in soup.find_all('div', {'type': 'act'}):
                if act['n'] == str(n):
                    continue
                for scene in act.find_all('div', {'type': 'scene'}):
                    edges, lonely = _scene_edge_list(scene)
                    edge_list += edges
                    if lonely:
                        lonely_nodes.append(lonely)

            without_n = nx.from_edgelist(set(edge_list))
            for node in lonely_nodes:
                if node not in without_n.nodes:
                    without_n.add_node(node)

            shake_networks[name]['wo' + str(n)] = without_n

    return shake_networks


def build_shakespeare_cumulative(soups, acts=None):
    """
    Build cumulative co-appearance networks for the given act sequence.
    acts is a list of act-number strings, e.g. ['1','2','3','4','5'] or ['2','3','4','5'].
    Each snapshot includes all acts up to and including the current one.
    Returns a dict keyed by play name.
    """
    if acts is None:
        acts = ['1', '2', '3', '4', '5']

    cumulative_G_list = defaultdict(dict)

    for name, soup in soups.items():
        acts_included = []

        for n in acts:
            acts_included.append(n)
            edge_list = []
            lonely_nodes = []

            for act in soup.find_all('div', {'type': 'act'}):
                if act['n'] not in acts_included:
                    continue
                for scene in act.find_all('div', {'type': 'scene'}):
                    edges, lonely = _scene_edge_list(scene)
                    edge_list += edges
                    if lonely:
                        lonely_nodes.append(lonely)

            network = nx.from_edgelist(set(edge_list))
            for node in lonely_nodes:
                if node not in network.nodes:
                    network.add_node(node)

            label = f"acts_{'-'.join(acts_included)}"
            cumulative_G_list[name]['title_pretty'] = soup.find('title').get_text(strip=True)
            cumulative_G_list[name][label] = network

    return cumulative_G_list


def write_shakespeare_removed_csv(G_list, filename):
    """
    Write per-play network metrics (whole + wo1..wo5) to CSV.
    Columns: density, diameter, average_clustering for each variant.
    """
    metric_names = ['density', 'diameter', 'average_clustering']
    column_names = ['title', 'title_pretty', 'genre', 'kept_characters', 'character_count',
                    'removed_characters', 'removed_characters_count']
    for variant in ['whole', 'wo1', 'wo2', 'wo3', 'wo4', 'wo5']:
        for metric in metric_names:
            column_names.append(f'{variant}_{metric}')

    with open(filename, 'w', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=column_names)
        writer.writeheader()

        for name, graph_dict in G_list.items():
            row = {
                'title': name,
                'title_pretty': graph_dict['title_pretty'],
                'genre': SHAKESPEAR_GENRES[name],
                'kept_characters': graph_dict['kept_characters'],
                'character_count': graph_dict['character_count'],
                'whole_density': nx.density(graph_dict['whole']),
                'whole_diameter': nx.diameter(get_largest_G(graph_dict['whole'])),
                'whole_average_clustering': nx.average_clustering(graph_dict['whole']),
            }

            for variant in ['wo1', 'wo2', 'wo3', 'wo4', 'wo5']:
                G = graph_dict[variant]
                row[f'{variant}_density'] = nx.density(G)
                row[f'{variant}_diameter'] = nx.diameter(get_largest_G(G))
                row[f'{variant}_average_clustering'] = nx.average_clustering(G)

            writer.writerow(row)

    logger.info('Removed-act metrics written to %s (%d plays)', filename, len(G_list))


def write_shakespeare_cumulative_csv(cumulative_G_list, filename, acts=None):
    """Write cumulative network metrics to CSV for the given act sequence."""
    if acts is None:
        acts = ['1', '2', '3', '4', '5']

    # build variant labels: acts_1, acts_1-2, acts_1-2-3, ...
    variants = [f"acts_{'-'.join(acts[:i])}" for i in range(1, len(acts) + 1)]

    metric_names = ['density', 'diameter', 'average_clustering']
    column_names = ['title', 'title_pretty', 'genre']
    for variant in variants:
        for metric in metric_names:
            column_names.append(f'{variant}_{metric}')

    with open(filename, 'w', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=column_names)
        writer.writeheader()

        for name, drama_data in cumulative_G_list.items():
            row = {
                'title': name,
                'title_pretty': drama_data['title_pretty'],
                'genre': SHAKESPEAR_GENRES[name],
            }
            for variant in variants:
                G = drama_data[variant]
                row[f'{variant}_density'] = nx.density(G)
                row[f'{variant}_diameter'] = nx.diameter(get_largest_G(G))
                row[f'{variant}_average_clustering'] = nx.average_clustering(G)

            writer.writerow(row)
