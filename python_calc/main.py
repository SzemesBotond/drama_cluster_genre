import csv
import networkx as nx
from pathlib import Path
from bs4 import BeautifulSoup
from itertools import combinations
from shakespear_extra import SHAKESPEAR_GENRES

# ShakeDracor - wo1 wo2 wo3 wo4 wo5
# GerDracor - same, just 5 act dramas
# density, diameter, average_clustering,


def process_act_list(act_list):

    scenes_list = []
    for act in act_list:
        scenes = act.find_all('div', {'type': 'scene'})
        if len(scenes) > 0:
            scenes_list += scenes  # here scenes are a list
        elif len(scenes) == 0 and act.find('sp') is not None:
            scenes_list.append(act)  # here act is a single obj

    scene_edges = generate_scene_edges(scenes_list)
    if len(scene_edges) > 0:
        return scene_edges
    else:
        raise ValueError(f'Input soup resulted in zero scene edges from {len(act_list)} length act list!')


def generate_scene_edges(scenes):
    scene_edges = []
    for scene in scenes:
        speakers = scene.find_all('sp', {'who': True})
        unique_characters_in_scene = set([s['who'] for s in speakers])
        for edge in combinations(unique_characters_in_scene, 2):
            scene_edges.append((edge[0], edge[1]))

    if len(scene_edges) > 0:
        return scene_edges
    else:
        raise ValueError(f'Input soup resulted in zero scene edges from {len(scenes)} scenes!')


def process_whole_drama(input_soup):
    scenes = input_soup.find_all('div', {'type': 'scene'})
    scene_edges = generate_scene_edges(scenes)

    if len(scene_edges) > 0:
        return scene_edges
    else:
        raise ValueError('Input soup resulted in zero scene edges!')


def wo_drama_act_list_generator(input_soup):
    acts = input_soup.find_all('div', {'type': 'act'})
    for i in range(1, len(acts)+1):
        yield 'wo'+str(i), acts[0:i-1]+acts[i:]


def generate_withouts_edges(whole_soup):
    withouts_dict = {}
    for wo_name, act_list in wo_drama_act_list_generator(whole_soup):
        graph_edges = process_act_list(act_list)
        withouts_dict[wo_name] = graph_edges
    return withouts_dict


def withouts_edges_to_graphs(withouts_dict):
    for wo_name, edge_list in withouts_dict.items():
        withouts_dict.update({wo_name: nx.from_edgelist(edge_list)})
    return withouts_dict


def get_whole_graph(input_soup):
    act_list = input_soup.find_all('div', {'type': 'act'})
    graph_edges = process_act_list(act_list)
    return {'whole': nx.from_edgelist(graph_edges)}


def calculate_metrics(name_graph_dict):
    metrics_dict = {}
    for name, G in name_graph_dict.items():

        metrics_dict.update({
            f'{name} density': nx.density(G),
            f'{name} average clustering': nx.average_clustering(G)
        })
        if nx.is_connected(G):
            metrics_dict[f'{name} diameter'] = nx.diameter(G)
        else:
            metrics_dict[f'{name} diameter'] = 0  # TODO connected components
    return metrics_dict


def main_proc(input_soup):
    edges_dict = generate_withouts_edges(input_soup)
    graphs_dict = withouts_edges_to_graphs(edges_dict)
    graphs_dict.update(get_whole_graph(input_soup))  # get whole graph as well
    return calculate_metrics(graphs_dict)


if __name__ == '__main__':

    tei_files_dir = 'shakedracor/tei'
    results_csv_file_name = 'shakedracor_metrics.csv'

    names = []

    name_bases = ['whole'] + ['wo' + str(i) for i in range(1, 6)]
    for name in name_bases:
        for metric in [' density', ' diameter', ' average clustering']:
            names.append(name + metric)

    with open(results_csv_file_name, 'w') as csv_fh:
        columns = ['drama_name', 'genre'] + names

        writer = csv.DictWriter(csv_fh, fieldnames=columns)
        writer.writeheader()
        num_files = len(list(Path(tei_files_dir).glob('*.xml')))
        for num, file in enumerate(Path(tei_files_dir).glob('*.xml')):
            if num % 10 == 0:
                print(f'{round(num/num_files*100, 2)}%')

            with open(file, 'r') as in_xml:

                soup = BeautifulSoup(in_xml.read(), 'lxml-xml')

                title = soup.find('titleStmt').find('title', {'type': 'main'})
                if title is None:
                    title = soup.find('titleStmt').find('title')
                title_text = title.get_text(strip=True)
                sub_title = soup.find('titleStmt').find('title', {'type': 'sub'})
                if sub_title is not None:
                    title_text += ' - ' + sub_title.get_text(strip=True)

                # genre_tag = soup.find('textClass').find('keywords').find('term', {'type': 'genreTitle'})
                #genre = genre_tag.get_text(strip=True)
                genre = SHAKESPEAR_GENRES[file.stem]
                cast_list = soup.find('profileDesc').find('listPerson').find_all('person')

                if cast_list is None:
                    raise ValueError('No cast list found!')
                cast_num = len(cast_list)

                act = soup.find_all('div', {'type': 'act'})
                if act:
                    if len(act) == 5 and cast_num > 5 and genre.lower() in ['tragedy', 'comedy']:

                        drama_dict = {'drama_name': title_text, 'genre': genre}
                        drama_dict.update(main_proc(soup))

                        writer.writerow(drama_dict)
