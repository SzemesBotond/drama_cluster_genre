import csv
from pathlib import Path
from itertools import combinations
from collections import defaultdict

import networkx as nx
from bs4 import BeautifulSoup


# SETTINGS

# Change these variables if needed!
# List of dramas to exclude (bad/different structure)
BLACKLIST = ['anonyme-vende']

# Tags to consider as acts
ACT_TAGS = ['act', 'acte', 'ate']  # These are for sure act tags

# The tags in here will be considered as extra "act" tags
POSSIBLE_ACT_TAGS = ['partie', 'critique', 'tableau', 'intermede']

# If True: Only tags that are in ACT_TAGS are considered as part of the 5-act structure. If False: Both ACT_TAGS
# and POSSIBLE_ACT_TAGS are considered part of the 5-act structure. "prologue" tags are not considered part of the 5
# act structure. i.e: If STRICT is False, there are 4 act tags, one prologue tag and one "tableau" tag, it will be a
# 5-act structure. If STRICT is True, then it will be a 4 act structure as "tableau" is filtered out.
STRICT_ACTS = True

# Tags to consider as scenes
SCENE_TAGS = ['scene', 'ecene', 'zcene', 'scne', 'type', 'vaudeville', 'ballet', 'divertissement', 'epilogue',
              'couplet', 'couplets', 'marche']

# Metadata from dracor
METADATA_FILENAME = 'fredracor-metadata.csv'

# Where dracor XMLs are located
XML_DIRECTORY = '/home/misinagy/Projects/fredracor/tei'

# Filepath where results should be written
OUTPUT_CSV_NAME = 'fredracor_cumulative.csv'


def read_genres(metadata_filepath):
    comedies_and_tragedies = {}
    with open(metadata_filepath) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            if row['normalizedGenre'] in ['Tragedy', 'Comedy']:
                comedies_and_tragedies[row['name']] = row['normalizedGenre']
    return comedies_and_tragedies


def one_appearance_unit_edge_list(unit, lonely_nodes):
    """
    Take unit of BS4 tag and extract all connections between speakers in unit.
    """
    characters = []

    for sp in unit.find_all('sp', {'who': True}):
        # get each speaker from speaker tag
        speakers = sp['who'].split(' ')

        for split_sp in speakers:
            characters.append(split_sp)
    edge_list = list(combinations(set(characters), 2))
    # if only one character in scene, add lonely node
    if len(characters) == 1:
        lonely_nodes.append(characters[0])

    return edge_list, lonely_nodes


def get_largest_G(input_G, name=None, strict=True):
    """
    Function to extract largest connected section of nx Graph object.
    """
    if len(input_G.nodes) == 0:
        if strict:
            raise ValueError(f'ZERO NODE GRAPH PASSED TO get_largest_G - {input_G}: {name}')
        else:
            return input_G

    if nx.is_connected(input_G) is False:
        # larges connected section
        nodes_in_largest = max(nx.connected_components(input_G), key=len)
        nodes_to_remove = set(input_G.nodes) - nodes_in_largest
        G_copy = input_G.copy()
        G_copy.remove_nodes_from(nodes_to_remove)
        return G_copy
    else:
        return input_G


def div_type_tag_count(s: BeautifulSoup, tag_names: list):
    total = 0
    for tag_name in tag_names:
        total += len(s.find_all('div', {'type': tag_name}))
    return total


def soup_creation(fredracor_tei_files, comedies_and_tragedies: dict):
    # open XML files of tragedy and comedy dramas, and parse soups where 'act' div is 5+ and there are
    # 5+ characters.

    print(f'Reading XML files from {fredracor_tei_files}')
    fre_soups = {}
    for xml_path in Path(fredracor_tei_files).glob('*.xml'):

        # Genres handled according to dracor metadata csv for consistency
        if xml_path.stem in comedies_and_tragedies and xml_path.stem not in BLACKLIST:
            with open(xml_path, 'r') as fh:
                # Filter 5 act, 5+ actor, Tragedy and Comedy dramas from GerDracor
                soup = BeautifulSoup(fh.read(), 'lxml-xml')

                # This is the number of acts that are named as <div type="act">
                acts_len = div_type_tag_count(soup, ACT_TAGS)

                # This is the number of acts that are named as <div type="act"> + number of possible act tags
                acts_and_possible_acts_len = div_type_tag_count(soup, ACT_TAGS + POSSIBLE_ACT_TAGS)

                cast_list = soup.find('profileDesc').find('listPerson').find_all('person')
                if cast_list is None:
                    raise ValueError('No cast list found!')

                if STRICT_ACTS:
                    act_condition = acts_len == 5
                else:
                    act_condition = acts_and_possible_acts_len == 5

                if act_condition and len(cast_list) > 5:
                    fre_soups[xml_path.stem] = soup

    print(f'{len(fre_soups)} collected')
    return fre_soups


# CREATE THE MAIN EXTRACTOR FUNCTION
# take first n acts from drama
def edge_list_extractor(list_of_soup_segments, name):
    """
    Takes list of soup elements, returns edge list for shared scenes, and if no scenes, just shared acts.
    This is GerDracor specific, and should only be used for the 128 dramas --> len(acts) == 5 and len(cast_list) >
    5 and genre in ['Tragedy', 'Comedy']
    """

    lonely_nodes = []
    edge_list_in_iteration = []

    # iterate over n acts
    for c, act in enumerate(list_of_soup_segments, start=1):
        if act is None:
            if c == 1 or c == len(list_of_soup_segments):
                continue
            else:
                raise ValueError(f'ACT {c} IS NONE IN {name}')

        all_scene_type_tags = []
        for scene_type_tag in SCENE_TAGS:
            scene_type_tags = act.find_all('div', {'type': scene_type_tag})
            if len(scene_type_tags) > 0:
                all_scene_type_tags.extend(scene_type_tags)

        # IF IT HAS SCENES
        if len(all_scene_type_tags) > 0:
            for scene_tag in all_scene_type_tags:
                if scene_tag.find('div', {'type': 'scene'}) is not None:
                    print(f'Scene tag has scene tags')
                else:
                    scene_edge_list, lonely_nodes = one_appearance_unit_edge_list(scene_tag, lonely_nodes)
                    edge_list_in_iteration += scene_edge_list

        else:
            if any(act.find(elem) is not None for elem in ACT_TAGS):
                raise ValueError(f'Unaccounted div type in {name} !')
            act_edge_list, lonely_nodes = one_appearance_unit_edge_list(act, lonely_nodes)
            edge_list_in_iteration += act_edge_list

    return edge_list_in_iteration, lonely_nodes


def network_creation(fre_soups: dict, comedies_and_tragedies: dict):
    """From dramas that meet the predefined rules + 5 act/5+ actor rule create whole actor networks and networks from
    cumulative act numbers."""

    cumulative_G_list_fre = defaultdict(dict)

    print('Creating whole and cumulative networks')
    for name, soup in fre_soups.items():

        # Metadata annotation for dict
        cumulative_G_list_fre[name]['genre'] = comedies_and_tragedies[name]
        cumulative_G_list_fre[name]['title_pretty'] = soup.find('title').get_text(strip=True)

        # These are used to determine what structural elements there are.
        act_tags = ACT_TAGS
        if not STRICT_ACTS:
            act_tags = ACT_TAGS + POSSIBLE_ACT_TAGS

        all_acts = []
        for act_tag_type in act_tags:
            all_acts.extend(soup.find_all('div', {'type': act_tag_type}))

        # Prologue
        prologue = soup.body.find_all('div', type="prologue")
        if len(prologue) > 0:
            all_acts = prologue + all_acts

        edge_list, lonely_nodes_main = edge_list_extractor(all_acts, name)
        # create network from edge list
        whole_drama = nx.from_edgelist(set(edge_list))

        # add those with independent scenes
        for lonely_node in lonely_nodes_main:
            if lonely_node not in whole_drama.nodes:
                whole_drama.add_node(lonely_node)

        cumulative_G_list_fre[name]['whole'] = whole_drama

        # Cumulative calculation
        print(f'Processing {name}')
        # create iterations for 1, 1-2, 1-2-3, ... acts
        if len(prologue) > 0:
            edge_list_in_iteration_out, lonely_nodes_main = edge_list_extractor(all_acts[:1], name)
            n_acts_whole = nx.from_edgelist(set(edge_list_in_iteration_out))

            # add those with independent scenes
            for lonely_node in lonely_nodes_main:
                if lonely_node not in n_acts_whole.nodes:
                    n_acts_whole.add_node(lonely_node)

            cumulative_G_list_fre[name]['prologue'] = n_acts_whole

            get_cumulative_snippets(name, 2, cumulative_G_list_fre, 'prologue_', all_acts)


        get_cumulative_snippets(name, 1, cumulative_G_list_fre, '', all_acts)
    return cumulative_G_list_fre


def get_cumulative_snippets(name: str, start: int, networks_collector: dict, pro_label: str, all_acts: list):
    """This function creates the networks which include a certain number of acts, starting from the first unit to the
    last."""
    current_iteration_rounds = []
    for iteration_round in range(start, len(all_acts) + 1):  # was all_acts
        # take first n acts from drama
        acts_included = all_acts[:iteration_round]  # was all_acts

        if start == 2:
            current_iteration_rounds.append(iteration_round - 1)
        else:
            current_iteration_rounds.append(iteration_round)

        edgelist, lonely = edge_list_extractor(acts_included, name)

        # create network from edge list
        n_acts_w = nx.from_edgelist(set(edgelist))

        # add those with independent scenes
        for l_node in lonely:
            if l_node not in n_acts_w.nodes:
                n_acts_w.add_node(l_node)

        label = f"{pro_label}acts_{'-'.join([str(i) for i in current_iteration_rounds])}"

        networks_collector[name][label] = n_acts_w


def calc_metrics_and_write_csvs(cumulative_G_list_fre: dict, csv_filename: str):
    print(f'Writing outputs to {csv_filename}')
    # Write csvs
    column_names = ['title', 'title_pretty', 'genre']
    metric_names = ['density', 'diameter', 'average_clustering']

    for act in range(1, 5 + 1):
        for n in metric_names:
            acts_string = f"acts_{'-'.join([str(n) for n in range(1, act + 1)])}_{n}"
            column_names.append(acts_string)  # for normal results
            column_names.append(f"prologue_{acts_string}")  # for results with prologue
    for n in metric_names:
        column_names.append(f"prologue_{n}")

    with open(csv_filename, 'w', newline='') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=column_names)
        writer.writeheader()

        for name, drama_data in cumulative_G_list_fre.items():
            write_row_dict = {'title': name, 'title_pretty': drama_data['title_pretty'], 'genre': drama_data['genre']}

            density, diameter, average_clustering = None, None, None
            if drama_data.get('prologue') is not None:
                if len(drama_data['prologue'].nodes()) > 0:
                    density = nx.density(drama_data['prologue'])
                    diameter = nx.diameter(get_largest_G(drama_data['prologue']))
                    average_clustering = nx.average_clustering(drama_data['prologue'])

            write_row_dict[f"prologue_density"] = density
            write_row_dict[f'prologue_diameter'] = diameter
            write_row_dict[f'prologue_average_clustering'] = average_clustering

            for act in range(1, 5 + 1):
                acts_name = f"acts_{'-'.join([str(n) for n in range(1, act + 1)])}"
                if acts_name in drama_data.keys():
                    write_row_dict[f"{acts_name}_density"] = nx.density(drama_data[acts_name])
                    write_row_dict[f'{acts_name}_diameter'] = nx.diameter(get_largest_G(drama_data[acts_name], name))
                    write_row_dict[f'{acts_name}_average_clustering'] = nx.average_clustering(drama_data[acts_name])
                else:
                    raise ValueError(f'{acts_name} not in {name}')

                prologue_acts_name = f'prologue_{acts_name}'
                if prologue_acts_name in drama_data.keys():
                    write_row_dict[f"{prologue_acts_name}_density"] = nx.density(drama_data[prologue_acts_name])
                    write_row_dict[f'{prologue_acts_name}_diameter'] = nx.diameter(
                        get_largest_G(drama_data[prologue_acts_name], name))
                    write_row_dict[f'{prologue_acts_name}_average_clustering'] = nx.average_clustering(
                        drama_data[prologue_acts_name])

            writer.writerow(write_row_dict)


def main():
    """
    To change running parameters, look at top of script for global variables.
    """
    comedies_and_tragedies = read_genres(METADATA_FILENAME)  # Set this to the dracor metadata CSV file
    fredracor_tei_files = XML_DIRECTORY  # Set this to the location of the XML files
    fre_soups = soup_creation(fredracor_tei_files, comedies_and_tragedies)
    cumulative_G_list_fre = network_creation(fre_soups, comedies_and_tragedies)
    calc_metrics_and_write_csvs(cumulative_G_list_fre, OUTPUT_CSV_NAME)  # change output filename here


if __name__ == '__main__':
    main()