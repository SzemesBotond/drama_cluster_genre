import csv

import networkx as nx


def calculate_and_write_removed_csv(networks_data, genres_dict, filename):
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

        for name, graph_dict in networks_data.items():
            row = {
                'title': name,
                'title_pretty': graph_dict['title_pretty'],
                'genre': genres_dict[name], # SHAKESPEAR_GENRES[name],
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