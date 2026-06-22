import csv
import logging

import requests


def load_genres_from_metadata(metadata_filepath, outputs_dir):
    """Return {play_name: genre} for Tragedy/Comedy plays with more than 5 speakers."""

    if metadata_filepath.startswith('http'):
        response = requests.get(
            metadata_filepath,
            headers={'accept': 'text/csv'}
        )
        metadata_filepath = outputs_dir / 'dracor-metadata.csv'
        with open(metadata_filepath, 'w', encoding='utf-8') as f:
            f.write(response.text)
        logging.info(f'METADATA: {metadata_filepath} has been downloaded from api.')

    genres = {}
    with open(metadata_filepath,  newline="", encoding="utf-8") as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            if row['normalizedGenre'] in ['Tragedy', 'Comedy'] and int(row['numOfSpeakers']) > 5:
                genres[row['name']] = row['normalizedGenre']
    return genres
