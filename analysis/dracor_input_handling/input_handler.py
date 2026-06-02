import logging
import requests
from pathlib import Path

def yield_corpora_tei(source: str):
    """This function gets a string as input and decides if its a domain or a file path, and
    starts yielding data as string accordingly.
    """
    if isinstance(source, str):

        if source.startswith("http"):
            logging.info(f"Getting input from {source}")
            if 'corpora' not in source:
                raise NotImplementedError("Currently only corpora are supported")
            corpora_metadata = requests.get(source).json()

            plays = corpora_metadata.get('plays', [])
            for play in plays:
                play_uri = play.get('uri')
                play_name = play.get('name')
                yield play_name, requests.get(f"{play_uri}/tei").text

        else:
            source_path = Path(source)
            if source_path.is_dir():  # presume directory contains xml files
                for xml_path in source_path.glob('*.xml'):
                    yield xml_path.stem, xml_path.read_text()
            elif source_path.is_file():  # presume file contains a list of xml file paths
                with open(source_path, 'r') as fh:
                    for line in fh:
                        xml_path = Path(line.strip())
                        yield xml_path.stem, xml_path.read_text()

            else:
                raise ValueError(f"Invalid source: {source}")

    else:
        raise ValueError(f"Invalid source: {source}")


if __name__ == "__main__":
    print(next(yield_corpora_tei("http://localhost:8088/api/v1/corpora/hun")))