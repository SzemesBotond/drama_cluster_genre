import sys
import logging
import argparse
from pathlib import Path
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from dracor_input_handling.input_handler import yield_corpora_tei

from fredracor_analysis import (
    load_fredracor_metadata,
    load_fredracor_soups,
    build_fredracor_cumulative,
    write_fredracor_cumulative_csv,
)

from schemas_and_mappings import FREDRACOR_TEI_FILES, FREDRACOR_METADATA_PATH

ALL_ACTS = ['1', '2', '3', '4', '5']


def cumulative_filename(acts):
    if acts == ALL_ACTS:
        suffix = 'all_acts'
    else:
        suffix = f'{acts[0]}-{acts[-1]}_acts'
    return f'fredracor_cumulative_{suffix}.csv'


def setup_logging(output_dir=None):
    if output_dir:
        logs_dir = Path(output_dir)
    else:
        logs_dir = Path(__file__).parent / 'logs'
    logs_dir.mkdir(exist_ok=True)

    log_file = logs_dir / f"fre_{datetime.now().strftime('%Y-%m-%d_%H-%M')}.log"

    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s  %(levelname)s  %(message)s',
        datefmt='%Y-%m-%d %H:%M',
        handlers=[
            logging.FileHandler(log_file),
            logging.StreamHandler(),
        ],
    )
    return log_file


def parse_args():
    parser = argparse.ArgumentParser(description='FreDracor co-appearance network analysis')
    parser.add_argument(
        '--input',
        default=FREDRACOR_TEI_FILES,
        help='Path or URL to FreDracor TEI files (default: %(default)s)',
    )
    parser.add_argument(
        '--metadata',
        default=FREDRACOR_METADATA_PATH,
        help='Path to fredracor metadata CSV (default: %(default)s)',
    )
    parser.add_argument(
        '--cumulative_acts',
        default='1,2,3,4,5',
        help='Comma-separated act numbers for cumulative analysis, e.g. "2,3,4,5" (default: %(default)s)',
    )
    parser.add_argument(
        '--output_dir',
        help='Output directory (default: %(default)s)',
    )
    return parser.parse_args()


def main():
    args = parse_args()
    if args.output_dir:
        outputs_dir = Path(args.output_dir)
    else:
        outputs_dir = Path(__file__).parent / 'outputs'
    outputs_dir.mkdir(exist_ok=True)

    log_file = setup_logging(args.output_dir)
    logger = logging.getLogger(__name__)

    acts = args.cumulative_acts.split(',')

    logger.info('Starting FreDracor analysis')
    logger.info('Input: %s', args.input)
    logger.info('Metadata: %s', args.metadata)
    logger.info('Cumulative acts: %s', ', '.join(acts))
    logger.info('Output directory: %s', outputs_dir)
    logger.info('Log file: %s', log_file)

    genres = load_fredracor_metadata(args.metadata, outputs_dir)
    logger.info('Metadata loaded: %d qualifying plays', len(genres))

    dracor_tei_generator = yield_corpora_tei(args.input)
    soups = load_fredracor_soups(dracor_tei_generator, genres)
    logger.info('Loaded %d plays', len(soups))

    logger.info('Building cumulative networks')
    cumulative = build_fredracor_cumulative(soups, genres, acts=acts)
    write_fredracor_cumulative_csv(
        cumulative, outputs_dir / cumulative_filename(acts), acts=acts
    )

    logger.info('Done')


if __name__ == '__main__':
    main()