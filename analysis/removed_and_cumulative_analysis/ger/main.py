import sys
import logging
import argparse
from pathlib import Path
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from dracor_input_handling.input_handler import yield_corpora_tei
from dracor_input_handling.dracor_metadata import load_genres_from_metadata

from gerdracor_analysis import (
    load_gerdracor_soups,
    build_gerdracor_cumulative,
    build_ger_removed_networks,
    write_gerdracor_cumulative_csv,
    write_gerdracor_stats_csv,
)

from schemas_and_mappings import GERDRACOR_METADATA_PATH

from removed_and_cumulative_analysis.removed_act_analysis_utils import calculate_and_write_removed_csv

ALL_ACTS = ['1', '2', '3', '4', '5']


def cumulative_filename(acts):
    if acts == ALL_ACTS:
        suffix = 'all_acts'
    else:
        suffix = f'{acts[0]}-{acts[-1]}_acts'
    return f'gerdracor_cumulative_{suffix}.csv'


def setup_logging(output_dir=None):
    if output_dir:
        logs_dir = Path(output_dir)
    else:
        logs_dir = Path(__file__).parent / 'logs'
    logs_dir.mkdir(exist_ok=True)

    log_file = logs_dir / f"ger_{datetime.now().strftime('%Y-%m-%d_%H-%M')}.log"

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
    parser = argparse.ArgumentParser(description='GerDracor co-appearance network analysis')
    parser.add_argument(
        '--input',
        required=True,
        help='Path to GerDracor TEI files or URI of dracor corpora API',
    )
    parser.add_argument(
        '--cumulative_acts',
        default='1,2,3,4,5',
        help=(
            'Comma-separated cumulative depth steps: each number N computes a snapshot '
            'of the first N acts. E.g. "2,3,4,5" skips the single-act snapshot. '
            '(default: %(default)s)'
        ),
    )
    parser.add_argument(
        '--output_dir',
        help='Output directory (default: %(default)s)',
    )
    parser.add_argument(
        '--metadata',
        default=GERDRACOR_METADATA_PATH,
        help='Path to gerdracor metadata CSV (default: %(default)s)',
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

    logger.info('Starting GerDracor analysis')
    logger.info('Input: %s', args.input)
    logger.info('Output directory: %s', outputs_dir)
    logger.info('Log file: %s', log_file)
    dracor_tei_generator = yield_corpora_tei(args.input)
    soups = load_gerdracor_soups(dracor_tei_generator)
    logger.info('Loaded %d qualifying GerDracor dramas', len(soups))

    genres = load_genres_from_metadata(args.metadata, outputs_dir)


    # CUMULATIVE ANALYSIS
    logger.info('Cumulative depths: %s', ', '.join(acts))

    logger.info('Building cumulative networks')
    cumulative = build_gerdracor_cumulative(soups, acts=acts)
    write_gerdracor_cumulative_csv(
        cumulative, outputs_dir / cumulative_filename(acts), acts=acts
    )

    logger.info('Building whole and removed networks')
    ger_networks = build_ger_removed_networks(soups)
    calculate_and_write_removed_csv(ger_networks, genres, outputs_dir / 'gerdracor_removed.csv')

    logger.info('Writing structural stats')
    write_gerdracor_stats_csv(soups, outputs_dir / 'gerdracor_content_stats.csv')

    logger.info('Done')


if __name__ == '__main__':
    main()
