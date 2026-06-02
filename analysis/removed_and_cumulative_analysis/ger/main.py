import sys
import logging
import argparse
from pathlib import Path
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent.parent.parent))


from schemas_and_mappings import GERDRACOR_TEI_FILES
from gerdracor_analysis import (
    load_gerdracor_soups,
    build_gerdracor_cumulative,
    write_gerdracor_cumulative_csv,
    write_gerdracor_stats_csv,
)

ALL_ACTS = ['1', '2', '3', '4', '5']


def cumulative_filename(acts):
    if acts == ALL_ACTS:
        suffix = 'all_acts'
    else:
        suffix = f'{acts[0]}-{acts[-1]}_acts'
    return f'gerdracor_cumulative_{suffix}.csv'


def setup_logging():
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
        default=GERDRACOR_TEI_FILES,
        help='Path to GerDracor TEI files (default: %(default)s)',
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
    return parser.parse_args()


def main():
    log_file = setup_logging()
    logger = logging.getLogger(__name__)

    args = parse_args()
    acts = args.cumulative_acts.split(',')

    outputs_dir = Path(__file__).parent / 'outputs'
    outputs_dir.mkdir(exist_ok=True)

    logger.info('Starting GerDracor analysis')
    logger.info('Input: %s', args.input)
    logger.info('Cumulative depths: %s', ', '.join(acts))
    logger.info('Output directory: %s', outputs_dir)
    logger.info('Log file: %s', log_file)

    soups = load_gerdracor_soups(args.input)
    logger.info('Loaded %d qualifying GerDracor dramas', len(soups))

    logger.info('Building cumulative networks')
    cumulative = build_gerdracor_cumulative(soups, acts=acts)
    write_gerdracor_cumulative_csv(
        cumulative, outputs_dir / cumulative_filename(acts), acts=acts
    )

    logger.info('Writing structural stats')
    write_gerdracor_stats_csv(soups, outputs_dir / 'gerdracor_content_stats.csv')

    logger.info('Done')


if __name__ == '__main__':
    main()
