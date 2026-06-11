# removed_and_cumulative_analysis

Co-appearance network analysis across three DRaCor corpora. Each subproject is self-contained and runs with `python main.py [args]`.

> Requires Python 3.6+ and requirements.txt at root of repository to be installed into venv. 
### `--input` flag

All subprojects accept `--input` in three forms:

| Form                               | Example                                           |
|------------------------------------|---------------------------------------------------|
| Directory of XMLs                  | `--input /path/to/tei`                            |
| Text file of paths (one per line)  | `--input my_plays.txt`                            |
| DRaCor API corpus URL              | `--input https://dracor.org/api/v1/corpora/ger`   |

```
python main.py --input /data/gerdracor/tei
python main.py --input my_selection.txt
python main.py --input https://dracor.org/api/v1/corpora/shake
```

## Subprojects

### `shake/` — Shakespeare DRaCor
Builds cumulative co-appearance networks (first N acts) and removed-act networks for Shakespeare plays.

TEI input: `/home/misinagy/Projects/shakedracor/tei`

`--cumulative_acts` — comma-separated act numbers to include, e.g. `2,3,4,5` (default: `1,2,3,4,5`)

`--output_dir` — directory where all output files are written (default: `shake/outputs/`)

---

### `ger/` — German DRaCor
Builds cumulative co-appearance networks and structural stats for GerDracor plays (5-act, 5+ speakers, Comedy/Tragedy).

TEI input: `gerdracor/tei` (relative to working directory)

`--output_dir` — directory where all output files are written (default: `ger/outputs/`)

---

### `fre/` — French DRaCor
Builds cumulative co-appearance networks for FreDracor plays (5-act, 5+ speakers, Comedy/Tragedy).

`--input` — path or URL to TEI files (default: `/home/misinagy/Projects/fredracor/tei`)

`--metadata` — path to fredracor metadata CSV (default: `fredracor-metadata.csv`)

`--cumulative_acts` — comma-separated act numbers to include, e.g. `2,3,4,5` (default: `1,2,3,4,5`)

`--output_dir` — directory where all output files are written (default: `fre/outputs/`)

Settings (blacklist, act/scene tag mappings, strict mode) are in `schemas_and_mappings.py`.

---

## French corpus tag quirks

Non-standard `div` type attributes found in FreDracor and how they are handled:

**Treated as scenes:**
`ecene`, `zcene`, `scne`, `type`, `ballet`, `marche`, `divertissement`
(`vaudeville`, `epilogue`, `couplet`, `couplets` — not currently in `SCENE_TAGS`)

**Treated as acts:**
`ate`, `partie`, `tableau`, `critique`, `intermede`
(`prologue` — excluded from the 5-act count)

**Blacklisted dramas** (bad/atypical structure):
`anonyme-vende`, `moliere-divertissement-chambord`, `dumas-don-juan`, `moliere-princesse-d-elide`