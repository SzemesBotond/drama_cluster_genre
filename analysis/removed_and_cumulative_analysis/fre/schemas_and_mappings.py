BLACKLIST = [
    'anonyme-vende',
    'moliere-divertissement-chambord',
    'dumas-don-juan',
    'moliere-princesse-d-elide',
]

ACT_TAGS = ['act', 'acte', 'ate', 'partie', 'tableau']
POSSIBLE_ACT_TAGS = ['critique', 'intermede']

# If True: only ACT_TAGS count toward the 5-act structure.
# If False: ACT_TAGS + POSSIBLE_ACT_TAGS both count.
STRICT_ACTS = True

SCENE_TAGS = ['scene', 'ecene', 'zcene', 'scne', 'type', 'ballet', 'marche', 'divertissement']

FREDRACOR_CORPORA = 'https://dracor.org/api/v1/corpora/fre'
FREDRACOR_METADATA_PATH = 'https://dracor.org/api/v1/corpora/fre/metadata/csv'

