
# This file is the codebook for the dataset. It is used to convert the
# numeric values in the dataset to meaningful labels. It only includes the
# variables that were recoded to numeric labels. 

codebook = {
    'sample_source': {
        0: 'student',
        1: 'employee',
        2: 'student-employee',
        3: 'mixture'
    },
    'sample_method': {
        0: 'random',
        1: 'volunteer',
        2: 'snowball',
        3: 'conveniance'
    },
    'sample_platform': {
        -1: 'unspecified/NA/ambiguous',
        0: 'mturk',
        1: 'qualtrics',
        2: 'prolific',
        3: 'university platform',
        4: 'mixture',
        5: 'other'
    },
    'sample_mturk': {
        -1: 'unspecified/NA',
        0: 'master',
        1: 'non-master'
    },
    'sample_level': {
        -1: 'unspecified',
        0: 'undergraduate',
        1: 'graduate',
        2: 'non-managerial',
        3: 'managerial',
        4: 'mixture'
    },
    'sample_incentive': {
        -1: 'unspecified',
        0: 'none',
        1: 'course credit',
        2: 'financial',
        3: 'raffle',
        4: 'results/feedback',
        5: 'mixture'
    },
    'sample_country': {
        -1: 'unspecified',
        0: 'north america',
        1: 'international mixture',
        2: 'UK',
        3: 'Germany'
    },
    'cr_multiple': {
        0: 'no',
        1: 'yes'
    },
    'cr_sequential': {
        -1: 'NA',
        0: 'no',
        1: 'yes'
    },
    'cr_method': {
        -1: '',
        0: 'attention check items / bogus items / instructed response',
        1: 'completion pace / response time',
        2: 'avg longstring',
        3: 'max longstring',
        4: 'mahalanobis D',
        5: 'univariate outlier',
        6: 'SRSI use me',
        7: 'SRSI effort',
        8: 'SRSI attention',
        9: 'consistency',
        10: 'PsySyn',
        11: 'PsyAnt'
    },
    'cr_method_type': {
        'response_time': [1],
        'outlier_analysis': [4, 5],
        'bogus_items': [0],
        'consistency_indices': [9, 10, 11],
        'response_pattern': [2, 3],
        'self-reported': [6, 7, 8]
    },
    'design_time': {
        -1: '',
        0: 'cross-sectional',
        1: 'repeated measures'
    },
    'design_method': {
        -1: '',
        0: 'observational',
        1: 'experimental'
    },
    'design_location': {
        -1: '',
        0: 'online',
        1: 'in-person'
    }
}



