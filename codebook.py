
# This file is the codebook for the dataset. It is used to convert the
# numeric values in the dataset to meaningful labels. It only includes the
# variables that were recoded to numeric labels. 

#TODO: add new coding scheme for cr method type

codebook = { 
    'journal_code': {
        'Journal of Applied Psychology': 0,
        'Personnel Psychology': 1,
        'Academy of Management Journal': 2,
        'Organizational Research Methods': 3,
        'Journal of Management': 4,
        'Organizational Behavior and Human Decision Processes': 5,
        'Journal of Organizational Behavior': 6,
        'The Leadership Quarterly': 7,
        'Journal of Occupational Health Psychology': 8,
        'Journal of Vocational Behavior': 9,
        'Journal of Business and Psychology': 10,
        'Journal of Occupational and Organizational Psychology': 11,
        'Educational and Psychological Measurement': 12,
        'Journal of Applied Social Psychology': 13,
        'Academy of Management Learning and Education': 14,
        'International Journal of Selection and Assessment': 15,
        'European Journal of Work and Organizational Psychology': 16,
        'Human Performance': 17,
        'Human Relations': 18,
        'Personality and Individual Differences': 19,
        'Applied Psychology: An International Review': 20,
        'Work & Stress': 21,
        'Group and Organization Management': 22,
        'Journal of Personnel Psychology': 23,
        'Journal of Managerial Psychology': 24,
        'Dissertations': 25
    },
    'journal': {
        0: 'Journal of Applied Psychology',
        1: 'Personnel Psychology',
        2: 'Academy of Management Journal',
        3: 'Organizational Research Methods',
        4: 'Journal of Management',
        5: 'Organizational Behavior and Human Decision Processes',
        6: 'Journal of Organizational Behavior',
        7: 'The Leadership Quarterly',
        8: 'Journal of Occupational Health Psychology',
        9: 'Journal of Vocational Behavior',
        10: 'Journal of Business and Psychology',
        11: 'Journal of Occupational and Organizational Psychology',
        12: 'Educational and Psychological Measurement',
        13: 'Journal of Applied Social Psychology',
        14: 'Academy of Management Learning and Education',
        15: 'International Journal of Selection and Assessment',
        16: 'European Journal of Work and Organizational Psychology',
        17: 'Human Performance',
        18: 'Human Relations',
        19: 'Personality and Individual Differences',
        20: 'Applied Psychology: An International Review',
        21: 'Work & Stress',
        22: 'Group and Organization Management',
        23: 'Journal of Personnel Psychology',
        24: 'Journal of Managerial Psychology',
        25: 'Dissertations'
    },
    'sample_source': {
        -1: 'unspecified',
        0: 'student',
        1: 'employee',
        2: 'student-employee',
        3: 'mixture'
    },
    'sample_recruitment': {
        -1: 'unspecified',
        0: 'university platform',
        1: 'crowdsource platform',
        2: 'workplace',
        3: 'social media',
        4: 'listserv',
        5: 'mixture',
        6: 'other'
    },
    'sample_method': {
        -1: 'unspecified',
        0: 'random',
        1: 'volunteer',
        2: 'snowball',
        3: 'conveniance'
    },
    'sample_platform': {
        -1: 'unspecified',
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
        'self_reported': [6, 7, 8]
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



