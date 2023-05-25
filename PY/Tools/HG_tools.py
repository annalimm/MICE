import pyampute
from pyampute.ampute import MultivariateAmputation


def amp(df):
    if len(df.columns) == 3:
        wei = [0, 1, 0]
    else:
        wei = [0, 1, 0, 0]
    
    ma = MultivariateAmputation(
                patterns = [ {'incomplete_vars': [0, 2], 'weights': wei, 'mechanism': 'MAR', 'score_to_probability_func': 'sigmoid-right'} ],
                prop = 0.25
                )

    df_miss_MAR = ma.fit_transform(df)
    return df_miss_MAR

def amp_45(df):
    if len(df.columns) == 3:
        wei = [0, 1, 0]
    else:
        wei = [0, 1, 0, 0]
    
    ma = MultivariateAmputation(
                patterns = [ {'incomplete_vars': [0, 2], 'weights': wei, 'mechanism': 'MAR', 'score_to_probability_func': 'sigmoid-right'} ],
                prop = 0.45
                )

    df_miss_MAR = ma.fit_transform(df)
    return df_miss_MAR


def amp_MNAR(df):
    if len(df.columns) == 3:
        wei = [1, 0, 1]
    else:
        wei = [1, 0, 1, 0]
    
    ma = MultivariateAmputation(
                patterns = [ {'incomplete_vars': [0, 2], 'weights': wei, 'mechanism': 'MNAR', 'score_to_probability_func': 'sigmoid-right'} ],
                prop = 0.25
                )

    df_miss_MAR = ma.fit_transform(df)
    return df_miss_MAR