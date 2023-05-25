import os
import rpy2.robjects as ro
from rpy2.robjects.conversion import localconverter
from rpy2.robjects import pandas2ri

import numpy as np
import pandas as pd

def ampute_R(df, prop = 0.25, mech = "MAR", type_="RIGHT"):
    
    df = pd.DataFrame(np.array(df)).iloc[:, :]
    
    df.columns = [f"Col{i}" for i in range(df.columns.shape[0])]
    with localconverter(ro.default_converter + pandas2ri.converter):
        df_r = ro.conversion.py2rpy(df)

    r = ro.r
    r['source']('/Users/ash/Desktop/MICE/Tools/mice.R')
    ampute_r = ro.globalenv['ampute_mice']
      
    df_result_r = ampute_r(df_r, prop = prop, mech = mech, type_=type_)

    with localconverter(ro.default_converter + pandas2ri.converter):
        pd_from_r_df = ro.conversion.rpy2py(df_result_r)
    return np.array(pd_from_r_df)



def mice_R(X_miss, maxit = 5, m = 5, seed = 1, meth = None):
    
    df = pd.DataFrame(np.array(X_miss)).iloc[:, :]
    
    df.columns = [f"Col{i}" for i in range(df.columns.shape[0])]
    with localconverter(ro.default_converter + pandas2ri.converter):
        df_r = ro.conversion.py2rpy(df)

    r = ro.r
    r['source']('/Users/ash/Desktop/MICE/Tools/mice.R')
    impute_mice_r = ro.globalenv['impute_mice']
    if meth is None:
        df_result_r = impute_mice_r(df_r, maxit=maxit, m=m, seed=seed)
    else:
        df_result_r = impute_mice_r(df_r, maxit=maxit, m=m, seed=seed, meth=meth)

    with localconverter(ro.default_converter + pandas2ri.converter):
        pd_from_r_df = ro.conversion.rpy2py(df_result_r)
    return np.array(pd_from_r_df)


def missForest_R(X_miss):
    
    df = pd.DataFrame(np.array(X_miss)).iloc[:, :]
    
    df.columns = [f"Col{i}" for i in range(df.columns.shape[0])]
    with localconverter(ro.default_converter + pandas2ri.converter):
        df_r = ro.conversion.py2rpy(df)

    r = ro.r
    r['source']('/Users/ash/Desktop/MICE/Tools/missForest.R')
    missFor_r = ro.globalenv['impute_missForest']
    df_result_r = missFor_r(df_r)

    with localconverter(ro.default_converter + pandas2ri.converter):
        pd_from_r_df = ro.conversion.rpy2py(df_result_r)
    return np.array(pd_from_r_df)