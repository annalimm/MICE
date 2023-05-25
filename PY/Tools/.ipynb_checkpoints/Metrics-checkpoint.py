import pandas as pd
import numpy as np
import math
import statsmodels.api as sm

from Tools import MI

# def df_mean_col(imp_mice, col):
#     m_means = []
    
#     for imp in imp_mice:
        
#         imp_mean = pd.DataFrame(imp).loc[:, col].mean()
#         m_means.append(imp_mean)
        

#     fin_min = np.array(m_means).mean()

#     return fin_min



def metr(df_tr, X_miss, imputer_name, df_imp, metr_name, col2, scenar):
  
    metr_col2 = None

    if metr_name == 'rel_bias':

        df_X_mean = df_tr.loc[:, 'X'].mean()
        df_col2_mean = df_tr.loc[:, col2].mean()

        if imputer_name in ['mice_rf', 'mice_br']:
            df_imp = pd.DataFrame(np.array(df_imp).mean(axis = 0), columns = df_tr.columns)

        df_imp_X_mean = df_imp.loc[:, 'X'].mean()
        df_imp_col2_mean = df_imp.loc[:, col2].mean()

        metr = (df_imp_X_mean / df_X_mean - 1)
        metr_col2 = (df_imp_col2_mean / df_col2_mean - 1)


    elif metr_name == 'nrmse':
        
        if imputer_name in ['mice_rf', 'mice_br']:
            df_imp = pd.DataFrame(np.array(df_imp).mean(axis=0), columns = df_tr.columns)

        mis_pos = X_miss.isnull().any(axis = 1).index
        up = ( ( df_imp.iloc[mis_pos][['X', col2]] - df_tr.iloc[mis_pos][['X', col2]] ) ** 2 ).mean()
        down = np.var(df_tr.iloc[mis_pos][['X', col2]])
        dev = up / down
        
        metr = math.sqrt(dev['X'])
        metr_col2 = math.sqrt(dev[col2])
        

    elif metr_name == 'relb_coef':
        
        if scenar in ['lr_quadr', 'lr_inter']:
            
            coef_tr, var_tr = MI.coefs_vars(df_tr, 'lin_regr')
            beta_tr_X = coef_tr['X']
            beta_tr_col2 = coef_tr[col2]

            if imputer_name in ['mice_rf', 'mice_br']:
                mice_coef, mice_vars = MI.pooling(imp_mice = df_imp, regr_type = 'lin_regr')

                coefs_mean = pd.Series(mice_coef, index = df_tr.columns[df_tr.columns != 'Y'].insert(0, 'interc'))
                beta_imp_X = coefs_mean['X']
                beta_imp_col2 = coefs_mean[col2]
            else:
                coef_imp, var_imp = MI.coefs_vars(df_imp, 'lin_regr')
                beta_imp_X = coef_imp['X']
                beta_imp_col2 = coef_imp[col2]
                
                
        elif scenar in ['logr_quadr', 'logr_inter']:
            
            coef_tr, var_tr = MI.coefs_vars(df_tr, 'log_regr')
            beta_tr_X = coef_tr['X']
            beta_tr_col2 = coef_tr[col2]

            if imputer_name in ['mice_rf', 'mice_br']:
                mice_coef, mice_vars = MI.pooling(imp_mice = df_imp, regr_type = 'log_regr')

                coefs_mean = pd.Series(mice_coef, index = df_tr.columns[df_tr.columns != 'Y'].insert(0, 'interc'))
                beta_imp_X = coefs_mean['X']
                beta_imp_col2 = coefs_mean[col2]
            else:
                coef_imp, var_imp = MI.coefs_vars(df_imp, 'log_regr')
                beta_imp_X = coef_imp['X']
                beta_imp_col2 = coef_imp[col2]
        

        metr = (beta_imp_X - beta_tr_X) / beta_tr_X 
        metr_col2 = (beta_imp_col2 - beta_tr_col2) / beta_tr_col2


    return(metr, metr_col2)
