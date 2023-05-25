import numpy as np
import pandas as pd

from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer 

from sklearn.linear_model import BayesianRidge
import skopt
from skopt.learning import RandomForestRegressor

import statsmodels.api as sm




def coefs_vars(df, regr_type):
    
    predictor = sm.add_constant(df.loc[:, df.columns != 'Y'])
    
    if regr_type == 'lin_regr':
        regr = sm.OLS(df[['Y']], predictor).fit()
    elif regr_type == 'log_regr':
        # regr = sm.Logit(df[['Y']], predictor).fit()
        regr = sm.GLM(df[['Y']], predictor, family = sm.families.Binomial()).fit()
        
#     beta_X = ols_imp.params['X']
#     beta_col2 = ols_imp.params[col2]

    coefss = regr.params
    ses = regr.bse #this is SE!
    
    return coefss, ses


def pooling(imp_mice, regr_type):
    m_coefs = []
    m_ses = []
    
    for imp in imp_mice:
        
        n_row = imp.shape[0]
        mice_coefs, mice_ses = coefs_vars(pd.DataFrame(imp), regr_type)
        #mice_vars = mice_ses * mice_ses * n_row

        m_coefs.append(mice_coefs)
        m_ses.append(mice_ses)

    m_coefs = np.array(m_coefs)
    m_ses = np.array(m_ses)
    
    
    mice_means = rubins_pooling_rules(m_coefs, m_ses)[0]
    mice_ses = rubins_pooling_rules(m_coefs, m_ses)[1]
#     mice_conf_int = 2 * mice_varis / np.sqrt(imp_mice.shape[0])
    #mice_ses = np.sqrt(mice_varis / n_row)
    
    return mice_means, mice_ses


def rubins_pooling_rules(m_coefs, m_ses):
    """Applies Rubin's pooling rules.
    The final weights is defined as the mean of the weights across the imputed
    datasets while the total variance is defined as the combination of the mean
    of the variance of the weights and the variance of the coefficients.
    Parameters
    ----------
    m_coefs : ndarray, shape (n_imputations, n_features)
        The weights of the model fitted on each imputed dataset.
    m_vars_coefs : ndarray, shape (n_imputations, n_features)
        An estimate of the variance of the weights on each imputed dataset.
    Returns
    -------
    mean_coefs : ndarray, shape (n_features,)
        The mean coefficients computed across the imputed datasets.
    total_var_coefs : ndarray, shape (n_features,)
        An estimate of the total variation of the weights across the imputed
        datasets.
    """
    mean_coefs = np.mean(m_coefs, axis = 0)
    V_with = np.mean(m_ses * m_ses, axis = 0)
    V_betw = np.var(m_coefs, axis = 0, ddof = 1) #sample variance of the parameter of interest
    V_tot = V_with + V_betw * (1 + 1 / V_betw.shape[0]) 
    SE_pooled = np.sqrt(V_tot) #on Vbuur site
  
    return mean_coefs, SE_pooled

# def rubins_pooling_rules(m_coefs, m_vars_coefs):
#     """Applies Rubin's pooling rules.
#     The final weights is defined as the mean of the weights across the imputed
#     datasets while the total variance is defined as the combination of the mean
#     of the variance of the weights and the variance of the coefficients.
#     Parameters
#     ----------
#     m_coefs : ndarray, shape (n_imputations, n_features)
#         The weights of the model fitted on each imputed dataset.
#     m_vars_coefs : ndarray, shape (n_imputations, n_features)
#         An estimate of the variance of the weights on each imputed dataset.
#     Returns
#     -------
#     mean_coefs : ndarray, shape (n_features,)
#         The mean coefficients computed across the imputed datasets.
#     total_var_coefs : ndarray, shape (n_features,)
#         An estimate of the total variation of the weights across the imputed
#         datasets.
#     """
#     mean_coefs = np.mean(m_coefs, axis = 0)
#     mean_vars_coefs = np.mean(m_vars_coefs, axis = 0)
#     vars_coefs = np.var(m_coefs, axis = 0, ddof = 1)
#     total_var_coefs = mean_vars_coefs + (1 + 1 / vars_coefs.shape[0]) * vars_coefs
  
#     return mean_coefs, total_var_coefs




def mice(X_miss, estimator, n_imp = 5):
    imp_mice = []

    for i in range(n_imp):
        imp = IterativeImputer(random_state = i, sample_posterior = True, estimator = estimator, max_iter = 10).fit_transform(X_miss)
        imp = pd.DataFrame(imp, columns = X_miss.columns)
        imp_mice.append(imp)
    
    return imp_mice