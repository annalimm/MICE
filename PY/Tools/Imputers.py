import sys 
sys.path.append('..')
# from Tools import MI, MI_R
from Tools import MI 

import numpy as np
import pandas as pd

from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import SimpleImputer, IterativeImputer, KNNImputer
import miceforest as mf

from sklearn.linear_model import BayesianRidge
import skopt
from skopt.learning import RandomForestRegressor

import os
# os.chdir(__file__)


def regr_func(df, missing_columns):
    
    #firstly we randomly (by param) impute missing values in feature + '_imp'
    for feature in missing_columns:
        #extra column for imputations
        df[feature + '_imp'] = df[feature]
        number_missing = df[feature].isnull().sum()
        observed_values = df.loc[df[feature].notnull(), feature]
        df.loc[df[feature].isnull(), feature + '_imp'] = np.random.choice(observed_values, number_missing, replace = True)


    for feature in missing_columns:
        
        #all params withous missing values
        parameters = list(set(df.columns) - set(missing_columns) - {feature + '_imp'})

        #a Linear Regression model
        model = linear_model.LinearRegression()
        model.fit(X = df[parameters], y = df[feature + '_imp'])

        #preserve the index of the missing data from the original dataframe
        df.loc[df[feature].isnull(), feature] = model.predict(df[parameters])[df[feature].isnull()]

    return df


#add estimator = None and mice w/ estimator parametr
def impute(X_miss, imputer_name = 'mf'):
    
    name = imputer_name
    
    if name == 'mf':
        imp = pd.DataFrame(SimpleImputer(strategy = 'most_frequent').fit_transform(X_miss), index = X_miss.index, columns = X_miss.columns)
    elif name == 'const':
        imp = SimpleImputer(strategy='constant').fit_transform(X_miss)
    elif name == 'mean':
        imp = pd.DataFrame(SimpleImputer(strategy='mean').fit_transform(X_miss), columns = X_miss.columns)
        
    elif name == 'ice_rf':
        
        RFR = RandomForestRegressor(
        n_estimators = 10, #the number of trees
        max_features = 1.0,
        random_state = 1)
            
        imp = pd.DataFrame(IterativeImputer(random_state = 0, sample_posterior = True, estimator = RFR, max_iter = 10).fit_transform(X_miss), columns = X_miss.columns)
    
    elif name == 'mice_br':
        imp = MI.mice(X_miss, estimator = BayesianRidge(), n_imp = 5)
    
    #поработать с параметрами RFR
    elif name == 'mice_rf':
        RFR = RandomForestRegressor(
        n_estimators = 10,
#         max_depth = 10,
        max_features = 1.0,
#         bootstrap = True,
#         n_jobs = 2,
#         criterion = "squared_error", 
        random_state = 1)
            
        imp = MI.mice(X_miss, estimator = RFR, n_imp = 5)
    
    elif name == 'miceforest':
        # Create kernel. 
        imputer_forest = mf.ImputationKernel(
                                        X_miss,
                                        datasets=1,
#                                         mean_match_candidates=5,
                                        save_all_iterations=True,
                                        random_state=1991
                                        )
        optimal_parameters, losses = imputer_forest.tune_parameters(
                                        dataset=0,
                                        optimization_steps=5
                                        )
        imputer_forest.mice(iterations=1, boosting='gbdt', min_sum_hessian_in_leaf=0.01)
        imp = imputer_forest.complete_data(dataset=0)

        
#     elif name == 'R_mice_pmm':
#         imp = MI_R.mice_R(X_miss, maxit = 50, m = 5, seed = 1, meth = 'pmm')
        
#     #only 4 cont here      
#     elif name == 'R_mice_rf':
#         imp = MI_R.mice_R(X_miss, maxit = 50, m = 5, seed = 1, meth = 'rfcont')
        
#     elif name == 'R_missForest':
#         imp = MI_R.missForest_R(X_miss)

    elif name == 'knn':
        imp = pd.DataFrame(KNNImputer().fit_transform(X_miss), columns = X_miss.columns)


    return imp


# def assess_impute(X_full, mask, imp, mode = 'mae', y_full = None):
#     if mode == 'mae':
#         mae = MAE(imp, X_full, mask)
#         return mae, 0, imp
#     elif mode == 'bayesianRidge':
#             # дальше оцениваем связь в данных,  внашем случае с BayesianRidge, но надо проверить еще и другие метрики 
#         br_estimator = BayesianRidge()
#         CVS = cross_val_score(br_estimator, imp, y_full, scoring = "neg_mean_squared_error", cv = 5)

#         score_full_data = pd.DataFrame(CVS, columns = ["Full Data"], )

#         return -float(score_full_data.mean()), -float(score_full_data.std()), imp
#     else:# dataset_name == 'real':
#         print("No such mode")
#         return 0, 0, 0

    
    