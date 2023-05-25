import pandas as pd
import numpy as np
import numpy.random
from scipy import stats
import scipy.stats as ss
from scipy.stats import binom
import matplotlib.pyplot as plt
from scipy.special import expit
import math


def mix_norm(norm_params, n = 500):
    n_components = norm_params.shape[0]

    weights = np.ones(n_components, dtype=np.float64) / 2.0
    mixture_idx = numpy.random.choice(len(weights), size = n, replace = True, p = weights)

    # the mixture sample
    X = numpy.fromiter((ss.norm.rvs(*(norm_params[i])) for i in mixture_idx), dtype=np.float64)
    
    return X


def gener(X_distr, scenar):
    
    z = None
    
    if scenar in ['logr_quadr', 'logr_inter']:
        n = 1000
    else:
        n = 500

    #['norm', 'unif', 'lognorm1', 'lognorm2', 'gamma1', 'gamma2', 'norm_mix1', 'norm_mix2']
    if X_distr == 'norm':
        #1 Normal(4, 1)
        X = np.random.normal(4, 1, n)

    elif X_distr == 'unif':
        #2 Uniform(0, 8)
        X = np.random.uniform(0, 8, n)

    elif X_distr == 'lognorm1':
        #3 Lognormal(0, 0.25)
        X = np.random.lognormal(0, math.sqrt(0.25), n)

    elif X_distr == 'lognorm2':
        #4 Lognormal(0, 0.625)
        X = np.random.lognormal(0, math.sqrt(0.625), n)

    elif X_distr == 'gamma1':
        #5 Gamma(1, 1)
        X = np.random.gamma(1, 1 / 1, n)

    elif X_distr == 'gamma2':
        #6 Gamma(2, 0.5)
        X = np.random.gamma(2, 1 / 0.5, n)

    elif X_distr == 'norm_mix1':
        #7 N(1, 1), N(6, 3)
        X = mix_norm(np.array([[1, 1], [6, math.sqrt(3)]]), n)
        
#         d1 = stats.norm(1, math.sqrt(1))
#         d2 = stats.norm(6, math.sqrt(3))

#         # set mixture component weights
#         mc = [0.5, 0.5]

#         # where to evaluate the densities
#         x = np.linspace(0, 10, n)
#         # calculate density and apply mixture weights
#         c1 = d1.pdf(x) * mc[0]
#         c2 = d2.pdf(x) * mc[1]
#         X = c1 + c2

    elif X_distr == 'norm_mix2':
        #8  N(1, 1), N(6, 10)
        X = mix_norm(np.array([[1, 1], [6, math.sqrt(10)]]), n)
        
#         d1 = stats.norm(1, math.sqrt(1))
#         d2 = stats.norm(6, math.sqrt(10))

#         # set mixture component weights
#         mc = [0.5, 0.5]

#         # where to evaluate the densities
#         x = np.linspace(0, 10, n)
#         # calculate density and apply mixture weights
#         c1 = d1.pdf(x) * mc[0]
#         c2 = d2.pdf(x) * mc[1]
#         X = c1 + c2

        
    #['lr_quadr', 'logr_quadr', 'lr_inter', 'logr_inter']
    #sc.1
    if scenar == 'lr_quadr':
        eps = np.random.normal(0, 1)
        Y = (2 + 2*X + X*X + eps)

    #sc.2
    elif scenar == 'logr_quadr':
        p = expit(-1.2 + 0.1*X + 0.05*X*X)
        Y = np.random.binomial(1, p, n) 

    #sc.3
    elif scenar == 'lr_inter':
        z = np.random.normal(4, 2, n)
        eps = np.random.normal(0, 1)
        Y = (2 + X + X*z + z + eps)

    #sc.4
    elif scenar == 'logr_inter':
        z = np.random.normal(4, 2, n)
        p = expit(-2 + 0.5*X - 0.0625*X*z + 0.25*z)
        Y = np.random.binomial(1, p, n)

        
    if z is not None:
        df = pd.DataFrame({'X': list(X), 'Y': list(Y), 'XZ': list(X*z), 'Z': list(z)}, columns=['X', 'Y', 'XZ', 'Z'])
    else:
        df = pd.DataFrame({'X': list(X), 'Y': list(Y), 'X^2': list(X*X)}, columns=['X', 'Y', 'X^2'])


    return df