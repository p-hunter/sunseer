from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.linear_model import LogisticRegression, LinearRegression
import numpy as np
from scipy.stats import poisson
from scipy.optimize import minimize, least_squares
import statsmodels 
import statsmodels.api as sm
import statsmodels.discrete.count_model as reg_models
from sklearn.utils.estimator_checks import check_estimator
import warnings


def dpois(x, lamda):
    return np.exp(-lamda) * (lamda ** x) / np.math.factorial(x)

def zi_poisson(pars, trunc, _weights, _X, _y, _y0, _y1,  _Z, _kx, _kz, _offset_x, _offset_z):
    mean_val = np.exp(np.dot(X, pars[:_kx]) + _offset_x)
    if trunc:
        phi_val = np.repeat(0, len(mean_val))
    else:
        i1 = _kx 
        i2 = _kz + _kz - 1 
        phi_val = np.exp(np.dot(_Z, pars[i1:i2]) + _offset_z)
    log_lik_0 = np.log(phi_val + np.exp(np.log(1 - phi_val) - mean_val))
    log_lik_1 = np.log(1 - phi_val) + np.log(dpois(_y, mean_val))
    if trunc:
        out = sum(_weights[_y1] * log_lik_1[_y1]) - sum(_weights[_y1] * np.log(1 - np.exp(log_lik_1[_y1])))
    else:
        out = sum(_weights[_y0] * log_lik_0[_y0]) - sum(_weights[_y1] * log_lik_1[_y1])
    return out

def cg_poisson(pars, _weights, _X, _y,  _y1,  _Z, _kx, _kz, _offset_x, _offset_z):
    full_eta = np.dot(_X, pars[:_kx]) + _offset_x
    eta = full_eta[_y1]
    mu = np.exp(eta)
    out = (_y[_y1] - mu - np.exp(np.log(poisson.cdf(0, mu)) - np.log(1 - poisson.cdf(0, mu)) + eta)) * _weights[_y1] * _X[_y1]    
    return np.sum(out,axis=0)[1]

def g_poisson(pars, _weights, _X, _y,  _y1,  _Z, _kx, _kz, _offset_x, _offset_z):
    eta = np.dot(_X, pars[:_kx]) + _offset_x
    mu = np.exp(eta)
    i1 = _kx 
    i2 = _kx + _kz - 1
    if i1 is i2:
        pars_z = np.array(pars[i1-1])
    else:
        pars_z = pars[i1:i2]  
    eta_z = np.exp(np.dot(_Z, pars_z) )
    N = len(eta_z)
    mu_z = np.max(np.array([np.exp(eta_z), np.repeat(2.220446e-10, N).reshape(N,1)]), axis=0)
    clog_dens = -mu
    dens = mu_z * (1 - _y1.astype(int)) + np.exp(np.log(1 - -mu_z) + clog_dens)    
    res_count = np.where(_y1, _y - mu, -np.exp(-np.log(dens) + np.log(1- -mu_z)) + clog_dens + np.log(mu))[0]
    res_zero = np.where(_y1, -1 / (1 - mu_z) * np.exp(eta_z), (np.exp(eta_z) - np.exp(clog_dens) * np.exp(eta_z)) / dens)[0]
    p1 = res_count* _weights * _X.reshape(-1)
    p2 = res_zero * _weights * _Z.reshape(-1)
    p_array_out = np.array([p1, p2])
    p_array_sum = p_array_out.sum(axis=1)
    return p_array_sum[1]

def non_generalized_zero_inflated_poisson(X, y):
    """
    Extremely unfriendly and inflexible zero inflated poisson model
    Basically says, "no, you may not customise this model, not even with offsets!!!"

    """
    _y = y
    _X = X
    _Z = _X
    N = len(y)
    _kx = min(X.shape)
    _kz = _kx
    _weights = np.repeat(1, N)
    _y0 = y <= 0
    _y1 = y > 0
    _offset_x = np.repeat(0, N)
    _offset_z = np.repeat(0, N)
    zero_model = LogisticRegression(penalty = "none").fit(_Z, _y0.astype(int))
    #pars = LinearRegression().fit(_X[_y1], y[_y1]).coef_
    warnings.filterwarnings("ignore")
    count_model = reg_models.Poisson(endog=y,exog=_X.reshape(-1, 1) ).fit(disp=0)
    count_model_params = np.append(count_model.params, 0.1)
    warnings.filterwarnings("always")
    starts = dict({"Count":count_model, "Zero":zero_model})
    starts_coefs = dict({"Count":count_model_params, "Zero":zero_model.coef_})
    #fitted = minimize(fun=cg_poisson, x0=pars, args=(_weights, _X, _y,  _y1,  _Z, _kx, _kz, _offset_x, _offset_z), method="BFGS",  jac=g_poisson)
    #fitted = least_squares(fun=cg_poisson, x0=pars, args=(_weights, _X, _y,  _y1,  _Z, _kx, _kz, _offset_x, _offset_z),   jac=g_poisson)
    return starts #fitted

class NonGeneralizedZeroInflatedPoisson(BaseEstimator, RegressorMixin):
    def fit(self, X, y):
        model = reg_models.ZeroInflatedGeneralizedPoisson(endog=y,exog= X)
        self.fit = model.fit()
        self.model = model
        return self
    def predict(self, X):
        return self.fit.predict(X)


class SMWrapper(BaseEstimator, RegressorMixin):
    """ A universal sklearn-style wrapper for statsmodels regressors
     Doesn't work for zero-inflated, yet """
    def __init__(self, model_class, fit_intercept=True):
        self.model_class = model_class
        self.fit_intercept = fit_intercept
    def fit(self, X, y):
        if self.fit_intercept:
            X = sm.add_constant(X)
        self.model_ = self.model_class(y, X)
        self.results_ = self.model_.fit()
        return self
    def predict(self, X):
        if self.fit_intercept:
            X = sm.add_constant(X)
        return self.results_.predict(X)

#check_estimator(SMWrapper(sm.ZeroInflatedGeneralizedPoisson))
