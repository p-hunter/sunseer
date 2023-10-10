from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.linear_model import LogisticRegression, LinearRegression
import numpy as np
from scipy.stats import poisson
from scipy.optimize import minimize

def dpois(x, lamda):
    return np.exp(-lamda) * (lamda ** x) / np.math.factorial(x)

def ppois(x, lamda):
    return poisson.cdf(x, lamda)

def zi_poisson(pars, trunc, _weights, _X, _y, _y0, _y1,  _Z, _kx, _kz, _offset_x, _offset_z):
    mean_val = np.exp(np.dot(X, pars[:_kx]) + _offset_x)
    if trunc:
        phi_val = np.repeat(0, len(mean_val))
    else:
        i1 = _kx + 1
        i2 = _kz + _kz
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
    print(np.sum(out))
    return np.sum(out)

def g_poisson(pars, _weights, _X, _y,  _y1,  _Z, _kx, _kz, _offset_x, _offset_z):
    print("g is running")
    eta = np.dot(_X, pars[:_kx]) + _offset_x
    print("g eta is running")

    mu = np.exp(eta)
    print("g mu is running")

    i1 = _kx + 1
    i2 = _kz + _kz
    eta_z = np.exp(np.dot(_Z, pars[i1:i2]) + _offset_z)

    mu_z = np.exp(eta_z)
    clog_dens = -mu
    dens = mu_z * (1 - _y1) + np.exp(np.log(1-mu_z)+clog_dens + np.log(mu))
    print(dens)
    res_count = np.where(_y1, _y - mu, -np.exp(-np.log(dens) + np.log(1-mu_z)) + clog_dens + np.log(mu))
    res_zero = np.where(_y1, -1 / (1 - mu_z) * np.exp(eta_z), (np.exp(eta_z) - np.exp(clog_dens) * np.exp(eta_z)) / dens)
    return np.array([res_count * _weights * _X], [res_zero * _weights * _Z]).sum()#axis=0)

def non_generalized_zero_inflated_poisson(X, y):
    """
    Extremely unfriendly and inflexible zero inflated poisson model
    Basically says, "no, you may not customise this model, not even with offsets!!!"

    """
    _y = y
    _X = X.copy()
    _Z = X
    N = len(y)
    _kx = min(X.shape)
    _kz = _kx
    _weights = np.repeat(1, N)
    _y0 = y <= 0
    _y1 = y > 0
    _offset_x = np.repeat(0, N)
    _offset_z = np.repeat(0, N)
    zero_model = LogisticRegression(penalty = "none").fit(_Z, _y0.astype(int))
    pars = LinearRegression().fit(_X[_y1], y[_y1]).coef_
    fitted = minimize(fun=cg_poisson, x0=pars, args=(_weights, _X, _y,  _y1,  _Z, _kx, _kz, _offset_x, _offset_z),  jac=g_poisson )
    # not yet complete
    return fitted
