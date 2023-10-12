import Python.non_generalized_zero_inflated_poisson as m
from Python.pv import *
import pandas as pd
import datetime
from sklearn.preprocessing import  SplineTransformer
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from scipy import stats
import sklearn.model_selection as sk_model
import warnings
import numpy as np
warnings.filterwarnings("ignore")
from sklearn.base import BaseEstimator, RegressorMixin
import statsmodels.api as sm
import statsmodels.discrete.count_model as reg_models

pv_df = pd.read_csv("pv/pv_live.csv")

pv_df["date_gmt"] = pd.to_datetime(pv_df["datetime_gmt"]).dt.date.astype('datetime64[ns]')

pv_df = pv_df.query("date_gmt > '2019-01-01' ")

cloud_cube = pd.read_csv("cloud_cube/cloud_cube_data.csv")

cloud_cube_agg = cloud_cube \
 .rename(columns = {"Date":"date_gmt"}) \
 .query("cloud_cover > 0") \
 .loc[:,["date_gmt", "cloud_cover"]] \
 .groupby("date_gmt") \
 .agg({"cloud_cover":"mean"}) 
 
cloud_cube_agg = cloud_cube_agg \
  .assign(lag_mean_cloud_cover=cloud_cube_agg.cloud_cover.shift(-1)) \
  .drop("cloud_cover", axis = 1) \
  .query("lag_mean_cloud_cover.notna()") \
  .reset_index()

cloud_cube_agg["date_gmt"] = pd.to_datetime(cloud_cube_agg["date_gmt"]).dt.date.astype('datetime64[ns]')

pv_df = pv_df.assign(
  Period =  hh_period(pv_df["datetime_gmt"]),
  doy = pv_df["date_gmt"].dt.dayofyear,
  lag_1_generation_mw =  pv_df["generation_mw"].shift(-1),
  lag_2_generation_mw = pv_df["generation_mw"].shift(-2)
  )
pv_df = pv_df.query("lag_2_generation_mw.notna()")

pv_df = pv_df \
  .merge( cloud_cube_agg, how="left", on = "date_gmt") \
  .loc[:,["datetime_gmt", "generation_mw",
       "date_gmt", "Period", "doy", "lag_1_generation_mw",
       "lag_2_generation_mw", "lag_mean_cloud_cover"]] \
  .dropna() 


pv_df["generation_mw"] = np.round(pv_df["generation_mw"])

spline_transform = Pipeline(
    [("bs_trans", SplineTransformer(knots="quantile",include_bias=False))]
)
preprocessor = ColumnTransformer(
  [
    ("bs1", spline_transform, ["Period"]),
    ("bs2", spline_transform, ["doy"])
    ], 
    remainder = "passthrough"
    )

class NonGeneralizedZeroInflatedPoisson(BaseEstimator, RegressorMixin):
    def fit(self, X, y):
        X = sm.add_constant(X)        
        self.model_ = reg_models.ZeroInflatedGeneralizedPoisson(endog = y, exog = X, exog_infl = X)
        self.fit = self.model_.fit(method = "nm")
        self.params = self.fit.params
        self.results_= self.fit      
        return self
    def predict(self, X):
        X = sm.add_constant(X)
        return self.fit.predict(exog = X, exog_infl = X)    

model_workflow = Pipeline([   
      ("preprocessing", preprocessor),      
      ("clf", m.NonGeneralizedZeroInflatedPoisson())
      ])

params = {"clf": [m.NonGeneralizedZeroInflatedPoisson()],   
        "preprocessing__bs1__bs_trans__degree" : stats.randint(3,6),
       "preprocessing__bs2__bs_trans__degree" : stats.randint(3,14)#,
       #  "preprocessing__bs1__bs_trans__n_knots" : stats.randint(5,9),
       # "preprocessing__bs2__bs_trans__n_knots" : stats.randint(3,14)
        }

x_cols = ["Period", "doy", "lag_1_generation_mw", "lag_2_generation_mw", "lag_mean_cloud_cover"]

y = pv_df["generation_mw"].to_numpy().astype(int)

X = pv_df[x_cols]

X_train, X_test, y_train, y_test = sk_model.train_test_split(X, y, test_size=.2)

data_folds = sk_model.KFold(n_splits = 5)

cv = sk_model.RandomizedSearchCV(model_workflow, params, scoring= "neg_root_mean_squared_error",  n_iter = 14, cv = data_folds, n_jobs = -1,   verbose = 3, refit = True)

model_fit=cv.fit(X_train, y_train)
