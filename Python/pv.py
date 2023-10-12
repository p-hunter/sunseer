import datetime
import pandas as pd

def hh_period(date_time):
  if str(date_time.dtype) != "datetime64[ns]":
    date_time =  pd.to_datetime(date_time)
  return 1 + 2 * date_time.dt.hour + (date_time.dt.minute > 21).astype(int)
