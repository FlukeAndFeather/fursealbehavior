
library(reticulate)
use_virtualenv("C:/Users/carey.kuhn.NMFS/my-venv-1")

library(rstickleback)

### warnings(?): C:\Users\CAREYK~1.NMF\MY-VEN~1\lib\site-packages\sktime\datatypes\_series\_check.py:43: 
### FutureWarning: pandas.Int64Index is deprecated and will be removed from pandas in a future version. 
### Use pandas.Index with the appropriate dtype instead.

############ Prep data ##################### from preparing-data.Rmd #####
library(fs)
library(lubridate)
library(tidyverse)

accel<-read.csv("~/Accelerometer analysis/test_data/test.accel2.csv")
events<-read.csv("~/Accelerometer analysis/test_data/test.events2.csv")

op <- options(digits.secs=6)
events$gmt<-lubridate::ymd_hms(events$gmt)

op <- options(digits.secs=6)
accel$gmt<-lubridate::ymd_hms(accel$gmt)

## sort events by id, gmt
events<-events %>% arrange(Animal.ID,gmt)


######## create rstickleback objects ########
fs_events <- Events(events, 
                    deployid_col = "Animal.ID", 
                    datetime_col = "gmt")
fs_sensors <- Sensors(accel,
                      deployid_col = "Animal.ID",
                      datetime_col = "gmt", 
                      sensor_cols = c("Corrected.Depth", "int.aX", "int.aY", "int.aZ"))

fs_events
fs_sensors


################ Run package ################ 
deployid <- deployments(fs_sensors)[1]
sb_plot_data(deployid, fs_sensors, fs_events)

## Error in py_call_impl(callable, dots....), tried to reduce dataset to only 2000 rows, #s changed
## but same error, interpolated depth, same error

test_deployids <- deployments(fs_sensors)[1]
c(sensors_test, sensors_train) %<-% divide(fs_sensors, test_deployids)

## Error in `$<-.data.frame`(`*tmp*`, "datetime", value = numeric(0)) : 
## replacement has 0 rows, data has 3743985

c(events_test, events_train) %<-% divide(fs_events, test_deployids)

tsc <- compose_tsc(module = "interval_based", 
                   algorithm = "SupervisedTimeSeriesForest",
                   params = list(n_estimators = 2L, random_state = 4321L),
                   columns = columns(fs_sensors))

## win_size = sample rate * window
sb <- Stickleback(tsc, 
                  win_size = 80, 
                  tol = 5, 
                  nth = 10, 
                  n_folds = 2, 
                  seed = 1234)

sb_fit(sb, sensors_train, events_train)

predictions <- sb_predict(sb, sensors_test)
outcomes <- sb_assess(sb, predictions, events_test)
outcomes

deployid <- deployments(sensors_test)[1]
sb_plot_predictions(deployid, sensors_test, predictions, outcomes)

