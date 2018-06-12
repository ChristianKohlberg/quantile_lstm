### Installation
make sure to have all packages properly installed. You can invoke the keras backend installation using install_keras().

### Usage
the code is taken from: http://www.business-science.io/timeseries-analysis/2018/04/18/keras-lstm-sunspots-time-series-prediction.html

The model has been given a custom loss function for quantile regression.

It was designed as a proof of concept of implementing asymmetric cost functions in lstm models.

### Further work
for now the model only take a single feature as input. Incorporating more lags and summary statistics will likely improve the model further.
