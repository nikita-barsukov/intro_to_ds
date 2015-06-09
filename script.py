import numpy
from pandas import *
import scipy.stats
from ggplot import *
import statsmodels.api as sm
from sklearn.linear_model import SGDRegressor

df = pandas.read_csv('turnstile_weather_v2.csv')

# t-tests rain vs no-rain and fog vs no-fog
df['turnaround'] = df['ENTRIESn_hourly'] + df['EXITSn_hourly']

## rain vs no-rain
def assess_model(values, predictions):
    r_squared = 1 - numpy.sum((values-predictions)**2)/numpy.sum((values - numpy.mean(values))**2)
    abs_err = numpy.absolute(predictions - values)
    mean_err = numpy.mean(abs_err)
    return r_squared, mean_err

def p_value(u, x, y):
    m_u = len(x)*len(y)/2
    sigma_u = numpy.sqrt(len(x)*len(y)*(len(x)+len(y)+1)/12)
    z = (u - m_u)/sigma_u
    pval = 2*scipy.stats.norm.cdf(z)
    return pval

def predict_ols(features, values):
    features = sm.add_constant(features)
    model = sm.OLS(values, features)
    model = model.fit()
    intercept = model.params[0]
    params = model.params[1:]
    return intercept, params

def normalize_features(features):
    means = numpy.mean(features, axis=0)
    std_devs = numpy.std(features, axis=0)
    normalized_features = (features - means) / std_devs
    return means, std_devs, normalized_features

def recover_params(means, std_devs, norm_intercept, norm_params):
    intercept = norm_intercept - numpy.sum(means * norm_params / std_devs)
    params = norm_params / std_devs
    return intercept, params

def predict_gd(features, values):
    regr = SGDRegressor(n_iter=100)
    regr.fit(features, values)
    return regr.intercept_, regr.coef_

print("Total mean turnaround", numpy.mean(df['turnaround']))
without_rain_mean = numpy.mean(df[df['rain'] == 0]['turnaround'])
with_rain_mean = numpy.mean(df[df['rain'] == 1]['turnaround'])
without_fog_mean = numpy.mean(df[df['fog'] == 0]['turnaround'])
with_fog_mean = numpy.mean(df[df['fog'] == 1]['turnaround'])

rain_vs_norain = scipy.stats.mannwhitneyu(df[df['rain'] == 1]['turnaround'], df[df['rain'] == 0]['turnaround'])
print('Mean turnaround, rain vs no rain:', with_rain_mean, without_rain_mean)
print('P-value of rain vs norain:',p_value(rain_vs_norain[0], df[df['rain'] == 1]['turnaround'],df[df['rain'] == 0]['turnaround']) )

## fog vs no-fog

fog_vs_nofog = scipy.stats.mannwhitneyu(df[df['fog'] == 1]['turnaround'], df[df['fog'] == 0]['turnaround'])
print('Mean turnaround, fog vs no fog:', with_fog_mean, without_fog_mean)
print('P-value of fog vs nofog:',p_value(fog_vs_nofog[0], df[df['fog'] == 1]['turnaround'],df[df['fog'] == 0]['turnaround']))

# Linear regression: which method predits turnaround better
print("=================================")
r_sq_ols = []
mean_err_ols = []
r_sq_gr = []
mean_err_gr = []

predictors = ['rain', 'precipi', 'hour', 'meantempi', 'day_week','fog','wspdi' ]
values = df['ENTRIESn_hourly']

for i in range(0, len(predictors)):
    cols = predictors[0:i+1]
    features = df[cols]
    dummy_units = pandas.get_dummies(df['UNIT'], prefix='unit')
    features = features.join(dummy_units)
    features_array = features.values
    values_array = values.values

    intercept_ols, params_ols = predict_ols(features_array, values_array)
    predictions_ols = intercept_ols + numpy.dot(features_array, params_ols)

    r, m = assess_model(values, predictions_ols)
    r_sq_ols.append(r)
    mean_err_ols.append(m)

    # gradient descent
    means, std_devs, normalized_features_array = normalize_features(features_array)
    norm_intercept, norm_params = predict_gd(normalized_features_array, values_array)
    intercept_gd, params_gd = recover_params(means, std_devs, norm_intercept, norm_params)
    predictions_gd = intercept_gd + numpy.dot(features_array, params_gd)
    r, m = assess_model(values, predictions_gd)
    r_sq_gr.append(r)
    mean_err_gr.append(m)

r_sq_df = pandas.melt(DataFrame({'x': list(range(1, len(r_sq_ols)+1)), 'OLS': r_sq_ols, 'Gradient descent': r_sq_gr}), id_vars=['x'])
mean_err_df = pandas.melt(DataFrame({'x': list(range(1, len(r_sq_ols)+1)),'OLS': mean_err_ols, 'Gradient descent': mean_err_gr}),id_vars=['x'])

plot_rsq = ggplot(aes(x='x', y='value',color='variable'), data=r_sq_df) + geom_line() + ggtitle('R-squared') + ylab('') + xlab('Number of features') + theme_bw()
print(plot_rsq)
#ggsave('rsq.png', plot_rsq)

plot_mean = ggplot(aes(x='x', y='value',color='variable'), data=mean_err_df) + geom_line() + ggtitle('Mean absolute error') + ylab('') +  xlab('Number of features') + theme_bw()
print(plot_mean)
ggsave('mean.png', plot_mean)






