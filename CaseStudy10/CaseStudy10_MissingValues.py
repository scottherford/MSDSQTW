import pandas as pd 
import numpy as np 
import scipy.stats as stats 
import matplotlib.pyplot as plt 
import seaborn as sns
sns.set_style("whitegrid")
sns.set_context("poster")
import sklearn
from sklearn import datasets
from sklearn.datasets import load_boston
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score
import statsmodels.api as sm 
from matplotlib import rcParams

get_ipython().run_line_magic('matplotlib', 'inline')

boston = datasets.load_boston()
print(boston.data.shape) # shape of dataset

print(boston.feature_names) #columns

print(boston.DESCR)

bos = pd.DataFrame(boston.data, columns=boston=feature_names)
print(bos.head())

Dest = pd.DataFrame(boston.target, columns=["MEDV"])
print(Dest.head)

bos.columns = boston.feature_names
print(bos.head())

print(boston.target.shape)

print(bos.describe())

print(boston.keys()) # available dictionary keys

# strong positive correlations are blue
# strong negative correlations are red

ax, fig = plt.subplot(figsize = (16, 10))
sns.heatmap(bos.corr(), annot=True, cmap = 'RdBu')
plt.show()

f = plt.Figure(figsize = (8, 6))
pd.plotting.scatter_matrix(bos[bos.columns], figsize = (30, 30), s = 75)
plt.plot()

print('DataFrame Null values:' , bos.isnull().values.ravel().sum())
print('Number of unique classes:\n',bos.nunique())

features = ['LSTAT', 'RM']
X_b = np.c_[np.ones((len(bos[features]), 1)), bos[features].values] # X matrix
X_b.shape

from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error

reg = LinearRegression().fit(bos, Dest)
baselineR2 = reg.score(bos, Dest)
print('R Squared:', baselineR2)
reg.coef_

Dest_pred = reg.predict(bos)
mse = mean_squared_error(Dest, Dest_pred)
print('Baseline MSE with intercept:', mse)

bos['PRICE'] = boston.target

print(bos.head()) # dateframe with price column

from sklearn.model_selection import train_test_split

#splitting dataset to train-test
# Y = Boston Housing Price
# X = All other features

X = bos.drop('PRICE', axis = 1)
Y = bos['PRICE']

X_train, X_test, Y_train, Y_test = train_test_split(X_b, Y, test_size = 0.33, random_state = 5)

print(X_train.shape)
print(X_test.shape)
print(Y_train.shape)
print(Y_test.shape)

def imputer(ImputeSeries):
    from sklearn.preprocessing import Imputer

    imp = Imputer(missing_values=np.nan, 
                  strategy='mean', #median
                 copy=False)
    imp.fit(ImputeSeries)
    imp.transform(ImputeSeries)
    return


from sklearn.metrics import r2_score
NANDests = [1,5,10,20,33,50]

MCARresults = []
for x in NANDests:
    bos2 = bos.copy()
    bos2.AGE[bos2.AGE.sample(frac=(x/100)).index] = np.nan
    print((bos2.isna().sum()/len(bos2))*100)

    imputer(bos2)
Dest


NANDests = [10,20,30]
NANcols = ['AGE','DIS']
Third = bos2.RM > 6.5

for x in NANDests:
    bos2 = bos.copy()
    bos2.loc[bos2.loc[Third].sample(frac=(x/100)).index, NANcols] = np.nan
    imputer(bos2)
Dest


X_train, X_test, Y_train, Y_test = train_test_split(X_b, Y, test_size = 0.2, random_state = 5)

print(X_train.shape)
print(X_test.shape)
print(Y_train.shape)
print(Y_test.shape)


X_train, X_test, Y_train, Y_test = train_test_split(X_b, Y, test_size = 0.5, random_state = 5)

print(X_train.shape)
print(X_test.shape)
print(Y_train.shape)
print(Y_test.shape)


X_train, X_test, Y_train, Y_test = train_test_split(X_b, Y, test_size = 0.10, random_state = 5)

print(X_train.shape)
print(X_test.shape)
print(Y_train.shape)
print(Y_test.shape)


X_train, X_test, Y_train, Y_test = train_test_split(X_b, Y, test_size = 0.05, random_state = 5)

print(X_train.shape)
print(X_test.shape)
print(Y_train.shape)
print(Y_test.shape)


X_train, X_test, Y_train, Y_test = train_test_split(X_b, Y, test_size = 0.01, random_state = 5)

print(X_train.shape)
print(X_test.shape)
print(Y_train.shape)
print(Y_test.shape)

#normal equation will compute the correct weights and bias in a closed form solution

def normal_equation(x,y):
    return np.matmul(np.matmul(np.linalg.inv(np.matmul(x.T, x)), x.T),y)


theta = normal_equation(X_train, Y_train)
theta

predictions = np.dot(X_train, theta)


ax, fig = plt.subplots(figsize = (10,8))
plt.plot(predictions, 'b.', marker = '*')
plt.plot(Y_train, 'r.')
plt.legend(['predictions', 'true'])
plt.show()


from sklearn.metrics import mean_squared_error
mse_train = sklearn.metrics.mean_squared_error(predictions, Y_train)
print(mse_train)

test_pred = np.dot(X_test, theta)

ax, fig = plt.subplots(figsize = (10,8))
plt.plot(test_pred, 'b.', marker = '*')
plt.plot(Y_test, 'r.')
plt.legend(['test_pred', 'true'])
plt.show()

mse_test = sklearn.metrics.mean_squared_error(test_pred, Y_test)
print(mse_test)

lm = LinearRegression()
lm.fit(X_train, Y_train)

Y_pred = lm.predict(X_test)

plt.scatter(Y_test, Y_pred)
plt.xlabel("Prices: $Y_i$")
plt.ylabel("Predicted prices: $\hat{Y}_i$")
plt.title("Prices vs Predicted prices: $Y_i$ vs $\hat{Y}_i$")

mse = sklearn.metrics.mean_squared_error(Y_test, Y_pred)
print(mse)

def performance_metric(Y_test, Y_pred):
    score = r2_score(Y_test, Y_pred)
    return score

print(performance_metric(Y_test, Y_pred))

from sklearn.metrics import make_scorer
from sklearn.model_selection import GridSearchCV 
from sklearn.model_selection import ShuffleSplit
from sklearn.tree import DecisionTreeRegressor

X = bos.drop('PRICE', axis = 1)
Y = bos['PRICE']

def fit_model(X,Y):
    cv_sets = ShuffleSplit(X_b.shape[0], test_size = 0.20, random_state = 0)
    regressor = DecisionTreeRegressor()
    params = {'max_depth': [i+1 for i in range(10)]}
    scoring_fnc = make_scorer(performance_metric)
    grid = GridSearchCV(regressor, param_grid=params, cv=cv_sets, scoring=scoring_fnc)
    grid = grid.fit(X,Y)
    return grid.best_estimator_

print(fit_model(X,Y))


