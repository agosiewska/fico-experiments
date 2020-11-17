import pandas as pd
from supervised.automl import AutoML
import numpy as np
from sklearn.metrics import roc_auc_score, roc_curve, accuracy_score
from matplotlib import pyplot
import pickle

train = pd.read_csv("./data/train.csv", sep=";")
test = pd.read_csv("./data/test.csv", sep=";")

X = train.drop(["RiskPerformance"], axis=1)
y = train["RiskPerformance"]
X_test = test.drop(["RiskPerformance"], axis=1)
y_test = test["RiskPerformance"]

#########################
# mljar for 1 hour
automl = AutoML(total_time_limit = 1 * 3600, learner_time_limit = 240)
automl.fit(X, y)

predictions = automl.predict(X_test)
roc_auc_score(y_test, predictions["p_Good"])

predictions = automl.predict(X)
roc_auc_score(y, predictions["p_Good"])



########################
# mljar for 8 hours
automl = AutoML(total_time_limit = 8 * 3600, learner_time_limit = 240)
automl.fit(X, y)

predictions = automl.predict(X_test)
roc_auc_score(y_test, predictions["p_Good"])

predictions = automl.predict(X)
roc_auc_score(y, predictions["p_Good"])