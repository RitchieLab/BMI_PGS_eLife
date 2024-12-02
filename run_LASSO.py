import sklearn
import pandas as pd
import numpy as np
import math
import sys
import warnings
warnings.filterwarnings('ignore')

from sklearn import linear_model
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_validate, train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import r2_score


d = pd.read_csv(sys.argv[1], sep="\t") #/Users/danielhui/Documents/RitchieLab/GxE_PRS/UKBB_EUR/all_vars_toModel_withInteractions.txt 

try:
    d = d.drop(["NeverVsCurrent", "NeverVsCurrent_x_PRS"], axis = 1)
except:
    print("NeverVsCurrent not in data")

d = d.dropna()


BMI = d["BMI"]
logBMI = d["logBMI"]
all_vars = d.drop(["BMI", "logBMI", "IID"], axis = 1)
all_vars_noInt = all_vars[all_vars.columns.drop(list(all_vars.filter(regex='_x_PRS')))]

print(all_vars.shape)
print(all_vars_noInt.shape)

#scale
scaler = MinMaxScaler()

X_withInt = scaler.fit_transform(all_vars)
X_noInt =  scaler.fit_transform(all_vars_noInt)


# Validation step using k-fold CV
def runModels(X_train, y_train, variables):

    maxR2 = 0
    best_model = ""

    scale = 1.0
    for i in range(0, 6):

        #build and run models
        lasso10 = linear_model.Lasso(alpha=1.0/scale, max_iter=1000)
        lasso05 = linear_model.Lasso(alpha=.5/scale, max_iter=1000)
        lasso02 = linear_model.Lasso(alpha=.2/scale, max_iter=1000)

        scoring="r2" #this can be multiple as well

        n_cv = 10
        lasso10_scores = cross_validate(lasso10, X_train, y_train, scoring=scoring, return_estimator=True, cv=n_cv)
        lasso05_scores = cross_validate(lasso05, X_train, y_train, scoring=scoring, return_estimator=True, cv=n_cv)
        lasso02_scores = cross_validate(lasso02, X_train, y_train, scoring=scoring, return_estimator=True, cv=n_cv)

        #print models
        print("Mean R2, lambda " + str(1.0/scale) + ":", lasso10_scores["test_score"].mean())
        print("Mean R2, lambda " + str(.5/scale) + ":", lasso05_scores["test_score"].mean())
        print("Mean R2, lambda " + str(.2/scale) + ":", lasso02_scores["test_score"].mean())

        #keep best model
        if lasso10_scores["test_score"].mean() > maxR2:
            maxR2 = lasso10_scores["test_score"].mean()
            best_model = variables + " " + str(1.0/scale)
        if lasso05_scores["test_score"].mean() > maxR2:
            maxR2 = lasso05_scores["test_score"].mean()
            best_model = variables + " " + str(0.5/scale)
        if lasso02_scores["test_score"].mean() > maxR2:
            maxR2 = lasso02_scores["test_score"].mean()
            best_model = variables + " " + str(0.2/scale)

        #adjust scaling factor for lambda
        scale = scale*10.0

    #no LASSO
    linreg = LinearRegression()
    lr_scores = cross_validate(linreg, X_train, y_train, scoring=scoring, return_estimator=True, cv=n_cv)
    print("Mean R2, no LASSO:", lr_scores["test_score"].mean())
    
    if lr_scores["test_score"].mean() > maxR2:
        maxR2 = lr_scores["test_score"].mean()
        best_model = variables + " no LASSO"

    #return best model
    return maxR2, best_model



##################
# run all models #
##################
print("#No interaction terms, BMI")
maxR2, best = runModels(X_noInt, BMI, "No_interaction_BMI")
maxR2_noInt = maxR2
best_noInt = best

print("#With interaction terms, BMI")
maxR2, best = runModels(X_withInt, BMI, "With_interaction_BMI")
maxR2_withInt = maxR2
best_withInt = best

print("#No interaction terms, log(BMI)")
maxR2, best = runModels(X_noInt, logBMI, "No_interaction_logBMI")
if float(maxR2) > float(maxR2_noInt):
    maxR2_noInt = maxR2
    best_noInt = best

print("#With interaction terms, log(BMI)")
maxR2, best = runModels(X_withInt, logBMI, "With_interaction_logBMI")
if float(maxR2) > float(maxR2_withInt):
    maxR2_withInt = maxR2
    best_withInt = best

print("#Best performing model no interaction")
print(maxR2_noInt)
print(best_noInt)

print("#Best performing model with interaction")
print(maxR2_withInt)
print(best_withInt)

print("#Best performing model, all")
if maxR2_withInt > maxR2_noInt:
	print(maxR2_withInt)
	print(best_withInt)
else:
	print(maxR2_noInt)
	print(best_noInt)
