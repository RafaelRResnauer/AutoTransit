# -*- coding: utf-8 -*-
"""
Created on Sun Sep 13 15:33:38 2020

@author: Rafael
"""

import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import cross_val_predict
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split

csvfile = "D:\Rafael\Pendrive\Faculdade\10º semestre\TCC\Dataset_PRF_1_0_csv_600.csv"
dataset = np.loadtxt(csvfile, delimiter=",")

print(dataset.shape)

x1=dataset[:,0:8]
x2=dataset[:,10:24]
X = np.concatenate((x1,x2),axis=1)
y = dataset[:,9]


X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.3, random_state=42, stratify=y)


clfa = RandomForestClassifier(max_depth=2, random_state=0)

clfa = clfa.fit(X_train, y_train)

predicted=clfa.predict(X_test)

score=clfa.score(X_test, y_test)

matrix = confusion_matrix(y_test, predicted)

print("Accuracy = %.2f " % score)
print("Confusion Matrix:")
print(matrix)

clfb = RandomForestClassifier(max_depth=2, random_state=0)
folds=10
result = cross_val_score(clfb, X, y, cv=folds)
print("\nCross Validation Results %d folds:" % folds)
print("Mean Accuracy: %.2f" % result.mean())
print("Mean Std: %.2f" % result.std())

Z = cross_val_predict(clfb, X, y, cv=folds)
cm=confusion_matrix(y, Z)
print("Confusion Matrix:")
print(cm)

