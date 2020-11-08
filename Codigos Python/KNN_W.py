# -*- coding: utf-8 -*-
"""
Created on Fri Sep 11 15:49:48 2020

@authors: Rafael, Lucas
"""

import pandas as pd
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import cross_val_predict
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split


# Importar os dados da base
print("KNN Weighted")
xcsvfile = "X_2017_numbers.csv"
ycsvfile = "y_2017_numbers.csv"
datasetx = pd.read_csv(xcsvfile,header = None)
datasety = pd.read_csv(ycsvfile,header = None)
print(datasetx.shape)
print(datasety.shape)
X = datasetx
y = datasety

"""
Testes usando Holdout
"""

# Separacao da base em treinamento (70% da base) e teste (30% da base)
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.3, random_state=42, stratify=y)

# Criacao do classificador
clfa = KNeighborsClassifier(n_neighbors=19, leaf_size=16, p=1, weights='distance')

# Treinamento do classificador
clfa = clfa.fit(X_train, y_train.values.ravel())

# Resultados dos testes
predicted=clfa.predict(X_test)

# Porcentagem de acuracia 
score=clfa.score(X_test, y_test)

# Criacao da matriz de confusao
matrix = confusion_matrix(y_test, predicted)
print("Accuracia = %.2f " % score)
print("Matriz de confusao:")
print(matrix)

"""
Testes usando Validacao Cruzada com 10 folds
"""

# Criacao do classificador
clfb = KNeighborsClassifier(n_neighbors=19, leaf_size=16, p=1, weights='distance')

# Treinamento do classificador
folds=10
result = cross_val_score(clfb, X, y.values.ravel(), cv=folds)

# Resultados numericos
print("\nResultado da validacao cruzada com %d folds:" % folds)
print("Acuracia media: %.2f" % result.mean())
print("Desvio Padrao: %.2f" % result.std())

# Matriz de confusao
Z = cross_val_predict(clfb, X, y.values.ravel(), cv=folds)
cm=confusion_matrix(y, Z)
print("Matriz de confusao:")
print(cm)

