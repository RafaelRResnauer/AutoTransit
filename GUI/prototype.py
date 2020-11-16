# -*- coding: utf-8 -*-
"""
Created on Fri Nov 13 21:05:29 2020

@author: Rafael
"""
import PySimpleGUI as sg
import os.path
import pickle
import pandas as pd
from sklearn.neural_network import MLPClassifier

# Abrir o classificador treinado
mlp_filename = 'MLP_Classifier.pkl'
clfa_loaded = pickle.load(open(mlp_filename,'rb'))

# Dicionario para a classificação
thisdict =	{
  1: "Feridos Graves",
  2: "Feridos Leves",
  3: "Ilesos",
  4: "Mortos"
}
results=[]

# Layout da coluna de selecao de arquivos
file_list_column = [

    [

        sg.Text("Pasta de dados"),

        sg.In(size=(25, 1), enable_events=True, key="-FOLDER-"),

        sg.FolderBrowse(),

    ],

    [

        sg.Listbox(

            values=[], enable_events=True, size=(40, 20), key="-FILE LIST-"

        )

    ],

]


# Layout da coluna de resultados da classificacao

result_viewer_column = [

    [sg.Text("Escolha o dataset que deseja classificar:")],

    [sg.Text(size=(40, 1), key="-TOUT-")],

    [sg.Text(size=(40, 1), key="-RESULT-")],

]


# Layout completo

layout = [

    [

        sg.Column(file_list_column),

        sg.VSeperator(),

        sg.Column(result_viewer_column),

    ]

]


window = sg.Window("Autotransit", layout)


# Loop de evento

while True:

    event, values = window.read()

    if event == "Exit" or event == sg.WIN_CLOSED:

        break

    # Nome da pasta foi inserida e cria uma lista de arquivos da pasta

    if event == "-FOLDER-":

        folder = values["-FOLDER-"]

        try:

            # Pega a lista de arquivos da pasta

            file_list = os.listdir(folder)

        except:

            file_list = []


        fnames = [

            f

            for f in file_list

            if os.path.isfile(os.path.join(folder, f))

            and f.lower().endswith((".csv"))

        ]

        window["-FILE LIST-"].update(fnames)

    elif event == "-FILE LIST-":  # Um arquivo foi escolhido

        try:

            filename = os.path.join(

                values["-FOLDER-"], values["-FILE LIST-"][0]

            )

            window["-TOUT-"].update(os.path.split(filename)[1])
            
            # Dataset e aberto, verificado se tem o formato correto,
            # classificado e seu resultado int e convertido em string via dicionario
            dataset = pd.read_csv(filename,header = None)
            shape = dataset.shape
            if(shape[1] == 17):
                X = dataset
                # Resultados
                predicted=clfa_loaded.predict(X)
                for x in predicted:
                    results.append(thisdict.get(x))
            else:
                predicted = 'Tipo de arquivo incorreto'

            window["-RESULT-"].update(results)


        except:

            pass


window.close()