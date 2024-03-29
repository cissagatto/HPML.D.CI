##############################################################################
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - PhD Elaine Cecilia Gatto | Prof PhD Ricardo Cerri                      #
# 2 - Prof PhD Mauri Ferrandin                                               #
# 3 - Prof PhD Celine Vens | PhD Felipe Nakano Kenji                         #
# 4 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            #
# 2 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 3 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium           #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 4 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################


import sys
import numpy as np
import pandas as pd
from ecc import ECC
from sklearn.ensemble import RandomForestClassifier  
from sklearn.metrics import average_precision_score

if __name__ == '__main__':

    
    # obtendo argumentos da linha de comando
    train = pd.read_csv(sys.argv[1]) # conjunto de treino
    valid = pd.read_csv(sys.argv[2]) # conjunto de validação
    test = pd.read_csv(sys.argv[3])  # conjunto de teste
    start = int(sys.argv[4])         # inicio do espaço de rótulos  
    directory = sys.argv[5]          # diretório para salvar as predições 
    
    # train = pd.read_csv("/dev/shm/lcpjws-GpositiveGO/Tested/Split-1/Group-1/GpositiveGO-split-tr-1-group-1.csv")
    # valid = pd.read_csv("/dev/shm/lcpjws-GpositiveGO/Tested/Split-1/Group-1/GpositiveGO-split-vl-1-group-1.csv") 
    # test = pd.read_csv("/dev/shm/lcpjws-GpositiveGO/Tested/Split-1/Group-1/GpositiveGO-split-ts-1-group-1.csv")
    # start = 912
    # directory = "/dev/shm/lcpjws-GpositiveGO/Tested/Split-1/Group-1/"
    
    # juntando treino com validação
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    # treino: separando os atributos e os rótulos
    X_train = train.iloc[:, :start]    # atributos 
    Y_train = train.iloc[:, start:] # rótulos 
    
    # teste: separando os atributos e os rótulos
    X_test = test.iloc[:, :start]     # atributos
    Y_test = test.iloc[:, start:] # rótulos verdadeiros
    
    # obtendo os nomes dos rótulos
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    
    # obtendo os nomes dos atributos
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)
    
    # parametros do classificador base
    random_state = 0    
    n_estimators = 200

    # inicializa o classificador base
    rf = RandomForestClassifier(n_estimators = n_estimators, random_state = random_state)
    
    n_chains = 10
    
    # treino
    model = ECC(rf, n_chains)

    # teste
    model.fit(X_train, Y_train)

    # predições probabilísticas
    y_pred_d = pd.DataFrame(model.predict_proba(X_test)) 
    
    # renomeando as colunas
    y_pred_d.columns = labels_y_test
    
    # obtendo os rótulos verdadeiros
    y_true_a = np.array(Y_test)      # array
    y_true_d = pd.DataFrame(Y_test)  # dataframe
    
    # setando nome do diretorio e arquivo para salvar
    true = (directory + "/cluster_true.csv")          # salva os rótulos verdadeiros
    proba = (directory + "/cluster_proba.csv")          # salva as predições binárias
    
    # salvando true labels and predict labels
    y_pred_d.to_csv(proba, index=False)
    y_true_d.to_csv(true, index=False)   
    
    
