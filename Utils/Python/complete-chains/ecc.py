##############################################################################
# HYBRID PARTITIONS FOR MULTI-LABEL CLASSIFICATION (HPML)                    #
# COMPLETE CHAINS OF HPML - EXTERNAL AND INTERNAL CHAINS                     #
# THIS IS THE STANDARD ENSEMBLE OF CLASSIFIER CHAINS                         #
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
# PhD Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri       #
# Ferrandin | Prof. Dr. Celine Vens | PhD Felipe Nakano Kenji                #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium               #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
##############################################################################



import numpy as np
import pandas as pd
from sklearn.base import clone
from sklearn.multioutput import ClassifierChain

class ECC:
    random_seed = 1234
    rng = np.random.default_rng(random_seed)

    def __init__(self,
                 model,
                 n_chains = 10,
                 ):
       self.model = model
       self.n_chains = n_chains
       self.chains = None
       
    def fit(self,
            x, ## dataframe
            y,
            ):
        self.chains = [ClassifierChain(self.model, order = "random") for i in range(self.n_chains)]
        for chain in self.chains:
            chain.fit(x, y)
        
    def predict(self,
                x):
        if self.chains is None:
            raise Exception('Oh no no no no!', 'Model has not been fitted yet.')
        return np.array([chain.predict(x) for chain in
                          self.chains]).mean(axis=0)