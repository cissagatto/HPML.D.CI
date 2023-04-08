##############################################################################
# LABELS CHAINS HPML                                                         #
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


FolderRoot = "~/Labels-Chains-HPML"
FolderScripts = "~/Labels-Chains-HPML/R"



##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             #
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
execute.run.python <- function(parameters){
  
  FolderRoot = "~/Labels-Chains-HPML"
  FolderScripts = "~/Labels-Chains-HPML/R"
  
  setwd(FolderScripts)
  source("utils.R")
  
  setwd(FolderScripts)
  source("test-python-silho.R")
  
  
  if(parameters$Config$Number.Cores == 0){
    
    cat("\n\n########################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please  #")
    cat("\n# choose a value greater than or equal to 1.           #")
    cat("\n########################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(parameters$Config$Number.Cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(parameters$Config$Number.Cores==1){
      cat("\n\n######################################################")
      cat("\n# Running Sequentially!                              #")
      cat("\n######################################################\n\n")
    } else {
      cat("\n\n#############################################################################")
      cat("\n# Running in parallel with ", parameters$Config$Number.Cores, " cores! #")
      cat("\n#############################################################################\n\n")
    }
  }
  cl = cl
  
  cat("\n\n#######################################################")
  cat("\n# RUN RF: Get labels                                  #")
  cat("\n#######################################################\n\n")
  arquivo = paste(parameters$Folders$folderNamesLabels, "/" ,
                  dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  parameters$Config$NamesLabels = namesLabels
  
  
  cat("\n\n#########################################################")
  cat("\n# RUN python: Get the label space                         #")
  cat("\n#########################################################\n\n")
  timeLabelSpace = system.time(resLS <- labelSpace(parameters))
  parameters$LabelSpace = resLS
  
  
  cat("\n\n###################################################")
  cat("\n# RUN python: Get all partitions                    #")
  cat("\n###################################################\n\n")
  timeAllPartitions = system.time(resAP <- get.all.partitions(parameters))
  parameters$All.Partitions = resAP
  
  
  if(parameters$Config$Criteria=="maf1"){
    # 
    # cat("\n\n##########################################################")
    # cat("\n# RUN python MACRO-F1: Build and Test Partitions           #")
    # cat("\n##########################################################\n\n")
    # timeBuild = system.time(resBuild <- build.python.maf1(parameters))
    # 
    # 
    # cat("\n\n#########################################################")
    # cat("\n# RUN python MACRO-F1: Matrix Confusion                   #")
    # cat("\n#########################################################\n\n")
    # timePreds = system.time(resGather <- gather.preds.python.maf1(parameters))
    # 
    # 
    # cat("\n\n##########################################################")
    # cat("\n# RUN python MACRO-F1: Evaluation                          #")
    # cat("\n##########################################################\n\n")
    # timeEvaluate = system.time(resEval <- evaluate.python.maf1(parameters))
    # 
    # 
    # cat("\n\n##########################################################")
    # cat("\n# RUN python MACRO-F1: Mean 10 Folds                       #")
    # cat("\n##########################################################\n\n")
    # timeGather = system.time(resGE <- gather.eval.python.maf1(parameters))
    # 
    # 
    # cat("\n\n#########################################################")
    # cat("\n# RUN python MACRO-F1: Save Runtime                       #")
    # cat("\n#########################################################\n\n")
    # timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
    #                      timeBuild, timePreds,
    #                      timeEvaluate, timeGather)
    # setwd(parameters$Folders$folderTested)
    # write.csv(timesExecute, "Run-Time-Maf1-python.csv")
    
    
  } else if(parameters$Config$Criteria=="mif1"){
    # 
    # cat("\n\n##########################################################")
    # cat("\n# RUN python MICRO-F1: Build and Test Partitions           #")
    # cat("\n##########################################################\n\n")
    # timeBuild = system.time(resBuild <- build.python.mif1(parameters))
    # 
    # 
    # cat("\n\n#########################################################")
    # cat("\n# RUN python MICRO-F1: Matrix Confusion                   #")
    # cat("\n#########################################################\n\n")
    # timePreds = system.time(resGather <- gather.preds.python.mif1(parameters))
    # 
    # 
    # cat("\n\n##########################################################")
    # cat("\n# RUN python MICRO-F1: Evaluation                          #")
    # cat("\n##########################################################\n\n")
    # timeEvaluate = system.time(resEval <- evaluate.python.mif1(parameters))
    # 
    # 
    # cat("\n\n##########################################################")
    # cat("\n# RUN python MICRO-F1: Mean 10 Folds                       #")
    # cat("\n##########################################################\n\n")
    # timeGather = system.time(resGE <- gather.eval.python.mif1(parameters))
    # 
    # 
    # cat("\n\n#########################################################")
    # cat("\n# RUN python MICRO-F1: Save Runtime                       #")
    # cat("\n#########################################################\n\n")
    # timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
    #                      timeBuild, timePreds,
    #                      timeEvaluate, timeGather)
    # setwd(parameters$Folders$folderTested)
    # write.csv(timesExecute, "Run-Time-Mif1-python.csv")
    
    
    
  } else {
    
    cat("\n\n########################################################")
    cat("\n# RUN python SILHOUETTE: Build and Test Partitions       #")
    cat("\n########################################################\n\n")
    timeBuild = system.time(resBuild <- build.python.silho(parameters))
    
    
    cat("\n\n#########################################################")
    cat("\n# RUN python SILHOUETTE: Matrix Confusion                 #")
    cat("\n#########################################################\n\n")
    timePreds = system.time(resGather <- gather.preds.rf.silho(parameters))
    
    
    cat("\n\n########################################################")
    cat("\n# RUN python SILHOUETTE: Evaluation                      #")
    cat("\n########################################################\n\n")
    timeEvaluate = system.time(resEval <- evaluate.python.silho(parameters))
    
    
    cat("\n\n########################################################")
    cat("\n# RUN python SILHOUETTE: Mean 10 Folds                   #")
    cat("\n########################################################\n\n")
    timeGather = system.time(resGE <- gather.eval.python.silho(parameters))
    
    
    cat("\n\n#######################################################")
    cat("\n# RUN python SILHOUETTE: Save Runtime                   #")
    cat("\n#######################################################\n\n")
    timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
                         timeBuild, timePreds,
                         timeEvaluate, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(timesExecute, "Run-Time-Silho-Python.csv")
    
  }
  
  
  cat("\n\n##########################################################")
  cat("\n# RUN: Stop Parallel                                     #")
  cat("\n##########################################################\n\n")
  parallel::stopCluster(cl) 	
  
  cat("\n\n##########################################################")
  cat("\n# RUN: END                                               #")
  cat("\n##########################################################\n\n")
  gc()
  
  
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
