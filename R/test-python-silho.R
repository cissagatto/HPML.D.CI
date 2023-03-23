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
# Phd Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri       #
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


FolderRoot = "~/Labels-Chains-HPML"
FolderScripts = "~/Labels-Chains-HPML/R"


#########################################################################
#
#########################################################################
build.python.silho <- function(parameters) {
  
  f = 1
  bthpkParalel <- foreach(f = 1:number_folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\nFold: ", f)
    
    #########################################################################
    FolderRoot = "~/Labels-Chains-HPML"
    FolderScripts = "~/Labels-Chains-HPML/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    #########################################################################
    cat("\nGetting information")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold ==
                                             f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    if (dir.create(Folder.Tested.Split) == FALSE){dir.create(Folder.Tested.Split)}
    
    Folder.BP = paste(parameters$Folders$folderPartitions, "/", 
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", best.part.info.f$num.part,
                        sep = "")
    
    #########################################################################
    cat("\n\nOpen Train file")
    train.name.file = paste(parameters$Folders$folderCVTR, "/",
                            parameters$Config$Dataset.Name, "-Split-Tr-",
                            f, ".csv", sep = "")
    train.dataset = data.frame(read.csv(train.name.file))
    
    #########################################################################
    cat("\n\nOpen Test file")
    test.name.file = paste(parameters$Folders$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset = data.frame(read.csv(test.name.file))
    
    #########################################################################
    cat("\n\nOpen Validation file")
    val.name.file = paste(parameters$Folders$folderCVVL,
                          "/", parameters$Config$Dataset.Name,
                          "-Split-Vl-", f, ".csv", sep = "")
    val.dataset = data.frame(read.csv(val.name.file))
    
    
    #########################################################################
    cat("\n\nJoin validation with train")
    train.dataset.final = rbind(train.dataset, val.dataset)
    
     
    # ##################################################################
    # # EXECUTE ECC PYTHON
    # # /home/biomal/Local-Partitions/Utils/br-python
    # str.execute = paste("python3 ", 
    #                     parameters$Folders$folderUtils,
    #                     "/ecc-python/main.py ", 
    #                     train.name.file, " ",
    #                     val.name.file,  " ",
    #                     test.name.file, " ", 
    #                     parameters$DatasetInfo$AttEnd, " ",
    #                     parameters$DatasetInfo$LabelEnd, " ",
    #                     Folder.Tested.Split,
    #                     sep="")
    # 
    # # EXECUTA
    # res = print(system(str.execute))
    # 
    # if(res!=0){
    #   break
    # }
    # 
    # setwd(Folder.Tested.Split)
    # y_preds = data.frame(read.csv("y_pred.csv"))
    # y_trues = data.frame(read.csv("y_true.csv"))
    # 
    # #####################################################################
    # cat("\n\tUTIML Threshold\n")
    # utiml.threshold <- scut_threshold(y_preds, test.dataset)
    # final.predictions <- data.frame(as.matrix(fixed_threshold(y_preds, 
    #                                                           utiml.threshold)))
    # 
    # setwd(Folder.Tested.Split)
    # write.csv(final.predictions, "y_br_predict.csv", row.names = FALSE)
    # 
    # 
    # #####################################################################
    # cat("\n\tSave original and pruned predictions\n")
    # pred.o = paste(colnames(y_preds), "-pred-ori", sep="")
    # names(y_preds) = pred.o
    # 
    # pred.c = paste(colnames(final.predictions), "-pred-cut", sep="")
    # names(final.predictions) = pred.c
    # 
    # true.labels = paste(colnames(y_trues), "-true", sep="")
    # names(y_trues) = true.labels
    # 
    # all.predictions = cbind(y_preds, final.predictions, y_trues)
    # setwd(Folder.Tested.Split)
    # write.csv(all.predictions, "br-folder-predictions.csv", row.names = FALSE)
    # 
    # unlink("y_pred.csv")
    # unlink("y_true.csv")
    
    #########################################################################
    g = 1
    while(g <= best.part.info.f$num.group) {
      
      cat("\n\nPartition: ", g)
      
      #########################################################################
      cat("\nCreating folder")
      Folder.Tested.Group = paste(Folder.Tested.Split, "/Group-", g, sep="")
      if(dir.exists(Folder.Tested.Group) == FALSE){dir.create(Folder.Tested.Group)}
      
      
      #########################################################################
      cat("\nSpecific Group")
      specificGroup = data.frame(filter(partition, group == g))
      
      
      #########################################################################
      cat("\nTrain: Mount Group")
      train.attributes = train.dataset[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      train.classes = select(train.dataset, specificGroup$label)
      train.dataset.cluster = cbind(train.attributes, train.classes)
      
      
      #########################################################################
      cat("\nTrain: Save Group")
      train.name.cluster.csv = paste(Folder.Tested.Group,"/",
                                     parameters$Config$Dataset.Name, 
                                     "-split-tr-",f, "-group-",g,
                                     ".csv",sep = "")
      write.csv(train.dataset.cluster, train.name.cluster.csv, row.names = FALSE)
      
      
      #########################################################################
      cat("\nTest: Mount Group")
      test.attributes = test.dataset[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      test.classes = select(test.dataset, specificGroup$label)
      test.dataset.cluster = cbind(test.attributes, test.classes)
      
      
      #########################################################################
      cat("\nTest: Save Group")
      test.name.cluster.csv = paste(Folder.Tested.Group, "/", 
                             parameters$Config$Dataset.Name, "-split-ts-",
                             f, "-group-", g, ".csv", sep = "")
      write.csv(test.dataset.cluster, test.name.cluster.csv,
                row.names = FALSE)
      
      
      #########################################################################
      cat("\nTrain: Mount Group")
      val.attributes = val.dataset[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      val.classes = select(val.dataset, specificGroup$label)
      val.dataset.cluster = cbind(val.attributes, val.classes)
      
      
      #########################################################################
      cat("\nTrain: Save Group")
      val.name.cluster.csv = paste(Folder.Tested.Group,"/",
                                   parameters$Config$Dataset.Name, 
                                   "-split-vl-",f, "-group-",g,
                                   ".csv", sep = "")
      write.csv(val.dataset.cluster, val.name.cluster.csv, row.names = FALSE)
      
      
      ####################################
      cat("\nJunta treino com validação")
      tv.dataset.cluster = rbind(train.dataset.cluster, val.dataset.cluster)
      
      
      ########################################################################
      cat("\nIndices dos rotulos")
      fim = parameters$DatasetInfo$LabelStart + (nrow(specificGroup)-1)
      labels.indices = seq(parameters$DatasetInfo$LabelStart, fim, by=1)
      nomes.labels.clusters = specificGroup$label
      
      #########################################################################
      # start = ncol(train.attributes) + 1
      # end = ncol(train.dataset.cluster)
      
      ##################################################################
      # EXECUTE ECC PYTHON
      str.execute = paste("python3 ", parameters$Folders$folderUtils,
                          "/Python/ecc-python/main.py ", 
                          train.name.cluster.csv, " ",
                          val.name.cluster.csv, " ",
                          test.name.cluster.csv, " ", 
                          start = as.numeric(parameters$DatasetInfo$AttEnd), " ", 
                          Folder.Tested.Group,
                          sep="")
      
      # EXECUTA
      res = print(system(str.execute))
      
      if(res!=0){
        break
      }
      
      if(nrow(specificGroup)==1){
        cat("\nGrupo com um rótulo")
        
      } else {
        
        cat("\nGrupo com mais de um rótulo")
        train.mldr = mldr_from_dataframe(train.dataset.cluster, labelIndices = labels.indices)
        test.mldr = mldr_from_dataframe(test.dataset.cluster, labelIndices = labels.indices)
        val.mldr = mldr_from_dataframe(val.dataset.cluster, labelIndices = labels.indices)
        tv.mldr = mldr_from_dataframe(tv.dataset.cluster, labelIndices = labels.indices)
        
        ##############################################
        cat("\nPropriedades dos clusters")
        properties.clusters(nomes.labels.clusters,
                            fold = f,
                            cluster = g,
                            folderSave = Folder.Tested.Group,
                            labels.indices,
                            train = train.dataset.cluster,
                            test = test.dataset.cluster,
                            val = val.dataset.cluster,
                            tv = tv.dataset.cluster)
        
      } # FIM DO IF ELSE
      
      setwd(Folder.Tested.Group)
      unlink(val.name.cluster.csv)
      unlink(train.name.cluster.csv)
      unlink(test.name.cluster.csv)
      
      
      g = g + 1
      gc()
    } # end grupos
    
    #f = f + 1  
    gc()
  } # ending folds
  
  gc()
  cat("\n############################################################")
  cat("\n# Python Silhouette: end build.python.silho                #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}


######################################################################
#
######################################################################
gather.preds.python.silho <- function(parameters) {
  
  f = 1
  gatherR <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
  
    cat("\nFold: ", f)
    
    FolderRoot = "~/Labels-Chains-HPML"
    FolderScripts = "~/Labels-Chains-HPML/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ###################################################################
    apagar = c(0)
    y_true = data.frame(apagar)
    y_proba = data.frame(apagar)
    y_pred = data.frame(apagar)
    
    ###################################################################
    Folder.Split.Test = paste(parameters$Folders$folderTested, 
                              "/Split-", f, sep = "")
    
    
    #########################################################################
    cat("\nGetting information about pythonters")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold ==
                                             f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    
    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    
    Folder.BP = paste(parameters$Folders$folderPartitions, "/", 
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", 
                        best.part.info.f$num.part, sep = "")
    
    
    #########################################################################
    g = 1
    while (g <= best.part.info.f $num.group) {
      cat("\n\nGroup: ", g)
      
      Folder.Group.Test = paste(Folder.Split.Test, "/Group-", g, sep = "")
      
      cat("\nGather y_true")
      setwd(Folder.Group.Test)
      y_true_gr = data.frame(read.csv("cluster_true.csv"))
      y_true = cbind(y_true, y_true_gr)
      
      setwd(Folder.Group.Test)
      cat("\nGather y_predict")
      y_proba_gr = data.frame(read.csv("cluster_proba.csv"))
      y_proba = cbind(y_proba, y_proba_gr)
      
      g = g + 1
      gc()
    }
    
    
    #########################################################################
    cat("\n\nSave files")
    y_proba = y_proba[, -1]
    y_true = y_true[, -1]
    
    nomes.rotulos = colnames(y_proba)
    
    setwd(Folder.Split.Test)
    write.csv(y_proba, "folder_proba.csv", row.names = FALSE)
    write.csv(y_true, "folder_true.csv", row.names = FALSE)
    
    
    #########################################################################
    cat("\n\nOpen Test file")
    test.name.file = paste(parameters$Folders$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset = data.frame(read.csv(test.name.file))
    
    labels.indices = seq(parameters$DatasetInfo$LabelStart,
                         parameters$DatasetInfo$LabelEnd, by=1)
    test.mldr = mldr_from_dataframe(test.dataset, labelIndices = labels.indices)
    
    y_pred <- data.frame(as.matrix(fixed_threshold(y_proba, 
                                                   threshold = 0.5)))
    
    setwd(Folder.Split.Test)
    write.csv(y_pred, "folder_pred.csv", row.names = FALSE)
    
    #####################################################################
    roc.curva(predictions = y_pred,
              probabilities = y_proba,
              test = test.mldr,
              Folder = Folder.Split.Test)
    
    
    ##############################################
    cat("\nInformações das predições")
    predictions.information(nomes.rotulos=nomes.rotulos, 
                            proba = y_proba, 
                            preds = y_pred, 
                            trues = y_true, 
                            folder = Folder.Split.Test)
    
    
    #######################################################################
    pred = paste(Folder.Split.Test, "/folder_proba.csv", sep="" )
    true = paste(Folder.Split.Test, "/folder_true.csv", sep="" )
    str.execute = paste("python3 ",
                        parameters$Folders$folderUtils,
                        "/Python/auprc.py ",
                        true, " ",
                        pred,  " ",
                        Folder.Split.Test,
                        sep="")
    
    # EXECUTA
    res = print(system(str.execute))
    
    if(res!=0){
      break
    }
    
    
    #####################################################################
    cat("\nSave original and pruned predictions")
    pred.o = paste(colnames(y_pred), "-pred", sep="")
    names(y_pred) = pred.o
    
    true.labels = paste(colnames(y_true), "-true", sep="")
    names(y_true) = true.labels
    
    proba = paste(colnames(y_proba), "-proba", sep="")
    names(y_proba) = proba
    
    all.predictions = cbind(y_proba, y_pred, y_true)
    
    setwd(Folder.Split.Test)
    write.csv(all.predictions, "folder-predictions.csv", row.names = FALSE)
    
    
    #f = f + 1
    gc()
  } # end do foreach
  
  gc()
  cat("\n#####################################################")
  cat("\n# Python Silhouette: end gather.preds.python.silho  #")
  cat("\n######################################################")
  cat("\n\n\n\n")
  
} # end da função


#######################################################################
#
#######################################################################
evaluate.python.silho <- function(parameters) {
  
  f = 1
  avalParal <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
    
    cat("\nFold: ", f)
    
    FolderRoot = "~/Labels-Chains-HPML"
    FolderScripts = "~/Labels-Chains-HPML/R"
    
    # data frame
    apagar = c(0)
    confMatPartitions = data.frame(apagar)
    partitions = c()
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    
    # get the true and predict lables
    setwd(Folder.Tested.Split)
    y_pred = data.frame(read.csv("folder_pred.csv"))
    y_true = data.frame(read.csv("folder_true.csv"))
    y_pred = data.frame(read.csv("folder_proba.csv"))
    
    # compute measures multilabel
    y_true2 = data.frame(sapply(y_true, function(x)
      as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 ,
                                  labelIndices = seq(1, ncol(y_true2)),
                                  name = "y_true2")
    
    y_pred2 = sapply(y_pred, function(x)
      as.numeric(as.character(x)))
    
    #cat("\n\t\tSave Confusion Matrix")
    setwd(Folder.Tested.Split)
    salva3 = paste("utiml-conf-mat-fold-", f, ".txt", sep = "")
    sink(file = salva3, type = "output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    # creating a data frame
    confMatPart = multilabel_evaluate(confmat)
    confMatPart = data.frame(confMatPart)
    names(confMatPart) = paste("Fold-", f, sep = "")
    namae = paste("Split-", f, "-Evaluated.csv", sep = "")
    write.csv(confMatPart, namae)
    
    
    ###############################################################
    conf.mat = data.frame(confmat$TPl, confmat$FPl,
                          confmat$FNl, confmat$TNl)
    names(conf.mat) = c("TP", "FP", "FN", "TN")
    
    
    # porcentagem
    conf.mat.perc = data.frame(conf.mat/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    
    # calculando o total de rótulos classificados errados
    wrong = conf.mat$FP + conf.mat$FN
    
    # calculando a porcentagem de rótulos classificados errados
    wrong.perc = wrong/nrow(y_true)
    
    # calculando o total de rótulos classificados corretamente
    correct = conf.mat$TP + conf.mat$TN
    
    # calculando a porcentagem de rótulos classificados corretamente
    correct.perc = correct/nrow(y_true)
    
    conf.mat = data.frame(conf.mat, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
    
    setwd(Folder.Tested.Split)
    write.csv(conf.mat, "utiml-matrix-confusion.csv")
    
    gc()
  } # end folds
  
  gc()
  cat("\n############################################################")
  cat("\n# Python Silhouette: End evaluate.python.silho             #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}




######################################################################
#
######################################################################
gather.eval.python.silho <- function(parameters) {
  
  
  ##########################################################################
  apagar = c(0)
  avaliado.final = data.frame(apagar)
  nomes = c("")
  
  
  final.proba.auc = c(0)
  final.proba.micro.auc = c(0)
  final.proba.macro.auc = c(0)
  final.proba.ma.mi.auc.fold = c(0)
  final.proba.ma.mi.auc.cluster = c(0)
  
  final.pred.micro.auc = c(0)
  final.pred.macro.auc = c(0)
  final.pred.auc = c(0)
  
  
  # from fold = 1 to number_folders
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    cat("\n#======================================================")
    cat("\n# Fold: ", f)
    cat("\n#======================================================\n")
    
    # vector with names
    measures = c("accuracy","average-precision","clp","coverage","F1",
                 "hamming-loss","macro-AUC", "macro-F1","macro-precision",
                 "macro-recall","margin-loss","micro-AUC","micro-F1",
                 "micro-precision","micro-recall","mlp","one-error",
                 "precision","ranking-loss", "recall","subset-accuracy","wlp")
    
    ##########################################################################
    # "/dev/shm/ej3-GpositiveGO/Tested/Split-1"
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep="")
    
    
    ######################################################################
    setwd(Folder.Tested.Split)
    str = paste("Split-", f, "-Evaluated.csv", sep="")
    avaliado = data.frame(read.csv(str))
    avaliado.final= cbind(avaliado.final, avaliado[,2])
    nomes[f] = paste("Fold-", f, sep="")
    
    
    #################################
    setwd(Folder.Tested.Split)
    proba.auc = data.frame(read.csv("proba-auc.csv"))
    names(proba.auc) = c("fold", "value")
    final.proba.auc = rbind(final.proba.auc, proba.auc)
    
    proba.micro.auc = data.frame(read.csv("proba-micro-auc.csv"))
    names(proba.micro.auc) = c("fold", "value")
    final.proba.micro.auc = rbind(final.proba.micro.auc, proba.micro.auc)
    
    proba.macro.auc = data.frame(read.csv("proba-macro-auc.csv"))
    names(proba.macro.auc) = c("fold", "value")
    final.proba.macro.auc = rbind(final.proba.macro.auc, proba.macro.auc)
    
    proba.ma.mi.auc.fold = data.frame(read.csv("folder_proba_ma_mi.csv"))
    final.proba.ma.mi.auc.fold = rbind(final.proba.ma.mi.auc.fold,
                                       proba.ma.mi.auc.fold)
    
    ##################
    pred.auc = data.frame(read.csv("bin-auc.csv"))
    names(pred.auc) = c("fold", "value")
    final.pred.auc = rbind(final.pred.auc, pred.auc)
    
    pred.micro.auc = data.frame(read.csv("bin-micro-auc.csv"))
    names(pred.micro.auc) = c("fold", "value")
    final.pred.micro.auc = rbind(final.pred.micro.auc, pred.micro.auc)
    
    pred.macro.auc = data.frame(read.csv("bin-macro-auc.csv"))
    names(pred.macro.auc) = c("fold", "value")
    final.pred.macro.auc = rbind(final.pred.macro.auc, pred.macro.auc)
    
    f = f + 1
    gc()
    
  } # end folds
  
  
  fold = seq(1, number_folds, by =1)
  
  final.proba.auc = final.proba.auc[-1,]
  final.proba.auc = data.frame(fold, auc = final.proba.auc$value)
  
  final.proba.micro.auc = final.proba.micro.auc[-1,]
  final.proba.micro.auc = data.frame(fold, micro.auc = final.proba.micro.auc$value)
  
  final.proba.macro.auc = final.proba.macro.auc[-1,]
  final.proba.macro.auc = data.frame(fold, macro.auc = final.proba.macro.auc$value)
  
  final.proba.ma.mi.auc.fold = final.proba.ma.mi.auc.fold[-1,]
  final.proba.ma.mi.auc.fold = data.frame(fold, final.proba.ma.mi.auc.fold)
  
  setwd(parameters$Folders$folderTested)
  write.csv(final.proba.auc, "proba-auc.csv", row.names = FALSE)  
  write.csv(final.proba.macro.auc, "proba-macro-auc.csv", row.names = FALSE)  
  write.csv(final.proba.micro.auc, "proba-micro-auc.csv", row.names = FALSE)
  write.csv(final.proba.ma.mi.auc.fold, "proba-ma-mi-auprc-fold.csv", row.names = FALSE)  
  
  #################
  final.pred.auc = final.pred.auc[-1,]
  final.pred.auc = data.frame(fold, auc = final.pred.auc$value)
  
  final.pred.micro.auc = final.pred.micro.auc[-1,]
  final.pred.micro.auc = data.frame(fold, micro.auc = final.pred.micro.auc$value)
  
  final.pred.macro.auc = final.pred.macro.auc[-1,]
  final.pred.macro.auc = data.frame(fold, macro.auc = final.pred.macro.auc$value)
  
  setwd(parameters$Folders$folderTested)
  write.csv(final.pred.auc, "bin-auc.csv", row.names = FALSE)  
  write.csv(final.pred.macro.auc, "bin-macro-auc.csv", row.names = FALSE)  
  write.csv(final.pred.micro.auc, "bin-micro-auc.csv", row.names = FALSE)  
  
  avaliado.final = avaliado.final[,-1]
  names(avaliado.final) = nomes
  avaliado.final = cbind(measures, avaliado.final)
  setwd(Folder.Tested.Split)
  write.csv(avaliado, paste("Evaluated-Fold-", f, ".csv", sep=""),
            row.names = FALSE)
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(avaliado.final[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(avaliado.final[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(avaliado.final[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(dp, "standard-deviation-10-folds.csv", row.names = FALSE)
  
  gc()
  cat("\n#############################################################")
  cat("\n# Python Silhouette: End gather.eval.python.silho          #")
  cat("\n#############################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
