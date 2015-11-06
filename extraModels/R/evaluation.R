# @file evaluation.R
#
# Copyright 2015 ...
#
# This file is part of extraModels
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Trains a list og user specified machine learning models on the data
#'
#' @description
#' This function evaluates the models across the different datasets and calculates the discriminative performance, model calibration and generalizability of each model.
#'
#' @details
#' Given the list of user specified models, the function applies each trained model on the test set for each dataset and
#' calculates the AUC comparing the prediction with the observered label.  The performance is stored in a csv and ROC plots/calibration plots
#' are created and saved.
#'
#'
#' @param modelList                 A character vector specifying the machine
#'                                  learning methods to train
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the model evaluation is saved in the specified outputFolder
#'
#' @export

evaluation <- function(outputFolder, rocOptions=list(rocPlotter=FALSE), summaryOptions=list(summaryPlotter=TRUE)){


  # load predictions and plot calibration
  ##plotCalibration(pred.test.lassolr, plpData.split[[2]], numberOfStrata = 10, truncateFraction = 0.01, fileName = "s:/temp/calibration.png")

if(rocOptions$rocPlotter==TRUE){
  rocPlotter <- function(method=NULL, trainSet=NULL, predSet=NULL){
    list.files(path = file.path(outputFolder, 'prediction',
                                'discrimination', 'plots'), pattern='gbm')

    # load rocs and do nice plot:
    first <- T
    for(roc.file in roc.plot){
      roc.plot <- readRDS( file.path(outputFolder, 'prediction','discrimination', 'plots',
                                     roc.file ))
      # extract details from file name
      if(first){plot(roc.res, .... )}
      if(!first){plot(roc.res, add=T)}
      first <- F
    }
  }
}

  if(summaryOptions$summaryPlotter==TRUE){
    allres <- read.csv(file.path(outputFolder, 'prediction','discrimination', 'allresults.csv'), header=T)
    allres <- allres[,-1]
    colnames(allres) <- c('Model','Train_set', 'Pred_set', 'AUC')
    allres$Train_set <- paste('Train: ',
                              apply(allres, 1, function(x) strsplit(x[2],'\\.')[[1]][1]))
    allres$Pred_set <- paste('Pred: ',
                              apply(allres, 1, function(x) strsplit(x[3],'\\.')[[1]][1]))

    ggplot2::ggplot(data=allres, aes(x=Model, y=AUC, fill=Model, group=Model)) +

      geom_bar(stat="identity") + #, position=position_dodge, colour='black') +
                 facet_grid(Train_set~Pred_set) +
      coord_flip() +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
            strip.text = element_text(size = 16))
  }




}
