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

evaluation <- function(modelSettings, cohortId, outcomeId, outputFolder){

  plotCalibration(pred.test.lassolr, plpData.split[[2]], numberOfStrata = 10, truncateFraction = 0.01, fileName = "s:/temp/calibration.png")


  pred.train.lassolr <-merge(pred.train.lassolr, plpData.split[[1]]$outcomes, by='rowId', all.x=T )
  pred.train.lassolr$outcomeCount[is.na(pred.train.lassolr$outcomeCount)] <- 0
  pred.test.lassolr <-merge(pred.test.lassolr, plpData.split[[2]]$outcomes, by='rowId', all.x=T )
  pred.test.lassolr$outcomeCount[is.na(pred.test.lassolr$outcomeCount)] <- 0


  roc.train.lassolr <- pROC::roc(pred.train.lassolr$outcomeCount, pred.train.lassolr$value)
  roc.test.lassolr <- pROC::roc(pred.test.lassolr$outcomeCount, pred.test.lassolr$value)

  plot(roc.train.lassolr, col='red')
  plot(roc.test.lassolr, col='blue', add=T)

  # create discrim table: rows as methods, columns as databases and entry as AUC

  # create calibration table: rows as methods, columns as databases and entry as AUC




}
