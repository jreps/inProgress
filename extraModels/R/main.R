# @file gestdiaMain.R
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

#' Trains user specified machine learning models on range of user specified datasets and then evaluates the models discriminative performance, calibration and generalizability across all datasets
#'
#' @description
#' User specifies cohortId/outcomeId and a list of databases in the OMOP CDM and then the function develops and evaluates numerous risk prediction models to predict the outcome in the cohort population.
#'
#' @details
#' This function uses the data in the CDM to develop and evaluate a range of machine leanring techniques for risk prediction.
#' The cohorts are assumed to be in a table with the same structure as the cohort table in
#' the OMOP CDM. The subject_id in this table must refer to person_ids in the CDM. One person can
#' occurr multiple times, but the combination of subject_id and cohort_start_date is assumed to be
#' unique.
#'
#'
#' @param connectionDetails         An R object of type \code{connectionDetails} created using the
#'                                  function \code{createConnectionDetails} in the
#'                                  \code{DatabaseConnector} package.
#' @param outputFolder              The directory you want the results to be stored in
#' @param cdmCohortTable            If not using an existing \code{cohort_person} temp table, what is
#'                                  the name of the source cohort table?
#' @param cdmVersion                Define the OMOP CDM version used: currently support "4" and "5".
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param cdmDatabaseList           A list of the database schema that contains the OMOP CDM
#'                                  you train and evaluate model across.  Requires read permissions to this database. On SQL
#'                                  Server, this should specifiy both the database and the schema, so
#'                                  for example 'cdm_instance.dbo'.
#' @param modelList                 A character vector specifying the machine
#'                                  learning methods to train
#'
#' @return
#' Nothing - the model evaluation is saved in the specified outputFolder
#'
#' @export

gestdiaMain <- function(connectionDetails, outputFolder=NULL,cdmCohortTable='cohort', cdmVersion="5",
                        cohortId=572, outcomeId=573,
                        cdmDatabaseList=c('cdm_truven_mdcd_v5.dbo'), modelList=c('lassLR','nnet','randomForest','gbm')){

  if(is.null(outputFolder))
    outputFolder=getwd()

  # create folder structure
  if(!dir.exists(file.path(outputFolder,'datasets','train')))
    dir.create(file.path(outputFolder,'datasets','train'), recursive=T)
  if(!dir.exists(file.path(outputFolder,'datasets','test')))
    dir.create(file.path(outputFolder,'datasets','test'), recursive=T)
  if(!dir.exists(file.path(outputFolder,'datasets','lasso')))
    dir.create(file.path(outputFolder,'datasets','lasso'), recursive=T)
  if(!dir.exists(file.path(outputFolder,'datasets','filter')))
    dir.create(file.path(outputFolder,'datasets','filter'), recursive=T)

  if(!dir.exists(file.path(outputFolder,'models')))
    dir.create(file.path(outputFolder,'models'), recursive=T)

  if(!dir.exists(file.path(outputFolder,'prediction', 'discrimination', 'plots')))
    dir.create(file.path(outputFolder,'prediction', 'discrimination', 'plots'), recursive=T)
  if(!dir.exists(file.path(outputFolder,'prediction', 'calibration', 'plots')))
    dir.create(file.path(outputFolder,'prediction', 'calibration', 'plots'), recursive=T)

  # create the model train/test sets for each database
  writeLines(paste('Extracting all the data for the databases: ',paste(cdmDatabaseList, collapse=','), sep=''))
  extractDatasets(connectionDetails, oracleTempSchema=NULL, cdmCohortTable, cdmVersion=5, cdmDatabaseList, cohortId, outcomeId, outputFolder)
  writeLines(paste('Data extracted into: ',file.path(outputFolder,'datasets'), sep=''))

  # train all the models
  writeLines('Training models')
  trainModels(cdmDatabaseList, modelList, cohortId, outcomeId, outputFolder)
  writeLines(paste0('Following models trained: ', paste(modelList, collapse=',') ))
  writeLines(paste('Models save to: ',file.path(outputFolder,'models'), sep=''))

  # save prediction across models and datasets
  writeLines('Calculating predictions across datasets')
  predictions(cdmDatabaseList, modelList, cohortId, outcomeId, outputFolder)
  writeLines(paste('Predictions save to: ',file.path(outputFolder,'prediction'), sep=''))

  # evaluate predictions
  writeLines('Evaluating Predictions')
  evaluation(outputFolder)



  writeLines('All completed...')


}
