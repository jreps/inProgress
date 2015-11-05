# @file trainModels.R
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
#' User specifies a list of machine learning models to use and each model is trained automatically using cross validation
#'
#' @details
#' This function trains various machine learning models including lasso logistic regression
#' neural network, random forest, gradient boosting machine and stacking.  Feature selection is applied
#' for some models to reduce the data dimensionality and improve training speed.
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
trainModels <- function(cdmDatabaseList,modelList, cohortId, outcomeId, outputFolder){
  writeLines('step 1')
  models <- modelList

  # for each database train all the specified models
  for (i in 1:length(cdmDatabaseList)){
    databaseSchema <-  cdmDatabaseList[i]
    writeLines('step 2')
    plpData <- PatientLevelPrediction::loadPlpData(file.path(outputFolder,'datasets','train',
                                                               paste(strsplit(databaseSchema, '\\.')[[1]][1], cohortId, outcomeId, sep='_')))
    writeLines(paste(length(plpData)))
    # do the lassLR to find features to use in other models
    if(!file.exists(file.path(outputFolder,'models',paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'lassLR', cohortId, outcomeId, sep='_'), '.rds', sep='')) ))
      lassLR(plpData, databaseSchema,cohortId, outcomeId, outputFolder)

    if('nnet'%in%models){
      nnet(plpData, databaseSchema,cohortId, outcomeId, outputFolder)
    }
    if('decTree'%in%models){
      decTree(plpData, databaseSchema,cohortId, outcomeId, outputFolder)
    }
    if('randomForest'%in%models){
      randomForest(plpData, databaseSchema,cohortId, outcomeId, outputFolder)
    }
    if('gbm'%in%models){
      gbm(plpData, databaseSchema,cohortId, outcomeId, outputFolder)
    }
    if('stacker'%in%models){
      stacker(plpData, databaseSchema,cohortId, outcomeId, outputFolder)
    }
    if('cox'%in%models){
      cox(plpData, databaseSchema,cohortId, outcomeId, outputFolder)
    }
    if('poisson'%in%models){
      poisson(plpData, databaseSchema, cohortId, outcomeId, outputFolder)
    }

  }


}

#' lassLR: Trains a lasso logistic regression model using cross-validation
#'
#' @description
#' This function trains a lasso logistic regression model on the data
#'
#' @details
#' This function trains a lasso logistic regression model on the data
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema            The database schema that contains the OMOP CDM to train the model on
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the trained model and details are saved in the specified outputFolder
#'
#' @export
lassLR <- function(plpdata, databaseSchema,cohortId, outcomeId, outputFolder){
  model <- PatientLevelPrediction::fitPredictiveModel(plpData,
                                             modelType = "logistic",
                                             removeDropoutsForLr = TRUE,
                                             cohortId = cohortId,
                                             outcomeId = outcomeId,
                                             prior = createPrior("laplace",
                                                                 exclude = c(0),
                                                                 useCrossValidation = TRUE),
                                             control = createControl(noiseLevel = "silent",
                                                                     cvType = "auto",
                                                                     startingVariance = 0.1))
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'lassLR', cohortId, outcomeId, sep='_'), '.rds', sep=''))
  saveRDS(model, file = file.name)

  # add coef extracttion and saving
  coefs.detail <- PatientLevelPrediction::getModelDetails(model, plpData)
  coef.file <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'lassLR', cohortId, outcomeId, sep='_'), '.csv', sep=''))
  write.csv(coefs.detail, coef.file, row.names=F, col.names=F)
}

#' Cox: Trains a lasso cox regression model using cross-validation
#'
#' @description
#' This function trains a lasso cox regression model on the data
#'
#' @details
#' This function trains a lasso cox regression model on the data
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema            The database schema that contains the OMOP CDM to train the model on
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the trained model and details are saved in the specified outputFolder
#'
#' @export
cox <- function(plpdata, databaseSchema,cohortId, outcomeId, outputFolder){
  model <- PatientLevelPrediction::fitPredictiveModel(plpData,
                                                      modelType = "survival",
                                                      removeDropoutsForLr = TRUE,
                                                      cohortId = cohortId,
                                                      outcomeId = outcomeId,
                                                      prior = createPrior("laplace",
                                                                          exclude = c(0),
                                                                          useCrossValidation = TRUE),
                                                      control = createControl(noiseLevel = "silent",
                                                                              cvType = "auto",
                                                                              startingVariance = 0.1))
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'cox', cohortId, outcomeId, sep='_'), '.rds', sep=''))
  saveRDS(model, file = file.name)

  # add coef extracttion and saving
  coefs.detail <- PatientLevelPrediction::getModelDetails(model, plpData)
  coef.file <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'cox', cohortId, outcomeId, sep='_'), '.csv', sep=''))
  write.csv(coefs.detail, coef.file)

}

#' Poisson: Trains a lasso poisson regression model using cross-validation
#'
#' @description
#' This function trains a lasso poisson regression model on the data
#'
#' @details
#' This function trains a lasso poisson regression model on the data
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema            The database schema that contains the OMOP CDM to train the model on
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the trained model and details are saved in the specified outputFolder
#'
#' @export
poisson <- function(plpdata,  databaseSchema,cohortId, outcomeId, outputFolder){
  model <- PatientLevelPrediction::fitPredictiveModel(plpData,
                                                      modelType = "poisson",
                                                      removeDropoutsForLr = TRUE,
                                                      cohortId = cohortId,
                                                      outcomeId = outcomeId,
                                                      prior = createPrior("laplace",
                                                                          exclude = c(0),
                                                                          useCrossValidation = TRUE),
                                                      control = createControl(noiseLevel = "silent",
                                                                              cvType = "auto",
                                                                              startingVariance = 0.1))
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'poisson', cohortId, outcomeId, sep='_'), '.rds', sep=''))
  saveRDS(model, file = file.name)

  # add coef extracttion and saving
  coefs.detail <- PatientLevelPrediction::getModelDetails(model, plpData)
  coef.file <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'poisson', cohortId, outcomeId, sep='_'), '.csv', sep=''))
  write.csv(coefs.detail, coef.file)

}

#' nnet: Trains a neural model using cross-validation
#'
#' @description
#' This function trains a neural network using the caret package
#'
#' @details
#' The hyper-parameters are selected using 5 fold cross validation with a grid search
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema            The database schema that contains the OMOP CDM to train the model on
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the trained model and details are saved in the specified outputFolder
#'
#' @export
nnet <- function(plpData, databaseSchema,cohortId, outcomeId, outputFolder){
  model <- fitPredictiveModel2(plpData,
                              databaseSchema,outputFolder,
                              modelType = "nnet",
                              cohortId = cohortId,
                              outcomeId = outcomeId)
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'nnet', cohortId, outcomeId, sep='_'), '.rds', sep=''))
  saveRDS(model, file = file.name)
}

#' randomForest: Trains a randomForest model
#'
#' @description
#' This function trains a neural network using the h2o package
#'
#' @details
#' ...
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema            The database schema that contains the OMOP CDM to train the model on
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the trained model and details are saved in the specified outputFolder
#'
#' @export
randomForest <- function(plpdata, databaseSchema,cohortId, outcomeId, outputFolder){
  model <- fitPredictiveModel2(plpData,
                               databaseSchema,outputFolder,
                               modelType = "randomForest",
                               cohortId = cohortId,
                               outcomeId = outcomeId)
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'randomForest', cohortId, outcomeId, sep='_'), '.rds', sep=''))
  saveRDS(model, file = file.name)
}

#' gbm: Trains a gradient boosting machine using cross-validation
#'
#' @description
#' This function trains a gradient boosting machine using the h2o package
#'
#' @details
#' The hyper-parameter (ntrees - number of decision tree) is selected using 5 fold cross validation with a grid search
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema            The database schema that contains the OMOP CDM to train the model on
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the trained model and details are saved in the specified outputFolder
#'
#' @export
gbm <- function(plpdata, databaseSchema,cohortId, outcomeId, outputFolder){
  model <- fitPredictiveModel2(plpData,
                               databaseSchema,outputFolder,
                               modelType = "gbm",
                               cohortId = cohortId,
                               outcomeId = outcomeId)
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'gbm', cohortId, outcomeId, sep='_'), '.rds', sep=''))
  saveRDS(model, file = file.name)
}

#' decTree: Trains a decision tree model
#'
#' @description
#' This function trains a decision tree using the rparts package
#'
#' @details
#' ...
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema            The database schema that contains the OMOP CDM to train the model on
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' Nothing - the trained model and details are saved in the specified outputFolder
#'
#' @export
decTree <- function(plpdata, databaseSchema,cohortId, outcomeId, outputFolder){
  model <- fitPredictiveModel2(plpData,
                               databaseSchema,outputFolder,
                               modelType = "decTree",
                               cohortId = cohortId,
                               outcomeId = outcomeId)
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(databaseSchema, '\\.')[[1]][1],'decTree', cohortId, outcomeId, sep='_'), '.rds', sep=''))
  saveRDS(model, file = file.name)
}
