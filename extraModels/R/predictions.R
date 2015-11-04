# @file predictions.R
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

#' Calculates the model predictions on the test set
#'
#' @description
#' Calculates the model predictions on the test set
#'
#' @details
#' This function calculates the risk predictions on the test set
#'
#' @param modelList                 A character vector specifying the machine
#'                                  learning methods to train
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#' @param quiet                     Should the function output its progression (TRUE=yes)?
#'
#' @return
#' Nothing - the model evaluation is saved in the specified outputFolder
#'
#' @export
predictions <- function(cdmDatabaseList, modelList, cohortId, outcomeId, outputFolder, quiet=F){

  all.res <-c()

  for (i in 1:length(cdmDatabaseList)){
    p.databaseSchema <-  cdmDatabaseList[i]

    # load the test set
    plpData <- PatientlevelPrediction::loadPlpData(file.path(outputFolder,'datasets','test',
                                                  paste(strsplit(p.databaseSchema, '\\.')[[1]][1], cohortId, outcomeId, sep='_')
    ))
    for (j in 1:length(modelList)){

      for (k in 1:length(cdmDatabaseList)){
        t.databaseSchema <-  cdmDatabaseList[k]

        writeLines(paste('Applying ', modelList[j], ' trained on ', t.databaseSchema, ' to data from ',p.databaseSchema, sep=''))
        result <- predictProbabilities(plpData,modelList[j], t.databaseSchema,p.databaseSchema,outputFolder)
        writeLines(paste('Prediction complete for  ', modelList[j], ' trained on ', t.databaseSchema, ' applied to data from ',p.databaseSchema, sep=''))


        roc.result <- pROC::roc(result$outcomeCount, result$value)
        saveRDS(roc.result, file.path(outputFolder, 'prediction','discrimination', 'plots',
                                      paste(paste(modelList[j],t.databaseSchema,p.databaseSchema, sep='_'),'.rds', sep='') ))
        writeLines(paste0('AUC obtained: ', roc.result))

        all.res <- rbind(all.res, c(modelList[j],t.databaseSchema,p.databaseSchema,  roc.result))

      }




    }
  }

  write.csv(all.res, file.path(outputFolder, 'prediction','discrimination','allresults.csv'))

}


predictProbabilities <- function(plpData,modelType, t.databaseSchema,p.databaseSchema,outputFolder){

  pred.loc <- file.path(outputFolder, 'prediction','discrimination', paste(modelType,'_',t.databaseSchema,'_',p.databaseSchema,'.csv', sep=''))
  # load the predictiveModel:
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(t.databaseSchema, '\\.')[[1]][1],modelType, plpData$metaData$cohortIds, plpData$metaData$outcomeIds, sep='_'), '.rds', sep=''))
  predictiveModel <- loadRDS(file.name)

  start <- Sys.time()

  covariates <- plpData$covariates
  cohorts <- plpData$cohorts

  if (length(plpData$metaData$cohortIds) > 1) {
    # Filter by cohort ID:
    cohortId <- predictiveModel$cohortId
    t <- cohorts$cohortId == cohortId
    if (!ffbase::any.ff(t)) {
      stop(paste("No cohorts with cohort ID", cohortId))
    }
    cohorts <- cohorts[ffbase::ffwhich(t, t == TRUE), ]

    idx <- ffbase::ffmatch(x = covariates$rowId, table = cohorts$rowId)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    covariates <- covariates[idx, ]
  }

  if (!is.null(plpData$exclude) && nrow(plpData$exclude) != 0) {
    # Filter subjects with previous outcomes:
    exclude <- plpData$exclude
    outcomeId <- predictiveModel$outcomeId
    t <- exclude$outcomeId == outcomeId
    if (ffbase::any.ff(t)) {
      exclude <- exclude[ffbase::ffwhich(t, t == TRUE), ]
      t <- ffbase::ffmatch(x = cohorts$rowId, table = exclude$rowId, nomatch = 0L) > 0L
      if (ffbase::any.ff(t)) {
        cohorts <- cohorts[ffbase::ffwhich(t, t == FALSE), ]
      }
      t <- ffbase::ffmatch(x = covariates$rowId, table = exclude$rowId, nomatch = 0L) > 0L
      if (ffbase::any.ff(t)) {
        covariates <- covariates[ffbase::ffwhich(t, t == FALSE), ]
      }
    }
  }

  if(is.null(modelType)){
    prediction <- PatientlevelPrediction::predictFfdf(predictiveModel$coefficients,
                                                      cohorts,
                                                      covariates,
                                                      predictiveModel$modelType)
    prediction$time <- NULL
    attr(prediction, "modelType") <- predictiveModel$modelType
    attr(prediction, "cohortId") <- predictiveModel$cohortId
    attr(prediction, "outcomeId") <- predictiveModel$outcomeId

    saveRDS(prediction, pred.loc)

    delta <- Sys.time() - start
    writeLines(paste("Prediction took", signif(delta, 3), attr(delta, "units")))
    return(prediction)
  }


  if(modelType%in%c('randomForest','gbm')){
    nonSparse.loc <- sparseToMat(plpData,t.databaseSchema,p.databaseSchema,outputFolder )
    # load h2o
    localH2O <- h2o.init(startH2O = TRUE,nthreads = -1)

    #load data from file:
    h.data <- h2o.importFile(localH2O,path = nonSparse.loc, header=T)
    rowIds <- as.data.frame(h.data[,colnames(h.data)=='rowId'])
    h.data <- h.data[,!colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]

    x.names <- (1:ncol(h.data))[!colnames(h.data)%in%c('y','rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]
    observed <- as.data.frame(h.data$y)

    # load model
    model <- h2o.loadModel(predictiveModel$model.loc)

    # use predict function
    prediction <- h2o.predict(object = model, newdata = h.data[,x.names])
    prediction <- data.frame(rowIds=rowIds, value=as.data.frame(prediction[1:10,'p1']))

    prediction <- merge(ff::as.ram(plpData$outcomes), prediction, by = "rowId", all.y = TRUE)

    # return in same format as plp prediction
    prediction$time <- NULL
    attr(prediction, "modelType") <- predictiveModel$modelType
    attr(prediction, "cohortId") <- predictiveModel$cohortId
    attr(prediction, "outcomeId") <- predictiveModel$outcomeId

    saveRDS(prediction, pred.loc)

    h2o.shutdown(localH2O, prompt = F)

    delta <- Sys.time() - start
    writeLines(paste("Prediction took", signif(delta, 3), attr(delta, "units")))
    return(prediction)
  }

  if(modelType%in%c('nnet')){
    nonSparse.loc <- sparseToMat(plpData,t.databaseSchema,p.databaseSchema,outputFolder )
    h.data <- read.csv(nonSparse.loc, header=T)
    model <- readRDS(predictiveModel$model.loc)

    prediction <- caret::predict.train(model, h.data[,!colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')], type='prob')$Outcome

    prediction <- data.frame(rowIds=h.data$rowId, value=prediction)
    prediction <- merge(ff::as.ram(plpData$outcomes), prediction, by = "rowId", all.y = TRUE)

    # return in same format as plp prediction
    prediction$time <- NULL
    attr(prediction, "modelType") <- predictiveModel$modelType
    attr(prediction, "cohortId") <- predictiveModel$cohortId
    attr(prediction, "outcomeId") <- predictiveModel$outcomeId

    saveRDS(prediction, pred.loc)

    delta <- Sys.time() - start
    writeLines(paste("Prediction took", signif(delta, 3), attr(delta, "units")))
    return(prediction)
  }

  if(modelType%in%c('decTree')){
    nonSparse.loc <- sparseToMat(plpData,t.databaseSchema,p.databaseSchema,outputFolder )
    h.data <- read.csv(nonSparse.loc, header=T)
    model <- readRDS(predictiveModel$model.loc)

    prediction <- predict(model, h.data[,!colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')])[,'1']
    prediction <- data.frame(rowIds=h.data$rowId, value=prediction)
    prediction <- merge(ff::as.ram(plpData$outcomes), prediction, by = "rowId", all.y = TRUE)

    # return in same format as plp prediction
    prediction$time <- NULL
    attr(prediction, "modelType") <- predictiveModel$modelType
    attr(prediction, "cohortId") <- predictiveModel$cohortId
    attr(prediction, "outcomeId") <- predictiveModel$outcomeId

    saveRDS(prediction, pred.loc)

    delta <- Sys.time() - start
    writeLines(paste("Prediction took", signif(delta, 3), attr(delta, "units")))
    return(prediction)
  }

}
