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
predictions <- function(cdmDatabaseList, modelList, cohortId, outcomeId, outputFolder){
  h2o.conn <- h2o.init(startH2O = TRUE,nthreads = -1)

  all.res <-c()

  for (i in 1:length(cdmDatabaseList)){
    p.databaseSchema <-  cdmDatabaseList[i]

    # load the test set
    plpData <- PatientLevelPrediction::loadPlpData(file.path(outputFolder,'datasets','test',
                                                  paste(strsplit(p.databaseSchema, '\\.')[[1]][1], cohortId, outcomeId, sep='_')
    ))
    for (j in 1:length(modelList)){

      for (k in 1:length(cdmDatabaseList)){
        t.databaseSchema <-  cdmDatabaseList[k]

        writeLines(paste('Applying ', modelList[j], ' trained on ', t.databaseSchema, ' to data from ',p.databaseSchema, sep=''))
        result <- predictProbabilities(h2o.conn,plpData,modelList[j], t.databaseSchema,p.databaseSchema,outputFolder)
        writeLines(paste('Prediction complete for  ', modelList[j], ' trained on ', t.databaseSchema, ' applied to data from ',p.databaseSchema, sep=''))


        roc.result <- pROC::roc(result$y, result$value)
        saveRDS(roc.result, file.path(outputFolder, 'prediction','discrimination', 'plots',
                                      paste(paste(modelList[j],t.databaseSchema,p.databaseSchema, sep='_'),'.rds', sep='') ))
        writeLines(paste0('AUC obtained: ', roc.result$auc))

        all.res <- rbind(all.res, c(modelList[j],t.databaseSchema,p.databaseSchema,  roc.result$auc))

      }




    }
  }

  write.csv(all.res, file.path(outputFolder, 'prediction','discrimination','allresults.csv'))
  h2o.shutdown(h2o.conn, prompt = F)

}


predictProbabilities <- function(localH2O,plpData,modelType, t.databaseSchema,p.databaseSchema,outputFolder){

  pred.loc <- file.path(outputFolder, 'prediction','discrimination', paste(modelType,'_',t.databaseSchema,'_',p.databaseSchema,'.csv', sep=''))
  # load the predictiveModel:
  file.name <- file.path(outputFolder,'models',
                         paste(paste(strsplit(t.databaseSchema, '\\.')[[1]][1],modelType, plpData$metaData$cohortIds, plpData$metaData$outcomeIds, sep='_'), '.rds', sep=''))
  predictiveModel <- readRDS(file.name)


  if(modelType%in%c('lassLR','cox','poisson')){
    start <- Sys.time()
  covariates <- plpData$covariates
  cohorts <- plpData$cohorts
  outcomes <- plpData$outcomes

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

  if (modelType == "logistic" | modelType == "survival") {
    outcomes$y <- ff::ff(1, length = nrow(outcomes), vmode = "double")
  } else {
    # modelType == 'Poisson'
    outcomes$y <- outcomes$outcomeCount
  }
  outcomes <- merge(cohorts, outcomes, by = c("rowId"), all.x = TRUE)
  idx <- ffbase::is.na.ff(outcomes$y)
  idx <- ffbase::ffwhich(idx, idx == TRUE)
  outcomes$y <- ff::ffindexset(x = outcomes$y,
                               index = idx,
                               value = ff::ff(0, length = length(idx), vmode = "double"))

    prediction <- PatientLevelPrediction::predictFfdf(predictiveModel$coefficients,
                                                      outcomes,
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

    nonSparse.loc <- sparseToMatPred(plpData,t.databaseSchema,p.databaseSchema,outputFolder )
    start <- Sys.time()
    # load h2o
    #localH2O <- h2o.init(startH2O = TRUE,nthreads = -1)

    #load data from file:
    h.data <- h2o.importFile(localH2O,path = nonSparse.loc, header=T)
    rowIds <- as.data.frame(h.data[,h2o.colnames(h.data)=='rowId'])
    h.data <- h.data[,!h2o.colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]

    x.names <- (1:ncol(h.data))[!h2o.colnames(h.data)%in%c('y','rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]
    observed <- as.data.frame(h.data$y)

    # load model
    model <- h2o.loadModel(predictiveModel$model.loc)

    # use predict function
    prediction <- h2o.predict(object = model, newdata = h.data[,x.names])
    prediction <- data.frame(y=observed, value=as.data.frame(prediction[,'p1']))
    colnames(prediction)[colnames(prediction)=='p1'] <- 'value'
    # return in same format as plp prediction
    prediction$time <- NULL
    attr(prediction, "modelType") <- predictiveModel$modelType
    attr(prediction, "cohortId") <- predictiveModel$cohortId
    attr(prediction, "outcomeId") <- predictiveModel$outcomeId

    saveRDS(prediction, pred.loc)

    #h2o.shutdown(localH2O, prompt = F)

    delta <- Sys.time() - start
    writeLines(paste("Prediction took", signif(delta, 3), attr(delta, "units")))
    return(prediction)
  }

  if(modelType%in%c('nnet')){
    nonSparse.loc <- sparseToMatPred(plpData,t.databaseSchema,p.databaseSchema,outputFolder )
    start <- Sys.time()
    h.data <- read.csv(nonSparse.loc, header=T)
    model <- readRDS(predictiveModel$model.loc)

    prediction <- caret::predict.train(model, h.data[,colnames( h.data)%in%colnames(model$trainingData)&!colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')], type='prob')$Outcome

    ##prediction <- data.frame(rowId=h.data$rowId, value=prediction)
    ##prediction <- merge(ff::as.ram(outcomes), prediction, by = "rowId", all.y = TRUE)
    y <- rep(1, nrow(h.data))
    y[is.na(h.data$outcomeCount)] <- 0
    writeLines(paste(sum(y==1)))
    writeLines(paste(sum(y==0)))
    prediction <- data.frame(y, value=prediction)

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
    nonSparse.loc <- sparseToMatPred(plpData,t.databaseSchema,p.databaseSchema,outputFolder )
    start <- Sys.time()
    h.data <- read.csv(nonSparse.loc, header=T)
    model <- readRDS(predictiveModel$model.loc)

    prediction <- predict(model, h.data[,!colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')])[,'1']
    ##prediction <- data.frame(rowId=h.data$rowId, value=prediction)
    ##prediction <- merge(ff::as.ram(outcomes), prediction, by = "rowId", all.y = TRUE)

    y <- rep(1, nrow(h.data))
    y[is.na(h.data$outcomeCount)] <- 0
    writeLines(sum(y==1))
    writeLines(sum(y==0))
    prediction <- data.frame(y, value=prediction)

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






sparseToMatPred <- function(plpData,  t.databaseSchema, p.databaseSchema,outputFolder){
  data.loc <- file.path(outputFolder, 'datasets','lasso',paste('pred_t_',t.databaseSchema,'_p_',p.databaseSchema,'_',plpData$metaData$cohortIds,'_',plpData$metaData$outcomeIds,'.csv', sep=''))

  # check if the data has already been created for cohortId, outcomeId and cdmDatabaseSchema:
  if(!file.exists(data.loc)){
    # first extract cohort/outcome data
    if (is.null(cohortId) && length(plpData$metaData$cohortIds) != 1){
      stop("No cohort ID specified, but multiple cohorts found")
    }
    if (is.null(outcomeId) && length(plpData$metaData$outcomeIds) != 1){
      stop("No outcome ID specified, but multiple outcomes found")
    }
    if (!is.null(cohortId) && !(cohortId %in% plpData$metaData$cohortIds)){
      stop("Cohort ID not found")
    }
    if (!is.null(outcomeId) && !(outcomeId %in% plpData$metaData$outcomeIds)){
      stop("Outcome ID not found")
    }

    start <- Sys.time()

    covariates <- plpData$covariates
    cohorts <- plpData$cohorts
    outcomes <- plpData$outcomes

    if (!is.null(cohortId) && length(plpData$metaData$cohortIds) > 1) {
      # Filter by cohort ID:
      t <- cohorts$cohortId == cohortId
      if (!ffbase::any.ff(t)) {
        stop(paste("No cohorts with cohort ID", cohortId))
      }
      cohorts <- cohorts[ffbase::ffwhich(t, t == TRUE), ]

      idx <- ffbase::ffmatch(x = covariates$rowId, table = cohorts$rowId)
      idx <- ffbase::ffwhich(idx, !is.na(idx))
      covariates <- covariates[idx, ]

      # No need to filter outcomes since we'll merge outcomes with cohorts later
    }

    if (!is.null(outcomeId) && length(plpData$metaData$outcomeIds) > 1) {
      # Filter by outcome ID:
      t <- outcomes$outcomeId == outcomeId
      if (!ffbase::any.ff(t)) {
        stop(paste("No outcomes with outcome ID", outcomeId))
      }
      outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE), ]
    }

    if (!is.null(plpData$exclude) && nrow(plpData$exclude) != 0) {
      # Filter subjects with previous outcomes:
      if (!is.null(outcomeId)) {
        exclude <- plpData$exclude
        t <- exclude$outcomeId == outcomeId
        if (ffbase::any.ff(t)){
          exclude <- exclude[ffbase::ffwhich(t, t == TRUE), ]

          t <- ffbase::ffmatch(x = cohorts$rowId, table = exclude$rowId, nomatch = 0L) > 0L
          if (ffbase::any.ff(t)) {
            cohorts <- cohorts[ffbase::ffwhich(t, t == FALSE), ]
          }

          t <- ffbase::ffmatch(x = covariates$rowId, table = exclude$rowId, nomatch = 0L) > 0L
          if (ffbase::any.ff(t)) {
            covariates <- covariates[ffbase::ffwhich(t, t == FALSE), ]
          }

          # No need to filter outcomes since we'll merge outcomes with cohorts later
        }
      }
    }


    outcomes$y <- ff::ff(1, length = nrow(outcomes), vmode = "double")

    # Merge outcomes with cohorts so we also have the subjects with 0 outcomes:
    outcomes <- merge(cohorts, outcomes, by = c("rowId"), all.x = TRUE)
    idx <- ffbase::is.na.ff(outcomes$y)
    idx <- ffbase::ffwhich(idx, idx == TRUE)
    outcomes$y <- ff::ffindexset(x = outcomes$y,
                                 index = idx,
                                 value = ff::ff(0, length = length(idx), vmode = "double"))

    fullWindowLength <- ffbase::max.ff(plpData$cohorts$time)
    t <- outcomes$y != 0 | outcomes$time == fullWindowLength
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE),]

    idx <- ffbase::ffmatch(x = covariates$rowId, table = outcomes$rowId)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    covariates <- covariates[idx, ]


    # need to select the features choosen by lasso
    coef.file <- file.path(outputFolder,'models',
                           paste(paste(strsplit(t.databaseSchema, '\\.')[[1]][1],'lassLR', cohortId, outcomeId, sep='_'), '.csv', sep=''))
    coefs.detail <- read.csv(coef.file)
    coefs.unfiltered <- coefs.detail$id[!coefs.detail$id==0]

    # now reduce data
    t <- ff::as.ram(covariates$covariateId)%in%plpData$covariateRef$covariateId[plpData$covariateRef[,1]%in%coefs.detail$id]
    covariates <- covariates[ffbase::ffwhich(ff::as.ff(t), t==TRUE ),]
    ##plpData$covariates <- merge(plpData$covariates, coefs.unfiltered, by='covariateId')

    # make sure all features are included:
    all.covs <- plpData$covariateRef$covariateId[plpData$covariateRef[,1]%in%coefs.detail$id]
    miss.covs <- all.covs[!all.covs%in%unique(ff::as.ram(covariates$covariateId))]

    if(length(miss.covs)>1){
      extras <- ff::as.ffdf(data.frame(rowId=rep(-1,length(miss.covs)), covariateId=miss.covs,
                         covariateValue=rep(1,length(miss.covs))))
      ffbase::ffdfappend(covariates,extras)
    }

    # once reduced now cast into matrix format
    covariates <- reshape2::dcast(ff::as.ram(covariates), rowId ~covariateId, fill=0)
    covariates <- covariates[covariates$rowId!=-1,]
    # now merge covariates with outcomes
    all.data <- merge(covariates, outcomes, by='rowId', all.x=T)

    # save data to be used by other non-sparse models
    write.csv(all.data, data.loc, row.names=F)
  }

  return(data.loc)

}
