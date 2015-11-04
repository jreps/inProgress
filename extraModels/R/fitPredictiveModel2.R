# @file fitPredictiveModel2.R
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
#' This function applies the model training and pre-processing (feature selection)
#'
#' @details
#' The function picks the specified model and trains it using the data
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param databaseSchema           The database schema that contains the OMOP CDM to train the model on
#' @param outputFolder              The directory you want the results to be stored in
#' @param modelType                 The specific machine learning methods to train
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @return
#' The train model
#'
#' @export

fitPredictiveModel2 <- function(plpData,databaseSchema, outputFolder,
                                modelType, cohortId, outcomeId){

  data.loc <- sparseToMat(plpData,databaseSchema,databaseSchema,outputFolder)

  if(modelType=="nnet"){
    all.data <- read.csv(data.loc, header=T)
    # now apply carets cross validation grid search to train neural network
    start <- Sys.time()

    fitControl <- trainControl(## 5-fold CV
      method = "repeatedcv",
      number = 5,
      summaryFunction = twoClassSummary,
      repeats = 1,
      classProbs=T)

    class.lab <- factor(all.data$y)
    levels(class.lab) <- c('None','Outcome')
    nnet.model <- caret::train(as.matrix(all.data[,!colnames(all.data)%in%c('rowId','y','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]),
                                class.lab,
                                method = "nnet",
                                preProcess = NULL,
                                weights = NULL,
                                metric = 'ROC',
                                MaxNWts = 8000,
                                maximize = TRUE,
                                trControl = fitControl,
                                tuneGrid = expand.grid(.size=c(3,5,10),.decay=c(0,0.001,0.1))
                               )

    train.auc <- nnet.model$results$ROC[nnet.model$results['size']==nnet.model$bestTune['size'] &
                                          nnet.model$results['decay']==nnet.model$bestTune['decay']]

    # save the model
    save.loc <- file.path(outputFolder, 'models', paste(paste('raw',strsplit(databaseSchema, '\\.')[[1]][1],'nnet', cohortId, outcomeId, sep='_'), '.rds', sep=''))
    saveRDS(nnet.model, save.loc)

    # finally return
    trainSetStatistics <- list(numberOfPeriods = nrow(outcomes),
                               numberOfPeriodsWithOutcomes = ffbase::sum.ff(outcomes$y != 0),
                               numberOfOutcomes = ffbase::sum.ff(outcomes$y),
                               cvAUC = train.auc,
                               modeldetails =nnet.model$results)
    predictiveModel <- list(cohortId = cohortId,
                            outcomeId = outcomeId,
                            modelType = 'nnet',
                            removeDropouts = TRUE,
                            model.loc = save.loc,
                            databaseSchema =databaseSchema,
                            trainSetStatistics = trainSetStatistics)
    class(predictiveModel) <- append(class(predictiveModel), "predictiveModel")
    delta <- Sys.time() - start
    writeLines(paste("Fitting nnet model took", signif(delta, 3), attr(delta, "units")))
    return(predictiveModel)
  }

  if(modelType=="decTree"){
    # now apply carets cross validation grid search to train neural network
    start <- Sys.time()

    rpart.model <-rpart::rpart(y~., method = 'class', data =all.data[,!colnames(all.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')])

    # save model
    save.loc <- file.path(outputFolder, 'models', paste(paste('raw',strsplit(databaseSchema, '\\.')[[1]][1],'decTree', cohortId, outcomeId, sep='_'), '.rds', sep=''))
    saveRDS(rpart.model, save.loc)

    # finally return
    trainSetStatistics <- list(numberOfPeriods = nrow(outcomes),
                               numberOfPeriodsWithOutcomes = ffbase::sum.ff(outcomes$y != 0),
                               numberOfOutcomes = ffbase::sum.ff(outcomes$y),
                               cvAUC = NULL,
                               modeldetails = rpart.model$call)
    predictiveModel <- list(cohortId = cohortId,
                            outcomeId = outcomeId,
                            modelType = 'decTree',
                            removeDropouts = TRUE,
                            model.loc = save.loc,
                            databaseSchema=databaseSchema,
                            trainSetStatistics = trainSetStatistics)
    class(predictiveModel) <- append(class(predictiveModel), "predictiveModel")
    delta <- Sys.time() - start
    writeLines(paste("Fitting nnet model took", signif(delta, 3), attr(delta, "units")))
    return(predictiveModel)
  }


 if(modelType=="randomForest"){
    # now initial h2o and load data
    localH2O <- h2o.init(startH2O = TRUE,nthreads = -1)

    #load data from file:
    h.data <- h2o.importFile(localH2O,path = file.path(outputFolder, 'models',paste(databaseSchema,'_',cohortId,'_',outcomeId,'.csv', sep='')),
                             header=T)
    h.data <- h.data[,!colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]

    x.names <- (1:ncol(h.data))[!colnames(h.data)%in%c('y','rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]
    y.names <- (1:ncol(h.data))[colnames(h.data)%in%c('y')]
    h.data$y <- as.factor(h.data$y)

    # train model
    rf.model <-    h2o.randomForest(x=x.names, y=y.names, h.data,
                                    mtries = -1, sample_rate = 0.8, build_tree_one_node = FALSE,
                                    ntrees = 500, max_depth = 6, min_rows = 1, nbins = 20,
                                    nbins_cats = 1024, binomial_double_trees = FALSE,
                                    balance_classes = FALSE, max_after_balance_size = 5,
                                    offset_column = NULL, weights_column = NULL, nfolds = 5,
                                    fold_column = NULL, fold_assignment = c("AUTO"),
                                    keep_cross_validation_predictions = FALSE)

    model.detail <- rf.model@model
    auc.train<- h2o.auc(rf.model@model$cross_validation_metrics)

    # save model
    filename=paste('file:///',file.path(outputFolder,'models'), '/h2o', sep='' )
    save.loc <- h2o.saveModel(object = rf.model, filename)

    # save in plp format:

    # finally return
    h2o.shutdown(localH2O, prompt = F)

    # finally return
    trainSetStatistics <- list(numberOfPeriods = nrow(outcomes),
                               numberOfPeriodsWithOutcomes = ffbase::sum.ff(outcomes$y != 0),
                               numberOfOutcomes = ffbase::sum.ff(outcomes$y),
                               trainingAUC = auc.train,
                               modeldetails = model.detail)
    predictiveModel <- list(cohortId = cohortId,
                            outcomeId = outcomeId,
                            modelType = 'randomForest',
                            removeDropouts = TRUE,
                            model.loc = save.loc,
                            databaseSchema=databaseSchema,
                            trainSetStatistics = trainSetStatistics)
    class(predictiveModel) <- append(class(predictiveModel), "predictiveModel")
    delta <- Sys.time() - start
    writeLines(paste("Fitting randomForest model took", signif(delta, 3), attr(delta, "units")))
    return(predictiveModel)

  }


  if(modelType=="gbm"){
    # now initial h2o and load data
    localH2O <- h2o.init(startH2O = TRUE,nthreads = -1)

    #load data from file:
    h.data <- h2o.importFile(localH2O,path = file.path(outputFolder, 'models',paste(databaseSchema,'_',cohortId,'_',outcomeId,'.csv', sep='')),
                             header=T)
    h.data <- h.data[,!colnames(h.data)%in%c('rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]

    x.names <- (1:ncol(h.data))[!colnames(h.data)%in%c('y','rowId','cohortId','outcomeId', 'outcomeCount', 'timeToEvent')]
    y.names <- (1:ncol(h.data))[colnames(h.data)%in%c('y')]
    h.data$y <- as.factor(h.data$y)

    # train model
    max.auc <- 0
    model.detail <- c()
    for(ntree in seq(5,50,5)){
      gbm.model <- h2o.gbm(x=x.names, y=y.names, h.data,
                           nfolds = 5,
                           ntrees = ntree,
                           max_depth = 6
      )
      auc.train<- h2o.auc(gbm.model@model$cross_validation_metrics)
      if(auc.train>max.auc){
        model.detail <- gbm.model@model
        max.auc <- auc.train}
    }
    auc.train <- max.auc
    # save model
    filename=paste('file:///',file.path(outputFolder,'models'), '/h2o', sep='' )
    save.loc <- h2o.saveModel(object = gbm.model, filename)

    # save in plp format:

    # finally return
    h2o.shutdown(localH2O, prompt = F)

    # finally return
    trainSetStatistics <- list(numberOfPeriods = nrow(outcomes),
                               numberOfPeriodsWithOutcomes = ffbase::sum.ff(outcomes$y != 0),
                               numberOfOutcomes = ffbase::sum.ff(outcomes$y),
                               trainingAUC = auc.train,
                               modeldetails = model.detail)
    predictiveModel <- list(cohortId = cohortId,
                            outcomeId = outcomeId,
                            modelType = 'gbm',
                            removeDropouts = TRUE,
                            model.loc = save.loc,
                            databaseSchema=databaseSchema,
                            trainSetStatistics = trainSetStatistics)
    class(predictiveModel) <- append(class(predictiveModel), "predictiveModel")
    delta <- Sys.time() - start
    writeLines(paste("Fitting gradient boosting machine model took", signif(delta, 3), attr(delta, "units")))
    return(predictiveModel)

  }








  if(modelType=="stacking"){
    # split training data

    #train all other models

    # train combination

    # save as plpPrediction - list of basic models, then ensemble model

  }



}




#' Converts the sparse data into a matrix
#'
#' @description
#' This function performs wrapper feature selection (using the features choosen by lasso logistic regression)
#' and then converts the sparse represented data into matrix form
#'
#' @details
#' The function performs feature selection on the plpData by finding the feature choosen by the lasso
#' logistic regression using the training data extracted from the t.databaseSchema and then selecting
#' all the training data extracted from the p.databaseSchema where the features correspond to the ones
#' returned by feature selection.  The data are then transformed from sparse format to a matrix with
#' rows corresponding to people and columns corresponding to features, the matrix is saved in the outputFolder.
#'
#' @param plpData                   The extracted cohort, outcome and covariate data stored as a list with metadata
#' @param t.databaseSchema          The database schema that contains the OMOP CDM to train the model on
#' @param t.databaseSchema          The database schema that contains the OMOP CDM to predict the model on
#' @param outputFolder              The directory you want the results to be stored in
#' @return
#' The file location to the matrix
#'
#' @export

sparseToMat <- function(plpData, t.databaseSchema,p.databaseSchema,outputFolder){
  data.loc <- file.path(outputFolder, 'datasets','lasso',paste('t_',t.databaseSchema,'p_',p.databaseSchema,'_',plpData$metaData$cohortIds,'_',plpData$metaData$outcomeIds,'.csv', sep=''))

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
    coefs.detail <- write.csv(coef.file)
    coefs.unfiltered <- coefs.detail$id

    # now reduce data
    t <- as.ram(covariates$covariateId)%in%plpData$covariateRef$covariateId[plpData$covariateRef[,1]%in%coefs.detail$id]
    covariates <- covariates[ffbase::ffwhich(as.ff(t), t==TRUE ),]
    ##plpData$covariates <- merge(plpData$covariates, coefs.unfiltered, by='covariateId')

    # once reduced now cast into matrix format
    covariates <- reshape2::dcast(as.ram(covariates), rowId ~covariateId, fill=0)

    # now merge covariates with outcomes
    all.data <- merge(covariates, outcomes, by='rowId', all.x=T)

    # save data to be used by other non-sparse models
    write.csv(all.data, data.loc, row.names=F)
  }

  return(data.loc)

}
