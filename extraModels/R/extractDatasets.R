# @file extractdatasets.R
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

#' Extracts the training/test data from a list of databases
#'
#' @description
#' User specified cohortId/outcomeId and a list of databases in the OMOP CDM and the code extract the labelled data required to develop a risk prediction model
#'
#' @details
#' This function uses the data in the CDM to extract the data consisting of a large set of features for the provided
#' cohorts. The cohorts are assumed to be in a table with the same structure as the cohort table in
#' the OMOP CDM. The subject_id in this table must refer to person_ids in the CDM. One person can
#' occurr multiple times, but the combination of subject_id and cohort_start_date is assumed to be
#' unique.
#'
#' This function is called automatically by the \code{\link{extractDatasets}} function.
#'
#' @param connectionDetails         An R object of type \code{connectionDetails} created using the
#'                                  function \code{createConnectionDetails} in the
#'                                  \code{DatabaseConnector} package.
#' @param oracleTempSchema          A schema where temp tables can be created in Oracle.
#' @param cdmCohortTable            If not using an existing \code{cohort_person} temp table, what is
#'                                  the name of the source cohort table?
#' @param cdmVersion                Define the OMOP CDM version used: currently support "4" and "5".
#' @param cdmDatabaseList           A list of the database schema that contains the OMOP CDM
#'                                  you train and evaluate model across.  Requires read permissions to this database. On SQL
#'                                  Server, this should specifiy both the database and the schema, so
#'                                  for example 'cdm_instance.dbo'.
#' @param cohortId                  The ID of the cohort in the cohort table for which we want to
#'                                  build covariates.
#' @param outcomeId                 The ID of the outcome cohort in the cohort table for which we want to
#'                                  predict the occurance of.
#' @param outputFolder              The directory you want the results to be stored in
#'
#'
#' @return
#' Nothing - the extracted data are saved in the specified outputFolder
#'
#' @export

extractDatasets <- function(connectionDetails, oracleTempSchema=NULL, cdmCohortTable, cdmVersion,
                cdmDatabaseList, cohortId, outcomeId, outputFolder){


  ### Create covariateSettings ###
  covariateSettings <- PatientLevelPrediction::createCovariateSettings(useCovariateDemographics = TRUE,
                                                                       useCovariateDemographicsGender = TRUE,
                                                                       useCovariateDemographicsRace = TRUE,
                                                                       useCovariateDemographicsEthnicity = TRUE,
                                                                       useCovariateDemographicsAge = TRUE,
                                                                       useCovariateDemographicsYear = TRUE,
                                                                       useCovariateDemographicsMonth = TRUE,
                                                                       useCovariateConditionOccurrence = TRUE,
                                                                       useCovariateConditionOccurrence365d = TRUE,
                                                                       useCovariateConditionOccurrence30d = TRUE,
                                                                       useCovariateConditionOccurrenceInpt180d = F,
                                                                       useCovariateConditionEra = F,
                                                                       useCovariateConditionEraEver = F,
                                                                       useCovariateConditionEraOverlap = F,
                                                                       useCovariateConditionGroup = TRUE,
                                                                       useCovariateConditionGroupMeddra = TRUE,
                                                                       useCovariateConditionGroupSnomed = TRUE,
                                                                       useCovariateDrugExposure = T,
                                                                       useCovariateDrugExposure365d = T,
                                                                       useCovariateDrugExposure30d = T,
                                                                       useCovariateDrugEra = F,
                                                                       useCovariateDrugEra365d = F,
                                                                       useCovariateDrugEra30d = F,
                                                                       useCovariateDrugEraOverlap = F,
                                                                       useCovariateDrugEraEver = F,
                                                                       useCovariateDrugGroup = TRUE,
                                                                       useCovariateProcedureOccurrence = TRUE,
                                                                       useCovariateProcedureOccurrence365d = TRUE,
                                                                       useCovariateProcedureOccurrence30d = TRUE,
                                                                       useCovariateProcedureGroup = TRUE,
                                                                       useCovariateObservation = TRUE,
                                                                       useCovariateObservation365d = TRUE,
                                                                       useCovariateObservation30d = TRUE,
                                                                       useCovariateObservationCount365d = TRUE,
                                                                       useCovariateMeasurement = TRUE,
                                                                       useCovariateMeasurement365d = TRUE,
                                                                       useCovariateMeasurement30d = TRUE,
                                                                       useCovariateMeasurementCount365d = TRUE,
                                                                       useCovariateMeasurementBelow = TRUE,
                                                                       useCovariateMeasurementAbove = TRUE,
                                                                       useCovariateConceptCounts = TRUE,
                                                                       useCovariateRiskScores = TRUE,
                                                                       useCovariateRiskScoresCharlson = TRUE,
                                                                       useCovariateRiskScoresDCSI = TRUE,
                                                                       useCovariateRiskScoresCHADS2 = TRUE,
                                                                       useCovariateRiskScoresCHADS2VASc = TRUE,
                                                                       useCovariateInteractionYear = F,
                                                                       useCovariateInteractionMonth = F,
                                                                       excludedCovariateConceptIds = NULL,
                                                                       includedCovariateConceptIds = c(),
                                                                       deleteCovariatesSmallCount = 100)
  for (i in 1:length(cdmDatabaseList)){
   cdmDatabaseSchema <-  cdmDatabaseList[i]

   plpData <- PatientLevelPrediction::getDbPlpData(connectionDetails = connectionDetails,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           oracleTempSchema = oracleTempSchema,
                           cohortDatabaseSchema = cdmDatabaseSchema,
                           cohortTable = cdmCohortTable,
                           cohortIds = cohortId,
                           useCohortEndDate = FALSE,
                           windowPersistence = 365*2,
                           covariateSettings = covariateSettings,
                           outcomeDatabaseSchema = cdmDatabaseSchema,
                           outcomeTable = cdmCohortTable,
                           outcomeIds = outcomeId,
                           firstOutcomeOnly = TRUE,
                           cdmVersion = cdmVersion)

   PatientLevelPrediction::savePlpData(plpData, file.path(outputFolder,'datasets',
                                  paste(strsplit(cdmDatabaseSchema, '\\.')[[1]][1], cohortId, outcomeId, sep='_')
                                  ))

   splits <- PatientLevelPrediction::splitData(plpData, splits = c(0.80,0.20))

   PatientLevelPrediction::savePlpData(splits[[1]], file.path(outputFolder,'datasets','train',
                                                          paste(strsplit(cdmDatabaseSchema, '\\.')[[1]][1], cohortId, outcomeId, sep='_')
   ))
   PatientLevelPrediction::savePlpData(splits[[2]], file.path(outputFolder,'datasets','test',
                                                              paste(strsplit(cdmDatabaseSchema, '\\.')[[1]][1], cohortId, outcomeId, sep='_')
   ))

  }




}
