% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/trainModels.R
\name{nnet}
\alias{nnet}
\title{nnet: Trains a neural model using cross-validation}
\usage{
nnet(plpdata, databaseSchema, cohortId, outcomeId, outputFolder)
}
\arguments{
\item{databaseSchema}{The database schema that contains the OMOP CDM to train the model on}

\item{cohortId}{The ID of the cohort in the cohort table for which we want to
build covariates.}

\item{outcomeId}{The ID of the outcome cohort in the cohort table for which we want to
predict the occurance of.}

\item{outputFolder}{The directory you want the results to be stored in}

\item{plpData}{The extracted cohort, outcome and covariate data stored as a list with metadata}
}
\value{
Nothing - the trained model and details are saved in the specified outputFolder
}
\description{
This function trains a neural network using the caret package
}
\details{
The hyper-parameters are selected using 5 fold cross validation with a grid search
}

