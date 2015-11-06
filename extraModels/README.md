extraModels
======================

Introduction
============
An R package for building a range of machine learning methods for patient level predictive using data in Common Data Model format.

Features
========
- Takes a cohort and outcome of interest as input.
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Uses a large set of covariates including for example all drugs, diagnoses, procedures, as well as age, comorbidity indexes, etc.
- Creates models include lasso logistic regression, nerual network, random forest, gradient boosting machines and stacking
- Includes function for evaluating predictive models across multiple datasets
- Supported outcome models are classification and time to event.


Technology
==========
extraModels is an R package, with use of H2o that runs on Java.

System Requirements
===================
Requires R (version 3.1.0 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in PatientLevelPrediction require Java.

Dependencies
============
 * Cyclops
 * DatabaseConnector
 * SqlRender
 * PatientLevelPrediction
 * H2o
 * pROC
 * nnet
 * rpart

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector, SqlRender and H2o packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. Install H2o onto your computer: see [H2o](http://h2o.ai/download/)
4. In R, use the following commands to download and install extraModels:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/SqlRender") 
  install_github("ohdsi/DatabaseConnector") 
  install_github("ohdsi/OhdsiRTools") 
  install_github("ohdsi/Cyclops") 
  install_github("ohdsi/PatientLevelPrediction") 
  install_github(".../extraModels") 
  ```

Getting Involved
================
* Vignette: [Building various models for ...]()
* Package manual: [extramodels.pdf]() 
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements
 
License
=======
extraModels is licensed under Apache License 2.0

Development
===========
extraModels is being developed in R Studio.


Beta

# Acknowledgements
