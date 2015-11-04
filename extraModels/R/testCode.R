.testCode <- function(){

  modelSettings <- c('lassoLR','nnet','decTree', 'randomForest','gbm','stacker','cox','poisson')

  cdmDatabases <- c("cdm_truven_mdcd_v5.dbo",...)
  cohortId <- 572
  outcomeId <- 573
  cdmCohortTable <- "cohort"
  cdmVersion <- "5"

  dbms <- "pdw"
  user <- NULL
  pw <- NULL
  server <- "JRDUSAPSCTL01"
  port <- 17001
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)

  options(fftempdir = "s:/FFtemp")
  outputFolder <- "S:/temp/extraModels"


  gestdiaMain(connectionDetails, outputFolder,
              cdmDatabases, cohortId, outcomeId, cdmVersion,
              modelList)




}




