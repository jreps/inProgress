.testCode <- function(){

  modelSettings <- c('lassLR','nnet','decTree', 'randomForest','gbm','stacker','cox','poisson')

  modelList <- c('lassLR','randomForest','gbm')

  cdmDatabaseList <- c("cdm_cprd_v5.dbo","cdm_jmdc_v5.dbo","cdm_truven_mdcd_v5.dbo")
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


  gestdiaMain(connectionDetails, outputFolder,cdmCohortTable='cohort', cdmVersion="5",
              cohortId=572, outcomeId=573,
              cdmDatabaseList, modelList)




}




