
#' Execute Cohort method analysis
#' @description
#' Take cohort method settings and execute on CDM
#' @export
#' @param config        cdmConfiguration
#' @param cmSettings    cohortMethod settings object to execute
executeCohortMethodAnalysis <- function(config, cmSettings) {
  multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(parallel::detectCores())
  cmSettings$connectionDetails <- config$connectionDetails
  cmSettings$cdmDatabaseSchema <- config$cdmDatabaseSchema
  cmSettings$exposureDatabaseSchema <- config$resultSchema
  cmSettings$exposureTable <- config$tables$exposureCohort
  cmSettings$outcomeDatabaseSchema <- config$resultSchema
  cmSettings$outcomeTable <- config$outcomeCohort
  cmSettings$outputFolder <- config$exportPath
  cmSettings$multiThreadingSettings <- multiThreadingSettings
  cmSettings$cmDiagnosticThresholds  <- NULL
  do.call(CohortMethod::runCmAnalyses, args)

  CohortMethod::exportToCsv(outputFolder = cmSettings$outputFolder,
                            databaseId = config$databaseId,
                            maxCores = parallel::detectCores() - 1)
}