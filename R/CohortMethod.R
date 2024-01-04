
#' Execute Cohort method analysis
#' @description
#' Take cohort method settings and execute on CDM
#' @export
#' @param config        cdmConfiguration
#' @param cmSettings    cohortMethod settings object to execute
executeCohortMethodAnalysis <- function(config, cmConfig) {
  multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(12)

  if (!dir.exists(config$workDir)) {
    dir.create(config$workDir)
  }
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  # Get outcome cohorts
  cmConfig$settings <- createCmDesign(targetId = cmConfig$targetId,
                                      comparatorId = cmConfig$comparatorId,
                                      indicationId = cmConfig$indicationId,
                                      outcomeCohortIds =  getAtlasOutcomeIds(connection, config),
                                      excludedCovariateConceptIds = cmConfig$excludedCovariateConceptIds)

  cmConfig$settings$connectionDetails <- config$connectionDetails
  cmConfig$settings$cdmDatabaseSchema <- config$cdmSchema
  cmConfig$settings$exposureDatabaseSchema <- config$resultSchema
  cmConfig$settings$exposureTable <- config$tables$cohort
  cmConfig$settings$outcomeDatabaseSchema <- config$resultSchema
  cmConfig$settings$outcomeTable <- config$tables$cohort
  cmConfig$settings$outputFolder <- file.path(config$workDir, "CohortMethodOutput")
  cmConfig$settings$multiThreadingSettings <- multiThreadingSettings
  cmConfig$settings$cmDiagnosticThresholds  <- NULL

  do.call(CohortMethod::runCmAnalyses, cmConfig$settings)

  tryCatch({
    CohortMethod::exportToCsv(outputFolder = cmConfig$settings$outputFolder,
                              exportFolder = config$exportPath,
                              databaseId = config$sourceId,
                              maxCores = parallel::detectCores() - 1)
  }, error = function(err) {
    if (grepl("java.lang.RuntimeException: java.io.FileNotFoundException", err$message)) {
      zipName <- file.path(config$exportPath, sprintf("Results_%s.zip", config$sourceId))
      utils::zip(zipName, files = list.files(config$exportPath, pattern = ".*\\.csv$", full.names = TRUE))
    } else {
      # Re-throw the error if it's not the one we're looking for
      stop(err)
    }
  })
}
