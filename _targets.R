# _targets.R
library(targets)

cdmConfigPath <- "myCdm.yml"
refPath <- "tests/testthat/test_reference_files/reward-references.zip"

ParallelLogger::addDefaultFileLogger("REWARD_EXECUTION.log")

cohortCreation <- function(config, referencesImported) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::createCohorts(connection, config)
  return(TRUE)
}

computeSccResults <- function(config, cohortsCreated) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::computeSccResults(connection, config)
  return(TRUE)
}

importReferences <- function(config, referenceFilePath) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::importReferenceTables(connection, config, referenceFilePath)
}

loadConfiguration <- function(cdmConfigPath) {
  config <- RewardExecutionPackage::loadCdmConfiguration(cdmConfigPath)
  dir.create(config$exportPath, showWarnings = FALSE)
  config
}

tar_option_set(packages = c("RewardExecutionPackage", "DatabaseConnector"))
list(
  tar_target(configPath, file.path(cdmConfigPath), format = "file"),
  tar_target(config, loadConfiguration(configPath)),
  tar_target(referenceFilePath, file.path(refPath), format = "file"),
  tar_target(referencesImported, importReferences(config, referenceFilePath)),
  tar_target(cohortsCreated, cohortCreation(config, referencesImported)),
  tar_target(resultsExported, computeSccResults(config, cohortsCreated))
)