# _targets.R
library(targets)

cdmConfigPath <- Sys.getenv("REWARD_CDM_CONFIG")
refPath <- Sys.getenv("REWARD_REF_FILE_PATH")

loadConfiguration <- function(cdmConfigPath) {
  config <- RewardExecutionPackage::loadCdmConfiguration(cdmConfigPath)
  dir.create(config$exportPath, showWarnings = FALSE)
  config
}

cohortCreation <- function(config, referencesImported) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::createCohorts(connection, config)
  return(TRUE)
}

importReferences <- function(config, referenceFilePath) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::importReferenceTables(connection, config, referenceFilePath)
}

# Return scc setting objects
getAnalysisSettings <- function(config, referencesImported) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  return(RewardExecutionPackage::getSccSettings(connection, config))
}

# For a given scc setting, export the time at risk stats to an RDF
getTarStats <- function(cohortsCreated, config, analysisSettings) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  tarStats <- RewardExecutionPackage::getSccRiskWindowStats(connection, config, analysisSettings$options[[1]], analysisSettings$analysisId)
  RewardExecutionPackage::exportSccTarStats(tarStats, config, analysisSettings$analysisId)
}

getSccResults <- function(cohortsCreated, config, analysisSettings) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::runScc(connection, config, analysisSettings$options[[1]], analysisSettings$analysisId)
}

zipSccResults <- function(timeAtRiskStats, sccReuslts, config) {
  zipfilePath <- paste0(config$database, "RewardResults.zip")
  DatabaseConnector::createZipFile(zipfilePath, file.path(config$export))
  return(zipfilePath)
}

tar_option_set(packages = c("RewardExecutionPackage", "DatabaseConnector"))
# Required by parallel logger or it complains
options(threadNumber = "main")

list(
  tar_target(configPath, file.path(cdmConfigPath), format = "file"),
  tar_target(config, loadConfiguration(configPath)),
  tar_target(referenceFilePath, file.path(refPath), format = "file"),
  tar_target(referenceImport, importReferences(config, referenceFilePath)),
  tar_target(cohortExecution, cohortCreation(config, referenceImport)),
  tar_target(analysisSettings, getAnalysisSettings(config, referenceImport)),
  tar_target(timeAtRiskStats, getTarStats(cohortExecution, config, analysisSettings),
             pattern = cross(analysisSettings),
             format = "file"),
  tar_target(sccResults, getSccResults(cohortExecution, config, analysisSettings),
             pattern = cross(analysisSettings),
             format = "file"),
  tar_target(zipResults,
             zipSccResults(timeAtRiskStats, sccResults, config),
             format = "file")
)
