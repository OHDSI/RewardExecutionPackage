#' Execute package
#' @description
#' Upload cohorts and references
#' Execute SCC methods
#' Store results (and optionally transfer them to main REWARD server location)
#' @inheritParams loadCdmConfiguration
#' @param referenceZipFile                  Path to rewardb cohort references zip file
#' @param deleteExistingCohorts             If results for cohorts already exist, delete and start again?
#' @export
execute <- function(cdmConfigPath, referenceZipFile, deleteExistingCohorts = FALSE) {
  config <- loadCdmConfiguration(cdmConfigPath)
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  importReferenceTables(connection, config, referenceZipFile)
  createCustomDrugEras(connection, config)
  createCohorts(connection, config, deleteExisting = deleteExistingCohorts)
  createOutcomeCohorts(connection, config, deleteExisting = deleteExistingCohorts)
  #generateSccResults(connection, config)
}