test_that("SelfControlledCohort works", {
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  connectionDetails <- cdmConfig$connectionDetails
  connection <- DatabaseConnector::connect(connectionDetails)
  importReferenceTables(cdmConfig, referenceZipPath)
  createCohorts(connection, cdmConfig)
  computeSccResults(connection, cdmConfig)

  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-1.csv")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-2.csv")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-3.csv")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-4.csv")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-5.csv")))
})
