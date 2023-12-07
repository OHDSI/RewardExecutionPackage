test_that("SelfControlledCohort works", {
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  importReferenceTables(cdmConfig, referenceZipPath)

  connection <- DatabaseConnector::connect(cdmConfig$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  createCohorts(connection, cdmConfig)
  computeSccResults(connection, cdmConfig)

  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-1.csv.gz")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-2.csv.gz")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-3.csv.gz")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-4.csv.gz")))
  checkmate::expect_file_exists(file.path(cdmConfig$exportPath, paste0("scc-results-", cdmConfig$database, "-aid-5.csv.gz")))
})
