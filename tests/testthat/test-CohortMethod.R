test_that("Cohort Method code execs study specs", {
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  importReferenceTables(cdmConfig, referenceZipPath)
  cmConfig <- ParallelLogger::loadSettingsFromJson("config/cmDesign.json")

  connection <- DatabaseConnector::connect(cdmConfig$connectionDetails)
  on.exit({
    if (DatabaseConnector::dbIsValid(connection)) {
      DatabaseConnector::disconnect(connection)
    }
    unlink(cdmConfig$workDir, recursive = T, force = T)
  })

  createCohorts(connection, cdmConfig)
  generateAtlasCohortSet(cdmConfig, connection)
  DatabaseConnector::disconnect(connection)

  executeCohortMethodAnalysis(cdmConfig, cmConfig)

  expect_true(TRUE)
})
