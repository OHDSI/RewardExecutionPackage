test_that("Cohort construction works", {
  unlink(cfg$connectionDetails$server)
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(databaseFile = cfg$connectionDetails$server)
  connection <- DatabaseConnector::connect(connectionDetails)
  importReferenceTables(connection, cdmConfig, referenceZipPath)
  createCohorts(connection, cdmConfig)

  count <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                      "SELECT count(*) as ct FROM @results_schema.@cohort_table",
                                                      results_schema = cdmConfig$resultSchema,
                                                      cohort_table = cdmConfig$tables$cohort)

  expect_true(count$CT[1] > 0)
})
