test_that("Cohort construction works", {
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  importReferenceTables(connection, cdmConfig, referenceZipPath)
  createCohorts(connection, cdmConfig)

  count <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                      "SELECT count(*) as ct FROM @results_schema.@cohort_table",
                                                      results_schema = cdmConfig$resultSchema,
                                                      cohort_table = cdmConfig$tables$cohort)
  expect_true(count$CT[1] > 0)

  createOutcomeCohorts(connection, cdmConfig)

  count <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                      "SELECT count(*) as ct FROM @results_schema.@cohort_table",
                                                      results_schema = cdmConfig$resultSchema,
                                                      cohort_table = cdmConfig$tables$outcomeCohort)

  expect_true(count$CT[1] > 0)
})
