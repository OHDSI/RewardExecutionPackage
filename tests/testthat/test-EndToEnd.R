test_that("Cohort construction works", {
  unlink(cfg$connectionDetails$server)
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(databaseFile = cfg$connectionDetails$server)
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  unlink(cdmConfig$referencePath)
  on.exit(unlink(cdmConfig$referencePath))
  importReferenceTables(connection, cdmConfig, referenceZipPath)

  # Test all tables are present and populated
  for (table in CONST_REFERENCE_TABLES) {
    sql <- "SELECT COUNT(*) as ct FROM @reference_schema.@table"
    count <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                        sql,
                                                        table = table,
                                                        reference_schema = cdmConfig$referenceSchema)

    expect_true(count$CT[1] >= 0)
  }

  createCohorts(connection, cdmConfig)

  count <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                      "SELECT count(*) as ct FROM @results_schema.@cohort_table ct
                                                      INNER JOIN @reference_schema.@outcome_cohort oc ON oc.cohort_definition_id = ct.cohort_definition_id
                                                      ",
                                                      reference_schema = cdmConfig$referenceSchema,
                                                      outcome_cohort = cdmConfig$tables$outcomeCohort,
                                                      results_schema = cdmConfig$resultSchema,
                                                      cohort_table = cdmConfig$tables$cohort)

  expect_true(count$CT[1] > 0)

  count <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                      "SELECT count(*) as ct FROM @results_schema.@cohort_table ct
                                                      INNER JOIN @reference_schema.@exposure_cohort ec ON ec.cohort_definition_id = ct.cohort_definition_id
                                                      ",
                                                      reference_schema = cdmConfig$referenceSchema,
                                                      exposure_cohort = cdmConfig$tables$exposureCohort,
                                                      results_schema = cdmConfig$resultSchema,
                                                      cohort_table = cdmConfig$tables$cohort)

  expect_true(count$CT[1] > 0)

  generateAtlasCohortSet(cdmConfig, connection)


  computeSccResults(connection, cdmConfig)
  unlink("export", recursive = TRUE, force = TRUE)
})
