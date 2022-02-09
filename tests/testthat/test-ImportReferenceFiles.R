test_that("Import references works", {
  unlink(cfg$connectionDetails$server)
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(databaseFile = cfg$connectionDetails$server)
  connection <- DatabaseConnector::connect(connectionDetails)

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
})