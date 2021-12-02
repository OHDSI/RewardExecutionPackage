test_that("Import references works", {
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  importReferenceTables(connection, cdmConfig, referenceZipPath)

  # Test all tables are present and populated
  for (table in CONST_REFERENCE_TABLES) {
    sql <- "SELECT COUNT(*) as ct FROM @reference_schema.@table"
    count <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                        sql,
                                                        table = table,
                                                        reference_schema = cdmConfig$referenceSchema)

    expect_true(count$CT[1] > 0)
  }
})