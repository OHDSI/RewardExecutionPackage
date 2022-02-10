test_that("Check configuration file loads", {
  requiredNames <- c("sourceId",
                     "database",
                     "name",
                     "connectionDetails",
                     "resultSchema",
                     "referenceSchema",
                     "vocabularySchema",
                     "cdmSchema")

  config <- loadCdmConfiguration(cdmConfigPath)
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(databaseFile = cfg$connectionDetails$server)
  checkmate::expect_names(names(config), must.include = requiredNames)
  validateCdmConfigFile(cdmConfigPath)

  expect_s3_class(config, "CdmConfig")
})