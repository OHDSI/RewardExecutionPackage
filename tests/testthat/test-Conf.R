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

  expect_s3_class(config, "CdmConfig")

})

test_that("Copy targets file", {
  dest <- file.path(tempdir(), "_targets.R")
  unlink(dest)
  on.exit(unlink(dest))
  createTargetsFile(path = dest)
  checkmate::expect_file_exists(dest)
  expect_error(createTargetsFile(path = dest))

  createTargetsFile(path = dest, overwrite = TRUE)
  checkmate::expect_file_exists(dest)
})