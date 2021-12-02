cdmConfigPath <- "config/test.cdm.yml"
cfg <- yaml::read_yaml(cdmConfigPath)
unlink(cfg$connectionDetails$server)
# Eunomia DB is stored relative to testing directory
connectionDetails <- Eunomia::getEunomiaConnectionDetails(databaseFile = cfg$connectionDetails$server)
connection <- DatabaseConnector::connect(connectionDetails)

referenceZipPath <- tempfile(fileext = ".zip")
makeTestReferenceZip(pathToZip = referenceZipPath)

withr::defer({
  # Always disconnect
  DatabaseConnector::disconnect(connection)
  # Clean up Eunomia instance
  unlink(cfg$connectionDetails$server)
  unlink(referenceZipPath)
}, testthat::teardown_env())