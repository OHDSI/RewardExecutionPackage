cdmConfigPath <- "config/test.cdm.yml"
cfg <- yaml::read_yaml(cdmConfigPath)
unlink(cfg$connectionDetails$server)
referenceZipPath <- file.path("test_reference_files", "reward-references.zip")

eunomiaDir <- "eunomia_files"
if (!dir.exists(eunomiaDir)) {
  Eunomia::exportToCsv(outputFolder = eunomiaDir)
}

connection <- do.call(DatabaseConnector::connect, cfg$connectionDetails)

schemaCreation <- "
CREATE SCHEMA @cdmSchema;
CREATE SCHEMA IF NOT EXISTS @resultSchema;
CREATE SCHEMA IF NOT EXISTS @referenceSchema;
"

DatabaseConnector::renderTranslateExecuteSql(connection,
                                             schemaCreation,
                                             resultSchema = cfg$resultSchema,
                                             referenceSchema = cfg$referenceSchema,
                                             cdmSchema = cfg$cdmSchema)

lapply(list.files(eunomiaDir, full.names = T), function(file) {
  DatabaseConnector::insertTable(connection,
                                 tableName = gsub(".csv", "", basename(file)),
                                 data = readr::read_csv(file, col_types = readr::cols()),
                                 databaseSchema = cfg$cdmSchema)
})
DatabaseConnector::disconnect(connection)

withr::defer({
  # Clean up Eunomia instance
  unlink(cfg$connectionDetails$server)
  unlink("export", recursive = TRUE, force = TRUE)
}, testthat::teardown_env())

