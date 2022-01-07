CONST_META_FILE_NAME <- "rb-meta.json"

CONST_REFERENCE_TABLES <- c(
  "concept_set_definition",
  "cohort_definition",
  "outcome_cohort_definition",
  "atlas_outcome_reference",
  "atlas_outcome_concept",
  "atlas_exposure_reference",
  "atlas_exposure_concept",
  "analysis_setting"
)

CONST_RESULTS_TABLES <- c(
  "outcome_cohort",
  "cohort",
  "scc_result"
)

CONST_EXCLUDE_REF_COLS <- list(
  "atlasOutcomeReference" = c("SQL_DEFINITION", "DEFINITION"),
  "atlasExposureReference" = c("SQL_DEFINITION", "DEFINITION")
)


#' @title
#' Unzip and verify results zip with meta-data json
#' @description
#' Used to unzip and check all files in a zip folder with meta data file containing md5 hashes at time of creation
#' Used by both results generation and reference files
#' @param exportZipFilePath         zip file to inflate
#' @param unzipPath                 path to create
#' @param overwrite                 overwrite any existing files
#'
#' @import ParallelLogger
#' @importFrom utils unzip
#' @importFrom jsonlite read_json
#' @importFrom tools md5sum file_path_as_absolute
unzipAndVerify <- function(exportZipFilePath, unzipPath, overwrite) {
  ParallelLogger::logInfo("Inflating zip archive")
  if (!dir.exists(unzipPath)) {
    dir.create(unzipPath)
  } else if (!overwrite) {
    stop(paste("Folder", unzipPath, "exists and overwite = FALSE "))
  }
  # Unzip full file
  utils::unzip(zipfile = exportZipFilePath, exdir = unzipPath, overwrite = TRUE)
  # Perform checksum verifications
  metaFilePath <- file.path(unzipPath, CONST_META_FILE_NAME)
  checkmate::assert_file_exists(metaFilePath)
  meta <- jsonlite::read_json(file.path(unzipPath, CONST_META_FILE_NAME))

  ParallelLogger::logInfo(paste("Verifying file checksums"))
  # Check files are valid

  for (file in names(meta$hashList)) {
    hash <- meta$hashList[[file]]
    ParallelLogger::logInfo(paste("checking file hash", file, hash))
    unzipFile <- file.path(unzipPath, file)
    checkmate::assert_file_exists(unzipFile)
    verifyCheckSum <- tools::md5sum(unzipFile)[[1]]
    ParallelLogger::logInfo(paste(hash, verifyCheckSum))
    checkmate::assert_true(hash == verifyCheckSum)
  }

  return(lapply(names(meta$hashList), function(file) { tools::file_path_as_absolute(file.path(unzipPath, file)) }))
}

#' @title
#' Import reference tables
#' @description
#' Note that this always overwrites the existing reference tables stored in the database
#' @param connection                    DatabaseConnector connection
#' @param cdmConfig                     cdmConfig object
#' @param zipFilePath                   zip file path
#' @export
importReferenceTables <- function(connection, cdmConfig, zipFilePath) {
  checkmate::assertFileExists(zipFilePath)
  unzipAndVerify(zipFilePath, cdmConfig$referencePath, TRUE)

  ParallelLogger::logInfo("Creating reference tables")
  sql <- SqlRender::loadRenderTranslateSql(
    "create/referenceTables.sql",
    package = packageName(),
    dbms = connection@dbms,
    schema = cdmConfig$referenceSchema,
    concept_set_definition = cdmConfig$tables$conceptSetDefinition,
    cohort_definition = cdmConfig$tables$cohortDefinition,
    outcome_cohort_definition = cdmConfig$tables$outcomeCohortDefinition,
    atlas_outcome_reference = cdmConfig$tables$atlasOutcomeReference,
    atlas_outcome_concept = cdmConfig$tables$atlasOutcomeConcept,
    atlas_exposure_reference = cdmConfig$tables$atlasExposureReference,
    atlas_exposure_concept = cdmConfig$tables$atlasExposureConcept,
    analysis_setting = cdmConfig$tables$analysisSetting
  )
  DatabaseConnector::executeSql(connection, sql)

  fileList <- file.path(cdmConfig$referencePath, paste0(CONST_REFERENCE_TABLES, ".csv"))
  for (file in fileList) {
    camelName <- SqlRender::snakeCaseToCamelCase(strsplit(basename(file), ".csv")[[1]])
    tableName <- cdmConfig$tables[[camelName]]

    ParallelLogger::logInfo("Inserting reference table ", tableName)
    ParallelLogger::logDebug(paste("Using insert table", camelName, tableName, file))
    data <- read.csv(file)

    # Remove columns we don't want to store on the CDM (big text strings aren't friendly with redshift)
    if (camelName %in% names(CONST_EXCLUDE_REF_COLS)) {
      data <- data[, !(names(data) %in% CONST_EXCLUDE_REF_COLS[[camelName]])]
      ParallelLogger::logDebug(names(data))
    }

    DatabaseConnector::insertTable(
      connection = connection,
      databaseSchema = cdmConfig$referenceSchema,
      tableName = tableName,
      data = data,
      progressBar = TRUE,
      dropTableIfExists = TRUE,
      bulkLoad = cdmConfig$bulkUpload
    )

  }
}