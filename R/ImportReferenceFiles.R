# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCohort
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

CONST_META_FILE_NAME <- "reward-meta-info.json"

#' @title
#' Unzip and verify results zip with meta-data json
#' @description
#' Used to unzip and check all files in a zip folder with meta data file containing md5 hashes at time of creation
#' Used by both results generation and reference files
#' @param exportZipFilePath         zip file to inflate
#' @param unzipPath                 path to create
#' @param overwrite                 overwrite any existing files
#'
unzipAndVerify <- function(exportZipFilePath, unzipPath, overwrite) {
  message("Inflating zip archive")
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

  message(paste("Verifying file checksums"))
  for (file in names(meta$hashList)) {
    hash <- meta$hashList[[file]]
    message(paste("checking file hash", file, hash))
    unzipFile <- file.path(unzipPath, file)
    checkmate::assert_file_exists(unzipFile)
    verifyCheckSum <- tools::md5sum(unzipFile)[[1]]
    message(paste(hash, verifyCheckSum))
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
importReferenceTables <- function(cdmConfig, zipFilePath, overwriteReferences = FALSE) {
  checkmate::assertFileExists(zipFilePath)
  metaFilePath <- file.path(cdmConfig$referencePath, CONST_META_FILE_NAME)

  if (!file.exists(metaFilePath) || overwriteReferences) {
    unzipAndVerify(zipFilePath, cdmConfig$referencePath, overwriteReferences)
  }

  connection <- DatabaseConnector::connect(cdmConfig$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  message("Creating reference tables and copying temp data")
  sql <- SqlRender::loadRenderTranslateSql(
    file.path("create", "referenceSchema.sql"),
    package = utils::packageName(),
    dbms = connection@dbms,
    schema = cdmConfig$referenceSchema,
    concept_set_definition = cdmConfig$tables$conceptSetDefinition,
    cohort_concept_set = cdmConfig$tables$cohortConceptSet,
    cohort_definition = cdmConfig$tables$cohortDefinition,
    atlas_cohort_reference = cdmConfig$tables$atlasCohortReference,
    cohort_group_definition = cdmConfig$tables$cohortGroupDefinition,
    cohort_group = cdmConfig$tables$cohortGroup,
    analysis_setting = cdmConfig$tables$analysisSetting,
    include_constraints = cdmConfig$includeConstraints
  )
  DatabaseConnector::executeSql(connection, sql)
  DatabaseConnector::disconnect(connection)

  migrateDatabaseModel(cdmConfig)

  connection <- DatabaseConnector::connect(cdmConfig$connectionDetails)
  fileList <- file.path(cdmConfig$referencePath, paste0(CONST_REFERENCE_TABLES, ".csv"))
  for (file in fileList) {
    camelName <- SqlRender::snakeCaseToCamelCase(strsplit(basename(file), ".csv")[[1]])
    tableName <- cdmConfig$tables[[camelName]]

    message("Inserting reference table ", tableName)
    message(paste("Using insert table", camelName, tableName, file))
    data <- vroom::vroom(file, delim = ",", show_col_types = FALSE)

    # Create temp tables
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = paste0("#t_", tableName),
      data = data,
      tempTable = TRUE,
      progressBar = TRUE,
      createTable = TRUE,
      dropTableIfExists = TRUE,
      bulkLoad = cdmConfig$bulkUpload
    )
  }

  message("Copying temp tables")
  sql <- SqlRender::loadRenderTranslateSql(
    file.path("create", "copyTempTables.sql"),
    package = utils::packageName(),
    dbms = connection@dbms,
    schema = cdmConfig$referenceSchema,
    concept_set_definition = cdmConfig$tables$conceptSetDefinition,
    cohort_concept_set = cdmConfig$tables$cohortConceptSet,
    cohort_definition = cdmConfig$tables$cohortDefinition,
    atlas_cohort_reference = cdmConfig$tables$atlasCohortReference,
    cohort_group_definition = cdmConfig$tables$cohortGroupDefinition,
    cohort_group = cdmConfig$tables$cohortGroup,
    analysis_setting = cdmConfig$tables$analysisSetting,
    include_constraints = cdmConfig$includeConstraints
  )
  DatabaseConnector::executeSql(connection, sql)
}
