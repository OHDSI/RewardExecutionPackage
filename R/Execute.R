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

#' Create Results Zip
#' @description
#' Create zip for results including CDM meta-data
#' @param config        Reward config object
#' @export
createResultsZip <- function(config) {
  checkmate::assert_class(config, "CdmConfig")
  cdmInfo <- list(name = config$name, database = config$database, sourceId = config$sourceId)
  ParallelLogger::saveSettingsToJson(cdmInfo, file.path(config$export, "cdmInfo.json"))
  zipfilePath <- paste0(config$database, "RewardResults.zip")
  files <- file.path(config$export, list.files(config$export, pattern = "*.csv"))
  files <- c(files, file.path(config$export, "cdmInfo.json"))
  DatabaseConnector::createZipFile(zipfilePath, files, rootFolder = config$export)
  return(zipfilePath)
}

#' Create Results Manifest (s3)
#' @description
#' Create json manifest for results files to upload
#' @param config        Reward config object
#' @export
createResultsManifest <- function(config, studyName = NULL) {
  checkmate::assert_class(config, "CdmConfig")
  checkmate::assert_true(config$useAwsS3Export)

  manifest <- readr::read_csv(config$awsS3Log)

  cdmInfo <- list(name = config$name,
                  database = config$database,
                  sourceId = config$sourceId,
                  manifest = manifest)
  ParallelLogger::saveSettingsToJson(cdmInfo, file.path(config$export, paste0(config$database, studyName, "-cdmInfo.json")))
}

#' Execute package
#' @description
#' Upload cohorts and references
#' Execute SCC methods
#' Store results (and optionally transfer them to main REWARD server location)
#' @inheritParams loadCdmConfiguration
#' @param referenceZipFile                  Path to rewardb cohort references zip file
#' @param deleteExistingCohorts             If results for cohorts already exist, delete and start again?
#' @param analysisIds                       optionally only run certain analysis types
#' @export
execute <- function(cdmConfigPath, referenceZipFile, deleteExistingCohorts = FALSE, analysisIds = NULL) {
  config <- loadCdmConfiguration(cdmConfigPath)
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  importReferenceTables(connection, config, referenceZipFile)
  createCohorts(connection, config, deleteExisting = deleteExistingCohorts)
  generateAtlasCohortSet(config, connection)
  computeSccResults(connection, config, analysisIds = analysisIds)
}