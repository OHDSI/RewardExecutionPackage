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

# _targets.R
#################################################################
# This file was copied from the RewardExecutionPackage.
# Consult documentation before modifying
#################################################################
library(targets)

# Change these to match your desired locations
refPath <- "reward-references.zip"
cdmConfigDir <- "cdmConfig"

loadConfiguration <- function(cdmConfigPath) {
  config <- RewardExecutionPackage::loadCdmConfiguration(cdmConfigPath)
  dir.create(config$exportPath, showWarnings = FALSE)
  config
}

cohortCreation <- function(config, referencesImported) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::createCohorts(connection, config)
  return(TRUE)
}

getCohortDefinitionSet <- function(config, referencesImported) {
  tableNames <- CohortGenerator::getCohortTableNames(config$tables$cohort)
  CohortGenerator::createCohortTables(connectionDetails = config$connectionDetails,
                                      cohortDatabaseSchema = config$resultSchema,
                                      cohortTableNames = tableNames,
                                      incremental = TRUE)
  RewardExecutionPackage::getAtlasCohortDefinitionSet(config)
}

# Can be called with map individual cohorts or the entire set
#
generateAtlasCohorts <- function(config, cohortDefinitionSet) {
  tableNames <- CohortGenerator::getCohortTableNames(config$tables$cohort)
  CohortGenerator::generateCohortSet(connectionDetails = config$connectionDetails,
                                     cdmDatabaseSchema = config$cdmSchema,
                                     cohortDatabaseSchema = config$resultSchema,
                                     cohortTableNames = tableNames,
                                     cohortDefinitionSet = cohortDefinitionSet,
                                     stopOnError = FALSE,
                                     incremental = TRUE,
                                     incrementalFolder = file.path(config$referencePath, config$database, incremental))
  return(TRUE)
}

importReferences <- function(config, referenceFilePath) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::importReferenceTables(connection, config, referenceFilePath)
}

# Return scc setting objects
getAnalysisSettings <- function(config, referencesImported) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  return(RewardExecutionPackage::getSccSettings(connection, config))
}

# For a given scc setting, export the time at risk stats to an RDF
getTarStats <- function(cohortsCreated, config, analysisSettings, atlasCohorts) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  tarStats <- RewardExecutionPackage::getSccRiskWindowStats(connection, config, analysisSettings$options[[1]], analysisSettings$analysisId)
  RewardExecutionPackage::exportSccTarStats(tarStats, config, analysisSettings$analysisId)
}

getSccResults <- function(cohortsCreated, config, analysisSettings, atlasCohorts) {
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  RewardExecutionPackage::runScc(connection, config, analysisSettings$options[[1]], analysisSettings$analysisId)
}

zipSccResults <- function(timeAtRiskStats, sccReuslts, config) {
  # create cdm info json file
  zipFilePath <- RewardExecutionPackage::createResultsZip(config)
  return(zipFilePath)
}

tar_option_set(packages = c("RewardExecutionPackage", "DatabaseConnector"))
# Required by parallel logger or it complains
options(threadNumber = "main")
# for use with targets::tar_make_clustermq()
options(clustermq.scheduler = "multiprocess")

list(
  tar_target(configPath, file.path(cdmConfigDir, list.files(cdmConfigDir))),
  tar_target(config, loadConfiguration(configPath), pattern = cross(configPath)),
  tar_target(referenceFilePath, file.path(refPath), format = "file"),
  tar_target(referenceImport, importReferences(config, referenceFilePath), pattern = cross(config)),
  tar_target(cohortExecution, cohortCreation(config, referenceImport), pattern = cross(config)),
  tar_target(cohortDefinitionSet, getCohortDefinitionSet(config, referenceImport), pattern = cross(config)),
  tar_target(atlasCohortsGen, generateAtlasCohorts(config, cohortDefinitionSet), pattern = cross(config, cohortDefinitionSet)),
  tar_target(analysisSettings, getAnalysisSettings(config, referenceImport), pattern = cross(config)),
  tar_target(timeAtRiskStats, getTarStats(cohortExecution, config, analysisSettings, atlasCohortsGen),
             pattern = cross(analysisSettings, config),
             format = "file"),
  tar_target(sccResults, getSccResults(cohortExecution, config, analysisSettings, atlasCohortsGen),
             pattern = cross(analysisSettings, config),
             format = "file"),
  tar_target(zipResults,
             zipSccResults(timeAtRiskStats, sccResults, config),
             pattern = cross(config),
             format = "file")
)
