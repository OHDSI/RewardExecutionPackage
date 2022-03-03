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

#' Execute package
#' @description
#' Upload cohorts and references
#' Execute SCC methods
#' Store results (and optionally transfer them to main REWARD server location)
#' @inheritParams loadCdmConfiguration
#' @param referenceZipFile                  Path to rewardb cohort references zip file
#' @param deleteExistingCohorts             If results for cohorts already exist, delete and start again?
#' @export
execute <- function(cdmConfigPath, referenceZipFile, deleteExistingCohorts = FALSE) {
  config <- loadCdmConfiguration(cdmConfigPath)
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  importReferenceTables(connection, config, referenceZipFile)
  createCohorts(connection, config, deleteExisting = deleteExistingCohorts)
  computeSccResults(connection, config)
  exportResults(config)
}