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

#' Export results in to a single zipped file
#' @details
#' Zips files and records their checksums
#' @param config                    CdmConfiguration object
#' @param exportZipFile             Name of zip file to export
#' @importFrom RJSONIO toJSON
#' @importFrom tools md5sum
#' @export
exportResults <- function(config,
                          exportZipFile = paste0("reward-results-", config$database, ".zip")) {
  # Get scc files
  files <- list.files(config$exportPath, pattern = ".*\\.csv$")
  fileHashes <- tools::md5sum(file.path(config$exportPath, files))
  names(fileHashes) <- basename(names(fileHashes))
  jsonStr <- RJSONIO::toJSON(fileHashes)
  writeLines(jsonStr, file.path(config$exportPath, "meta-info.json"))
  files <- c(files, "meta-info.json")

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(config$exportPath)
  DatabaseConnector::createZipFile(zipFile = exportZipFile, files = files)
  ParallelLogger::logInfo("Results exported to ", file.path(config$exportPath, exportZipFile))
}
