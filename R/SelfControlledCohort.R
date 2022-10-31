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

#' get outcome cohort ids
#' @description
#' Return all outcome cohort ids
#' @param connection DatabaseConnector connection
#' @param config     cdm configuration
#' @export
getDefaultOutcomeIds <- function(connection, config) {
  sql <- "SELECT cohort_definition_id FROM @reference_schema.@outcome_cohort"
  DatabaseConnector::renderTranslateQuerySql(connection,
                                             sql,
                                             snakeCaseToCamelCase = TRUE,
                                             reference_schema = config$referenceSchema,
                                             outcome_cohort = config$tables$outcomeCohort)$cohortDefinitionId
}

#' get exposure cohort ids
#' @description
#' Return all outcome cohort ids
#' @param connection DatabaseConnector connection
#' @param config     cdm configuration
#' @export
getDefaultExposureIds <- function(connection, config) {
  sql <- "SELECT cohort_definition_id FROM @reference_schema.@exposure_cohort"
  DatabaseConnector::renderTranslateQuerySql(connection,
                                             sql,
                                             snakeCaseToCamelCase = TRUE,
                                             reference_schema = config$referenceSchema,
                                             exposure_cohort = config$tables$exposureCohort)$cohortDefinitionId
}

#' Get Self controlled cohort risk windows
#' @param connection DatabaseConnector connection
#' @param config     cdm configuration
#' @param analysisSettings  Id of analysis
#' @param analysisId     cdm configuration
#' @param exposureIds     Outcomes to run for
#' @param outcomeIds      Exposure to run for
#' @export
getSccRiskWindowStats <- function(connection,
                                  config,
                                  analysisSettings,
                                  exposureIds = getDefaultExposureIds(connection, config),
                                  outcomeIds = getDefaultOutcomeIds(connection, config)) {


  message(paste("Creating SCC risk windows on database ", config$database))
  SelfControlledCohort::runSccRiskWindows(connection,
                                          cdmDatabaseSchema = config$cdmSchema,
                                          exposureIds = exposureIds,
                                          resultsDatabaseSchema = config$resultSchema,
                                          riskWindowsTable = config$tables$sccRiskWindows,
                                          exposureTable = config$table$cohort,
                                          firstExposureOnly = analysisSettings$firstExposureOnly,
                                          minAge = analysisSettings$minAge,
                                          maxAge = analysisSettings$maxAge,
                                          studyStartDate = analysisSettings$studyStartDate,
                                          studyEndDate = analysisSettings$studyEndDate,
                                          addLengthOfExposureExposed = analysisSettings$addLengthOfExposureExposed,
                                          riskWindowStartExposed = analysisSettings$riskWindowStartExposed,
                                          riskWindowEndExposed = analysisSettings$riskWindowEndExposed,
                                          addLengthOfExposureUnexposed = analysisSettings$addLengthOfExposureUnexposed,
                                          riskWindowEndUnexposed = analysisSettings$riskWindowEndUnexposed,
                                          riskWindowStartUnexposed = analysisSettings$riskWindowStartUnexposed,
                                          hasFullTimeAtRisk = analysisSettings$hasFullTimeAtRisk,
                                          washoutPeriod = analysisSettings$washoutPeriod,
                                          followupPeriod = analysisSettings$followupPeriod)

  message(paste("Getting TAR statistics"))
  SelfControlledCohort::getSccRiskWindowStats(connection,
                                              outcomeDatabaseSchema = config$resultSchema,
                                              resultsDatabaseSchema = config$resultSchema,
                                              riskWindowsTable = config$tables$sccRiskWindows,
                                              outcomeTable = config$tables$cohort,
                                              outcomeIds = outcomeIds,
                                              firstOutcomeOnly = analysisSettings$firstOutcomeOnly)
}

#' @title
#' Clean up and export time at risk stats
#' @description
#' Prepares CSV ouput of time at risk stats for uploading to database
#' @param tarStats object returned by SelfControlledCohort getSccRiskWindowStats
#' @param statType string - one of "treatmentTimeDistribution", "timeToOutcomeDistribution","timeToOutcomeDistributionExposed",
#' "timeToOutcomeDistributionUnexposed"
#' @param config cdm configuration  object
#' @param analysisId id to mark output files with
#' @export
exportSccTarStats <- function(tarStats, config, analysisId) {
  append <- FALSE
  statTypes <- c("treatmentTimeDistribution",
                 "timeToOutcomeDistribution",
                 "timeToOutcomeDistributionExposed",
                 "timeToOutcomeDistributionUnexposed")

  dataFileName <- file.path(config$exportPath, paste0("time_at_risk_stats-", config$database, "-aid-", analysisId, ".csv.gz"))
  for (statType in statTypes) {
    data <- tarStats[[statType]]
    if (nrow(data) > 0) {
      data$analysis_id <- as.integer(analysisId)
      data$source_id <- config$sourceId
      message("Empty TAR stat data for analysis, ", analysisId)
    }
    data <- data %>%
      dplyr::rename("target_cohort_id" = "exposureId",
                    "outcome_cohort_id" = "outcomeId",
                    "p_10" = "p10",
                    "p_25" = "p25",
                    "p_75" = "p75",
                    "p_90" = "p90",
                    "maximum" = "max",
                    "minimum" = "min",
                    "stat_type" = "statType")

    if (config$useAwsS3Export) {
      dataFileName <- file.path(config$exportPath, paste0("time_at_risk_stats-", config$database, "-aid-", analysisId,
                                                          "stat-", statType, ".csv.gz"))

      checksum <- digest::digest(data, "sha1")
      success <- aws.s3::s3write_using(data,
                                       readr::write_csv,
                                       object = paste(Sys.getenv("AWS_OBJECT_KEY"), dataFileName, sep = "/"),
                                       na = "",
                                       append = FALSE,
                                       bucket = Sys.getenv("AWS_BUCKET_NAME"),
                                       opts = list(
                                         check_region = FALSE,
                                         headers = list(`x-amz-server-side-encryption` = Sys.getenv("AWS_SSE_TYPE")),
                                         multipart = TRUE
                                       ))

      log <- data.frame(filename = dataFileName, position = statType, success = success, checksum = checksum)
      readr::write_csv(log, file = config$awsS3Log, append = !file.exists(config$aws3storeLog))
    } else {
      readr::write_csv(data, dataFileName, na = "", append = append)
      append <- TRUE
    }

  }
  return(dataFileName)
}

#' Get default results file name
#' @export
getDefaultSccDataFileName <- function(config, analysisId, position = NULL) {
  if (is.null(position))
    return(file.path(config$exportPath, paste0("scc-results-", config$database, "-aid-", analysisId, ".csv.gz")))

  return(file.path(config$exportPath, paste0("scc-results-", config$database, "-aid-", analysisId, "pos-", position, ".csv.gz")))
}

# Export analysis results to a csv file in batches (assumes the data set is large)
batchStoreSccResults <- function(dataBatch,
                                 position,
                                 config,
                                 analysisId,
                                 dataFileName = getDefaultSccDataFileName(config, analysisId)) {
  if (nrow(dataBatch) == 0) {
    return(dataBatch)
  }
  dataBatch <- cleanUpSccDf(dataBatch, config$sourceId, analysisId)

  message("Saving results ", dataFileName, " position ", position)
  readr::write_csv(dataBatch, dataFileName, na = "", append = position != 1)
  return(dataBatch)
}


# Export analysis results to a csv file in batches to s3 bucket (assumes the data set is large)
batchStoreSccResultsToS3 <- function(dataBatch,
                                     position,
                                     config,
                                     analysisId,
                                     dataFileName = getDefaultSccDataFileName(config, analysisId, position = position)) {
  if (nrow(dataBatch) == 0) {
    return(dataBatch)
  }
  dataBatch <- cleanUpSccDf(dataBatch, config$sourceId, analysisId)

  message("Saving results ", dataFileName, " position ", position, " to aws.s3")
  checksum <- digest::digest(dataBatch, "sha1")
  # NOTE - a multipart upload would be better here
  success <- aws.s3::s3write_using(dataBatch,
                                   readr::write_csv,
                                   object = paste(Sys.getenv("AWS_OBJECT_KEY"), dataFileName, sep = "/"),
                                   na = "",
                                   append = FALSE,
                                   bucket = Sys.getenv("AWS_BUCKET_NAME"),
                                   opts = list(
                                     check_region = FALSE,
                                     headers = list(`x-amz-server-side-encryption` = Sys.getenv("AWS_SSE_TYPE"))
                                   ))

  ## log files that have been completed
  log <- data.frame(filename = dataFileName, position = position, success = success, checksum = checksum)
  readr::write_csv(log, file = config$awsS3Log, append = !file.exists(config$aws3storeLog))

  return(dataBatch)
}


#' Peform SCC from self controlled cohort package with rewardbs settings
#' @description
#' Used by targets for execution of steps. See computeSccResults for other settings
#' @export
runScc <- function(connection,
                   config,
                   analysisSettings,
                   analysisId,
                   postProcessFunction = batchStoreSccResults,
                   postProcessArgs = list(config = config, analysisId = analysisId),
                   exposureIds = getDefaultExposureIds(connection, config),
                   outcomeIds = getDefaultOutcomeIds(connection, config),
                   cores = parallel::detectCores() - 1) {
  message(paste("Starting SCC analysis on", config$database))
  opts <- list(connection = connection,
               cdmDatabaseSchema = config$cdmSchema,
               cdmVersion = 5,
               exposureIds = exposureIds,
               outcomeIds = outcomeIds,
               exposureDatabaseSchema = config$resultSchema,
               exposureTable = config$tables$cohort,
               outcomeDatabaseSchema = config$resultSchema,
               outcomeTable = config$tables$cohort,
               computeThreads = cores,
               postProcessFunction = postProcessFunction,
               postProcessArgs = postProcessArgs,
               resultsDatabaseSchema = config$resultSchema,
               riskWindowsTable = config$tables$sccRiskWindows,
               resultsTable = "scc_result",
               returnEstimates = FALSE,
               computeTarDistribution = FALSE)
  args <- c(analysisSettings, opts)
  do.call(SelfControlledCohort::runSelfControlledCohort, args)
  message(paste("Completed SCC for", config$database))
  return(getDefaultSccDataFileName(config, analysisId))
}

cleanUpSccDf <- function(data, sourceId, analysisId) {
  data <- base::do.call(data.frame, lapply(data, function(x) replace(x, is.infinite(x) | is.nan(x), NA)))
  if (nrow(data) > 0) {
    data$source_id <- sourceId
    data$analysis_id <- as.integer(analysisId)
  }
  data <- data %>%
    dplyr::rename("target_cohort_id" = "exposureId",
                  "outcome_cohort_id" = "outcomeId",
                  "rr" = "irr",
                  "c_at_risk" = "numPersons",
                  "se_log_rr" = "seLogRr",
                  "log_rr" = "logRr",
                  "t_at_risk" = "numPersons",
                  "t_pt" = "timeAtRiskExposed",
                  "t_cases" = "numOutcomesExposed",
                  "c_cases" = "numOutcomesUnexposed",
                  "c_pt" = "timeAtRiskUnexposed",
                  "lb_95" = "irrLb95",
                  "ub_95" = "irrUb95",
                  "p_value" = "p",
                  "num_exposures" = "numExposures")
  return(data)
}

#' @title
#' Get scc settings stored in database
#' @description
#' returns a set of json objects that are SCC parameters
#' @export
getSccSettings <- function(connection, config, analysisIds = NULL) {
  # Get all SCC analysis settings objects
  getSccSettingsSql <- "SELECT * FROM @reference_schema.@analysis_setting WHERE type_id = 'scc'
  {@analysis_ids != ''} ? {AND analysis_id IN (@analysis_ids)}"
  settings <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                         getSccSettingsSql,
                                                         analysis_ids = analysisIds,
                                                         reference_schema = config$referenceSchema,
                                                         analysis_setting = config$tables$analysisSetting,
                                                         snakeCaseToCamelCase = TRUE)

  # Convert Base 64 to json
  settings$options <- lapply(settings$options, function(x) { RJSONIO::fromJSON(rawToChar(base64enc::base64decode(x))) })
  settings
}

#' @title
#' Get Zipped Scc Results
#' @description
#' Get zip files for scc
#' Partial reward execution with a subset of targets or outcomes. If both are null this will generate SCC results for all
#' exposure and outcome pairs. This is only really useful if you're adding an cohort after the full result set has been
#' generated.
#' @param config cdm config loaded with loadCdmConfig function
#' @param connection DatabaseConnector connection
#' @param outcomeCohortIds - vector of outcome cohort ids or NULL
#' @param targetCohortIds - vector of exposure cohort ids or NULL
#' @param .generateCohortStats - generate time on treatment and time to outcome stats or not
#'
#' @importFrom vroom vroom_write
#' @export
computeSccResults <- function(connection,
                              config,
                              analysisIds = NULL,
                              outcomeCohortIds = getDefaultOutcomeIds(connection, config),
                              targetCohortIds = getDefaultExposureIds(connection, config)) {
  sccAnalysisSettings <- getSccSettings(connection, config, analysisIds = analysisIds)

  apply(sccAnalysisSettings, 1, function(analysis) {
    analysisId <- analysis$analysisId
    if (!dir.exists(config$exportPath)) {
      dir.create(config$exportPath)
    }

    batchStoreArgs <- list(config = config, analysisId = analysisId)
    message(paste("Generating scc results with setting id", analysisId))
    analysisSettings <- analysis$options
    tarStats <- getSccRiskWindowStats(connection, config, analysisSettings, targetCohortIds, outcomeCohortIds)
    exportSccTarStats(tarStats, config, analysisId)
    runScc(connection = connection,
           config = config,
           analysisId = analysisId,
           postProcessFunction = ifelse(config$useAwsS3Export, batchStoreSccResultsToS3, batchStoreSccResults),
           postProcessArgs = batchStoreArgs,
           analysisSettings = analysisSettings,
           exposureIds = targetCohortIds,
           outcomeIds = outcomeCohortIds)
  })
}
