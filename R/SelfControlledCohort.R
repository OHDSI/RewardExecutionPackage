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


getDefaultOutcomeIds <- function(connection, config) {
  sql <- "SELECT cohort_definition_id FROM @reference_schema.@outcome_cohort"
  DatabaseConnector::renderTranslateQuerySql(connection,
                                             sql,
                                             snakeCaseToCamelCase = TRUE,
                                             reference_schema = config$referenceSchema,
                                             outcome_cohort = config$tables$outcomeCohort)$cohortDefinitionId
}

getDefaultExposureIds <- function(connection, config) {
  sql <- "SELECT cohort_definition_id FROM @reference_schema.@exposure_cohort"
  DatabaseConnector::renderTranslateQuerySql(connection,
                                             sql,
                                             snakeCaseToCamelCase = TRUE,
                                             reference_schema = config$referenceSchema,
                                             exposure_cohort = config$tables$exposureCohort)$cohortDefinitionId
}


#' Peform SCC from self controlled cohort package with rewardbs settings
runScc <- function(connection,
                   config,
                   postProcessFunction,
                   postProcessArgs,
                   analysisSettings,
                   analysisId,
                   exposureIds,
                   outcomeIds,
                   cores = parallel::detectCores() - 1) {

  ParallelLogger::logInfo(paste("Creating SCC risk windows on database ", config$database))
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

  ParallelLogger::logInfo(paste("Getting TAR statistics"))
  tarStats <- SelfControlledCohort::getSccRiskWindowStats(connection,
                                                          outcomeDatabaseSchema = config$resultSchema,
                                                          resultsDatabaseSchema = config$resultSchema,
                                                          riskWindowsTable = config$tables$sccRiskWindows,
                                                          outcomeTable = config$tables$cohort,
                                                          outcomeIds = outcomeIds,
                                                          firstOutcomeOnly = analysisSettings$firstOutcomeOnly)

  statTypes <- c("treatmentTimeDistribution",
                 "timeToOutcomeDistribution",
                 "timeToOutcomeDistributionExposed",
                 "timeToOutcomeDistributionUnexposed")

  for (statType in statTypes) {
    data <- tarStats[[statType]]
    if (nrow(data) > 0) {
      dataFileName <- file.path(config$exportPath, paste0(statType, config$database, "-aid-", analysisId, ".csv"))
      data$analysis_id <- as.integer(analysisId)
      data$source_id <- config$sourceId
      data <- data %>%
        dplyr::rename("target_cohort_id" = "exposureId",
                      "outcome_cohort_id" = "outcomeId",
                      "p_10" = "p10",
                      "p_25" = "p25",
                      "p_75" = "p75",
                      "p_90" = "p90",
                      "stat_type" = "statType")
      vroom::vroom_write(data, dataFileName, delim = ",", na = "", append = FALSE)
    }
  }

  ParallelLogger::logInfo(paste("Starting SCC analysis on", config$database))
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
  ParallelLogger::logInfo(paste("Completed SCC for", config$database))
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
  # Get all SCC analysis settings objects
  getSccSettingsSql <- "SELECT * FROM @reference_schema.@analysis_setting WHERE type_id = 'scc'
  {@analysis_ids != ''} ? {AND analysis_id IN (@analysis_ids)}"
  sccAnalysisSettings <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                                    getSccSettingsSql,
                                                                    analysis_ids = analysisIds,
                                                                    reference_schema = config$referenceSchema,
                                                                    analysis_setting = config$tables$analysisSetting)
  apply(sccAnalysisSettings, 1, function(analysis) {
    analysisId <- analysis[["ANALYSIS_ID"]]
    if (!dir.exists(config$exportPath)) {
      dir.create(config$exportPath)
    }
    # By default, export analysis results to a csv file in batches (assumes the data set is large)
    postProcessFunction <- function(dataBatch, position, config, analysisId) {
      if (nrow(dataBatch) == 0) {
        return(dataBatch)
      }
      dataBatch <- cleanUpSccDf(dataBatch, config$sourceId, analysisId)
      dataFileName <- file.path(config$exportPath, paste0("scc-results-", config$database, "-aid-", analysisId, ".csv"))
      ParallelLogger::logInfo("Saving results ", dataFileName, " position ", position)
      vroom::vroom_write(dataBatch, dataFileName, delim = ",", na = "", append = position != 1)
      return(dataBatch)
    }

    postProcessArgs <- list(config = config, analysisId = analysisId)
    ParallelLogger::logInfo(paste("Generating scc results with setting id", analysisId))
    analysisSettings <- RJSONIO::fromJSON(rawToChar(base64enc::base64decode(analysis["OPTIONS"])))
    runScc(connection = connection,
           config = config,
           postProcessFunction = postProcessFunction,
           postProcessArgs = postProcessArgs,
           analysisSettings = analysisSettings,
           analysisId = analysisId,
           exposureIds = targetCohortIds,
           outcomeIds = outcomeCohortIds)
  })
}
