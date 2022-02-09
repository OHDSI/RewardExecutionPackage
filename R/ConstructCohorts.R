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

#' @title
#' Create exposure cohorts
#' @description
#' Create all the exposure cohorts on a CDM
#' Note, will not recompute if they already exist
#' @param connection                    DatabaseConnector connection to cdm
#' @param config                        cdm config
#' @param deleteExisting                delete any existing computed cohorts
#' @export
createCohorts <- function(connection, config, deleteExisting = FALSE) {
  sql <- SqlRender::loadRenderTranslateSql("cohorts/createCohortTable.sql",
                                           package = utils::packageName(),
                                           dbms = connection@dbms,
                                           cohort_database_schema = config$resultSchema,
                                           cohort_table = config$tables$cohort,
                                           delete_existing = deleteExisting)
  DatabaseConnector::executeSql(connection, sql)

  createExposureCohorts(connection, config)
  createOutcomeCohorts(connection, config)
  computeAtlasCohorts(connection, config)
}

createExposureCohorts <- function(connection, config) {
  sql <- SqlRender::loadRenderTranslateSql("cohorts/createCohorts.sql",
                                           package = utils::packageName(),
                                           dbms = connection@dbms,
                                           cdm_database_schema = config$cdmSchema,
                                           reference_schema = config$referenceSchema,
                                           cohort_database_schema = config$resultSchema,
                                           vocab_schema = config$vocabularySchema,
                                           cohort_table = config$tables$cohort,
                                           exposure_cohort_table = config$tables$exposureCohort,
                                           cohort_definition = config$tables$cohortDefinition)

  DatabaseConnector::executeSql(connection, sql)
}

#' @title
#' get Uncomputed Atlas Cohorts
#' @description
#' Get cohorts that haven't been computed and return their references from file on disk
#' SQL and JSON references are not stored in the CDM database's scratch schema
#' @param connection                    connection
#' @param config                        cdmConfig
#' @param exposureCohorts               get exposures or not
getUncomputedAtlasCohorts <- function(connection, config) {

  # Get only null atlas cohorts
  atlaSql <- "
  SELECT aor.cohort_definition_id
  FROM @reference_schema.@atlas_reference aor
  LEFT JOIN
    (
      SELECT DISTINCT cohort_definition_id
      FROM @result_schema.@cohort_table
    ) oct on oct.cohort_definition_id = aor.cohort_definition_id
  WHERE oct.cohort_definition_id IS NULL
  "
  atlasCohorts <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                             atlaSql,
                                                             reference_schema = config$referenceSchema,
                                                             result_schema = config$resultSchema,
                                                             cohort_table = config$tables$cohort,
                                                             atlas_reference = config$tables$atlasCohortReference)

  fullCohorts <- read.csv(file.path(config$referencePath, "atlas_cohort_reference.csv"))
  return(fullCohorts[fullCohorts$COHORT_DEFINITION_ID %in% atlasCohorts$COHORT_DEFINITION_ID,])
}

createOutcomeCohorts <- function(connection, config) {
  ParallelLogger::logInfo("Creating concept ancestor grouping and table")
  sql <- SqlRender::readSql(system.file("sql/sql_server/cohorts", "createOutcomeCohorts.sql", package = utils::packageName()))
  DatabaseConnector::renderTranslateExecuteSql(connection,
                                               sql = sql,
                                               cohort_database_schema = config$resultSchema,
                                               reference_schema = config$referenceSchema,
                                               outcome_cohort = config$tables$outcomeCohort,
                                               cohort_table = config$tables$cohort)

  outcomeTypes <- list(
    type0 = list(
      type = 0,
      sql = SqlRender::readSql(system.file("sql/sql_server/cohorts", "createType0OutcomeCohorts.sql", package = utils::packageName()))
    ),
    type1 = list(
      type = 1,
      sql = SqlRender::readSql(system.file("sql/sql_server/cohorts", "createType1OutcomeCohorts.sql", package = utils::packageName()))
    ),
    type2 = list(
      type = 2,
      sql = SqlRender::readSql(system.file("sql/sql_server/cohorts", "createType2OutcomeCohorts.sql", package = utils::packageName()))
    )
  )

  # Build our set of already computed cohorts (ones with records).
  computedCohortsSql <- SqlRender::readSql(system.file("sql/sql_server/cohorts", "outcomeComputedCohorts.sql", package = utils::packageName()))
  DatabaseConnector::renderTranslateExecuteSql(
    connection,
    computedCohortsSql,
    cohort_database_schema = config$resultSchema,
    reference_schema = config$referenceSchema,
    outcome_cohort = config$tables$outcomeCohort,
    cohort_table = config$tables$cohort
  )

  computeSql <- SqlRender::readSql(system.file("sql/sql_server/cohorts", "outcomeCohortsToCompute.sql", package = utils::packageName()))
  # Closure calls sql to create uncomputed cohorts
  cohortsToCompute <- function(oType) {
    DatabaseConnector::renderTranslateExecuteSql(connection,
                                                 computeSql,
                                                 reference_schema = config$referenceSchema,
                                                 outcome_cohort_definition = config$tables$outcomeCohort,
                                                 outcome_type = oType)
    DatabaseConnector::renderTranslateQuerySql(connection,
                                               "SELECT count(*) as c_count FROM #cohorts_to_compute")$C_COUNT[[1]]
  }

  for (cohortType in outcomeTypes) {
    count <- cohortsToCompute(cohortType$type)
    while (count) {
      ParallelLogger::logInfo(count, " Uncomputed cohorts of type", cohortType$type)
      DatabaseConnector::renderTranslateExecuteSql(connection,
                                                   sql = cohortType$sql,
                                                   reference_schema = config$referenceSchema,
                                                   cdm_database_schema = config$cdmSchema,
                                                   cohort_database_schema = config$resultSchema,
                                                   cohort_table = config$tables$cohort,
                                                   outcome_cohort = config$tables$outcomeCohort)
      count <- cohortsToCompute(cohortType$type)
    }
  }
}

#' @title
#' Compute Atlas cohorts
#' @description
#' Computes sql cohorts against the CDM
#' @param connection          DatabaseConnector connection to cdm
#' @param config              cdmConfiguration object
computeAtlasCohorts <- function(connection, config) {
  atlasCohorts <- getUncomputedAtlasCohorts(connection, config)
  if (nrow(atlasCohorts) > 0) {
    # Generate each cohort
    apply(atlasCohorts, 1, function(cohortReference) {
      ParallelLogger::logInfo("computing custom cohort: ", cohortReference["COHORT_DEFINITION_ID"])
      DatabaseConnector::renderTranslateExecuteSql(connection,
                                                   sql = rawToChar(base64enc::base64decode(cohortReference["SQL_DEFINITION"])),
                                                   cdm_database_schema = config$cdmSchema,
                                                   vocabulary_database_schema = config$vocabularySchema,
                                                   target_database_schema = config$resultSchema,
                                                   target_cohort_table = config$tables$cohort,
                                                   target_cohort_id = cohortReference["COHORT_DEFINITION_ID"])
    })
  }
}
