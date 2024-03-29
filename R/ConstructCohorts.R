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

#' Get Atlas Cohort Definition Set
#' @description
#' Get Reward atlas cohort definitions
#' @param config        CdmConfig object
#' @export
getAtlasCohortDefinitionSet <- function(config) {
  cds <- CohortGenerator::getCohortDefinitionSet(settingsFileName = file.path(config$referencePath, "atlas_cohorts.csv"),
                                                 jsonFolder = file.path(config$referencePath, "cohorts"),
                                                 sqlFolder = file.path(config$referencePath, "sql"),
                                                 subsetJsonFolder = NULL,
                                                 cohortFileNameFormat = "%s",
                                                 cohortFileNameValue = c("cohortId"))
  return(cds)
}

#' Generate bulk cohorts with cohort generator
#' @description
#' Use cohort generator to create cohorts
#' @param config        CdmConfig object
#' @param connection    DatabaseConnector::connection instance
#' @export
generateAtlasCohortSet <- function(config, connection = NULL) {
  tableNames <- CohortGenerator::getCohortTableNames(config$tables$cohort)

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(config$connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  CohortGenerator::createCohortTables(connection = connection,
                                      cohortDatabaseSchema = config$resultSchema,
                                      cohortTableNames = tableNames,
                                      incremental = TRUE)

  cohortDefinitionSet <- getAtlasCohortDefinitionSet(config)

  sql <- "SELECT * FROM @schema.@subset_table"
  subsetTargets <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                              schema = config$resultSchema,
                                                              subset_table = config$tables$cohortSubsetTarget,
                                                              snakeCaseToCamelCase = TRUE,
                                                              sql = sql)
  # Remove subset cohort defs
  cohortDefinitionSet <- cohortDefinitionSet %>%
    dplyr::filter(!.data$cohortId %in% subsetTargets$subsetCohortDefinitionId)

  CohortGenerator::generateCohortSet(connection = connection,
                                     cdmDatabaseSchema = config$cdmSchema,
                                     cohortDatabaseSchema = config$resultSchema,
                                     cohortTableNames = tableNames,
                                     cohortDefinitionSet = cohortDefinitionSet,
                                     stopOnError = FALSE,
                                     incremental = TRUE,
                                     incrementalFolder = file.path(config$referencePath, "incremental"))
  # Load subset definitions and compute them
  if (dir.exists(file.path(config$referencePath, "subset_definitions"))) {
    # Load subset definitions
    recordKeepingFile <- file.path(config$referencePath, "incremental", "GeneratedCohorts.csv")


    .GlobalEnv$subsetTargets <- subsetTargets
    for (subsetDefId in unique(subsetTargets$subsetDefinitionId)) {
      # Load definition json
      jsonPath <- file.path(config$referencePath, "subset_definitions", paste0(subsetDefId, ".json"))
      jsonDef <- SqlRender::readSql(jsonPath)
      # Load definition from json representation
      subsetDef <- CohortGenerator::CohortSubsetDefinition$new(jsonDef)

      .GlobalEnv$.getSubsetCohortId <- function(targetId, definitionId) {
        subsetTargets %>%
          dplyr::filter(.data$cohortDefinitionId == targetId,
                        .data$subsetDefinitionId == definitionId) %>%
          dplyr::select("subsetCohortDefinitionId") %>%
          dplyr::pull()
      }

      subsetDef$identifierExpression <- expression(.GlobalEnv$.getSubsetCohortId(targetId, definitionId))

      # Set id at runtime
      subsetDef$definitionId <- subsetDefId

      targetIds <- subsetTargets %>%
        dplyr::filter(.data$subsetDefinitionId == subsetDefId) %>%
        dplyr::select("cohortDefinitionId") %>%
        dplyr::pull()

      dummySet <- data.frame(
        cohortId = targetIds,
        json = "{}",
        sql = "DUMMMY",
        isSubset = FALSE,
        subsetParent = targetIds
      )

      cohortDefinitionSet <- dplyr::bind_rows(cohortDefinitionSet, dummySet) %>% dplyr::distinct()
      cohortDefinitionSet <- cohortDefinitionSet %>%
        CohortGenerator::addCohortSubsetDefinition(subsetDef,
                                                   targetCohortIds = targetIds)
    }


    sql <- "SELECT * FROM @schema.@cohort_table WHERE IS_SUBSET = 1"
    subsetCohorts <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                                schema = config$resultSchema,
                                                                cohort_table = config$tables$cohortDefinition,
                                                                snakeCaseToCamelCase = TRUE,
                                                                sql = sql)
    # Generate only subset cohorts
    for (cohortId in subsetCohorts$cohortDefinitionId) {
      CohortGenerator:::generateCohort(cohortId = cohortId,
                                       cohortDefinitionSet,
                                       connection = connection,
                                       cdmDatabaseSchema = config$cdmSchema,
                                       cohortDatabaseSchema = config$resultSchema,
                                       tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                       cohortTableNames = tableNames,
                                       stopIfError = FALSE,
                                       incremental = FALSE,
                                       recordKeepingFile = recordKeepingFile)
    }
  }
}

createOutcomeCohorts <- function(connection, config) {
  message("Creating concept ancestor grouping and table")
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
      message(count, " Uncomputed cohorts of type", cohortType$type)
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
