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

CONST_RESULTS_TABLES <- c(
  "cohort",
  "scc_result"
)

CONST_REFERENCE_TABLES <- c(
  'cohort_definition',
  'exposure_cohort',
  'outcome_cohort',
  'cohort_group_definition',
  'cohort_group',
  'concept_set_definition',
  'atlas_cohort_reference',
  'cohort_concept_set',
  'analysis_setting',
  'reference_version'
)

#' load cdm config object
#' @description
#' Loads config and prompt user for db password
#' Password can be set in envrionment variable passwordEnvironmentVariable of yaml file
#' @param cdmConfigPath                 path to cdm configuration file
#' @param keyring                       keyring::keyring. For systems that support multiple keyrings, specify the name of the keyring to use here.
#'                                      If NULL, then the default keyring is used.
#' @importFrom keyring key_get
#' @importFrom SqlRender snakeCaseToCamelCase
#' @export
loadCdmConfiguration <- function(cdmConfigPath, keyring = NULL) {
  checkmate::assertFileExists(cdmConfigPath)
  defaults <- list(
    useSecurePassword = FALSE,
    bulkUpload = TRUE,
    includeConstraints = FALSE,
    useAwsS3Export = FALSE,
    awsS3Log = "s3-log.csv"
  )
  config <- yaml::read_yaml(cdmConfigPath)
  config <- .setDefaultOptions(config, defaults)

  defaultTables <- list()
  for (table in c(CONST_REFERENCE_TABLES, CONST_RESULTS_TABLES)) {
    defaultTables[[SqlRender::snakeCaseToCamelCase(table)]] <- table
  }

  config$tables <- .setDefaultOptions(config$tables, defaultTables)

  if (is.null(config$connectionDetails$user)) {
    user <- Sys.getenv("REWARD_CDM_USER", Sys.info()[["user"]])
    config$connectionDetails$user <- user
  }

  if (config$useSecurePassword & !is.null(config$keyringService)) {
    message("Using keyring service ", config$keyringService, " to set database password for ", config$connectionDetails$user)
    config$connectionDetails$password <- keyring::key_get(config$keyringService, username = config$connectionDetails$user, keyring = keyring)
  }
  config$connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, config$connectionDetails)

  if (!is.null(config$sqlRenderTempEmulationSchema)) {
    message("Setting temp emulation schema to ", config$sqlRenderTempEmulationSchema)
    options("sqlRenderTempEmulationSchema" = config$sqlRenderTempEmulationSchema)
  }

  class(config) <- "CdmConfig"
  return(config)
}

.setDefaultOptions <- function(config, defaults) {
  for (n in names(defaults)) {
    if (is.null(config[[n]])) {
      config[[n]] <- defaults[[n]]
    }
  }
  return(config)
}

#' Create CDM configuration file
#' @description
#' Opens a file for editing that contains the default settings for a cdm
#'
#' @inheritParams validateCdmConfigFile
#' @param overwrite                     Overwite existing file (if it exists)
#' @export
createCdmConfiguration <- function(cdmConfigPath,
                                   keyring = NULL,
                                   overwrite = FALSE,
                                   testConnection = TRUE) {
  # Copy default file
  defaultCdmPath <- system.file("yml", "default.cdm.yml", package = "RewardExecutionPackage")

  if (!base::file.exists(cdmConfigPath) | overwrite) {
    message("Creating new configuration file")
    base::file.copy(defaultCdmPath, cdmConfigPath)
  } else {
    message("Editing existing file")
  }

  utils::file.edit(cdmConfigPath)
  cdmConfig <- yaml::read_yaml(cdmConfigPath)
  if (!is.null(cdmConfig$useSecurePassword) & cdmConfig$useSecurePassword) {
    if (is.null(cdmConfig$keyringService)) {
      stop("keyringService parameter must be set when using keyring")
    }

    user <- cdmConfig$connectionDetails$user
    if (is.null(user)) {
      user <- Sys.getenv("REWARD_CDM_USER", Sys.info()[["user"]])
    }

    tryCatch({
      keyring::key_get(cdmConfig$keyringService, username = user, keyring = keyring)
      message("Password for", user, " with service ", cdmConfig$keyringService, " already exists. Use keyring::keyset to change")
    },
      error = function(...) {
        message("Set keyring password for cdm user ", user, " with service ", cdmConfig$keyringService)
        keyring::key_set(cdmConfig$keyringService,
                         username = user,
                         keyring = keyring,
                         prompt = paste("Enter CDM database password for user", user))
      })
  }

  validateCdmConfigFile(cdmConfigPath, testConnection = testConnection)
}

#' Validate a cdm configuration file
#' @description
#' Opens a file for editing that contains the default settings for a cdm
#'
#' @inheritParams loadCdmConfiguration
#' @param testConnection               Attempt to connect to database and write to schemas needed for writing?
#' @param testS3Rw                     If useAwsS3Export is set to true, test reding and writing of objects to s3
#' @export
validateCdmConfigFile <- function(cdmConfigPath, testConnection = TRUE, keyring = NULL, testS3Rw = TRUE) {
  # Check required parameters exist
  requiredNames <- c("sourceId",
                     "database",
                     "name",
                     "connectionDetails",
                     "resultSchema",
                     "referenceSchema",
                     "vocabularySchema",
                     "cdmSchema")

  checkmate::assertFileExists(cdmConfigPath)
  cdmConfig <- yaml::read_yaml(cdmConfigPath)
  checkmate::assertNames(names(cdmConfig), must.include = requiredNames)
  message("Required names are present.")

  if (isTRUE(cdmConfig$drugEraSchema == cdmConfig$cdmSchema)) {
    stop("drugEraSchema and cdmSchema cannot be the same.
    The drug era schema is for custom drug eras not captured as part of ETL process")
  }

  if (testConnection) {
    cdmConfig <- loadCdmConfiguration(cdmConfigPath, keyring = keyring)
    message("Configuration loads, checking database connection")
    tryCatch({
      connection <- DatabaseConnector::connect(cdmConfig$connectionDetails)
    }, error = function(msg) {
      stop(paste("Error with connection details could not connect to database"))
    })
    on.exit(DatabaseConnector::disconnect(connection))

    if (connection@dbms == "redshift" & system.file(package = "aws.s3") == "") {
      message("aws.s3 required for bulk upload. Installing package.
      See DatabaseConnector documentation for details on S3 configuration")
      install.packages("aws.s3")
    }

    # Test schemas exists
    message("Checking CDM schema")
    tryCatch({
      DatabaseConnector::renderTranslateQuerySql(connection,
                                                 "SELECT * FROM @cdm_schema.cdm_source",
                                                 cdm_schema = cdmConfig$cdmSchema)
    }, error = function(msg) {
      stop("Invalid CDM schema ", cdmConfig$cdmSchema)
    })

    message("Checking vocabulary schema")
    tryCatch({
      DatabaseConnector::renderTranslateQuerySql(connection,
                                                 "SELECT * FROM @vocabulary_schema.vocabulary",
                                                 vocabulary_schema = cdmConfig$vocabularySchema)
    }, error = function(msg) {
      stop("Invalid vocabulary schema ", cdmConfig$vocabularySchema)
    })

    message("Checking results schemas")
    # Test schemas exists and are writable
    testSql <- "
    CREATE TABLE @schema.@test_table_name (id INTEGER);
    DROP TABLE @schema.@test_table_name;
    "

    for (schema in unique(c(cdmConfig$resultsSchema, cdmConfig$referenceSchema, cdmConfig$drugEraSchema))) {
      if (!is.null(schema)) {
        tableName <- stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z]")
        tryCatch({
          DatabaseConnector::renderTranslateExecuteSql(connection,
                                                       testSql,
                                                       progressBar = FALSE,
                                                       test_table_name = tableName,
                                                       schema = schema)
        }, error = function(msg) {
          stop("Invalid schema: ", schema, " cannot write table")
        })
      }
    }
    message("Database configuration appears valid")
  }

  if (cdmConfig$useAwsS3Export) {
    if (system.file(package = "aws.s3") == "") {
      install.packages("aws.s3")
    }

    if (testS3Rw) {
      message("Testing AWS S3 read/write")
      object <- paste(Sys.getenv("AWS_OBJECT_KEY"), tempfile(fileext = ".csv"), sep = "/")
      tryCatch({
        aws.s3::s3write_using(mtcars,
                              readr::write_csv,
                              object = object,
                              bucket = Sys.getenv("AWS_BUCKET_NAME"),
                              na = "",
                              opts = list(
                                headers = list(`x-amz-server-side-encryption` = Sys.getenv("AWS_SSE_TYPE")),
                                check_region = FALSE
                              ),
                              append = FALSE)

        aws.s3::delete_object(object = object,
                              bucket = Sys.getenv("AWS_BUCKET_NAME"))
      }, error = function(err) {
        stop("Error using s3 bucket - ", err)
      })
    }
  }

  message("Configuration appears valid")
}

#' Create targets file
#' @description
#' Targets is a workflow manager for R execution tasks that prevents the reptition of work.
#' This utility function creates a targets file that can be used to execute Reward on a CDM
#'
#' @param path          Path where targets file will live - targets expects _targets.R by default
#' @export
createTargetsFile <- function(path = "_targets.R", overwrite = FALSE) {
  if (!overwrite & file.exists(path)) {
    stop("File ", path, "already exists. Set overwrite = TRUE to continue")
  }

  if (system.file(package = "targets") == "") {
    message("targets package not found. Installing.")
    install.packages("targets")
  }

  file.copy(system.file(file.path("templates", "targets_exec.R"), package = packageName()), path)
  message("Created targets file", path)
}