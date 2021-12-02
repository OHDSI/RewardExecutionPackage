.PackageName = "RewardStudyPackage"

#' load cdm config object
#' @description
#' Loads config and prompt user for db password
#' Password can be set in envrionment variable passwordEnvironmentVariable of yaml file
#' @param cdmConfigPath                 path to cdm configuration file
#' @importFrom keyring keyget
#' @importFrom SqlRender snakeCaseToCamelCase
#' @export
loadCdmConfiguration <- function(cdmConfigPath) {
  checkmate::assertFileExists(cdmConfigPath)
  defaults <- list(
    useSecurePassword = FALSE,
    bulkUpload = FALSE,
    performActiveDataTransfer = FALSE
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
    ParallelLogger::logInfo("Using keyring service ", config$keyringService, "to set database passwrod for", config$connectionDetails$user)
    config$connectionDetails$password <- keyring::key_get(config$keyringService, username = config$connectionDetails$user)
  }
  config$connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, config$connectionDetails)

  if (!is.null(config$sqlRenderTempEmulationSchema)) {
    ParallelLogger::logInfo("Setting temp emulation schema to ", config$sqlRenderTempEmulationSchema)
    options("sqlRenderTempEmulationSchema" = config$sqlRenderTempEmulationSchema)
  }

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
createCdmConfiguration <- function(cdmConfigPath, overwrite = FALSE, testConnection = TRUE) {
  # Copy default file
  defaultCdmPath <- system.file("yml", "default.cdm.yml", package = "RewardStudyPackage")

  if (!base::file.exists(cdmConfigPath) | overwrite) {
    ParallelLogger::logInfo("Creating new configuration file")
    base::file.copy(defaultCdmPath, cdmConfigPath)
  } else {
    ParallelLogger::logInfo("Editing existing file")
  }

  utils::file.edit(cdmConfigPath)
  cdmConfig <- yaml::read_yaml(cdmConfigPath)
  if (!is.null(cdmConfig$useSecurePassword)) {
    if (is.null(cdmConfig$keyringService)) {
      stop("keyringService parameter must be set when using keyring")
    }

    user <- cdmConfig$connectionDetails$user
    if (is.null(user)) {
      user <- Sys.getenv("REWARD_CDM_USER", Sys.info()[["user"]])
    }

    tryCatch({
      keyring::key_get(cdmConfig$keyringService, username = user)
      message("Password for", user, " with service ", cdmConfig$keyringService, " already exists. Use keyring::keyset to change")
    },
    error = function(...) {
      message("Set keyring password for cdm user ", user, " with service ", cdmConfig$keyringService)
      keyring::key_set(cdmConfig$keyringService, username = user)
    })
  }

  validateCdmConfigFile(cdmConfigPath, testConnection = testConnection)
}

#' Validate a cdm configuration file
#' @description
#' Opens a file for editing that contains the default settings for a cdm
#'
#' @param cdmConfigPath                 path to store configuration file
#' @param testConnection                Attempt to connect to database and write to schemas needed for writing?
#' @export
validateCdmConfigFile <- function(cdmConfigPath, testConnection = TRUE) {
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
  ParallelLogger::logInfo("Required names are present.")

  if (isTRUE(cdmConfig$drugEraSchema == cdmConfig$cdmSchema)) {
    stop("drugEraSchema and cdmSchema cannot be the same.
    The drug era schema is for custom drug eras not captured as part of ETL process")
  }

  if (testConnection) {
    cdmConfig <- loadCdmConfiguration(cdmConfigPath)
    ParallelLogger::logInfo("Configuration loads, checking database connection")
    tryCatch({
      connection <- DatabaseConnector::connect(cdmConfig$connectionDetails)
    }, error = function(msg) {
      stop(paste("Error with connection details could not connect to database"))
      ParallelLogger::logError(msg)
    })
    on.exit(DatabaseConnector::disconnect(connection))

    # Test schemas exists
    ParallelLogger::logInfo("Checking CDM schema")
    tryCatch({
      DatabaseConnector::renderTranslateQuerySql(connection,
                                                 "SELECT * FROM @cdm_schema.cdm_source",
                                                 cdm_schema = cdmConfig$cdmSchema)
    }, error = function(msg) {
      ParallelLogger::logError(msg)
      stop("Invalid CDM schema ", cdmConfig$cdmSchema)
    })

    ParallelLogger::logInfo("Checking vocabulary schema")
    tryCatch({
      DatabaseConnector::renderTranslateQuerySql(connection,
                                                 "SELECT * FROM @vocabulary_schema.vocabulary",
                                                 vocabulary_schema = cdmConfig$vocabularySchema)
    }, error = function(msg) {
      ParallelLogger::logError(msg)
      stop("Invalid vocabulary schema ", cdmConfig$vocabularySchema)
    })

    ParallelLogger::logInfo("Checking results schemas")
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
          ParallelLogger::logError(msg)
          stop("Invalid schema: ", schema, " cannot write table")
        })
      }
    }
    ParallelLogger::logInfo("Database configuration appears valid")
  }

   ParallelLogger::logInfo("Configuration appears valid")
}
