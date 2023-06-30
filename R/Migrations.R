RewardMigrationManager <- R6::R6Class(
  classname = "RewardMigrationManager",
  inherit = ResultModelManager::DataMigrationManager,
  public = list(
    includeConstraints = FALSE,
    initialize = function(connectionDetails, ...) {
      self$includeConstraints <- connectionDetails$dbms != "sqlite"
      super$initialize(connectionDetails = connectionDetails, ...)
    }
  ),
  private = list(
    executeMigration = function(migration) {
      private$logInfo("Executing migration: ", migration$migrationFile)
      # Load, render, translate and execute sql

      sql <- SqlRender::loadRenderTranslateSql(file.path(self$migrationPath, migration$migrationFile),
                                               dbms = private$connectionDetails$dbms,
                                               database_schema = self$databaseSchema,
                                               include_constraints = self$includeConstraints,
                                               table_prefix = self$tablePrefix,
                                               packageName = self$packageName)
      private$connectionHandler$executeSql(sql)

      private$logInfo("Saving migration: ", migration$migrationFile)
      # Save migration in set of migrations
      iSql <- "
      {DEFAULT @migration = migration}
      INSERT INTO @database_schema.@table_prefix@migration (migration_file, migration_order)
        VALUES ('@migration_file', @order);
      "
      private$connectionHandler$executeSql(iSql,
                                           database_schema = self$databaseSchema,
                                           migration_file = migration$migrationFile,
                                           table_prefix = self$tablePrefix,
                                           order = migration$migrationOrder
      )
      private$logInfo("Migration complete ", migration$migrationFile)
    }
  )
)

#' Get Migration Manager
#' @description
#' Get RMM database migration instance
#' @export
#' @param config cdm or reward global config
#' @param schema database schema to migrate
getMigrationManager <- function(config, schema = config$referenceSchema) {
  RewardMigrationManager$new(
    connectionDetails = config$connectionDetails,
    databaseSchema = schema,
    tablePrefix = "",
    migrationPath = "migrations",
    packageName = "RewardExecutionPackage"
  )
}

#' Migrate database Schema
#' @export
#' @description
#' RMM db migrator
#' @param ... see getMigrationManager
migrateDatabaseModel <- function(...) {
  manager <- getMigrationManager(...)
  manager$executeMigrations()
}
