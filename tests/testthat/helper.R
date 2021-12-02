# When devtools::load_all is run, create symbolic link for sql directory
# Allows testing with devtools::test on osx and linux
if (Sys.getenv("DEVTOOLS_LOAD") == "true" & .Platform$OS.type == "unix") {
  print("setting sql folder symobolic link")
  packageRoot <- normalizePath(system.file("..", package = .PackageName))
  # Create symbolic link so code can be used in devtools::test()
  linkPath <- file.path(packageRoot, "sql")
  if (!file.exists(linkPath)) {
    R.utils::createLink(link = linkPath, system.file("sql", package = .PackageName))
    options("use.devtools.sql_shim" = TRUE)
    #withr::defer(unlink(file.path(packageRoot, "sql")), testthat::teardown_env())
  }
}

# TODO: REWARD will export a test zip based on eunomia references
makeTestReferenceZip <- function(pathToReferenceFiles = "test_reference_files",
                                 pathToZip = "reward-test-refs.zip") {

  # Get md5 sums of files

  # Collect all files and make a hash
  meta <- list()
  meta$hashList <- list()
  meta$tableNames <- CONST_REFERENCE_TABLES

  for (table in CONST_REFERENCE_TABLES) {
    file <- file.path(pathToReferenceFiles, paste0(table, ".csv"))
    meta$hashList[[basename(file)]] <- tools::md5sum(file)[[1]]
  }

  metaDataFilename <- file.path(pathToReferenceFiles, CONST_META_FILE_NAME)
  jsonlite::write_json(meta, metaDataFilename)

  exportFiles <- file.path(pathToReferenceFiles, paste0(CONST_REFERENCE_TABLES, ".csv"))
  # Zip all csv files
  zip::zipr(pathToZip, append(exportFiles, metaDataFilename), include_directories = FALSE)

}
