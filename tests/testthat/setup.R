cdmConfigPath <- "config/test.cdm.yml"
cfg <- yaml::read_yaml(cdmConfigPath)
unlink(cfg$connectionDetails$server)
referenceZipPath <- file.path("test_reference_files", "reward-references.zip")

withr::defer({
  # Clean up Eunomia instance
  unlink(cfg$connectionDetails$server)
  unlink("export", recursive = TRUE, force = TRUE)
}, testthat::teardown_env())