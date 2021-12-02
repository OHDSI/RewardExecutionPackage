test_that("Cohort construction works", {
  cdmConfig <- loadCdmConfiguration(cdmConfigPath)
  importReferenceTables(connection, cdmConfig, referenceZipPath)
  createCohorts(connection, cdmConfig)
  createOutcomeCohorts(connection, cdmConfig)
  expect_true(TRUE)
})