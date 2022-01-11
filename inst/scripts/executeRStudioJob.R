library(RewardExecutionPackage)
requiredParams <- c("cdmConfigPath", "referenceZipFile")
optionalParams <- c("deleteExistingCohorts" = FALSE)

if (!all(sapply(requiredParams, exists))) {
  stop("Parameters no defined in environment scope")
}

execute(cdmConfigPath, referenceZipFile, deleteExistingCohorts = deleteExistingCohorts)
