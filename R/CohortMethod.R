#' get outcome cohort ids
#' @description
#' Return all outcome cohort ids
#' @param connection DatabaseConnector connection
#' @param config     cdm configuration
getAtlasOutcomeIds <- function(connection, config) {
  sql <- "SELECT oc.cohort_definition_id FROM @reference_schema.@outcome_cohort oc
  INNER JOIN @reference_schema.@atlas_cohort_reference ar on oc.cohort_definition_id = ar.cohort_definition_id"
  DatabaseConnector::renderTranslateQuerySql(connection,
                                             sql,
                                             snakeCaseToCamelCase = TRUE,
                                             reference_schema = config$referenceSchema,
                                             atlas_cohort_reference = config$tables$atlasCohortReference,
                                             outcome_cohort = config$tables$outcomeCohort)$cohortDefinitionId
}

createCohortMethodModuleSpecifications <- function(cmAnalysisList,
                                                   targetComparatorOutcomesList,
                                                   analysesToExclude = NULL,
                                                   refitPsForEveryOutcome = FALSE,
                                                   refitPsForEveryStudyPopulation = TRUE,
                                                   cmDiagnosticThresholds = createCmDiagnosticThresholds()) {
  analysis <- list()
  for (name in names(formals(createCohortMethodModuleSpecifications))) {
    analysis[[name]] <- get(name)
  }

  specifications <- list(module = "RewardCohortMethodSettings",
                         version = utils::packageVersion(utils::packageName()),
                         settings = analysis)
  class(specifications) <- c("CohortMethodModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}

createCmDesign <- function(targetId,
                           comparatorId,
                           indicationId,
                           outcomeCohortIds,
                           dataSources,
                           excludedCovariateConceptIds) {

  tcis <- list(
    #standard analyses that would be performed during routine signal detection
    list(
      targetId = targetId, # e.g New users of ACE inhibitors
      comparatorId = comparatorId, # e.g New users of Alpha-1 Blockers
      indicationId = indicationId, # e.g Hypertension
      genderConceptIds = c(8507, 8532), # use valid genders (remove unknown)
      minAge = NULL, # All ages In years. Can be NULL
      maxAge = NULL, # All ages In years. Can be NULL
      excludedCovariateConceptIds = excludedCovariateConceptIds
    )
  )
  outcomes <- tibble::tibble(
    cohortId = outcomeCohortIds,
    cleanWindow = c(365)
  )

  timeAtRisks <- tibble::tibble(
    label = c("On treatment", "fixed 365d"),
    riskWindowStart = c(1, 1),
    startAnchor = c("cohort start", "cohort start"),
    riskWindowEnd = c(0, 365),
    endAnchor = c("cohort end", "cohort start"),
  )

  studyStartDate <- "20010101" # YYYYMMDD, e.g. "2001-02-01" for January 1st, 2001
  studyEndDate <- "20251231" # YYYYMMDD

  useCleanWindowForPriorOutcomeLookback <- FALSE # If FALSE, lookback window is all time prior, i.e., including only first events
  psMatchMaxRatio <- 1 # If bigger than 1, the outcome model will be conditioned on the matched set

  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
    addDescendantsToExclude = TRUE # Keep TRUE because you're excluding concepts
  )
  outcomeList <-
    lapply(seq_len(nrow(outcomes)), function(i) {
      if (useCleanWindowForPriorOutcomeLookback)
        priorOutcomeLookback <- outcomes$cleanWindow[i]
      else
        priorOutcomeLookback <- 99999
      CohortMethod::createOutcome(
        outcomeId = outcomes$cohortId[i],
        outcomeOfInterest = TRUE,
        trueEffectSize = NA,
        priorOutcomeLookback = priorOutcomeLookback
      )
    })

  targetComparatorOutcomesList <- list()
  for (i in seq_along(tcis)) {
    tci <- tcis[[i]]
    targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
      targetId = tci$targetId,
      comparatorId = tci$comparatorId,
      outcomes = outcomeList,
      excludedCovariateConceptIds = tci$excludedCovariateConceptIds
    )
  }
  getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
    restrictToCommonPeriod = TRUE,
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate,
    maxCohortSize = 0,
    covariateSettings = covariateSettings
  )
  createPsArgs = CohortMethod::createCreatePsArgs(
    maxCohortSizeForFitting = 250000,
    errorOnHighCorrelation = TRUE,
    stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
    estimator = "att",
    prior = Cyclops::createPrior(
      priorType = "laplace",
      exclude = c(0),
      useCrossValidation = TRUE
    ),
    control = Cyclops::createControl(
      noiseLevel = "silent",
      cvType = "auto",
      seed = 1,
      resetCoefficients = TRUE,
      tolerance = 2e-07,
      cvRepetitions = 1,
      startingVariance = 0.01
    )
  )
  matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
    maxRatio = psMatchMaxRatio,
    caliper = 0.2,
    caliperScale = "standardized logit",
    allowReverseMatch = FALSE,
    stratificationColumns = c()
  )
  # stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
  #   numberOfStrata = 5,
  #   stratificationColumns = c(),
  #   baseSelection = "all"
  # )
  computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
    maxCohortSize = 250000,
    covariateFilter = NULL
  )
  computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
    maxCohortSize = 250000,
    covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
  )
  fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
    modelType = "cox",
    stratified = psMatchMaxRatio != 1,
    useCovariates = FALSE,
    inversePtWeighting = FALSE,
    prior = Cyclops::createPrior(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = Cyclops::createControl(
      cvType = "auto",
      seed = 1,
      resetCoefficients = TRUE,
      startingVariance = 0.01,
      tolerance = 2e-07,
      cvRepetitions = 10,
      noiseLevel = "quiet"
    )
  )
  cmAnalysisList <- list()
  for (i in seq_len(nrow(timeAtRisks))) {
    createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
      firstExposureOnly = FALSE,
      washoutPeriod = 0,
      removeDuplicateSubjects = "keep first",
      censorAtNewRiskWindow = TRUE,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 99999,
      riskWindowStart = timeAtRisks$riskWindowStart[[i]],
      startAnchor = timeAtRisks$startAnchor[[i]],
      riskWindowEnd = timeAtRisks$riskWindowEnd[[i]],
      endAnchor = timeAtRisks$endAnchor[[i]],
      minDaysAtRisk = 1,
      maxDaysAtRisk = 99999
    )
    cmAnalysisList[[i]] <- CohortMethod::createCmAnalysis(
      analysisId = i,
      description = sprintf(
        "Cohort method, %s",
        timeAtRisks$label[i]
      ),
      getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
      createStudyPopArgs = createStudyPopArgs,
      createPsArgs = createPsArgs,
      matchOnPsArgs = matchOnPsArgs,
      # stratifyByPsArgs = stratifyByPsArgs,
      computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
      computeCovariateBalanceArgs = computeCovariateBalanceArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs
    )
  }
  cohortMethodModuleSpecifications <- createCohortMethodModuleSpecifications(
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    analysesToExclude = NULL,
    refitPsForEveryOutcome = FALSE,
    refitPsForEveryStudyPopulation = FALSE,
    cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds(
      mdrrThreshold = Inf,
      easeThreshold = 0.25,
      sdmThreshold = 0.1,
      equipoiseThreshold = 0.2,
      generalizabilitySdmThreshold = 1
    )
  )


  return(cohortMethodModuleSpecifications)
}



#' Execute Cohort method analysis
#' @description
#' Take cohort method settings and execute on CDM
#' @export
#' @param config        cdmConfiguration
#' @param cmSettings    cohortMethod settings object to execute
executeCohortMethodAnalysis <- function(config, cmConfig) {
  multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(12)

  if (!dir.exists(config$workDir)) {
    dir.create(config$workDir)
  }
  connection <- DatabaseConnector::connect(config$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  settings <- createCmDesign(targetId = cmConfig$targetId,
                             comparatorId = cmConfig$comparatorId,
                             indicationId = cmConfig$indicationId,
                             outcomeCohortIds = getAtlasOutcomeIds(connection, config),
                             excludedCovariateConceptIds = cmConfig$excludedCovariateConceptIds)$settings

  settings$connectionDetails <- config$connectionDetails
  settings$cdmDatabaseSchema <- config$cdmSchema
  settings$exposureDatabaseSchema <- config$resultSchema
  settings$exposureTable <- config$tables$cohort
  settings$outcomeDatabaseSchema <- config$resultSchema
  settings$outcomeTable <- config$tables$cohort
  settings$outputFolder <- file.path(config$workDir, "CohortMethodOutput")
  settings$multiThreadingSettings <- multiThreadingSettings
  settings$cmDiagnosticThresholds  <- NULL
  do.call(CohortMethod::runCmAnalyses, settings)

  tryCatch({
    CohortMethod::exportToCsv(outputFolder = settings$outputFolder,
                              exportFolder = config$exportPath,
                              databaseId = config$sourceId,
                              maxCores = parallel::detectCores() - 1)
  }, error = function(err) {
    if (grepl("java.lang.RuntimeException: java.io.FileNotFoundException", err$message)) {
      zipName <- file.path(config$exportPath, sprintf("Results_%s.zip", config$sourceId))
      utils::zip(zipName, files = list.files(config$exportPath, pattern = ".*\\.csv$", full.names = TRUE))
    } else {
      # Re-throw the error if it's not the one we're looking for
      stop(err)
    }
  })
}
