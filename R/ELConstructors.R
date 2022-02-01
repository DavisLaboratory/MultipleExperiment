#' @export
ExperimentList <- function(experiments = SimpleList(),
                                    experimentData = DataFrame(),
                                    check.names = TRUE,
                                    change.names = TRUE) {
  if (length(experiments) == 0) {
    return(.SummarizedExperimentList())
  }

  #check experiments
  struct = unique(sapply(experiments, class))
  if (length(struct) != 1) {
    stop("each experiment in 'experiments' must be of the same type")
  }

  #identify constructor
  constructorFun = switch(
    struct,
    SummarizedExperiment = .SummarizedExperimentList,
    RangedSummarizedExperiment = .RangedSummarizedExperimentList,
    SingleCellExperiment = .SingleCellExperimentList,
    SpatialExperiment = .SpatialExperimentList
  )

  #coerce matrix and data.frame
  if (is(experimentData, 'matrix') | is(experimentData, 'data.frame')) {
    experimentData = as(experimentData, 'DataFrame')
  }

  #change colnames
  experimentNames = names(experiments)
  if (change.names) {
    if (is.null(experimentNames)) {
      eNames = seq_len(length(experiments))
    } else {
      eNames = experimentNames
    }

    experiments = mapply(function(x, y) {
      #if colnames present, change
      if (!is.null(colnames(x)))
        colnames(x) = paste(y, colnames(x), sep = '.')
      return(x)
    }, experiments, eNames, SIMPLIFY = FALSE)
  }

  #create experimentData if missing
  if (nrow(experimentData) == 0) {
    experimentData = S4Vectors::DataFrame(matrix(nrow = length(experiments), ncol = 0))
    rownames(experimentData) = experimentNames
  } else {
    stopifnot(length(experiments) == nrow(experimentData))
  }

  #check experimentData
  if (check.names) {
    stopifnot(all(names(experiments) %in% rownames(experimentData)))
  } else {
    stopifnot(length(experiments) == nrow(experimentData))
  }

  if (is(experimentData, 'data.frame'))
    experimentData = as(experimentData, 'DataFrame')

  #create reference map
  experimentIndex = rep(1:length(experiments), times = sapply(experiments, ncol))

  #combine SEs
  se = switch(
    struct,
    SummarizedExperiment = do.call(SummarizedExperiment::cbind, experiments),
    RangedSummarizedExperiment = do.call(SummarizedExperiment::cbind, experiments),
    SingleCellExperiment = do.call(SingleCellExperiment::cbind, experiments),
    SpatialExperiment = do.call(BiocGenerics::cbind, experiments)
  )

  #create list object
  selist = constructorFun(se,
                          experimentData = experimentData,
                          experimentIndex = experimentIndex)

  return(selist)
}

setValidity2('SummarizedExperimentList', function(object) {
  NR = NROW(object)
  NC = NCOL(object)
  NE = NROW(experimentData(object))
  msg = NULL

  if (any(is.na(object@experimentIndex))) {
    msg <- c(msg, "'experimentIndex' should not have NAs")
  }

  if (length(object@experimentIndex) != NC) {
    msg <- c(msg, "'experimentIndex' should have length equal to the number of samples across all experiments")
  }

  if (length(object@experimentIndex) != NC) {
    msg <- c(msg, "'experimentIndex' should have length equal to the number of samples across all experiments")
  }

  if (max(object@experimentIndex) > NE) {
    msg <- c(msg, "'experimentIndex' should not have indices greater than the number of experiments")
  }

  if (min(object@experimentIndex) < 0) {
    msg <- c(msg, "'experimentIndex' should have non-negative indices")
  }

  if (!is(object, 'SummarizedExperiment')) {
    msg <- c(msg, "'experimentIndex' should have non-negative indices")
  }

  if (length(msg)) {
    msg
  } else TRUE
})
