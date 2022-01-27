#' @inheritParams SummarizedExperimentList
#' @param experiments a named list of [SummarizedExperiment],
#'   [RangedSummarizedExperiment], [SingleCellExperiment], or
#'   [SpatialExperiment] objects.
#' @param experimentData a [DataFrame] with experiment specific annotations. The
#'   number of rows in the data frame should be equal to the number of
#'   experiments. Row names of the data frame should match names of experiments
#'   (unless check.names is FALSE).
#' @param check.names a logical specifying whether rownames of `experimentData`
#'   should be checked against experiment names in `experiments`.
#' @param change.names a logical specifying whether experiment names should be
#'   appended to the column names of individual observations.
#'
#' @export
SummarizedExperimentList <- function(experiments = SimpleList(),
                                     experimentData = DataFrame(),
                                     check.names = TRUE,
                                     change.names = TRUE) {
  #no experiment
  if (length(experiments) == 0) {
    se = SummarizedExperiment::SummarizedExperiment()
    return(.SummarizedExperimentList(se))
  }

  .processConstructorData(
    experiments = experiments,
    experimentData = experimentData,
    check.names = check.names,
    change.names = change.names,
    struct = 'SummarizedExperiment'
  )
}

#' @inheritParams SummarizedExperimentList
#' @export
RangedSummarizedExperimentList <-
  function(experiments = SimpleList(),
           experimentData = DataFrame(),
           check.names = TRUE,
           change.names = TRUE) {
    #no experiment
    if (length(experiments) == 0) {
      gr = GenomicRanges::GRangesList()
      se = SummarizedExperiment::SummarizedExperiment(rowRanges = gr)
      return(.RangedSummarizedExperimentList(se))
    }

    .processConstructorData(
      experiments = experiments,
      experimentData = experimentData,
      check.names = check.names,
      change.names = change.names,
      struct = 'RangedSummarizedExperiment'
    )
  }

#' @inheritParams SummarizedExperimentList
#' @export
SingleCellExperimentList <-
  function(experiments = SimpleList(),
           experimentData = DataFrame(),
           check.names = TRUE,
           change.names = TRUE) {
    #no experiment
    if (length(experiments) == 0) {
      sce = SingleCellExperiment::SingleCellExperiment()
      return(.SingleCellExperimentList(sce))
    }

    .processConstructorData(
      experiments = experiments,
      experimentData = experimentData,
      check.names = check.names,
      change.names = change.names,
      struct = 'SingleCellExperiment'
    )
  }

#' @inheritParams SummarizedExperimentList
#' @export
SpatialExperimentList <-
  function(experiments = SimpleList(),
           experimentData = DataFrame(),
           check.names = TRUE,
           change.names = TRUE) {
    #no experiment
    if (length(experiments) == 0) {
      spe = SpatialExperiment::SpatialExperiment()
      return(.SpatialExperimentList(spe))
    }


    .processConstructorData(
      experiments = experiments,
      experimentData = experimentData,
      check.names = check.names,
      change.names = change.names,
      struct = 'SpatialExperiment'
    )
  }

.processConstructorData <- function(experiments = SimpleList(),
                                    experimentData = DataFrame(),
                                    check.names = TRUE,
                                    change.names = TRUE,
                                    struct = c(
                                      'SummarizedExperiment',
                                      'RangedSummarizedExperiment',
                                      'SingleCellExperiment',
                                      'SpatialExperiment'
                                    )) {
  struct = match.arg(struct)

  #identify constructor
  constructorFun = switch(
    struct,
    SummarizedExperiment = .SummarizedExperimentList,
    RangedSummarizedExperiment = .RangedSummarizedExperimentList,
    SingleCellExperiment = .SingleCellExperimentList,
    SpatialExperiment = .SpatialExperimentList
  )

  #check experiments
  if (is.null(names(experiments))) {
    stop("'experiments' must be a named list")
  }

  if (any(!sapply(experiments, is, struct))) {
    stop("each experiment in 'experiments' must be of type: ", struct)
  }

  #change colnames
  experimentNames = names(experiments)
  if (change.names) {
    experiments = mapply(function(x, y) {
      #if colnames present, change
      if (!is.null(colnames(x)))
        colnames(x) = paste(y, colnames(x), sep = '.')
      return(x)
    }, experiments, experimentNames, SIMPLIFY = FALSE)
  }

  #create experimentData if missing
  if (nrow(experimentData) == 0) {
    experimentData = S4Vectors::DataFrame(row.names = experimentNames)
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

  # if (length(experiments(object)) != NE) {
  #   msg <- c(msg, "'experimentData' should be equal to the number of experiments")
  # }

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
