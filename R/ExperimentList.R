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
  stopifnot(length(experiments) == 0 | !is.null(names(experiments)))

  #no experiment
  if (length(experiments) == 0) {
    se = SummarizedExperiment::SummarizedExperiment()
    return(.SummarizedExperimentList(se))
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

  #combine SEs
  se = do.call(SummarizedExperiment::cbind, experiments)
  if (is(experimentData, 'data.frame'))
    experimentData = as(experimentData, 'DataFrame')

  #create reference map
  experimentIndex = rep(1:length(experiments), times = sapply(experiments, ncol))

  .SummarizedExperimentList(
    se,
    experimentData = experimentData,
    experimentIndex = experimentIndex
  )
}

#' @inheritParams SummarizedExperimentList
#' @export
RangedSummarizedExperimentList <-
  function(experiments = SimpleList(),
           experimentData = DataFrame(),
           check.names = TRUE,
           change.names = TRUE) {

  stopifnot(length(experiments) == 0 | !is.null(names(experiments)))

  #no experiment
  if (length(experiments) == 0) {
    gr = GenomicRanges::GRangesList()
    se = SummarizedExperiment::SummarizedExperiment(rowRanges = gr)
    return(.RangedSummarizedExperimentList(se))
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

  #combine SEs
  se = do.call(SummarizedExperiment::cbind, experiments)
  if (is(experimentData, 'data.frame'))
    experimentData = as(experimentData, 'DataFrame')

  #create reference map
  experimentIndex = rep(1:length(experiments), times = sapply(experiments, ncol))

  .RangedSummarizedExperimentList(
    se,
    experimentData = experimentData,
    experimentIndex = experimentIndex
  )
}

#' @inheritParams SummarizedExperimentList
#' @export
SingleCellExperimentList <-
  function(experiments = SimpleList(),
           experimentData = DataFrame(),
           check.names = TRUE,
           change.names = TRUE) {

  stopifnot(length(experiments) == 0 | !is.null(names(experiments)))

  #no experiment
  if (length(experiments) == 0) {
    sce = SingleCellExperiment::SingleCellExperiment()
    return(.SingleCellExperimentList(sce))
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

  #combine SEs
  sce = do.call(SingleCellExperiment::cbind, experiments)
  if (is(experimentData, 'data.frame'))
    experimentData = as(experimentData, 'DataFrame')

  #create reference map
  experimentIndex = rep(1:length(experiments), times = sapply(experiments, ncol))

  .SingleCellExperimentList(
    sce,
    experimentData = experimentData,
    experimentIndex = experimentIndex
  )
}

#' @inheritParams SummarizedExperimentList
#' @export
SpatialExperimentList <-
  function(experiments = SimpleList(),
           experimentData = DataFrame(),
           check.names = TRUE,
           change.names = TRUE) {

  stopifnot(length(experiments) == 0 | !is.null(names(experiments)))

  #no experiment
  if (length(experiments) == 0) {
    spe = SpatialExperiment::SpatialExperiment()
    return(.SpatialExperimentList(spe))
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

  #combine SEs
  sce = do.call(BiocGenerics::cbind, experiments)
  if (is(experimentData, 'data.frame'))
    experimentData = as(experimentData, 'DataFrame')

  #create reference map
  experimentIndex = rep(1:length(experiments), times = sapply(experiments, ncol))

  .SpatialExperimentList(
    sce,
    experimentData = experimentData,
    experimentIndex = experimentIndex
  )
}
