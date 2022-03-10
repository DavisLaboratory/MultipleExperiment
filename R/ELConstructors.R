#' @importFrom S4Vectors SimpleList DataFrame

#' @name ExperimentList
#'
#' @title ExperimentList objects
#'
#' @aliases ExperimentList ExperimentList-class SummarizedExperimentList
#'   SummarizedExperimentList-class RangedSummarizedExperimentList
#'   RangedSummarizedExperimentList-class SingleCellExperimentList
#'   SingleCellExperimentList-class SpatialExperimentList
#'   SpatialExperimentList-class
#'
#' @description Classes in \code{ExperimentList} are designed to store lists of
#'   experiments such as \code{SummarizedExperiment},
#'   \code{RangedSummarizedExperiment}, \code{SingleCellExperiment},
#'   \code{SpatialExperiment}. Each experiment object is stored in its own
#'   ExperimentList class respectively (one of \code{SummarizedExperimentList},
#'   \code{RangedSummarizedExperimentList}, \code{SingleCellExperimentList},
#'   \code{SpatialExperimentList}). In addition to storing experiment data, the
#'   class also stores experiment-specific annotations associated with each
#'   experiment, analogous to \code{colData} in a SummarizedExperiment object.
#'
#' @param experiments a \code{SimpleList} or \code{list} containing a list of
#'   \code{SummarizedExperiment}, \code{RangedSummarizedExperiment},
#'   \code{SingleCellExperiment}, or \code{SpatialExperiment} objects.
#' @param experimentData a \code{DataFrame} or \code{data.frame} containing
#'   experiment specific annotations. The number of rows in
#'   \code{experimentData} should match the number of \code{experiments}.
#' @param check.names a logical specifying whether names of \code{experiments}
#'   should be checked against row names of \code{experimentData} (default is
#'   TRUE).
#' @param change.names a logical indicating whether column names of columns in
#'   each individual experiment object should be concatenated with experiment
#'   names to ensure unique column names (default is TRUE).
#'
#' @details This class is designed to enable analysis of data from multiple
#'   experiments. In the context of transcriptomic data, these could be data
#'   from different studies. For single-cell or spatial transcriptomics data,
#'   these could be data from different samples but from the same study. In
#'   these scenarios, analysis of data from different biological samples is made
#'   possible by \code{ExperimentList} objects.
#'
#'   Similar to the design of \code{SummarizedExperiment}, \code{ExperimentList}
#'   enables storing and subsetting of experiment-specific annotations as part
#'   of the \code{experimentData} stored within each object.
#'
#' @return One of the following objects: \code{SummarizedExperimentList},
#'   \code{RangedSummarizedExperimentList}, \code{SingleCellExperimentList}, or
#'   \code{SpatialExperimentList}.
#'
#' @export
#' @examples
#' nrows <- 200; ncols <- 6
#'
#' #Experiment 1
#' counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                      row.names=LETTERS[1:6])
#' se1 <- SummarizedExperiment(assays=SimpleList(counts=counts),
#'                             colData=colData)
#'
#' #Experiment 2
#' counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                      row.names=LETTERS[1:6])
#' se2 <- SummarizedExperiment(assays=SimpleList(counts=counts),
#'                             colData=colData)
#'
#' #Experiment annotation
#' experimentData <- DataFrame(age = c(36, 72),
#'                             sex = c("F", "M"),
#'                             row.names = c("PatientA", "PatientB"))
#'
#' #Create ExperimentList
#' experiments = SimpleList("PatientA" = se1, "PatientB" = se2)
#' el = ExperimentList(experiments = experiments,
#'                     experimentData = experimentData)
#' el
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
