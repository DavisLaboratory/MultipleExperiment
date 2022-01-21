#' An S4 class to manage lists of related SummarizedExperiment objects
#'
#' @slot experimentData A [DataFrame] object describing the experiments. Row
#'   names, if present, become the experiment names of the Experiment objects.
#'   The number of rows of the DataFrame must equal the number of experiments.
#' @slot experimentIndex A [numeric] object containing experiment indices of
#'   experiment each column belongs to.
#'
#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
.SummarizedExperimentList <- setClass(
  'SummarizedExperimentList',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SummarizedExperiment'
)

#' An S4 class to manage lists of related RangedSummarizedExperiment objects
#'
#' @slot experimentData A [DataFrame] object describing the experiments. Row
#'   names, if present, become the experiment names of the Experiment objects.
#'   The number of rows of the DataFrame must equal the number of experiments.
#' @slot experimentIndex A [numeric] object containing experiment indices of
#'   experiment each column belongs to.
#'
#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
.RangedSummarizedExperimentList <- setClass(
  'RangedSummarizedExperimentList',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'RangedSummarizedExperiment'
)

#' An S4 class to manage lists of related SingleCellExperiment objects
#'
#' @slot experimentData A [DataFrame] object describing the experiments. Row
#'   names, if present, become the experiment names of the Experiment objects.
#'   The number of rows of the DataFrame must equal the number of experiments.
#' @slot experimentIndex A [numeric] object containing experiment indices of
#'   experiment each column belongs to.
#'
#' @export
#' @import methods
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
.SingleCellExperimentList <- setClass(
  'SingleCellExperimentList',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SingleCellExperiment'
)

#' An S4 class to manage lists of related SpatialExperiment objects
#'
#' @slot experimentData A [DataFrame] object describing the experiments. Row
#'   names, if present, become the experiment names of the Experiment objects.
#'   The number of rows of the DataFrame must equal the number of experiments.
#' @slot experimentIndex A [numeric] object containing experiment indices of
#'   experiment each column belongs to.
#'
#' @export
#' @import methods
#' @importClassesFrom SpatialExperiment SpatialExperiment
.SpatialExperimentList <- setClass(
  'SpatialExperimentList',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SpatialExperiment'
)

setClassUnion(
  'ExperimentList',
  c(
    'SummarizedExperimentList',
    'RangedSummarizedExperimentList',
    'SingleCellExperimentList',
    'SpatialExperimentList'
  )
)
