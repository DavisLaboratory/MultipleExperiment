#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
.SummarizedExperimentList <- setClass(
  'SummarizedExperimentList',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SummarizedExperiment'
)

#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
.RangedSummarizedExperimentList <- setClass(
  'RangedSummarizedExperimentList',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'RangedSummarizedExperiment'
)

#' @export
#' @import methods
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
.SingleCellExperimentList <- setClass(
  'SingleCellExperimentList',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SingleCellExperiment'
)

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
