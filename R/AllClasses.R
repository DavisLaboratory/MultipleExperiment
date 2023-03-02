#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
.SummarizedMultipleExperiment <- setClass(
  'SummarizedMultipleExperiment',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SummarizedExperiment'
)

#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
.RangedSummarizedMultipleExperiment <- setClass(
  'RangedSummarizedMultipleExperiment',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'RangedSummarizedExperiment'
)

#' @export
#' @import methods
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
.SingleCellMultipleExperiment <- setClass(
  'SingleCellMultipleExperiment',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SingleCellExperiment'
)

#' @export
#' @import methods
#' @importClassesFrom SpatialExperiment SpatialExperiment
.SpatialMultipleExperiment <- setClass(
  'SpatialMultipleExperiment',
  slots = representation(experimentData = 'DataFrame', experimentIndex = 'numeric'),
  contains = 'SpatialExperiment'
)

setClassUnion(
  'MultipleExperiment',
  c(
    'SummarizedMultipleExperiment',
    'RangedSummarizedMultipleExperiment',
    'SingleCellMultipleExperiment',
    'SpatialMultipleExperiment'
  )
)
