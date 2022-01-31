.ExperimentList.coerce <- function(x,
                                   destclass = c(
                                     'SummarizedExperimentList',
                                     'RangedSummarizedExperimentList',
                                     'SingleCellExperimentList',
                                     'SpatialExperimentList'
                                   )) {
  #extract data
  experimentData = experimentData(x)
  experimentIndex = x@experimentIndex

  #coerce
  x = x |>
    as(is(x)[2]) |>
    as(gsub('List', '', destclass)) |>
    as(destclass)
  x@experimentData = experimentData
  x@experimentIndex = experimentIndex

  validObject(x)
  return(x)
}

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
setAs("ExperimentList", "SummarizedExperimentList", function(from) {
  .ExperimentList.coerce(from, 'SummarizedExperimentList')
})

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("ExperimentList", "RangedSummarizedExperimentList", function(from) {
  .ExperimentList.coerce(from, 'RangedSummarizedExperimentList')
})

#' @exportMethod coerce
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
setAs("ExperimentList", "SingleCellExperimentList", function(from) {
  .ExperimentList.coerce(from, 'SingleCellExperimentList')
})

#' @exportMethod coerce
#' @importClassesFrom SpatialExperiment SpatialExperiment
setAs("ExperimentList", "SpatialExperimentList", function(from) {
  .ExperimentList.coerce(from, 'SpatialExperimentList')
})
