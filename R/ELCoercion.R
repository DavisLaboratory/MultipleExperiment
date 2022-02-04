#' @exportMethod coerce
#' @importClassesFrom SpatialExperiment SpatialExperiment
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment

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
  #convert to super class (e.g., from SpatialExperimentList to SpatialExperiment)
  x = x |>
    as(is(x)[2]) |>
    #convert to sibling class (e.g., SingleCellExperiment)
    as(gsub('List', '', destclass)) |>
    #convert to the respective ExperimentList subclass (e.g., SingleCellExperimentList)
    as(destclass)
  x@experimentData = experimentData
  x@experimentIndex = experimentIndex

  validObject(x)
  return(x)
}

setAs("ExperimentList", "SummarizedExperimentList", function(from) {
  .ExperimentList.coerce(from, 'SummarizedExperimentList')
})

setAs("ExperimentList", "RangedSummarizedExperimentList", function(from) {
  .ExperimentList.coerce(from, 'RangedSummarizedExperimentList')
})

setAs("ExperimentList", "SingleCellExperimentList", function(from) {
  .ExperimentList.coerce(from, 'SingleCellExperimentList')
})

setAs("ExperimentList", "SpatialExperimentList", function(from) {
  .ExperimentList.coerce(from, 'SpatialExperimentList')
})
