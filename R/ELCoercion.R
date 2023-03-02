#' @exportMethod coerce
#' @importClassesFrom SpatialExperiment SpatialExperiment
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment

.MultipleExperiment.coerce <- function(x,
                                   destclass = c(
                                     'SummarizedMultipleExperiment',
                                     'RangedSummarizedMultipleExperiment',
                                     'SingleCellMultipleExperiment',
                                     'SpatialMultipleExperiment'
                                   )) {
  #extract data
  experimentData = experimentData(x)
  experimentIndex = x@experimentIndex

  #coerce
  #convert to super class (e.g., from SpatialMultipleExperiment to SpatialExperiment)
  x = x |>
    as(is(x)[2]) |>
    #convert to sibling class (e.g., SingleCellExperiment)
    as(gsub('List', '', destclass)) |>
    #convert to the respective MultipleExperiment subclass (e.g., SingleCellMultipleExperiment)
    as(destclass)
  x@experimentData = experimentData
  x@experimentIndex = experimentIndex

  validObject(x)
  return(x)
}

setAs("MultipleExperiment", "SummarizedMultipleExperiment", function(from) {
  .MultipleExperiment.coerce(from, 'SummarizedMultipleExperiment')
})

setAs("MultipleExperiment", "RangedSummarizedMultipleExperiment", function(from) {
  .MultipleExperiment.coerce(from, 'RangedSummarizedMultipleExperiment')
})

setAs("MultipleExperiment", "SingleCellMultipleExperiment", function(from) {
  .MultipleExperiment.coerce(from, 'SingleCellMultipleExperiment')
})

setAs("MultipleExperiment", "SpatialMultipleExperiment", function(from) {
  .MultipleExperiment.coerce(from, 'SpatialMultipleExperiment')
})
