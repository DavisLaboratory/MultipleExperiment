#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
.showEL <- function(object) {
  ## header
  classLineage = is(object)
  cat('MultipleExperiment with ', nexp(object), ' ', classLineage[2], 's\n', sep = '')

  ## Experiment data
  callNextMethod()

  ## experimentData()
  cat('experiments: ', nexp(object), '\n', sep = '')
  coolcat('experimentNames (%d): %s\n', experimentNames(object))
  coolcat('experimentData names (%d): %s\n',colnames(experimentData(object)))

}

#' @export
#' @rdname MultipleExperiment-methods
setMethod("show", "SummarizedMultipleExperiment", .showEL)
#' @export
#' @rdname MultipleExperiment-methods
setMethod("show", "RangedSummarizedMultipleExperiment", .showEL)
#' @export
#' @rdname MultipleExperiment-methods
setMethod("show", "SingleCellMultipleExperiment", .showEL)
#' @export
#' @rdname MultipleExperiment-methods
setMethod("show", "SpatialMultipleExperiment", .showEL)
