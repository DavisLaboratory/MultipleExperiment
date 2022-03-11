#' @name ExperimentList-methods
#'
#' @title Methods for ExperimentList objects
#'
#' @aliases experiments nexp experimentData experimentData<- elapply show colData
#' @aliases coerce,ExperimentList,SummarizedExperimentList-method
#' @aliases coerce,ExperimentList,RangedSummarizedExperimentList-method
#' @aliases coerce,ExperimentList,SingleCellExperimentList-method
#' @aliases coerce,ExperimentList,SpatialExperimentList-method
#'
#' @description The \code{\link{ExperimentList}} class provides a family of
#'   methods to manage and process data from multiple experiments.
#'
#' @param x A \code{\link{SummarizedExperimentList}},
#'   \code{\link{RangedSummarizedExperimentList}},
#'   \code{\link{SingleCellExperimentList}}, or
#'   \code{\link{SpatialExperimentList}} object.
#' @param object A \code{\link{SummarizedExperimentList}},
#'   \code{\link{RangedSummarizedExperimentList}},
#'   \code{\link{SingleCellExperimentList}}, or
#'   \code{\link{SpatialExperimentList}} object.
#' @param value Replacement value for replacement methods.
#' @param i A subscript that can act to subset the rows of \code{x}.
#' @param j A subscript that can act to subset the columns of \code{x}.
#' @param exp A subscript that can act to subset experiments from \code{x}.
#' @param drop A \code{logical(1)}, ignored by these methods.
#' @param FUN The function to be applied to each element of \code{x}: see
#'   ‘Details’.
#' @param ... Further arguments to be passed to or from other methods.
#' @inheritParams ExperimentList
#'
#' @return Return value varies depending on method, as described below.
#'
#' @details Additional details for each type of data attribute are provided
#'   below.
#'
#' @section Constructor: \describe{ ExperimentList instances are constructed
#'   using the ExperimentList function documented in
#'   ?\code{\link{ExperimentList}}. }
#'
#' @section Accessors: \describe{ In the following code snippets, \code{x} is a
#'   ExperimentList object. \item{\code{experimentData(x), experimentData(x) <-
#'   value}: }{Get or set the experiment data. \code{value} is a
#'   \code{DataFrame} object. Row names of value must be \code{NULL} or
#'   consistent with the existing experiment names of x.}
#'   \item{\code{colWithExperimentData(x)}: }{Get the column data merged with
#'   experiment data.} \item{\code{nexp(x)}: }{Get the number of experiments in
#'   the \code{ExperimentList} object.} \item{\code{experimentNames(x),
#'   experimentNames(x) <- value}: }{Get or set the names of experiments.}
#'   \item{\code{experiments(x)}: }{Get a list of the experiments (for example,
#'   a list of \code{SummarizedExperiments}).} }
#'
#' @section Subsetting: \describe{ In the following code snippets, \code{x} is a
#'   ExperimentList object. \item{\code{x[i, j, exp]}: }{Create or replace a
#'   subset of x. \code{i, j, exp} can be \code{numeric}, \code{logical},
#'   \code{character}, or \code{missing}.} \item{\code{subset(x, subset, select,
#'   experiment)}: }{Create a subset of \code{x} using an expression subset
#'   referring to columns of \code{rowData(x)} and / or select referring to
#'   column names of \code{colData(x)} and / or experiment referring to column
#'   names of \code{experimentData(x)}.} }
#'
#' @section Apply: \describe{ In the following code snippets, \code{x} is a
#'   ExperimentList object. \item{\code{elapply(x, FUN, ..., simplify = TRUE,
#'   check.names = TRUE, change.names = TRUE)}: }{Apply functions to each
#'   experiment within an \code{ExperimentList} and potentially simplify the
#'   results.} }
#'
#' @seealso \code{\link{ExperimentList}}, \code{\link{SummarizedExperiment}},
#'   \code{\link{RangedSummarizedExperiment}},
#'   \code{\link{SingleCellExperiment}}, \code{\link{SpatialExperiment}}
#'
#' @examples
#' example(ExperimentList)
#'
#' nexp(el)
#' experimentData(el)
#' colData(el, experimentData = TRUE)
#' experimentNames(el)
#' el[1:5, 1:5]
#' el[, exp = 'PatientA']
#' elapply(el, dim)
#'
NULL

#' @rdname ExperimentList-methods
#' @export
setGeneric("nexp", function(x, ...) standardGeneric("nexp"))

#' @rdname ExperimentList-methods
#' @export
setGeneric("experiments", function(x, ...) standardGeneric("experiments"))

#' @rdname ExperimentList-methods
#' @export
setGeneric("experimentNames", function(x, ...) standardGeneric("experimentNames"))

#' @rdname ExperimentList-methods
#' @export
setGeneric("experimentNames<-", function(x, ..., value) standardGeneric("experimentNames<-"))

#' @rdname ExperimentList-methods
#' @export
setGeneric("experimentData", function(x, ...) standardGeneric("experimentData"))

#' @rdname ExperimentList-methods
#' @export
setGeneric("experimentData<-", function(x, ..., value) standardGeneric("experimentData<-"))

#' @rdname ExperimentList-methods
#' @export
setGeneric("elapply", function(x, ...) standardGeneric("elapply"))
