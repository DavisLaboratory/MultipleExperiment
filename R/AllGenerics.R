#' @name MultipleExperiment-methods
#'
#' @title Methods for MultipleExperiment objects
#'
#' @aliases experiments nexp experimentData experimentData<- elapply show colData
#' @aliases coerce,MultipleExperiment,SummarizedMultipleExperiment-method
#' @aliases coerce,MultipleExperiment,RangedSummarizedMultipleExperiment-method
#' @aliases coerce,MultipleExperiment,SingleCellMultipleExperiment-method
#' @aliases coerce,MultipleExperiment,SpatialMultipleExperiment-method
#'
#' @description The \code{\link{MultipleExperiment}} class provides a family of
#'   methods to manage and process data from multiple experiments.
#'
#' @param x A \code{\link{SummarizedMultipleExperiment}},
#'   \code{\link{RangedSummarizedMultipleExperiment}},
#'   \code{\link{SingleCellMultipleExperiment}}, or
#'   \code{\link{SpatialMultipleExperiment}} object.
#' @param object A \code{\link{SummarizedMultipleExperiment}},
#'   \code{\link{RangedSummarizedMultipleExperiment}},
#'   \code{\link{SingleCellMultipleExperiment}}, or
#'   \code{\link{SpatialMultipleExperiment}} object.
#' @param value Replacement value for replacement methods.
#' @param i A subscript that can act to subset the rows of \code{x}.
#' @param j A subscript that can act to subset the columns of \code{x}.
#' @param exp A subscript that can act to subset experiments from \code{x}.
#' @param drop A \code{logical(1)}, ignored by these methods.
#' @param FUN The function to be applied to each element of \code{x}: see
#'   ‘Details’.
#' @param ... Further arguments to be passed to or from other methods.
#' @inheritParams MultipleExperiment
#'
#' @return Return value varies depending on method, as described below.
#'
#' @details Additional details for each type of data attribute are provided
#'   below.
#'
#' @section Constructor: \describe{ MultipleExperiment instances are constructed
#'   using the MultipleExperiment function documented in
#'   ?\code{\link{MultipleExperiment}}. }
#'
#' @section Accessors: \describe{ In the following code snippets, \code{x} is a
#'   MultipleExperiment object. \item{\code{experimentData(x), experimentData(x) <-
#'   value}: }{Get or set the experiment data. \code{value} is a
#'   \code{DataFrame} object. Row names of value must be \code{NULL} or
#'   consistent with the existing experiment names of x.}
#'   \item{\code{colWithExperimentData(x)}: }{Get the column data merged with
#'   experiment data.} \item{\code{nexp(x)}: }{Get the number of experiments in
#'   the \code{MultipleExperiment} object.} \item{\code{experimentNames(x),
#'   experimentNames(x) <- value}: }{Get or set the names of experiments.}
#'   \item{\code{experiments(x)}: }{Get a list of the experiments (for example,
#'   a list of \code{SummarizedExperiments}).} }
#'
#' @section Subsetting: \describe{ In the following code snippets, \code{x} is a
#'   MultipleExperiment object. \item{\code{x[i, j, exp]}: }{Create or replace a
#'   subset of x. \code{i, j, exp} can be \code{numeric}, \code{logical},
#'   \code{character}, or \code{missing}.} \item{\code{subset(x, subset, select,
#'   experiment)}: }{Create a subset of \code{x} using an expression subset
#'   referring to columns of \code{rowData(x)} and / or select referring to
#'   column names of \code{colData(x)} and / or experiment referring to column
#'   names of \code{experimentData(x)}.} }
#'
#' @section Apply: \describe{ In the following code snippets, \code{x} is a
#'   MultipleExperiment object. \item{\code{elapply(x, FUN, ..., simplify = TRUE,
#'   check.names = TRUE, change.names = TRUE)}: }{Apply functions to each
#'   experiment within an \code{MultipleExperiment} and potentially simplify the
#'   results.} }
#'
#' @seealso \code{\link{MultipleExperiment}}, \code{\link{SummarizedExperiment}},
#'   \code{\link{RangedSummarizedExperiment}},
#'   \code{\link{SingleCellExperiment}}, \code{\link{SpatialExperiment}}
#'
#' @examples
#' example(MultipleExperiment)
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

#' @rdname MultipleExperiment-methods
#' @export
setGeneric("nexp", function(x, ...) standardGeneric("nexp"))

#' @rdname MultipleExperiment-methods
#' @export
setGeneric("experiments", function(x, ...) standardGeneric("experiments"))

#' @rdname MultipleExperiment-methods
#' @export
setGeneric("experimentNames", function(x, ...) standardGeneric("experimentNames"))

#' @rdname MultipleExperiment-methods
#' @export
setGeneric("experimentNames<-", function(x, ..., value) standardGeneric("experimentNames<-"))

#' @rdname MultipleExperiment-methods
#' @export
setGeneric("experimentData", function(x, ...) standardGeneric("experimentData"))

#' @rdname MultipleExperiment-methods
#' @export
setGeneric("experimentData<-", function(x, ..., value) standardGeneric("experimentData<-"))

#' @rdname MultipleExperiment-methods
#' @export
setGeneric("elapply", function(x, ...) standardGeneric("elapply"))
