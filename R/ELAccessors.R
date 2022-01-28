#----EL-specific----
#' @export
setMethod("experimentData", "ExperimentList", function(x) {
  return(x@experimentData)
})

#' @export
setReplaceMethod("experimentData", "ExperimentList", function(x, value) {
  #if null, replace with empty data frame
  if (is.null(value)) {
    value = S4Vectors::DataFrame(matrix(nrow = nrow(x@experimentData), ncol = 0))
    rownames(value) = rownames(x@experimentData)
  }

  #coerce matrix and data.frame
  if (is(value, 'matrix') | is(value, 'data.frame')) {
    value = as(value, 'DataFrame')
  }

  #warn if names are being erased
  if (is.null(rownames(value)) & !is.null(rownames(x@experimentData))) {
    warning("'experimentNames' will be removed because rownames are NULL")
  }

  x@experimentData = value
  validObject(x)
  return(x)
})

#' @export
setMethod("nexp", "ExperimentList", function(x) {
  return(nrow(experimentData(x)))
})

#' @export
setMethod("experimentNames", "ExperimentList", function(x) {
  return(rownames(experimentData(x)))
})

#' @export
setReplaceMethod("experimentNames", "ExperimentList", function(x, value) {
  rownames(experimentData(x)) = value
  validObject(x)
  return(x)
})

#' @export
setMethod("experiments", "ExperimentList", function(x) {
  #----TODO----
  return(rownames(x@experimentData))
})

#' @export
setReplaceMethod("experiments", "ExperimentList", function(x, value) {
  #----TODO----
  validObject(x)
  return(x)
})

#----Subsetting----
.indexSubsetEL <- function(x, i, j, ..., drop = TRUE) {
  if (missing(j)) {
    #is selecting rows
    x = callNextMethod()
  } else {
    #give temp names if missing
    missingNames = is.null(colnames(x))
    if (missingNames) {
      colnames(x) = as.character(1:ncol(x))
    }

    #create index map
    ixmap = x@experimentIndex
    names(ixmap) = colnames(x)

    #subset and/or select
    x = callNextMethod()

    #subset indices
    ixmap = ixmap[colnames(x)]
    names(ixmap) = NULL
    x@experimentIndex = ixmap

    #----TODO----
    #deal with cases where experimentData is NULL and entire
    # experiments are subsetted out

    #revert colnames if missing
    if (missingNames) {
      colnames(x) = NULL
    }
  }

  return(x)
}

setMethod("[", c("SummarizedExperimentList", "ANY", "ANY"), .indexSubsetEL)
setMethod("[", c("RangedSummarizedExperimentList", "ANY", "ANY"), .indexSubsetEL)
setMethod("[", c("SingleCellExperimentList", "ANY", "ANY"), .indexSubsetEL)
setMethod("[", c("SpatialExperimentList", "ANY", "ANY"), .indexSubsetEL)

setMethod("subset", "ExperimentList", function(x, subset, select, ...) {
  i = S4Vectors:::evalqForSubset(subset, rowData(x, use.names = FALSE), ...)
  j = S4Vectors:::evalqForSubset(select, colData(x), ...)
  return(x[i, j])
})
