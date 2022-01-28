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
.indexSubset <- function(x, i, j, ..., drop = TRUE) {
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

setMethod("[", c("SummarizedExperimentList", "ANY", "ANY"), .indexSubset)
setMethod("[", c("RangedSummarizedExperimentList", "ANY", "ANY"), .indexSubset)
setMethod("[", c("SingleCellExperimentList", "ANY", "ANY"), .indexSubset)
setMethod("[", c("SpatialExperimentList", "ANY", "ANY"), .indexSubset)

setMethod("subset", "ExperimentList", function(x, subset, select, ...) {
  i = S4Vectors:::evalqForSubset(subset, rowData(x, use.names = FALSE), ...)
  j = S4Vectors:::evalqForSubset(select, colData(x), ...)
  return(x[i, j])
})

#----colData access----
setMethod("colData", "ExperimentList", function(x, ..., withExperimentData = TRUE) {
  cdata = callNextMethod()

  #merge with sample data
  if (withExperimentData) {
    cdata = cbind(cdata, experimentData(x)[x@experimentIndex, ])
  }

  return(cdata)
})

setReplaceMethod("colData", "ExperimentList", function(x, ..., removeExperimentData = TRUE, value) {
  #remove fields overlapping with experiment data
  if (removeExperimentData)
    value = value[, setdiff(colnames(value), colnames(experimentData(x)))]
  x = callNextMethod()
  validObject(x)

  return(x)
})

setMethod("[[", c("ExperimentList", "ANY", "missing"),
          function(x, i, j, ...) {
            colData(x)[[i, ...]]
          })

setReplaceMethod("[[", c("ExperimentList", "ANY", "missing"),
                 function(x, i, j, ..., value)
                 {
                   colData(x)[[i, ...]] <- value
                   validObject(x)
                   return(x)
                 })

.DollarNames.ExperimentList <- function(x, pattern = "") {
  grep(pattern, names(colData(x)), value=TRUE)
}

setMethod("$", "ExperimentList", function(x, name) {
  colData(x)[[name]]
})

setReplaceMethod("$", "ExperimentList", function(x, name, value) {
  colData(x)[[name]] <- value
  return(x)
})
