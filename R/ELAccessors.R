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
setMethod("colWithExperimentData", "ExperimentList", function(x) {
  return(cbind(colData(x), experimentData(x)[x@experimentIndex, , drop = FALSE]))
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
#' Transform character indices to numeric indices
#'
#' @param idx a character vector containing character indices
#' @param txt a character vector containing the reference list
#' @param fmt a character stating the format to use for error reporting
#'
#' @return a numeric vector containing numeric indices
.ExperimentList.charbound <-  function(idx, txt, fmt) {
  orig = idx
  idx = match(idx, txt)
  #error reporting for missing names
  if (any(bad = is.na(idx))) {
    msg = paste(S4Vectors:::selectSome(orig[bad]), collapse = " ")
    stop(sprintf(fmt, msg))
  }
  return(idx)
}

#' Convert numeric indices to logical
#'
#' @param idx a numeric containing indices
#' @param len a numeric stating the maximum possible index
#' @param msg a character stating the error message to produce when indices are
#'   out of bounds
#'
#' @return a logical containing indices
#'
#' @examples
.ixNumericToLogical <- function(idx, len, msg) {
  if (any(idx < -len || idx > len)) {
    stop(msg)
  }
  #convert to logical
  idx_logical = rep(FALSE, len)
  idx_logical[idx] = TRUE
  return(idx_logical)
}

#' @importFrom methods callNextMethod
.indexSubsetEL <- function(x, i, j, ..., exp, drop = TRUE) {
  #no subsetting
  if (missing(i) && missing(j) && missing(exp))
    return(x)

  #subset rows only
  if (missing(j) && missing(exp)) {
    x = callNextMethod(x, i, j, ...)
    return(x)
  }

  #subset columns
  if (!missing(j)) {
    #convert indices to numeric indices
    if (is.character(j)) {
      #convert character indices to numeric
      fmt = paste0("<", class(x), ">[j,] index out of bounds: %s")
      j = .ExperimentList.charbound(j, colnames(x), fmt)
    }
    if (is.numeric(j))
      j = .ixNumericToLogical(j, ncol(x), paste0("<", class(x), ">[j,] index out of bounds"))
  } else {
    j = TRUE
  }

  #subset experiments
  if (!missing(exp)) {
    #convert indices to numeric indices
    if (is.character(exp)) {
      #convert character indices to numeric
      fmt = paste0("<", class(x), ">[exp,] index out of bounds: %s")
      exp = .ExperimentList.charbound(exp, experimentNames(x), fmt)
    }
    if (is.numeric(exp))
      exp = .ixNumericToLogical(exp, nexp(x), paste0("<", class(x), ">[exp,] index out of bounds"))

    #identify columns to select
    j = j & x@experimentIndex %in% seq_len(nexp(x))[exp]
  }

  #subset and/or select
  x = callNextMethod(x, i, j, ...)

  #subset experimentIndex
  x@experimentIndex = x@experimentIndex[j]

  #update experimentIndex and experimentData if subsetting experiment
  if (!missing(exp) && !any(exp)) {
    #build index map from old to new indices
    ixmap = rep(NA_integer_, length(exp))
    ixmap[exp] = seq_len(sum(exp))

    #transform indices
    x@experimentIndex = ixmap[x@experimentIndex]

    #update experiment Data
    experimentData(x) = experimentData(x)[exp, ]
  }

  validObject(x)
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
