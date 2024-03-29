#----EL-specific----
#' @rdname MultipleExperiment-methods
#' @export
setMethod("experimentData", "MultipleExperiment", function(x) {
  return(x@experimentData)
})

#' @rdname MultipleExperiment-methods
#' @export
setReplaceMethod("experimentData", "MultipleExperiment", function(x, value) {
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

#' @rdname MultipleExperiment-methods
#' @export
setMethod("nexp", "MultipleExperiment", function(x) {
  n = nrow(experimentData(x))
  if (length(n) == 0)
    n = 0
  return(n)
})

#' @rdname MultipleExperiment-methods
#' @export
setMethod("experimentNames", "MultipleExperiment", function(x) {
  return(rownames(experimentData(x)))
})

#' @rdname MultipleExperiment-methods
#' @export
setReplaceMethod("experimentNames", "MultipleExperiment", function(x, value) {
  rownames(experimentData(x)) = value
  validObject(x)
  return(x)
})

#colData
.colData_EL <- function(x, ..., experimentData = FALSE) {
  cdata = callNextMethod(x, ...)
  if (experimentData) {
    cdata = cbind(cdata, experimentData(x)[x@experimentIndex, , drop = FALSE])
  }

  return(cdata)
}

setMethod("colData", "SummarizedMultipleExperiment", .colData_EL)
setMethod("colData", "RangedSummarizedMultipleExperiment", .colData_EL)
setMethod("colData", "SingleCellMultipleExperiment", .colData_EL)
setMethod("colData", "SpatialMultipleExperiment", .colData_EL)

#----Subsetting----
#' Transform character indices to numeric indices
#'
#' @param idx a character vector containing character indices
#' @param txt a character vector containing the reference list
#' @param fmt a character stating the format to use for error reporting
#'
#' @return a numeric vector containing numeric indices
.MultipleExperiment.charbound <-  function(idx, txt, fmt) {
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
.ixNumericToLogical <- function(idx, len, msg) {
  if (any(idx < -len | idx > len)) {
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
      j = .MultipleExperiment.charbound(j, colnames(x), fmt)
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
      exp = .MultipleExperiment.charbound(exp, experimentNames(x), fmt)
    }
    if (is.numeric(exp))
      exp = .ixNumericToLogical(exp, nexp(x), paste0("<", class(x), ">[exp,] index out of bounds"))

    #subset imgData for SpatialMultipleExperiments
    if (is(x, 'SpatialMultipleExperiment') && nrow(SpatialExperiment::imgData(x)) == nexp(x)) {
      SpatialExperiment::imgData(x) = SpatialExperiment::imgData(x)[exp, , drop = FALSE]
    }

    #identify columns to select
    j = j & x@experimentIndex %in% seq_len(nexp(x))[exp]
  }

  #subset and/or select
  x = callNextMethod(x, i, j, ...)

  #subset experimentIndex
  x@experimentIndex = x@experimentIndex[j]

  #update experimentIndex and experimentData if subsetting experiment
  if (!missing(exp) && !all(exp)) {
    #build index map from old to new indices
    ixmap = rep(NA_integer_, length(exp))
    ixmap[exp] = seq_len(sum(exp))

    #transform indices
    x@experimentIndex = ixmap[x@experimentIndex]

    #update experiment Data
    experimentData(x) = experimentData(x)[exp, , drop = FALSE]
  }

  validObject(x)
  return(x)
}

#' @rdname MultipleExperiment-methods
setMethod("[", c("SummarizedMultipleExperiment", "ANY", "ANY"), .indexSubsetEL)
#' @rdname MultipleExperiment-methods
setMethod("[", c("RangedSummarizedMultipleExperiment", "ANY", "ANY"), .indexSubsetEL)
#' @rdname MultipleExperiment-methods
setMethod("[", c("SingleCellMultipleExperiment", "ANY", "ANY"), .indexSubsetEL)
#' @rdname MultipleExperiment-methods
setMethod("[", c("SpatialMultipleExperiment", "ANY", "ANY"), .indexSubsetEL)

# setMethod("subset", "MultipleExperiment", function(x, subset, select, ..., experiment) {
#   i = S4Vectors:::evalqForSubset(subset, SummarizedExperiment::rowData(x, use.names = FALSE), ...)
#   j = S4Vectors:::evalqForSubset(select, SummarizedExperiment::colData(x), ...)
#   exp = S4Vectors:::evalqForSubset(experiment, experimentData(x), ...)
#   return(x[i, j, exp = exp])
# })

#----experiments----
#' @rdname MultipleExperiment-methods
#' @export
setMethod("experiments", "MultipleExperiment", function(x, change.names = TRUE) {
  #revert names
  if (change.names && !is.null(experimentNames(x)) && !is.null(colnames(x))) {
    regex = gsub('\\.', '\\\\\\.', paste(experimentNames(x), collapse = '|'))
    regex = sprintf('(%s)\\.', regex)
    colnames(x) = gsub(regex, '', colnames(x))
  }

  #split into different assays
  exps = sapply(seq_len(nexp(x)), function(i) {
    #subset
    se = x[, x@experimentIndex == i]
    #coerce to super class
    se = as(se, is(se)[2])
  })
  names(exps) = experimentNames(x)

  return(exps)
})
