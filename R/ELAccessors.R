#----EL-specific----
#' @export
setMethod("experimentData", "ExperimentList", function(x) {
  return(x@experimentData)
})

#' @export
setReplaceMethod("experimentData", "ExperimentList", function(x, value) {
  x@experimentData = value
  validObject(x)
  return(x)
})

#' @export
setMethod("experimentNames", "ExperimentList", function(x) {
  return(rownames(x@experimentData))
})

#' @export
setReplaceMethod("experimentNames", "ExperimentList", function(x, value) {
  rownames(x@experimentData) = value
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

#' @export
setMethod("length", "ExperimentList", function(x) {
  return(nrow(experimentData(x)))
})

setMethod("names", "ExperimentList", function(x) experimentNames(x))
setReplaceMethod("names", "ExperimentList", function(x, value) {
  `experimentNames<-`(x, value)
  validObject(x)
  return(x)
})

#----Subsetting----
setMethod("[", c("ExperimentList", "ANY", "ANY"),
          function(x, i, j, ..., drop = TRUE)
          {
            if (is.missing(j)) {
              #is selecting rows
              x = callNextMethod()
            } else {
              #give temp names if missing
              misingNames = is.null(colnames(x))
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

              #revert colnames if missing
              if (missingNames) {
                colnames(x) = NULL
              }
            }
          })

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
