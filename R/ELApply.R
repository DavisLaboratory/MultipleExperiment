#' @rdname ExperimentList-methods
#' @export
setMethod("elapply", "ExperimentList", function(x, FUN, ..., check.names = TRUE, change.names = TRUE) {
  experimentData = experimentData(x)
  FUN = match.fun(FUN)

  #apply function
  results = lapply(experiments(x), FUN, ...)

  if (length(results) == 0)
    return(list())

  #simplify
  if (all(sapply(results, is, is(x)[2]))) {
    results = do.call(
      ExperimentList,
      args = list(
        experiments = results,
        experimentData = experimentData,
        check.names = check.names,
        change.names = change.names
      )
    )
  }

  return(results)
})
