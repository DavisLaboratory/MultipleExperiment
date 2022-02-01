#' @export
setMethod("elapply", "ExperimentList", function(x, FUN, ..., simplify = TRUE, check.names = TRUE, change.names = TRUE) {
  experimentData = experimentData(x)
  FUN = match.fun(FUN)

  #apply function
  results = lapply(experiments(x), FUN, ...)

  #simplify
  if (isTRUE(simplify)) {
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
    } else {
      results = simplify2array(results, higher = (simplify == "array"))
    }
  }

  return(results)
})
