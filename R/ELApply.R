#' @rdname MultipleExperiment-methods
#' @export
setMethod("elapply", "MultipleExperiment", function(x, FUN, ..., check.names = TRUE, change.names = TRUE) {
  FUN = match.fun(FUN)

  if (nexp(x) == 0)
    return(list())
  experimentData = experimentData(x)

  #apply function
  results = lapply(experiments(x), FUN, ...)

  #simplify
  if (all(sapply(results, is, is(x)[2]))) {
    results = do.call(
      MultipleExperiment,
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
