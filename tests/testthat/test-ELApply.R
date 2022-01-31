test_that("ExperimentList apply works", {
  mat = matrix(1, 3, 3, dimnames = list(paste0('row', 1:3), paste0('col', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = SpatialExperimentList(list('A' = spe, 'B' = spe), expdf)

  expect_equal(elapply(spe_list, ncol), c('A' = 3, 'B' = 3))
  expect_equal(elapply(spe_list, ncol, simplify = FALSE), list('A' = 3, 'B' = 3))
  expect_true(is(elapply(spe_list, dim), 'matrix'))
  expect_true(is(elapply(spe_list, function(x) {
    assay(x) = assay(x) + 1
    return(x)
  }), 'SpatialExperimentList'))
})
