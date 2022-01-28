test_that("ExperimentList-specific accessors work", {
  library(SpatialExperiment)

  mat = matrix(1, 3, 3)
  spe1 = SpatialExperiment(assays = list(mat))
  spe2 = SpatialExperiment(assays = list(mat))
  spe_list = SpatialExperimentList(list(spe1,spe2))

  #no names (colnames or experiment)
  expect_equal(dim(spe_list), c(3, 6))
  expect_equal(nexp(spe_list), 2)
  expect_equal(ncol(spe_list), 6)
  expect_equal(ncol(spe_list), 6)
  expect_null(experimentNames(spe_list))

  #experiment data access
  expect_equal(dim(experimentData(spe_list)), c(2, 0))
  expdf = matrix(1, 2, 1)
  experimentData(spe_list) = expdf
  expect_equal(dim(experimentData(spe_list)), c(2, 1))
  experimentData(spe_list) = NULL
  expect_equal(dim(experimentData(spe_list)), c(2, 0))

  #change experiment names
  experimentNames(spe_list) = LETTERS[1:2]
  expect_equal(experimentNames(spe_list), LETTERS[1:2])
  expect_error((experimentNames(spe_list) = LETTERS[1:3]))
  expect_warning((experimentNames(spe_list) = NULL), 'NULL')
  experimentNames(spe_list) = NULL
  expect_null(experimentNames(spe_list))
  experimentNames(spe_list) = LETTERS[1:2]
  expect_warning((experimentData(spe_list) = expdf), 'NULL')
})

test_that("Subset functions work", {
  library(SpatialExperiment)

  #no names
  mat = matrix(1, 3, 3)
  spe1 = SpatialExperiment(assays = list(mat))
  spe2 = SpatialExperiment(assays = list(mat))
  spe_list = SpatialExperimentList(list(spe1, spe2))

  #subset rows
  expect_equal(nrow(spe_list[1:2, ]), 2)
  expect_error(rownames(spe_list[c(3:4), ]))

  #subset cols
  expect_equal(ncol(spe_list[, 1:2]), 2)
  expect_error(colnames(spe_list[, 7:8]))
  expect_equal(spe_list[, -c(1, 4)]@experimentIndex, rep(1:2, each = 2))

  #with names
  mat = matrix(1, 3, 3, dimnames = list(paste0('r', 1:3), paste0('c', 1:3)))
  spe1 = SpatialExperiment(assays = list(mat))
  spe2 = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = SpatialExperimentList(list('A' = spe1, 'B' = spe2), expdf)

  #subset rows
  expect_equal(nrow(spe_list[1:2, ]), 2)
  expect_equal(rownames(spe_list[1:2, ]), c('r1', 'r2'))
  expect_equal(rownames(spe_list[c('r1', 'r3'), ]), c('r1', 'r3'))
  expect_error(rownames(spe_list[c('r1', 'r4'), ]))

  #subset cols
  expect_equal(ncol(spe_list[, 1:2]), 2)
  expect_equal(colnames(spe_list[, 1:2]), c('A.c1', 'A.c2'))
  expect_equal(colnames(spe_list[, c('A.c1', 'B.c1')]), c('A.c1', 'B.c1'))
  expect_error(colnames(spe_list[, c('A.c1', 'A.c4')]))
  expect_equal(spe_list[, -c(1, 4)]@experimentIndex, rep(1:2, each = 2))



})
