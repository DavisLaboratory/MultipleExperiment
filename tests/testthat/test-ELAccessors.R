test_that("ExperimentList-specific accessors work", {
  library(SpatialExperiment)

  mat = matrix(1, 3, 3)
  spe = SpatialExperiment(assays = list(mat))
  spe_list = ExperimentList(list(spe,spe))

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

  #colWithExperimentData
  mat = matrix(1, 3, 3, dimnames = list(paste0('r', 1:3), paste0('c', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = ExperimentList(list('A' = spe, 'B' = spe), expdf)
  expect_equal(ncol(colWithExperimentData(spe_list)), 2)

  mat = matrix(1, 3, 3)
  spe = SpatialExperiment(assays = list(mat))
  spe_list = ExperimentList(list(spe,spe))
  expect_equal(ncol(colWithExperimentData(spe_list)), 1)

  #experiments
  mat = matrix(1, 3, 3, dimnames = list(paste0('row', 1:3), paste0('col', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = ExperimentList(list('A' = spe, 'B' = spe), expdf)
  expect_equal(length(experiments(spe_list)), 2)
  expect_equal(names(experiments(spe_list)), LETTERS[1:2])
  expect_equal(colnames(experiments(spe_list)[[1]]), paste0('col', 1:3))
  expect_s4_class(experiments(spe_list)[[1]], 'SpatialExperiment')

  mat = matrix(1, 3, 3)
  spe = SpatialExperiment(assays = list(mat))
  spe_list = ExperimentList(list(spe,spe))
  expect_equal(length(experiments(spe_list)), 2)
  expect_null(names(experiments(spe_list)))
  expect_null(colnames(experiments(spe_list)[[1]]))
  expect_s4_class(experiments(spe_list)[[1]], 'SpatialExperiment')
})

test_that("Subset functions work", {
  library(SpatialExperiment)
  library(SingleCellExperiment)
  library(SummarizedExperiment)

  #no names
  mat = matrix(1, 3, 3)
  spe = SpatialExperiment(assays = list(mat))
  spe_list = ExperimentList(list(spe, spe))

  #subset rows
  expect_equal(nrow(spe_list[1:2, ]), 2)
  expect_error(rownames(spe_list[c(3:4), ]))

  #subset cols
  expect_equal(ncol(spe_list[, 1:2]), 2)
  expect_equal(dim(spe_list[1:2, 1:2]), c(2, 2))
  expect_error(colnames(spe_list[, 7:8]))
  expect_equal(spe_list[, -c(1, 4)]@experimentIndex, rep(1:2, each = 2))
  expect_equal(spe_list[, -c(1:3)]@experimentIndex, rep(2, 3))

  #subset experiments
  expect_equal(ncol(spe_list[, , exp = 1]), 3)
  expect_equal(dim(spe_list[1:2, 2:5, exp = 1]), c(2, 2))
  expect_error(colnames(spe_list[, , exp = 3]))
  expect_equal(spe_list[, -c(1, 4), exp = -2]@experimentIndex, rep(1, each = 2))
  expect_equal(dim(spe_list[, (1:3), exp = 2]), c(3, 0))
  expect_equal(spe_list[, c(FALSE, TRUE, TRUE), exp = -2]@experimentIndex, rep(1, each = 2))

  #with names
  mat = matrix(1, 3, 3, dimnames = list(paste0('r', 1:3), paste0('c', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = ExperimentList(list('A' = spe, 'B' = spe), expdf)

  #subset rows
  expect_equal(nrow(spe_list[1:2, ]), 2)
  expect_equal(rownames(spe_list[1:2, ]), c('r1', 'r2'))
  expect_equal(rownames(spe_list[c('r1', 'r3'), ]), c('r1', 'r3'))
  expect_error(rownames(spe_list[c('r1', 'r4'), ]))

  #subset cols
  expect_equal(ncol(spe_list[, c('A.c1', 'A.c2')]), 2)
  expect_equal(colnames(spe_list[, 1:2]), c('A.c1', 'A.c2'))
  expect_equal(colnames(spe_list[, c('A.c1', 'B.c1')]), c('A.c1', 'B.c1'))
  expect_error(colnames(spe_list[, c('A.c1', 'A.c4')]))

  #subset experiments
  expect_equal(ncol(spe_list[, , exp = 'A']), 3)
  expect_equal(dim(spe_list[c('r1', 'r2'), c('A.c2', 'A.c3', 'B.c1'), exp = 'A']), c(2, 2))
  expect_error(colnames(spe_list[, , exp = 'C']))
  expect_equal(spe_list[, -c(1, 4), exp = 'A']@experimentIndex, rep(1, each = 2))
  expect_equal(spe_list[, c(FALSE, TRUE, TRUE), exp = 'A']@experimentIndex, rep(1, each = 2))
  expect_equal(dim(spe_list[, (1:3), exp = c(FALSE, TRUE)]), c(3, 0))

  #SummarizedExperiment
  mat = matrix(1, 3, 3)
  se = SummarizedExperiment(assays = list(mat))
  se_list = ExperimentList(list(se, se))
  #subset rows
  expect_equal(nrow(se_list[1:2, ]), 2)
  expect_error(rownames(se_list[c(3:4), ]))
  #subset cols
  expect_equal(ncol(se_list[, 1:2]), 2)
  expect_equal(dim(se_list[1:2, 1:2]), c(2, 2))
  expect_error(colnames(se_list[, 7:8]))
  expect_equal(se_list[, -c(1, 4)]@experimentIndex, rep(1:2, each = 2))
  expect_equal(se_list[, -c(1:3)]@experimentIndex, rep(2, 3))

  #RangedSummarizedExperiment
  mat = matrix(1, 3, 3)
  se = SummarizedExperiment(assays = list(mat), rowRanges = GRanges("chr2", IRanges(3:5, 6:8)))
  se_list = ExperimentList(list(se, se))
  #subset rows
  expect_equal(nrow(se_list[1:2, ]), 2)
  expect_error(rownames(se_list[c(3:4), ]))
  #subset cols
  expect_equal(ncol(se_list[, 1:2]), 2)
  expect_equal(dim(se_list[1:2, 1:2]), c(2, 2))
  expect_error(colnames(se_list[, 7:8]))
  expect_equal(se_list[, -c(1, 4)]@experimentIndex, rep(1:2, each = 2))
  expect_equal(se_list[, -c(1:3)]@experimentIndex, rep(2, 3))

  #SingleCellExperiment
  mat = matrix(1, 3, 3)
  sce = SingleCellExperiment(assays = list(mat))
  sce_list = ExperimentList(list(sce, sce))
  #subset rows
  expect_equal(nrow(sce_list[1:2, ]), 2)
  expect_equal(dim(sce_list[1:2, 1:2]), c(2, 2))
  expect_error(rownames(sce_list[c(3:4), ]))
  #subset cols
  expect_equal(ncol(sce_list[, 1:2]), 2)
  expect_error(colnames(sce_list[, 7:8]))
  expect_equal(sce_list[, -c(1, 4)]@experimentIndex, rep(1:2, each = 2))
  expect_equal(sce_list[, -c(1:3)]@experimentIndex, rep(2, 3))
})

test_that("Subset functions work on imgData", {
  library(SpatialExperiment)

  dir = system.file(file.path("extdata", "10xVisium"), package = "SpatialExperiment")
  sample_ids = c("section1", "section2")
  samples = file.path(dir, sample_ids)
  spe = read10xVisium(samples, sample_ids,
                        type = "sparse", data = "raw",
                        images = "lowres", load = FALSE)

  #subset imgData works for SPEList
  imgData(spe) = imgData(spe)[1, ]
  el = ExperimentList(list(spe, spe))

  expect_equal(nrow(imgData(el)), 2)
  expect_equal(nrow(imgData(el[,, exp = 1])), 1)
  expect_equal(nrow(imgData(el[,, exp = -1])), 1)
})
