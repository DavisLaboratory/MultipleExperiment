test_that("Coercion works", {
  mat = matrix(1, 3, 3, dimnames = list(paste0('row', 1:3), paste0('col', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = MultipleExperiment(list('A' = spe, 'B' = spe), expdf)

  expect_s4_class(as(spe_list, 'SingleCellMultipleExperiment'), 'SingleCellMultipleExperiment')
  expect_s4_class(as(spe_list, 'SummarizedMultipleExperiment'), 'SummarizedMultipleExperiment')
  expect_s4_class(as(spe_list, 'RangedSummarizedMultipleExperiment'), 'RangedSummarizedMultipleExperiment')

  sce_list = as(spe_list, 'SingleCellMultipleExperiment')
  expect_s4_class(as(sce_list, 'SpatialMultipleExperiment'), 'SpatialMultipleExperiment')
  expect_s4_class(as(sce_list, 'RangedSummarizedMultipleExperiment'), 'RangedSummarizedMultipleExperiment')
  expect_s4_class(as(sce_list, 'SummarizedMultipleExperiment'), 'SummarizedMultipleExperiment')

  rse_list = as(spe_list, 'RangedSummarizedMultipleExperiment')
  # expect_s4_class(as(rse_list, 'SpatialMultipleExperiment'), 'SpatialMultipleExperiment') #issue with SpatialExperiment coercion
  expect_s4_class(as(rse_list, 'SingleCellMultipleExperiment'), 'SingleCellMultipleExperiment')
  expect_s4_class(as(rse_list, 'SummarizedMultipleExperiment'), 'SummarizedMultipleExperiment')

  se_list = as(spe_list, 'SummarizedMultipleExperiment')
  # expect_s4_class(as(se_list, 'SpatialMultipleExperiment'), 'SpatialMultipleExperiment') #issue with SpatialExperiment coercion
  expect_s4_class(as(se_list, 'SingleCellMultipleExperiment'), 'SingleCellMultipleExperiment')
  expect_s4_class(as(se_list, 'RangedSummarizedMultipleExperiment'), 'RangedSummarizedMultipleExperiment')

  #empty lists
  mat = matrix(1, 3, 3, dimnames = list(paste0('row', 1:3), paste0('col', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = MultipleExperiment(list('A' = spe, 'B' = spe), expdf)

  spe_list = spe_list[, , exp = FALSE]

  expect_s4_class(as(spe_list, 'SingleCellMultipleExperiment'), 'SingleCellMultipleExperiment')
  expect_s4_class(as(spe_list, 'SummarizedMultipleExperiment'), 'SummarizedMultipleExperiment')
  expect_s4_class(as(spe_list, 'RangedSummarizedMultipleExperiment'), 'RangedSummarizedMultipleExperiment')

  sce_list = as(spe_list, 'SingleCellMultipleExperiment')
  # expect_s4_class(as(sce_list, 'SpatialMultipleExperiment'), 'SpatialMultipleExperiment')
  expect_s4_class(as(sce_list, 'RangedSummarizedMultipleExperiment'), 'RangedSummarizedMultipleExperiment')
  expect_s4_class(as(sce_list, 'SummarizedMultipleExperiment'), 'SummarizedMultipleExperiment')

  rse_list = as(spe_list, 'RangedSummarizedMultipleExperiment')
  # expect_s4_class(as(rse_list, 'SpatialMultipleExperiment'), 'SpatialMultipleExperiment') #issue with SpatialExperiment coercion
  expect_s4_class(as(rse_list, 'SingleCellMultipleExperiment'), 'SingleCellMultipleExperiment')
  expect_s4_class(as(rse_list, 'SummarizedMultipleExperiment'), 'SummarizedMultipleExperiment')

  se_list = as(spe_list, 'SummarizedMultipleExperiment')
  # expect_s4_class(as(se_list, 'SpatialMultipleExperiment'), 'SpatialMultipleExperiment') #issue with SpatialExperiment coercion
  expect_s4_class(as(se_list, 'SingleCellMultipleExperiment'), 'SingleCellMultipleExperiment')
  expect_s4_class(as(se_list, 'RangedSummarizedMultipleExperiment'), 'RangedSummarizedMultipleExperiment')
})
