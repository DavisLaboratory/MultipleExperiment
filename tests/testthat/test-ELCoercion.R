test_that("Coercion works", {
  mat = matrix(1, 3, 3, dimnames = list(paste0('row', 1:3), paste0('col', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = ExperimentList(list('A' = spe, 'B' = spe), expdf)

  expect_s4_class(as(spe_list, 'SingleCellExperimentList'), 'SingleCellExperimentList')
  expect_s4_class(as(spe_list, 'SummarizedExperimentList'), 'SummarizedExperimentList')
  expect_s4_class(as(spe_list, 'RangedSummarizedExperimentList'), 'RangedSummarizedExperimentList')

  sce_list = as(spe_list, 'SingleCellExperimentList')
  expect_s4_class(as(sce_list, 'SpatialExperimentList'), 'SpatialExperimentList')
  expect_s4_class(as(sce_list, 'RangedSummarizedExperimentList'), 'RangedSummarizedExperimentList')
  expect_s4_class(as(sce_list, 'SummarizedExperimentList'), 'SummarizedExperimentList')

  rse_list = as(spe_list, 'RangedSummarizedExperimentList')
  # expect_s4_class(as(rse_list, 'SpatialExperimentList'), 'SpatialExperimentList') #issue with SpatialExperiment coercion
  expect_s4_class(as(rse_list, 'SingleCellExperimentList'), 'SingleCellExperimentList')
  expect_s4_class(as(rse_list, 'SummarizedExperimentList'), 'SummarizedExperimentList')

  se_list = as(spe_list, 'SummarizedExperimentList')
  # expect_s4_class(as(se_list, 'SpatialExperimentList'), 'SpatialExperimentList') #issue with SpatialExperiment coercion
  expect_s4_class(as(se_list, 'SingleCellExperimentList'), 'SingleCellExperimentList')
  expect_s4_class(as(se_list, 'RangedSummarizedExperimentList'), 'RangedSummarizedExperimentList')
})
