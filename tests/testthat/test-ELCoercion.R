test_that("Coercion works", {
  mat = matrix(1, 3, 3, dimnames = list(paste0('row', 1:3), paste0('col', 1:3)))
  spe = SpatialExperiment(assays = list(mat))
  expdf = matrix(1, 2, 1, dimnames = list(LETTERS[1:2], c('Col1')))
  spe_list = ExperimentList(list('A' = spe, 'B' = spe), expdf)

  expect_s4_class(as(spe_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(spe_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(spe_list, 'ExperimentList'), 'ExperimentList')

  sce_list = as(spe_list, 'ExperimentList')
  expect_s4_class(as(sce_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(sce_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(sce_list, 'ExperimentList'), 'ExperimentList')

  rse_list = as(spe_list, 'ExperimentList')
  expect_s4_class(as(rse_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(rse_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(rse_list, 'ExperimentList'), 'ExperimentList')

  se_list = as(spe_list, 'ExperimentList')
  expect_s4_class(as(se_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(se_list, 'ExperimentList'), 'ExperimentList')
  expect_s4_class(as(se_list, 'ExperimentList'), 'ExperimentList')
})
