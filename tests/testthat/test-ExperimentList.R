test_that("Constructor works", {
  library(SummarizedExperiment)
  library(SingleCellExperiment)
  library(SpatialExperiment)

  #empty object
  expect_true(validObject(.SummarizedExperimentList())) #internal
  expect_true(validObject(SummarizedExperimentList())) #exported

  #empty objects in the list
  se_empty1 = SummarizedExperiment()
  se_empty2 = SummarizedExperiment()
  se_empty_list = list(se_empty1, se_empty2)
  expect_error(SummarizedExperimentList(se_empty_list))
  names(se_empty_list) = LETTERS[1:2]
  expect_true(validObject(SummarizedExperimentList(se_empty_list)))

  #with annotations
  edata = data.frame('ID' = 1:3, row.names = letters[1:3])
  expect_error(SummarizedExperimentList(se_empty_list, experimentData = edata))

  edata = edata[1:2, , drop = FALSE]
  expect_error(SummarizedExperimentList(se_empty_list, experimentData = edata))

  rownames(edata) = NULL
  expect_error(SummarizedExperimentList(se_empty_list, experimentData = edata))
  expect_true(validObject(SummarizedExperimentList(se_empty_list, experimentData = edata, check.names = FALSE)))

  rownames(edata) = LETTERS[1:2]
  expect_true(validObject(SummarizedExperimentList(se_empty_list, experimentData = edata)))

  #non-empty data
  mat = matrix(1, 3, 3)
  se1 = SummarizedExperiment(assays = list(mat))
  se2 = SummarizedExperiment(assays = list(mat))
  se_list = list('SE1' = se1, 'SE2' = se2)
  expect_true(validObject(SummarizedExperimentList(se_list)))

  colnames(mat) = LETTERS[1:3]
  rownames(mat) = letters[1:3]
  se1 = SummarizedExperiment(assays = list(mat))
  se2 = SummarizedExperiment(assays = list(mat))
  se_list = list('SE1' = se1, 'SE2' = se2)
  expect_true(validObject(SummarizedExperimentList(se_list)))

  # expect_true(validObject(.RangedSummarizedExperimentList())) #internal
  # expect_true(validObject(RangedSummarizedExperimentList())) #exported
  # expect_true(validObject(.SingleCellExperimentList())) #internal
  # expect_true(validObject(SingleCellExperimentList())) #exported
  # expect_true(validObject(.SpatialExperimentList())) #internal
  # expect_true(validObject(SpatialExperimentList())) #exported
})
