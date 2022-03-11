test_that("Constructor works (SummarizedExperiment)", {
  library(SummarizedExperiment)
  library(SingleCellExperiment)
  library(SpatialExperiment)

  #empty object
  # expect_true(validObject(ExperimentList())) #exported

  #empty objects in the list
  se_empty1 = SummarizedExperiment()
  se_empty2 = SummarizedExperiment()
  se_empty_list = list(se_empty1, se_empty2)
  expect_true(validObject(ExperimentList(se_empty_list)))
  names(se_empty_list) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(se_empty_list)))

  #mixed objects
  expect_error(ExperimentList(list(SummarizedExperiment(), SingleCellExperiment())))

  #with annotations
  edata = data.frame('ID' = 1:3, row.names = letters[1:3])
  expect_error(ExperimentList(se_empty_list, experimentData = edata))

  edata = edata[1:2, , drop = FALSE]
  expect_error(ExperimentList(se_empty_list, experimentData = edata))

  rownames(edata) = NULL
  expect_error(ExperimentList(se_empty_list, experimentData = edata))
  expect_true(validObject(ExperimentList(se_empty_list, experimentData = edata, check.names = FALSE)))

  rownames(edata) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(se_empty_list, experimentData = edata)))

  #non-empty data
  mat = matrix(1, 3, 3)
  se1 = SummarizedExperiment(assays = list(mat))
  se2 = SummarizedExperiment(assays = list(mat))
  se_list = list('SE1' = se1, 'SE2' = se2)
  expect_true(validObject(ExperimentList(se_list)))

  colnames(mat) = LETTERS[1:3]
  rownames(mat) = letters[1:3]
  se1 = SummarizedExperiment(assays = list(mat))
  se2 = SummarizedExperiment(assays = list(mat))
  se_list = list('SE1' = se1, 'SE2' = se2)
  expect_true(validObject(ExperimentList(se_list)))
})

test_that("Constructor works (RangedSummarizedExperiment)", {
  library(SummarizedExperiment)
  library(SingleCellExperiment)
  library(SpatialExperiment)

  #empty objects in the list
  se_empty1 = SummarizedExperiment(rowRanges = GRangesList())
  se_empty2 = SummarizedExperiment(rowRanges = GRangesList())
  se_empty_list = list(se_empty1, se_empty2)
  expect_true(validObject(ExperimentList(se_empty_list)))
  names(se_empty_list) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(se_empty_list)))

  #mixed objects
  expect_error(ExperimentList(list(SummarizedExperiment(), SingleCellExperiment())))

  #with annotations
  edata = data.frame('ID' = 1:3, row.names = letters[1:3])
  expect_error(ExperimentList(se_empty_list, experimentData = edata))

  edata = edata[1:2, , drop = FALSE]
  expect_error(ExperimentList(se_empty_list, experimentData = edata))

  rownames(edata) = NULL
  expect_error(ExperimentList(se_empty_list, experimentData = edata))
  expect_true(validObject(ExperimentList(se_empty_list, experimentData = edata, check.names = FALSE)))

  rownames(edata) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(se_empty_list, experimentData = edata)))

  #non-empty data
  mat = matrix(1, 3, 3)
  gr = GRanges("chr2", IRanges(3:5, 6:8))
  se1 = SummarizedExperiment(assays = list(mat), rowRanges = gr)
  se2 = SummarizedExperiment(assays = list(mat), rowRanges = gr)
  se_list = list('SE1' = se1, 'SE2' = se2)
  expect_true(validObject(ExperimentList(se_list)))

  colnames(mat) = LETTERS[1:3]
  rownames(mat) = letters[1:3]
  se1 = SummarizedExperiment(assays = list(mat), rowRanges = gr)
  se2 = SummarizedExperiment(assays = list(mat), rowRanges = gr)
  se_list = list('SE1' = se1, 'SE2' = se2)
  expect_true(validObject(ExperimentList(se_list)))
})

test_that("Constructor works (SingleCellExperiment)", {
  library(SummarizedExperiment)
  library(SingleCellExperiment)
  library(SpatialExperiment)

  #empty objects in the list
  sce_empty1 = SingleCellExperiment()
  sce_empty2 = SingleCellExperiment()
  sce_empty_list = list(sce_empty1, sce_empty2)
  expect_true(validObject(ExperimentList(sce_empty_list)))
  names(sce_empty_list) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(sce_empty_list)))

  #mixed objects
  expect_error(ExperimentList(list(SummarizedExperiment(), SingleCellExperiment())))

  #with annotations
  edata = data.frame('ID' = 1:3, row.names = letters[1:3])
  expect_error(ExperimentList(sce_empty_list, experimentData = edata))

  edata = edata[1:2, , drop = FALSE]
  expect_error(ExperimentList(sce_empty_list, experimentData = edata))

  rownames(edata) = NULL
  expect_error(ExperimentList(sce_empty_list, experimentData = edata))
  expect_true(validObject(ExperimentList(sce_empty_list, experimentData = edata, check.names = FALSE)))

  rownames(edata) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(sce_empty_list, experimentData = edata)))

  #non-empty data
  mat = matrix(1, 3, 3)
  sce1 = SingleCellExperiment(assays = list(mat))
  sce2 = SingleCellExperiment(assays = list(mat))
  sce_list = list('SCE1' = sce1, 'SCE2' = sce2)
  expect_true(validObject(ExperimentList(sce_list)))

  colnames(mat) = LETTERS[1:3]
  rownames(mat) = letters[1:3]
  sce1 = SingleCellExperiment(assays = list(mat))
  sce2 = SingleCellExperiment(assays = list(mat))
  sce_list = list('SCE1' = sce1, 'SCE2' = sce2)
  expect_true(validObject(ExperimentList(sce_list)))
})

test_that("Constructor works (SpatialExperiment)", {
  library(SummarizedExperiment)
  library(SingleCellExperiment)
  library(SpatialExperiment)

  #empty objects in the list
  spe_empty1 = SpatialExperiment()
  spe_empty2 = SpatialExperiment()
  spe_empty_list = list(spe_empty1, spe_empty2)
  expect_true(validObject(ExperimentList(spe_empty_list)))
  names(spe_empty_list) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(spe_empty_list)))

  #mixed objects
  expect_error(ExperimentList(list(SpatialExperiment(), SingleCellExperiment())))

  #with annotations
  edata = data.frame('ID' = 1:3, row.names = letters[1:3])
  expect_error(ExperimentList(spe_empty_list, experimentData = edata))

  edata = edata[1:2, , drop = FALSE]
  expect_error(ExperimentList(spe_empty_list, experimentData = edata))

  rownames(edata) = NULL
  expect_error(ExperimentList(spe_empty_list, experimentData = edata))
  expect_true(validObject(ExperimentList(spe_empty_list, experimentData = edata, check.names = FALSE)))

  rownames(edata) = LETTERS[1:2]
  expect_true(validObject(ExperimentList(spe_empty_list, experimentData = edata)))

  #non-empty data
  mat = matrix(1, 3, 3)
  spe1 = SpatialExperiment(assays = list(mat))
  spe2 = SpatialExperiment(assays = list(mat))
  spe_list = list('SPE1' = spe1, 'SPE2' = spe2)
  expect_true(validObject(ExperimentList(spe_list)))

  colnames(mat) = LETTERS[1:3]
  rownames(mat) = letters[1:3]
  spe1 = SpatialExperiment(assays = list(mat))
  spe2 = SpatialExperiment(assays = list(mat))
  spe_list = list('SPE1' = spe1, 'SPE2' = spe2)
  expect_true(validObject(ExperimentList(spe_list)))
})
