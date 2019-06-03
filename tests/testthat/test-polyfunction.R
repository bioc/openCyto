context("polyfunction")

data("GvHD")
gs <- GatingSet(GvHD[1])
gs <- transform(gs, estimateLogicle(gs[[1]], colnames(gs)[3:7]))
test_that("polyfunction testing", {
  add_pop(gs, alias = "FL1+", parent = "root", dims = "FL1", gating_method = "tailgate")
  add_pop(gs, alias = "FL3+", parent = "root", dims = "FL3", gating_method = "tailgate")
  add_pop(gs, alias = "FL4+", parent = "root", dims = "FL4", gating_method = "tailgate")
  add_pop(gs, alias = "*", parent = "root", dims = "*", gating_method = "polyfunctions", gating_args = "FL1+:FL3+:FL4+")
  expect_equal(getNodes(gs)[-(1:4)], c("/FL1+&FL3+&FL4+", "/FL1+&FL3+&!FL4+", "/FL1+&!FL3+&FL4+", "/FL1+&!FL3+&!FL4+", "/!FL1+&FL3+&FL4+", "/!FL1+&FL3+&!FL4+", "/!FL1+&!FL3+&FL4+", "/!FL1+&!FL3+&!FL4+"))
  node <- "/FL1+&!FL3+&!FL4+"
  expect_equal(getTotal(gs[[1]], node), sum(getIndices(gs[[1]], "FL1+")&!getIndices(gs[[1]], "FL3+")&!getIndices(gs[[1]], "FL4+")))
  })