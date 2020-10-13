test_that("test that set up is correct", {
  a <- run_start(global_config = NA, local_config = NA)
  wd <- getwd()
  expect_equal(a$project, "project")
  expect_equal(a$log, "f_log")
})
