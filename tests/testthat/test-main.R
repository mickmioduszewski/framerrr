test_that("test set up", {
  a <- run_start(global_config = NA, local_config = NA)
  wd <- getwd()
  expect_equal(a$project, "project")
  expect_equal(a$log, "f_log")
})

test_that("test file name cleaning", {
  expect_equal(clean_file_name(c("fred123",
                                 "some file",
                                 "bad file##",
                                 "w##H$A%t^")),
               c("fred123",
                 "some file",
                 "bad file",
                 "w H A t"))

})
