
error1 <- testthat::test_that("Basic testing",{
  testthat::expect_error(countna(datasets::mtcars), "This data set has no missing values")
})



error2 <- testthat::test_that("ggplot2 testing",{
  testthat::expect_true(ggplot2::is.ggplot(countna(datateachr::vancouver_trees)))
})


a <- countna(datateachr::apt_buildings)
error3<- testthat::test_that("ggplot2 y labe axis",{
  testthat::expect_identical(a$labels$y,"Number of NAs")
})
