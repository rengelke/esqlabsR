test_that("PI tasks are created from an Excel file correctly", {
  projectConfiguration <- createDefaultProjectConfiguration(test_ProjectConfiguration())
  expect_no_error(task <- createPITaskFromExcel(projectConfiguration = projectConfiguration))
})
test_that("PI tasks created from an Excel file can be run correctly", {
  projectConfiguration <- esqlabsR::createDefaultProjectConfiguration("ProjectConfiguration.xlsx")
  task <- createPITaskFromExcel(projectConfiguration = projectConfiguration)
  expect_no_error(task$run())
})


