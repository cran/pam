test_that("test-walsby_etr_II generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 13.3218134)
    expect_equal(model_result[["etr_max"]], 64.6205229)
    expect_equal(model_result[["alpha"]], 0.19796623)
    expect_equal(model_result[["beta"]], -0.018109036)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 13.3218134)
    expect_equal(model_result[["etr_max"]], 64.6205229)
    expect_equal(model_result[["alpha"]], 0.19796623)
    expect_equal(model_result[["beta"]], -0.018109036)
  }
})

test_that("test-walsby_etr_II control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "walsby ETR II 20240925.csv",
        color_walsby
      )
    )
  )
})

test_that("test-walsby_etr_II generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 13.3218134)
    expect_equal(model_result[["a"]], 64.6205229)
    expect_equal(model_result[["b"]], 0.19796623)
    expect_equal(model_result[["c"]], -0.018109036)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.19796623)
    expect_equal(model_result[["beta"]], -0.018109036)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5716344)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 64.6205229)
    expect_equal(model_result[["ik_with_photoinhibition"]], 225.14766)
    expect_equal(model_result[["ik_without_photoinhibition"]], 326.421945)
    expect_equal(model_result[["im_with_photoinhibition"]], 781.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.449812727)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 13.3218134)
    expect_equal(model_result[["a"]], 64.6205229)
    expect_equal(model_result[["b"]], 0.19796623)
    expect_equal(model_result[["c"]], -0.018109036)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.19796623)
    expect_equal(model_result[["beta"]], -0.018109036)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5716344)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 64.6205229)
    expect_equal(model_result[["ik_with_photoinhibition"]], 225.14766)
    expect_equal(model_result[["ik_without_photoinhibition"]], 326.421945)
    expect_equal(model_result[["im_with_photoinhibition"]], 781.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.449812727)
  }
})

test_that("test-walsby_etr_II modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "walsby ETR II modified 20240925.csv",
        color_walsby
      )
    )
  )
})
