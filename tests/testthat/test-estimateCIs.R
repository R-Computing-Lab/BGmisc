test_that("Raykov method with known inputs matches expected values", {
  tolerance <- 1e-9

  test_data <- data.frame(rho = 0.5, se = 0.1)

  # Expected calculations
  adjust <- sqrt(2)
  rho <- 0.5
  se <- 0.1
  se_adj <- se * adjust
  z <- .fisherz(rho) # 0.5493
  sez <- se / (1 - rho^2) # 0.1 / (1 - 0.25) = 0.1333...
  sez_adj <- se_adj / (1 - rho^2) # = 0.18898...
  z_crit <- qnorm(0.975) # ~1.96
  upper_z <- z + z_crit * sez_adj
  lower_z <- z - z_crit * sez_adj
  upper_r <- .fisherz2r(upper_z)
  lower_r <- .fisherz2r(lower_z)
  z_test <- z / sez_adj
  p_two_tail <- 2 * pnorm(abs(z_test), lower.tail = FALSE)

  result <- calculateCIs(test_data,
    rho_var = "rho",
    se_var = "se",
    doubleentered = TRUE,
    method = "raykov"
  )

  expect_equal(result$rho_plusse, upper_r,
    tolerance = tolerance
  )
  expect_equal(result$rho_minusse, lower_r,
    tolerance = tolerance
  )
  expect_equal(result$rho_ztest, z_test,
    tolerance = tolerance
  )
  expect_equal(result$rho_zp2tail, p_two_tail,
    tolerance = tolerance
  )
})
test_that("basic CI calculation without method", {
  tbl <- data.frame(rho = c(0.5, 0.7, 0.3), se = c(0.1, 0.2, 0.05))
  result <- calculateCIs(tbl, rho_var = "rho", se_var = "se", method = "other")

  expect_true(all(c("rho_plusse", "rho_minusse") %in% colnames(result)))
  expect_false(any(c("rho_z", "rho_ztest", "rho_zp2tail") %in% colnames(result)))
  expect_equal(nrow(result), 3)
})


test_that("basic CI calculation with raykov method", {
  tbl <- data.frame(rho = c(0.5, 0.7, 0.3), se = c(0.1, 0.2, 0.05))
  result <- calculateCIs(tbl, rho_var = "rho", se_var = "se", method = "raykov")

  expect_true(all(c("rho_plusse", "rho_minusse", "rho_z", "rho_ztest", "rho_zp2tail") %in% colnames(result)))
  expect_equal(nrow(result), 3)
})

test_that("adjustment for double-entered data", {
  tbl <- data.frame(rho = c(0.6), se = c(0.1))
  result <- calculateCIs(tbl, rho_var = "rho", se_var = "se", doubleentered = TRUE, method = "raykov")

  expect_gt(result$se_se_adjusted, tbl$se)
})

test_that("vectorized design effect column use", {
  tbl <- data.frame(
    rho = c(0.5, 0.7),
    se = c(0.1, 0.15),
    m_col = c(3, 4),
    rho_col = c(0.1, 0.05)
  )

  result <- calculateCIs(tbl,
    rho_var = "rho",
    se_var = "se",
    method = "raykov",
    design_effect_m_col = "m_col",
    design_effect_rho_col = "rho_col"
  )

  expect_true("se_sez_adjusted" %in% colnames(result))
  expect_equal(nrow(result), 2)

  expect_gt(result$se_sez_adjusted[1], result$se_sez[1])
  expect_gt(result$se_sez_adjusted[2], result$se_sez[2])
})

test_that("scalar design effect override works", {
  tbl <- data.frame(rho = c(0.4), se = c(0.12))
  result <- calculateCIs(tbl,
    rho_var = "rho",
    se_var = "se",
    method = "raykov",
    design_effect_m = 5,
    design_effect_rho = 0.2
  )

  expected_adjustment <- sqrt(1 + (5 - 1) * 0.2)
  expect_equal(result$se_se_adjusted, tbl$se * expected_adjustment)
})
