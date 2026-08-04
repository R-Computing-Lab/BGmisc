test_that("buildOneTemporalFamilyGroup() works with and without ph", {

fsize <- 3
ids <- c("a", "b", "c")
A <- matrix(0.5, fsize, fsize); diag(A) <- 1
d <- as.data.frame(matrix(c(60, 62, 58), nrow = 1)); colnames(d) <- ids

for (ph in c(0, 1)) {
  H <- if (ph > 0) matrix(c(0, 1, 1), ncol = 1) else NULL
  g <- buildOneTemporalFamilyGroup(
    group_name = paste0("g", ph), Addmat = A,
    full_df_row = d, obs_ids = ids,
    param_year = c(-1, 0, 1), H = H,
    use_exp_loadings = FALSE, clean_ids = FALSE
  )
# cat("p_hist =", ph, "-> M formula: ", deparse(g$M$formula), "\n")
if(ph > 0){
    expect_equal(paste0(g$M$formula)[1], paste0("t"))
  expect_equal(paste0(g$M$formula)[2], paste0("Tpoly %*% ModelOne.B_mean + H %*% ModelOne.G_mean"))
}

if(ph == 0){
  expect_equal(paste0(g$M$formula)[1], paste0("t"))
  expect_equal(paste0(g$M$formula)[2], paste0("Tpoly %*% ModelOne.B_mean"))
}
}

m <- .pedigreeMeanPart(fsize = fsize, obs_ids = ids, label = "meanLI", mean_basis = NULL)

expect_equal(class(m)[1],"FullMatrix")
expect_equal(length(m$labels),fsize)

)
