suppressMessages({
  library(OpenMx); library(mvtnorm)
  pkgload::load_all(".", quiet = TRUE)      # dev source has the temporal builders
})
source(file.path("data-raw", "smoketest_helpers.R"))   # free_only()
set.seed(20260727)

# ---- settings ----
n_pairs    <- 600
tpmax      <- 1                              # linear time (degree of the poly basis)
half_range <- 3                              # birth-time index spread: [-3, 3]
tries      <- 5
group_props <- c(MZss = 1/3, DZss = 1/3, DZos = 1/3)
components_ae  <- c("a", "e")
components_ace <- c("a", "cn", "e")

# ---- true generating values ----
true_ace <- c(b_a_0 = 0.6, b_a_1 = 0.20,         # A loading = exp(0.6 + 0.20*t)
            b_cn_0 = 0.3, b_cn_1 = 0.10,         # C loading = exp(0.3 + 0.10*t)
            b_e_0 = 0.4, b_e_1 = -0.10)        # E loading = exp(0.4 - 0.10*t)
true_ae <- true_ace[names(true_ace) != "b_cn_0" & names(true_ace) != "b_cn_1"]
true <- true_ace
labels_to_free_ae <- c(names(true_ae), "mean_y")
labels_to_free_ace <- c(names(true_ace), "mean_y")

lam <- function(k, t,
                true=true_ace) exp(true[[paste0("b_", k, "_0")]] + true[[paste0("b_", k, "_1")]] * t)
Amat_for <- function(zyg) if (zyg == "MZss") matrix(1, 2, 2) else matrix(c(1, .5, .5, 1), 2, 2)
I2 <- diag(2)

simulate_pair <- function(id, zyg, components = components_ace) {
  t  <- runif(1, -half_range, half_range)    # shared birth-time index for the pair
  A  <- Amat_for(zyg)
  C  <- matrix(1, 2, 2)                          # shared environment
  la <- lam("a", t); le <- lam("e", t); lc <- lam("cn", t)
  if (!("cn" %in% components)) lc <- 0
  if (!("a"  %in% components)) la <- 0
  V  <- A * la^2 + C * lc^2 + I2 * le^2       # both twins share t -> tcrossprod = la^2 * ones
  list(A = A,
       C = C,
       y = as.numeric(mvtnorm::rmvnorm(1, sigma = V)),
       obs_ids = paste0("P", id, "_", 1:2), t = c(t, t))
}

zyg  <- sample(names(group_props), n_pairs, TRUE, group_props)
fams_ae <- lapply(seq_len(n_pairs), function(i) simulate_pair(i, zyg[i], components = components_ae))
fams_ace <- lapply(seq_len(n_pairs), function(i) simulate_pair(i, zyg[i], components = components_ace))
cat("Group counts:\n"); print(table(zyg))

groups_ae <- lapply(seq_along(fams_ae), function(i) {
  f <- fams_ae[[i]]
  buildOneTemporalFamilyGroup(
    group_name = paste0("pair", i),
    Addmat = f$A, Nucmat = NULL, Extmat = NULL, Mtdmat = NULL, Dmgmat = NULL,
    full_df_row = f$y, obs_ids = f$obs_ids,
    param_year = f$t, H = NULL,               # H = NULL -> no historical moderator
    use_exp_loadings = TRUE, time_point_max = tpmax
  )
})

model_ae <- buildTemporalPedigreeMx(
  model_name = "TwinTemporalAE_simple", group_models = groups_ae,
  p_hist = 0, components = components_ae, ci = T, time_point_max = tpmax
)

model_ae <- free_only(model_ae, labels_to_free_ae)
model_ae <- mxOption(model_ae, "Number of Threads", 1L)

fit_ae <- mxTryHard(model_ae, extraTries = tries, intervals = T, silent = F)


print(summary(fit_ae))




est_ae <- omxGetParameters(fit_ae)
tgt_ae <- true[names(true) %in% names(est_ae)]
out_ae <- data.frame(parameter = names(tgt_ae),
                  true = round(as.numeric(tgt_ae), 4),
                  estimate = round(as.numeric(est_ae[names(tgt_ae)]), 4))
out_ae$abs_err <- round(abs(out_ae$estimate - out_ae$true), 4)
cat("\nstatus code:", fit_ae$output$status$code,
    "  -2LL:", round(fit_ae$output$Minus2LogLikelihood, 2), "\n\n")
print(out_ae, row.names = FALSE)
cat("\nMax abs error:", round(max(out_ae$abs_err), 4), "\n")


# ACE model



groups_ace <- lapply(seq_along(fams_ace), function(i) {
  f <- fams_ace[[i]]
  buildOneTemporalFamilyGroup(
    group_name = paste0("pair", i),
    Addmat = f$A, Nucmat = f$C, Extmat = NULL, Mtdmat = NULL, Dmgmat = NULL,
    full_df_row = f$y, obs_ids = f$obs_ids,
    param_year = f$t, H = NULL,               # H = NULL -> no historical moderator
    use_exp_loadings = TRUE, time_point_max = tpmax
  )
})
model_ace <- buildTemporalPedigreeMx(
  model_name = "TwinTemporalACE_simple", group_models = groups_ace,
  p_hist = 0, components = components_ace, ci = T, time_point_max = tpmax
)


model_ace <- free_only(model_ace, labels_to_free_ace)
model_ace <- mxOption(model_ace, "Number of Threads", 1L)

fit_ace <- mxTryHard(model_ace, extraTries = tries, intervals = T, silent = F)

print(summary(fit_ace, verbose = TRUE))



est_ace <- omxGetParameters(fit_ace)
tgt_ace <- true[names(true) %in% names(est_ace)]
out_ace <- data.frame(parameter = names(tgt_ace),
                  true = round(as.numeric(tgt_ace), 4),
                  estimate = round(as.numeric(est_ace[names(tgt_ace)]), 4))
out_ace$abs_err <- round(abs(out_ace$estimate - out_ace$true), 4)

print(out_ace, row.names = FALSE)
