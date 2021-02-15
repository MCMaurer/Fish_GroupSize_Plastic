library(tidyverse)
library(brms)
library(performance)
library(patchwork)
library(tidybayes)

source("icc_functions.R")

theme_set(MCMsBasics::minimal_ggplot_theme())

known <- read_rds("latency/fit_models/lat_typ_int_hurd_nbin_hu_fit.rds")
known

meanlat <- read_rds("other_models/fit_models/mean_group_lat_all_assays_noncentered_fitted.rds")

variance_decomposition(model = known, robust = FALSE)


variance_decomposition(meanlat, robust = F)

variance_decomposition(meanlat, robust = T)


variance_decomposition


my_icc(known)

variance_decomposition(known)


# ICC on each part of hurdle model ----------------------------------------

# need to check if NA is the right way to do it
# negbinom part
ppdk <- posterior_predict(known)
ppdk[ppdk == 0] <- NA

ppdk
var_total_k <- apply(ppdk, 1, FUN = stats::var, na.rm = T)
var_total_k

ppd0k <- posterior_predict(known, re_formula = ~(1|tank))
ppd0k[ppd0k == 0] <- NA

var_sub_k <- apply(ppd0k, 1, FUN = stats::var, na.rm = T)

var_ratio <- var_sub_k/var_total_k

var_icc <- 1 - var_ratio

var_icc %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram()

var_icc %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var, y = 0)) +
  geom_halfeyeh(.width = 0.95) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  xlab("ICC for negbinom")




# hurdle part


x <- rnorm(1000)
y <- x

var(x)
var(y)

# randomly remove values
y[sample(1:length(y), size = 500, replace = F)] <- NA

var(x)
var(y, na.rm = T)


var(rbernoulli(10000, 0.4))

ppdk <- posterior_predict(known)
ppdk[ppdk > 0] <- 1

ppdk
var_total_k_h <- apply(ppdk, 1, FUN = stats::var, na.rm = T)
var_total_k_h

ppd0k <- posterior_predict(known, re_formula = ~(1|tank))
ppd0k[ppd0k > 0] <- 1

var_sub_k_h <- apply(ppd0k, 1, FUN = stats::var, na.rm = T)

var_ratio_h <- var_sub_k_h/var_total_k_h

var_icc_h <- 1 - var_ratio_h

var_icc_h %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram()

var_icc_h %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var, y = 0)) +
  geom_halfeyeh(.width = 0.95) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  xlab("ICC for hurdle")



# example plots for negative ICC values -----------------------------------

model <- meanlat
re_formula <- NULL
robust <- T
ci <- 0.95

PPD <- brms::posterior_predict(model, re_formula = re_formula, 
                               summary = FALSE)
var_total <- apply(PPD, MARGIN = 1, FUN = stats::var)

pvt <- var_total %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
    geom_histogram(bins = 100) +
    scale_x_log10(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(300,30000)) +
  xlab("variance with random effects")

PPD_0 <- brms::posterior_predict(model, re_formula = NA, 
                                 summary = FALSE)
var_rand_intercept <- apply(PPD_0, MARGIN = 1, FUN = stats::var)
if (robust) {
  fun <- get("median", asNamespace("stats"))
}
else {
  fun <- get("mean", asNamespace("base"))
}

pvri <- var_rand_intercept %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram(bins = 100) +
  scale_x_log10(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(300,30000)) +
  xlab("variance without random effects")

var_icc <- var_rand_intercept/var_total

pvratio <- var_icc %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0, 2)) +
  xlab("variance without random effects / variance with random effects")

pvicc <- var_icc %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = 1 - var)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(-2, 1)) +
  xlab("1 - ratio from plot 3")

p <- pvri / pvt / pvratio / pvicc

p

ggsave("variance_decomposition_demonstration.jpg", p,  width = 7, height = 9)


var_total <- var_total %>% 
  data.frame(var = .) %>% 
  rowid_to_column()

var_rand_intercept <- var_rand_intercept %>% 
  data.frame(var = .) %>% 
  rowid_to_column()

var_icc <- var_icc %>% 
  data.frame(var = .) %>% 
  rowid_to_column()

highlight_rows <- var_icc %>% 
  filter(var > 1) %>% 
  .$rowid

pvt_raw <- var_total %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = var)) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("First 100 variance estimates with random effects")

pvri_raw <- var_rand_intercept %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = var)) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("First 100 variance estimates without random effects")

pvratio_raw <- var_icc %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = var)) +
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.3) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("Each bar from plot 1 divided by the matching bar from plot 2")

pvicc_raw <- var_icc %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = 1 - var)) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("1 - values from plot 3")

pvicc_raw_hist <- var_icc %>% 
  #filter(rowid <= 100) %>% 
  ggplot(aes(x = 1 - var, fill = rowid %in% highlight_rows)) +
  geom_histogram(bins = 100) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("Histogram of values from plot 4 (all 10000, not just first 100)") +
  xlim(c(-1.5,1))

p_raw <- pvri_raw / pvt_raw / pvratio_raw / pvicc_raw / pvicc_raw_hist

p_raw

ggsave("variance_decomposition_demonstration_raw_estimates.jpg", p_raw,  width = 7, height = 9)

known


# to calculate the Bayesian variance ratio, we first take draws from the posterior distribution NOT conditioned on random effects, which is to say we are ignoring a source of variance. we then calculate the variance for each of these sets of draws, so we get a whole vector of variance values. We then repeat this process, but we condition on random effects as well, which means we are using ALL sources of variation from the model. Now we have two vectors of variance estimates, one of which contains only non-group variation, and the other which contains all variation. We then calculate a ratio which is "variance without random effects / variation with random effects". This ratio represents the proportion of variance due to stuff OTHER THAN group effects. To get the proportion of variance that IS due to group effects, we take this previous proportion, and subtract it from 1. Now, typically, the variance without random effects is going to be smaller, since we're ignoring a source of variation. However, when our two vectors of "without group" and "with group" variance THEMSELVES vary widely, we occasionally end up with an instance where the "without group" variance is larger than the "with group" variance. This means the numerator is larger than the denominator, which yields a ratio greater than 1. Then, when we subtract this value from 1, we end up with an ICC equivalent that is less than 0. This is simply a byproduct of the simulation approach used to calculate this metric, and it occurs when the vectors of variance estimates THEMSELVES vary widely. We are still working on figuring out the scenarios that lead to these extremely wide ranges of estimated variance vs. tighter ranges of estimated variances, but we believe it is to do with uncertainty regarding the group-level effects

var_total


var_residual <- var_total - var_rand_intercept

var_residual %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram()


ci_icc <- rev(1 - stats::quantile(var_rand_intercept/var_total, 
                                  probs = c((1 - ci)/2, (1 + ci)/2)))
result <- structure(class = "icc_decomposed", list(ICC_decomposed = 1 - 
                                                     fun(var_icc), ICC_CI = ci_icc))

result
attr(result, "var_rand_intercept") <- fun(var_rand_intercept)
attr(result, "var_residual") <- fun(var_residual)
attr(result, "var_total") <- fun(var_total)
attr(result, "ci.var_rand_intercept") <- bayestestR::ci(var_rand_intercept, 
                                                        ci = ci)
attr(result, "ci.var_residual") <- bayestestR::ci(var_residual, 
                                                  ci = ci)
attr(result, "ci.var_total") <- bayestestR::ci(var_total, 
                                               ci = ci)
attr(result, "ci") <- ci
attr(result, "re.form") <- re_formula
attr(result, "ranef") <- model$ranef$group[1]
attr(attr(result, "ci.var_rand_intercept"), "data") <- NULL
attr(attr(result, "ci.var_residual"), "data") <- NULL
attr(attr(result, "ci.var_total"), "data") <- NULL
result


var_icc





my_icc_plot(meanlat, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank), plot_each_var = T)




# what if PPD and PPD_0 were the same -----------------------------------

model <- meanlat
re_formula <- NULL
robust <- T
ci <- 0.95

PPD <- brms::posterior_predict(model, re_formula = re_formula, 
                               summary = FALSE)
var_total <- apply(PPD, MARGIN = 1, FUN = stats::var)

pvt <- var_total %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram(bins = 100) +
  scale_x_log10(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(300,30000)) +
  xlab("variance with random effects")

# this is the same as PPD
PPD_0 <- brms::posterior_predict(model, re_formula = re_formula, 
                                 summary = FALSE)
var_rand_intercept <- apply(PPD_0, MARGIN = 1, FUN = stats::var)
if (robust) {
  fun <- get("median", asNamespace("stats"))
}
else {
  fun <- get("mean", asNamespace("base"))
}

pvri <- var_rand_intercept %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram(bins = 100) +
  scale_x_log10(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(300,30000)) +
  xlab("variance without random effects")

var_icc <- var_rand_intercept/var_total

pvratio <- var_icc %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0, 2)) +
  xlab("variance without random effects / variance with random effects")

pvicc <- var_icc %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = 1 - var)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(-2, 1)) +
  xlab("1 - ratio from plot 3")

p <- pvri / pvt / pvratio / pvicc

p

ggsave("variance_decomposition_demonstration.jpg", p,  width = 7, height = 9)


var_total <- var_total %>% 
  data.frame(var = .) %>% 
  rowid_to_column()

var_rand_intercept <- var_rand_intercept %>% 
  data.frame(var = .) %>% 
  rowid_to_column()

var_icc <- var_icc %>% 
  data.frame(var = .) %>% 
  rowid_to_column()

highlight_rows <- var_icc %>% 
  filter(var > 1) %>% 
  .$rowid

pvt_raw <- var_total %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = var)) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("First 100 variance estimates with random effects")

pvri_raw <- var_rand_intercept %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = var)) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("First 100 variance estimates without random effects")

pvratio_raw <- var_icc %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = var)) +
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.3) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("Each bar from plot 1 divided by the matching bar from plot 2")

pvicc_raw <- var_icc %>% 
  filter(rowid <= 100) %>% 
  ggplot(aes(x = rowid, y = 1 - var)) +
  geom_col(aes(fill = rowid %in% highlight_rows)) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("1 - values from plot 3")

pvicc_raw_hist <- var_icc %>% 
  #filter(rowid <= 100) %>% 
  ggplot(aes(x = 1 - var, fill = rowid %in% highlight_rows)) +
  geom_histogram(bins = 100) +
  scale_fill_manual(values = c("grey40", "red")) +
  theme(legend.position = "none") +
  xlab("Histogram of values from plot 4 (all 10000, not just first 100)") +
  xlim(c(-1.5,1))

p_raw <- pvri_raw / pvt_raw / pvratio_raw / pvicc_raw / pvicc_raw_hist

p_raw



known


# to calculate the Bayesian variance ratio, we first take draws from the posterior distribution NOT conditioned on random effects, which is to say we are ignoring a source of variance. we then calculate the variance for each of these sets of draws, so we get a whole vector of variance values. We then repeat this process, but we condition on random effects as well, which means we are using ALL sources of variation from the model. Now we have two vectors of variance estimates, one of which contains only non-group variation, and the other which contains all variation. We then calculate a ratio which is "variance without random effects / variation with random effects". This ratio represents the proportion of variance due to stuff OTHER THAN group effects. To get the proportion of variance that IS due to group effects, we take this previous proportion, and subtract it from 1. Now, typically, the variance without random effects is going to be smaller, since we're ignoring a source of variation. However, when our two vectors of "without group" and "with group" variance THEMSELVES vary widely, we occasionally end up with an instance where the "without group" variance is larger than the "with group" variance. This means the numerator is larger than the denominator, which yields a ratio greater than 1. Then, when we subtract this value from 1, we end up with an ICC equivalent that is less than 0. This is simply a byproduct of the simulation approach used to calculate this metric, and it occurs when the vectors of variance estimates THEMSELVES vary widely. We are still working on figuring out the scenarios that lead to these extremely wide ranges of estimated variance vs. tighter ranges of estimated variances, but we believe it is to do with uncertainty regarding the group-level effects

var_total


var_residual <- var_total - var_rand_intercept

var_residual %>% 
  data.frame(var = .) %>% 
  ggplot(aes(x = var)) +
  geom_histogram()


ci_icc <- rev(1 - stats::quantile(var_rand_intercept/var_total, 
                                  probs = c((1 - ci)/2, (1 + ci)/2)))
result <- structure(class = "icc_decomposed", list(ICC_decomposed = 1 - 
                                                     fun(var_icc), ICC_CI = ci_icc))

result
attr(result, "var_rand_intercept") <- fun(var_rand_intercept)
attr(result, "var_residual") <- fun(var_residual)
attr(result, "var_total") <- fun(var_total)
attr(result, "ci.var_rand_intercept") <- bayestestR::ci(var_rand_intercept, 
                                                        ci = ci)
attr(result, "ci.var_residual") <- bayestestR::ci(var_residual, 
                                                  ci = ci)
attr(result, "ci.var_total") <- bayestestR::ci(var_total, 
                                               ci = ci)
attr(result, "ci") <- ci
attr(result, "re.form") <- re_formula
attr(result, "ranef") <- model$ranef$group[1]
attr(attr(result, "ci.var_rand_intercept"), "data") <- NULL
attr(attr(result, "ci.var_residual"), "data") <- NULL
attr(attr(result, "ci.var_total"), "data") <- NULL
result


var_icc





my_icc_plot(meanlat, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank), plot_each_var = T)



# simulating data to see what generates wide variance distributions -------

r2_bayes(known, robust = T) # 0.273
r2_bayes(meanlat, robust = T) # 0.354

r2_bayes

performance:::.r2_posterior

# doesn't look like it's linked to R2, so I don't think it has to do with residual variance...
rstantools::bayes_R2(known)
rstantools::bayes_R2(meanlat)

# ok weirdly it looks like the sd of group_ID on intercept is lower and more certain for meanlat.........
known
# sd(Intercept)        0.77      0.08     0.62     0.93

meanlat
# sd(Intercept)     0.54      0.10     0.34     0.72

known$fit
meanlat$fit

# WHY do the variance estimates for





??bayes_R2.brmsfit

my_icc_plot(meanlat, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank), plot_each_var = T)
my_icc_plot(known, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank), plot_each_var = T)

my_icc(known, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank))
my_icc_tibble(known, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank))

variance_decomposition(known, re_formula = ~(1|group_ID))
# 0.53 with -0.3 to 0.91 CI

my_icc_tibble(known, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank))
# 0.517 with -0.2 to 0.96 CI


variance_decomposition(meanlat, re_formula = ~(1|group_ID))
# 0.55 with -0.26 to 0.85 ratio

my_icc_tibble(meanlat, total_re.form = ~ (1 | group_ID) + (1 | tank), lesser_re.form = ~ (1 | tank))
# 0.534 with -0.11 to 0.90 ratio




my_icc_plot
my_icc
my_icc_tibble


