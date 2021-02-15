library(tidyverse)
library(brms)
library(tidybayes)
library(emmeans)

# ok, we are going to fit every main model again, but correlating random effects across hurdle and negbinom


# lat pred ----------------------------------------------------------------

pred <- read_rds("latency/data/cleaned/latency_pred_cue_final.rds")

# fit hurdle-inflated negbinom
pred_fit <- brm(data = pred, family = hurdle_negbinomial,
             bf(latency ~ 1 + treatment + trial +
                  (1 | dummy | group_ID) +
                  (1 | tank),
                hu ~ 1 + treatment + trial +
                  (1 | dummy | group_ID) +
                  (1 | tank)),
             prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                       set_prior("normal(0, 5)", class = "b"),
                       set_prior("cauchy(0, 3)", class = "sd")),
             iter = 5000, warmup = 1000, chains = 3, cores = 3,
             control = list(adapt_delta = 0.98, max_treedepth = 15))

# lower value for hu_Intercept = more likely to move, higher value for hu_Intercept = less likely to move
# so a positive value for cor(Intercept, hu_Intercept) means:
# low hu_Intercept = low Intercept, more likely to move = low latency value

conditional_effects(pred_fit)

# lat known ---------------------------------------------------------------

known <- read_rds("latency/data/cleaned/latency_typical_food.rds")

known_fit <- brm(data = known, family = hurdle_negbinomial,
                 bf(latency ~ 1 + treatment + trial +
                      (1 | dummy | group_ID) +
                      (1 | tank),
                    hu ~ 1 + treatment + trial +
                      (1 | dummy | group_ID) +
                      (1 | tank)),
                 prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                           set_prior("normal(0, 5)", class = "b"),
                           set_prior("cauchy(0, 3)", class = "sd")),
                 iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(),
                 control = list(adapt_delta = 0.98, max_treedepth = 15))

conditional_effects(known_fit)


# lat novel ---------------------------------------------------------------

novel <- read_rds("latency/data/cleaned/latency_novel_food.rds")

nov_fit <- brm(data = novel, family = hurdle_negbinomial,
                    bf(latency ~ 1 + treatment + novel_food +
                         (1 | dummy | group_ID) +
                         (1 | tank),
                       hu ~ 1 + treatment + novel_food +
                         (1 | dummy | group_ID) +
                         (1 | tank)),
                    prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                              set_prior("normal(0, 5)", class = "b"),
                              set_prior("cauchy(0, 3)", class = "sd")),
                    iter = 5000, warmup = 2000, chains = 3, cores = 3, 
                    control = list(adapt_delta = 0.99, max_treedepth = 15))


# TODO: double check on this add_fitted_draws stuff, compare to emmeans
mehu_f <- modelr::data_grid(novel, novel_food, treatment) %>% 
  add_fitted_draws(nov_fit, re_formula = ~0, dpar = TRUE)

mehu <- mehu_f %>% 
  group_by(.draw, novel_food) %>% 
  summarise(mu = mean(mu, na.rm = T),
            hu = mean(hu, na.rm = T),
            .value = mean(.value, na.rm = T))

mean_mu_diffs <- mehu %>% 
  compare_levels(mu, by = novel_food)

mean_hu_diffs <- mehu %>% 
  compare_levels(hu, by = novel_food)

mean_val_diffs <- mehu %>% 
  compare_levels(.value, by = novel_food)

mean_mu_diffs %>% 
  ggplot(aes(x = mu, y = novel_food)) +
  geom_halfeyeh()

mean_hu_diffs %>% 
  ggplot(aes(x = hu, y = novel_food)) +
  geom_halfeyeh()

mean_val_diffs %>% 
  ggplot(aes(x = .value, y = novel_food)) +
  geom_halfeyeh()

nov_fit %>%
  emmeans(~novel_food) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  mutate(contrast = str_replace_all(contrast, "-", "/")) %>%
  mutate(.value = exp(.value)) %>% 
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh()

source("figure_generating_functions.R")

fitted_contrasts(nov_fit, by = novel_food, plot = T, hurdle_sep = T)



d <- nov_fit %>% 
  emmeans::ref_grid(cov.keep = 4) %>% 
  .@grid %>% 
  tidybayes::add_fitted_draws(nov_fit, re_formula = ~0, dpar = TRUE) %>% 
  group_by(novel_food, .draw) %>% 
  summarise(mu = mean(mu, na.rm = T),
            hu = mean(hu, na.rm = T))

neworder <- d %>% 
  group_by(novel_food) %>% 
  summarise(mu = mean(mu)) %>% 
  arrange(desc(mu)) %>% 
  select(novel_food) %>% 
  unlist() %>% 
  as.character()



d_mu <- d %>% 
  compare_levels(variable = mu, by = novel_food, comparison = combn(neworder, 2, simplify = FALSE))

d_hu <- d %>% 
  compare_levels(variable = hu, by = novel_food, comparison = combn(neworder, 2, simplify = FALSE))

d_hu

d_mu

d %>% 
  ggplot(aes(x = mu, y = novel_food)) +
  geom_halfeyeh()

mehu_f %>% 
  ggplot(aes(x = mu, y = novel_food)) +
  geom_halfeyeh()

d_mu %>% 
  mutate(first_cat = str_extract(novel_food, "([^\\s]+)") %>% 
         factor(levels = neworder) %>% 
         as.numeric()) %>%
  ggplot(aes(x = mu, y = reorder(novel_food, desc(first_cat)))) +
  geom_halfeyeh() +
  ylab("Contrast Pair") +
  xlab("Difference in mean post-hurdle value")

d_hu %>% 
  mutate(first_cat = str_extract(novel_food, "([^\\s]+)") %>% 
           factor(levels = neworder) %>% 
           as.numeric()) %>%
  ggplot(aes(x = hu, y = reorder(novel_food, desc(first_cat)))) +
  geom_halfeyeh() +
  ylab("Contrast Pair") +
  xlab("Difference in mean post-hurdle value")


novel_binom <- novel %>% 
  mutate(did_eat = latency > 0)

nov_hurdle_fit <- brm(data = novel_binom, family = bernoulli,
                           bf(did_eat ~ 1 + treatment + novel_food +
                                (1 | group_ID) +
                                (1 | tank)),
                           prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                                     set_prior("normal(0, 5)", class = "b"),
                                     set_prior("cauchy(0, 3)", class = "sd")),
                           iter = 5000, warmup = 2000, chains = 4, cores = 4, 
                           control = list(adapt_delta = 0.99, max_treedepth = 15))

nov_hurdle_fit %>% 
  emmeans(~novel_food) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  mutate(contrast = str_replace_all(contrast, "-", "/")) %>%
  mutate(.value = exp(.value)) %>% 
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh()


# mean group lat all assays -----------------------------------------------

# TODO: check priors on all of these

mean_lat <- readRDS("other_models/data/cleaned/mean_group_lat_all_assays.rds")
mean_lat <- mean_lat %>% 
  mutate(trial_f = factor(trial))

bf_novel <- bf(novel_mean_lat ~ treatment + trial_f + (1 | dummy | group_ID) + (1 | tank)) + Gamma(link = "log")
bf_known <- bf(known_mean_lat ~ treatment + trial + (1 | dummy | group_ID) + (1 | tank)) + Gamma(link = "log")
bf_pred <- bf(pred_mean_lat ~ treatment + trial + (1 | dummy | group_ID) + (1 | tank)) + Gamma(link = "log")


mean_lat_fit <- brm(bf_novel + bf_known + bf_pred + set_rescor(FALSE), data = mean_lat, 
                    prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                              set_prior("normal(0, 5)", class = "b")),
                     iter = 10000, warmup = 2000, chains = 3, cores = 3,
                     control = list(adapt_delta = 0.999, max_treedepth = 15))



# group hurdle all assays -------------------------------------------------

group_hurdle <- read_rds("other_models/data/cleaned/group_hurdle_all_assays.rds")


bf_novel <- bf(nov_moved | trials(treatment) ~ treatment + trial_f + (1 | dummy | group_ID) + (1 | tank)) + binomial()
bf_known <- bf(known_moved | trials(treatment) ~ treatment + trial + (1 | dummy | group_ID) + (1 | tank)) + binomial()
bf_pred <- bf(pred_moved | trials(treatment) ~ treatment + trial + (1 | dummy | group_ID) + (1 | tank)) + binomial()


group_hurdle_fit <- brm(bf_novel + bf_known + bf_pred + set_rescor(FALSE), data = group_hurdle,
                        prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                                  set_prior("normal(0, 5)", class = "b")),
                           iter = 10000, warmup = 2000, chains = 3, cores = 3,
                           control = list(adapt_delta = 0.999, max_treedepth = 15))


# chases ------------------------------------------------------------------

chases <- readRDS("chases/data/cleaned/full_chase_group_size.rds")
chases <- chases %>% 
  filter(measurement == "numchases") %>% 
  mutate(percap_chases = value / treatment)

# fit hurdle_gamma
chases_fit <- brm(data = chases, family = hurdle_gamma, 
             bf(percap_chases ~ 1 + treatment + assay*trial +
                  (1 | dummy | group_ID) +
                  (1| tank),
                hu ~ 1 + treatment + assay*trial +
                  (1 | dummy | group_ID) +
                  (1| tank)),
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b"),
                       set_prior("cauchy(0, 2)", class = "sd")),
             iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
             control = list(adapt_delta = 0.999, max_treedepth = 15))



# save all models to an RDA file ------------------------------------------


save(pred_fit, known_fit, nov_fit, nov_hurdle_fit,
     mean_lat_fit, group_hurdle_fit, chases_fit, 
     file = "all_manuscript_models_fresh_fit.rda")


