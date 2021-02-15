# the goal here is to estimate the effect of group size on latency to approach novel food
# the model will be predicting indivual latency based on "treatment" of group size. The null model would indicate that an individual's latency is not affected by group size, which means all individuals are drawn from the same underlying distribution of latency values

# have to account for multiple measurements per group

library(tidyverse)
library(brms)
#library(ggthemes)
#library(ggExtra)
#library(ggeffects)
library(broom)
library(ggridges)
library(MCMsBasics)
library(sjstats)
library(plotly)

theme_set(MCMsBasics::minimal_ggplot_theme())

# make a custom theme


# read data
d <- read_csv("latency_novel_group_size/data/cleaned_data.csv")

# let's do some violin plots
d %>%
  filter(!is.na(latency)) %>%
  ggplot(aes(x=as.factor(treatment), y=max_minus_lat, group = treatment))+
  geom_violin()+
  geom_jitter(color = "black", stroke = 0, size = 2.1, alpha = 0.1, fill = "transparent")+
  custom_minimal_theme()

hist(d$latency)

# read in the model fit on the FARM
model_1 <- readRDS("latency_novel_group_size/fit_models/model_1_fit.rds")
plot(model_1)

launch_shinystan(model_1)

# only 5 divergent iterations finally!!
model_1$formula

summary(model_1)
model_1$fit
knitr::kable(model_1$fit)
plot(marginal_effects(model_1, effects = "treatment:trial"))
?marginal_effects
conditions <- data.frame(treatment = c(2,4,8), trial = c(1,2,3), cond__ = c("trial 1", "trial 2", "trial 3"))
#make_conditions(conditions, vars = c("treatment", "trial"))

plot(marginal_effects(model_1, effects = "treatment", conditions = conditions, spaghetti = T, nsamples = 300), theme = custom_minimal_theme())

int_conditions <- list(
  trial = setNames(c(1,2,3), c("trial 1", "trial 2", "trial 3")),
  treatment = 2:8)
int_conditions

#theme_2 <- theme(legend.background = element_blank(), legend.key = element_rect(fill = "white", colour = "black"))

effects <- marginal_effects(model_1, effects = "treatment:trial", int_conditions = int_conditions)
effects <- effects$`treatment:trial`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_jitter(data = model_6$data, aes(x = treatment, y = latency, color = factor(trial, labels = c("trial 1", "trial 2", "trial 3"))), alpha = 0.5) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")


ggsave("latency/images/marginal_effects_withpoints.jpg", width = 5, height = 5)


# now spaghetti plots
effects2 <- marginal_effects(model_1, effects = "treatment:trial", int_conditions = int_conditions, spaghetti = T, nsamples = 300)
plot(effects2)
str(effects2)

spag <- attributes(effects2$`treatment:trial`)$spaghetti
str(spag)
spag <- as.tibble(spag)
spag

spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")

ggsave("latency_novel_group_size/images/spaghetti_marginal_effects.jpg", width = 5, height = 5)


ps <- posterior_summary(model_1)
str(ps)
ps <- unlist(ps)
ps <- as.data.frame(ps)
ps
ps2 <- ps %>%
  head(n=15) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  custom_minimal_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")

ggsave("latency_novel_group_size/images/parameter_estimates.jpg", width = 5, height = 5)



preds <- posterior_samples(model_1, pars = ps2$variable)
str(preds)

preds <- as.tibble(preds)
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "white") +
  geom_vline(xintercept = 0, color = "white", linetype = 2)

theme_joy_division <-
  theme(text       = element_text(color = "white", family = "Roboto Condensed"),
        strip.text = element_text(color = "white", family = "Roboto Condensed"),
        axis.text  = element_text(color = "white", family = "Roboto Condensed"),
        axis.ticks = element_blank(),
        line       = element_line(color = "white"),
        plot.background   = element_rect(fill = "black", color = "black"),
        panel.background  = element_rect(fill = "black", color = "black"),
        strip.background  = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "black", color = "black"),
        legend.box.background = element_rect(fill = "black", color = "black"),
        axis.line = element_blank())

plot_dens + theme_joy_division

ggsave("latency_novel_group_size/images/parameter_estimates_joy_division.jpg", width = 5, height = 5)




preds <- posterior_samples(model_1)
preds <- preds[,1:75]


preds <- as.tibble(preds)
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "white")

plot_dens + theme_joy_division + theme(axis.text.y = element_blank(), axis.text.x = element_blank()) + xlab("") + ylab("")

ggsave("latency_novel_group_size/images/parameter_estimates_abstract_joy_division.jpg", width = 5, height = 5)


dfp <- ggpredict(model_1, terms = c("treatment", "trial"), type = "re") %>% 
  mutate(trial = group) %>% 
  mutate(group = NULL)


dfp %>% 
  ggplot(aes(x=x, y=predicted, color=trial)) +
    geom_line() +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = trial), color = "transparent", alpha = 0.1) +
    scale_fill_viridis_d() +
    scale_color_viridis_d()





# ran another model, but with a zero-inflated Poisson
model_2 <- readRDS("latency_novel_group_size/fit_models/model_2_fit.rds")
launch_shinystan(model_2)

waic(model_1, model_2)
loo(model_1, model_2)

# apparently LOO is problematic for these models, so it recommended this:
kfold(model_1, model_2, K = 10)



# now a model with interaction terms
model_3 <- readRDS("latency_novel_group_size/fit_models/model_3_fit.rds")
launch_shinystan(model_3)





model_4 <- readRDS("latency_novel_group_size/fit_models/model_4_fit.rds")

model_4$fit

launch_shinystan(model_4)


model_5 <- readRDS("latency_novel_group_size/fit_models/model_5_fit.rds")

model_5$family

launch_shinystan(model_5)

ps <- posterior_summary(model_5)
str(ps)
ps <- unlist(ps)
ps <- as.data.frame(ps)
ps
ps2 <- ps %>%
  head(n=15) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  custom_minimal_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")

preds <- posterior_samples(model_5, pars = ps2$variable)
str(preds)

preds <- as.tibble(preds)
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2)

plot_dens + custom_minimal_theme()


dfp <- ggpredict(model_5, terms = c("treatment", "trial"), type = "re") %>% 
  mutate(trial = group) %>% 
  mutate(group = NULL)


dfp %>% 
  ggplot(aes(x=x, y=predicted, color=trial)) +
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d


int_conditions <- list(
  trial = setNames(c(1,2,3), c("trial 1", "trial 2", "trial 3")),
  treatment = 2:8)
int_conditions


effects <- marginal_effects(model_5, effects = "treatment:trial", int_conditions = int_conditions)
effects <- effects$`treatment:trial`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")
# spaghetti plots

effects2 <- marginal_effects(model_5, effects = "treatment:trial", int_conditions = int_conditions, spaghetti = T, nsamples = 300)


spag <- attributes(effects2$`treatment:trial`)$spaghetti

spag <- as.tibble(spag)


spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")



#### BEST MODEL ####
# model 6 is a hurdle-negative binomial model
# data were transformed such that no observation of movement is recorded as a 0, with the fastest actual recorded movement is a latency of 1
# then, a hurdle model is used to account for the 0s, which indicate no observation
# once that "hurdle" is cleared, positive values are modeled as a negative binomial
model_6 <- readRDS("old_stuff/fit_models/model_6_fit.rds")

model_6$formula

marginal_effects(model_6)

icc_result <- icc(x = model_6, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 + treatment + trial | group_ID))
icc_result

?posterior_predict

equi_test(model_6, out = "plot")
# plot a barplot of frequency of zeroes (fish that never move) across each treatment, see if there's really any effect there

model_6$data



#### ICC Stuff with intercept-only model ####

typ_int <- readRDS("latency/fit_models/lat_typ_int_hurd_nbin_hu_fit.rds")
typ_hu <- readRDS("latency/fit_models/lat_typ_hurd_nbin_hu_fit.rds")

marginal_effects(typ_int)

icc_int_result <- icc(x = typ_int, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 | group_ID))
icc_int_result

icc_typ_hu_result <- icc(x = typ_hu, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 + treatment + trial | group_ID))

icc_typ_hu_result

?posterior_predict

equi_test(typ_int, out = "plot")

waic(model_6, typ_int)

add_loo(model_6)

loo(model_6, typ_int)

icc_int_result

icc(typ_int, ppd = T, typical = "median", re.form = ~(1|group_ID))






source("figure_generating_functions.R")

p <- marginal_effects_plot(model = model_6, effects = c("trial"), effect_types = c("categorical"))
p

p <- param_estimate_plot(model_6, 4)
p + xlab("estimated value") + ylab("parameter") + minimal_ggplot_theme()
ggsave("latency_novel_group_size/images/hurd_nbin_parameter_plot.jpg")

# figure out how to pull out the hurdle part of the model, maybe use marginal effects with a different resp=

# worst comes to worst, open an issue for brms

  
marginal_effects(model_6)


model_6$family
summary(model_6)

launch_shinystan(model_6)

waic(model_5, model_6)


ps <- posterior_summary(model_6)
str(ps)
ps <- unlist(ps)
ps <- as.data.frame(ps)
ps
ps2 <- ps %>%
  head(n=15) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  custom_minimal_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")
ggsave("latency_novel_group_size/images/hurd_nbin_parameter_plot.jpg", width = 5, height = 5)

preds <- posterior_samples(model_6, pars = ps2$variable)
str(preds)

preds <- as.tibble(preds)
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2)

plot_dens + custom_minimal_theme() + ggtitle("Parameter Density Estimates")
ggsave("latency_novel_group_size/images/hurd_nbin_parameter_dens_plot.jpg", width = 5, height = 5)

dfp <- ggpredict(model_6, terms = c("treatment", "trial"), type = "re") %>% 
  mutate(trial = group) %>% 
  mutate(group = NULL)


dfp %>% 
  ggplot(aes(x=x, y=predicted, color=trial)) +
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d


# int_conditions <- list(
#   trial = setNames(c(1,2,3), c("trial 1", "trial 2", "trial 3")),
#   treatment = 2:8)

int_conditions <- list(
  trial = unique(model_6$data[["trial"]]),
  treatment = 2:8)
int_conditions


effects <- marginal_effects(model_6, effects = "treatment:trial", int_conditions = int_conditions)
effects <- effects$`treatment:trial`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of hurdle-nbinom multilevel model") +
  ylab("estimated latency")
ggsave("latency_novel_group_size/images/hurd_nbin_marginal.jpg", width = 5, height = 5)
# spaghetti plots

effects2 <- marginal_effects(model_6, effects = "treatment:trial", int_conditions = int_conditions, spaghetti = T, nsamples = 300)


spag <- attributes(effects2$`treatment:trial`)$spaghetti

spag <- as.tibble(spag)


spag_plot <- spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of hurdle-nbin multilevel model") +
  ylab("estimated latency")
spag_plot
ggsave("latency_novel_group_size/images/hurd_nbin_spaghetti.jpg", width = 5, height = 5)

plotly::ggplotly(spag_plot)



#### novel food latency stuff now ####
novel_fit <- readRDS("latency_novel_group_size/fit_models/full_data_fit_latency_group_size_novel_hur_nbin_farm.rds")


# p <- param_estimate_plot(model_6, 4)
# p + xlab("estimated value") + ylab("parameter") + minimal_ggplot_theme()
# ggsave("latency_novel_group_size/images/latency_novel_parameters.jpg")

novel_fit$data

launch_shinystan(novel_fit)

plot(marginal_effects(novel_fit, effects = "novel_food"))

int_conditions <- list(
  novel_food = setNames(c("bead","brine","plastic"), c("bead","brine","plastic")),
  treatment = 2:8)

effects <- marginal_effects(novel_fit, effects = "treatment:novel_food", int_conditions = int_conditions, spaghetti = T, nsamples = 300)
spag <- attributes(effects$`treatment:novel_food`)$spaghetti %>% as.tibble()

spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=novel_food)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of hurdle-nbin multilevel model") +
  ylab("estimated latency")

ps <- posterior_summary(novel_fit) %>% unlist() %>% as.data.frame()

ps2 <- ps %>%
  head(n=4) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  minimal_ggplot_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")

ggsave("latency_novel_group_size/images/latency_novel_full_data_dot_parameters.jpg")

preds <- posterior_samples(novel_fit, pars = ps2$variable) %>% as.tibble()
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2)

plot_dens + minimal_ggplot_theme() + ggtitle("Parameter Density Estimates")
ggsave("latency_novel_group_size/images/latency_novel_full_data_parameters.jpg")

effects <- marginal_effects(novel_fit, effects = "treatment:novel_food", int_conditions = int_conditions)
effects <- effects$`treatment:novel_food`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=novel_food)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = novel_food), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of hurdle nbin multilevel model") +
  ylab("estimated latency") + xlab("group size")

ggsave("latency_novel_group_size/images/latency_novel_full_data_marginal_effects.jpg")


#### predator cue latency ####
# estimates for hurdle are funky bc there are only ~7 individuals that never moved

model <- readRDS("latency/fit_models/lat_pred_hurd_nbin_hu_fit.rds")

launch_shinystan(model)

ps <- posterior_summary(model) %>% unlist() %>% as.data.frame()
ps

ps2 <- ps[1:6,]
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2
ps2 %>% 
  filter(variable != "b_hu_Intercept") %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  minimal_ggplot_theme() +
  ggtitle("parameter estimates for nbinom hurdle model\nwith 95% credible intervals")

# making a table of the parameter values
jpeg("latency/images/param_table.jpeg", height=200, width=600, res = 100)
ps2 %>% 
  select(variable, Estimate, Est.Error, Q2.5, Q97.5) %>% 
  gridExtra::grid.table(theme = gridExtra::ttheme_minimal())
dev.off()


my_scores %>% 
  filter(!is.na(score)) %>% 
  mutate(prop = round(prop, 1)) %>% 
  grid.table(theme = ttheme_minimal())
dev.off()

ggsave("latency/images/latency_pred_full_data_dot_parameters.jpg")

preds <- posterior_samples(model, pars = ps2$variable) %>% as.tibble()
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2)

plot_dens + minimal_ggplot_theme() + ggtitle("Parameter Density Estimates")
ggsave("latency/images/latency_pred_full_data_parameters.jpg")

int_conditions <- list(
  trial = setNames(c(1,2,3), c("trial 1", "trial 2", "trial 3")),
  treatment = 2:8)
int_conditions

effects <- marginal_effects(model, effects = "treatment:trial", int_conditions = int_conditions)
effects <- effects$`treatment:trial`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_jitter(data = model$data, aes(x = treatment, y = latency, color = factor(trial, labels = c("trial 1", "trial 2", "trial 3"))), alpha = 0.5) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of hurdle nbin multilevel model") +
  ylab("estimated latency") + xlab("group size")

model$data

ggsave("latency/images/latency_pred_full_data_marginal_effects.jpg")

?equi_test
model_equi <- equi_test(model)
model_equi_plot <- equi_test(model, out = "plot")
model_equi_plot

marginal_effects(model)

marginal_effects(model, spaghetti = T)

hu_fits <- fitted(model, dpar = "hu") %>% as_tibble()
hist(hu_fits$Estimate)

sort(model$data$latency)

model$formula

?brms::hurdle_negbinomial()

model

test_data <- model$data

test_data$latency[test_data$latency > 0] <- 1

test_data

test_model <- brm(data = test_data, family = bernoulli,
             bf(latency ~ 1 + treatment + trial +
                  (1 + treatment + trial | group_ID) +
                  (1 + treatment + trial | tank)),
             iter = 1000, warmup = 500, chains = 3, cores = future::availableCores(),
             control = list(adapt_delta = 0.95, max_treedepth = 15))

test_model

marginal_effects(test_model)



# initiator-follower by group size ----------------------------------------

data <- read_rds("latency/data/cleaned/latency_typical_food.rds")

data %>% 
  filter(!is.na(latency)) %>% 
  arrange(group_ID, trial, individual)

data %>%
  select(-individual, -tank, -camera, -video) %>% 
  group_by(group_ID, trial) %>% 
  arrange(latency) %>% 
  mutate(order = rank(latency, ties.method = "first")) %>% 
  filter(order %in% 1:2) %>% 
  spread(key = order, value = latency) %>% 
  rename(first = `1`, second = `2`) %>% 
  mutate(diff = second-first) %>% 
  ggplot(aes(x = factor(treatment, ordered = T, levels = c("2", "4", "8")), y = diff)) +
  geom_violin() +
  geom_jitter(alpha = 0.1) +
  MCMsBasics::minimal_ggplot_theme() +
  xlab("Group Size") +
  ylab("Difference in Latency Between 1st and 2nd Individual")
  

# latency plateau plot ---------------------------------------------------

data <- read_rds("latency/data/cleaned/latency_typical_food.rds") %>% 
  select(group_ID, trial, treatment, latency)

data <- data %>%
  filter(!is.na(latency)) %>% 
  group_by(group_ID, trial, treatment) %>% 
  arrange(latency) %>% 
  mutate(order = rank(latency, ties.method = "first")) %>% 
  arrange(desc(treatment), group_ID, desc(order)) %>% 
  ungroup() %>% 
  group_by(group_ID, trial, latency, treatment) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(group_ID, trial, treatment) %>% 
  arrange(latency) %>% 
  mutate(cumu_n = cumsum(n)) %>% 
  arrange(desc(treatment), group_ID, trial, latency) %>% 
  select(-n) %>% 
  ungroup()

# data %>% 
#   filter(treatment == 2) %>% 
#   filter(!is.na(latency)) %>% 
#   select(-individual, -tank, -camera, -video) %>% 
#   split(f = list(data$group_ID, data$trial, data$treatment)) %>% 
#   map(~add_row(., latency = 0)) %>% 
#   bind_rows()

starts <- data %>%
  distinct(trial, group_ID, treatment) %>% 
  mutate(latency = 0, cumu_n = 0)

# data %>% 
#   filter(treatment == 8, trial == 1)
# 
# testplot <- data %>% 
#   #filter(treatment == 8, trial == 1) %>% 
#   ggplot(aes(x = latency, y = cumu_n, group = interaction(group_ID, trial), color = trial)) +
#   geom_step() +
#   facet_wrap(~treatment)
# testplot
# ggplotly(testplot)

ends <- data %>% 
  group_by(treatment, group_ID, trial) %>% 
  top_n(n = 1, wt = cumu_n) %>% 
  ungroup() %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3))

# regular x scale
data2 <- bind_rows(data, starts) %>% 
  arrange(group_ID, trial, cumu_n) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3))
data2
ends

treatment_names <- c(
  `2` = "Groups of 2",
  `4` = "Groups of 4",
  `8` = "Groups of 8"
)
  
ggplot() +
  geom_step(data = data2, alpha = 0.4, direction = "hv", aes(x = latency, y = cumu_n, color = trial, group = interaction(group_ID, trial))) +
  geom_point(data = ends, alpha = 0.4, aes(x = latency, y = cumu_n, color = trial, 
                              group = interaction(group_ID, trial))) +
  facet_grid(trial~treatment, labeller = labeller(
    trial = c(`1` = "Trial 1", `2` = "Trial 2", `3` = "Trial 3"),
    treatment = c(`2` = "Groups of 2", `4` = "Groups of 4", `8` = "Groups of 8")
  )) +
  ylab("# of Fish That Have Eaten") +
  xlab("Trial Time (s)") +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = NA, color = "gray80"), panel.spacing = unit(0, "mm"))

# log x scale
bind_rows(data, starts) %>% 
  arrange(group_ID, trial, cumu_n) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3)) %>% 
  ggplot(aes(x = latency+1, y = cumu_n, color = trial, 
             group = interaction(group_ID, trial))) +
  geom_step(alpha = 0.5, direction = "hv") +
  facet_wrap(~treatment) +
  scale_x_log10()


# quick look at slowest individuals ---------------------------------------


data <- read_rds("latency/data/cleaned/latency_typical_food.rds") %>% 
  select(group_ID, trial, treatment, latency) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3))

data %>% 
  group_by(group_ID, treatment, trial) %>% 
  top_n(n = 1, wt = latency) %>% 
  arrange(treatment, trial, group_ID) %>% 
  ggplot(aes(x = treatment, y = latency)) +
  geom_violin() +
  geom_jitter(aes(color = trial)) +
  ylab("latency of slowest individual per group")

# it looks like the slowest individual across all the treatments doesn't really differ that much! which is interesting, as it suggests a group of 8 isn't really any more likely to have a super slow individual than a group of 2, which suggests that something is *actually* going on here

data %>% 
  group_by(group_ID, treatment, trial) %>% 
  arrange(latency) %>% 
  top_n(n = -1, wt = latency) %>% 
  arrange(treatment, trial, group_ID) %>% 
  ggplot(aes(x = treatment, y = latency)) +
  geom_violin() +
  geom_jitter(aes(color = trial)) +
  ylab("latency of fastest individual per group")


# diff between fastest 2 --------------------------------------------------


