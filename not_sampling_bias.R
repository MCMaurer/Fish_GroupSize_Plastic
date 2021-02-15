library(tidyverse)

#  so, if you just take all the individuals from all groups of 8 and compare them to all the individuals of all the groups of 2, what do you see? as long as there are roughly the same number of individuals across treatments of 2, 4, or 8, then if it was a purely sampling effect, we would actually expect to see roughly the same distribution of speed. because now we've essentially created 3 giant groups. but if the distributions look different, then it probably isn't just sampling

# conjecture: IF group size has no effect, then any decrease in latency within larger groups is due to a higher likelihood of having randomly sampled faster individuals. if this is the case, then if we ignore group_id and simply look at 3 groups: treatment 2, treatment 4, and treatment 8, and collect the same number of individuals from each group, then we would expect the latency distributions to be similar. if group size treatment has no effect, then the sampling bias should be the same within each treatment group.

# so what we do is sample the same number of individuals from each treatment, and then plot their latency distributions

# this is DIFFERENT from the hypothesis that larger groups are more likely to randomly have a faster individual, which then leads to them acting as a keystone individual and inducing the OTHER fish in the group to move faster. THIS would actually show up as an effect of group size.

latency_typical_food <- readRDS("latency/data/cleaned/latency_typical_food.rds")
latency_novel_food <- readRDS("latency/data/cleaned/latency_novel_food.rds")
latency_pred_cue <- readRDS("latency/data/cleaned/latency_pred_cue_final.rds")


# random sampling within treatments KNOWN FOOD ---------------------------------------


resample_lat <- name <- function(rep, data, samples = 100) {
  data %>% 
    group_by(treatment) %>% 
    slice_sample(n = samples) %>% 
    mutate(replicate = rep)
}

d <- map_dfr(.x = 1:100, .f = resample_lat, data = latency_typical_food %>% filter(!is.na(latency), latency > 0))

# I think this shows it pretty definitively. 
d %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = replicate)) +
  geom_line(stat = "density", alpha = 0.05) +
  facet_grid(rows = vars(treatment), labeller = labeller(treatment = function(x) paste("100 randomly sampled\nindividuals from\ngroups of ", x))) +
  scale_color_viridis_d(option = "C") +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = "none") +
  xlab("Latency to move")

ggsave("latency/images/typical_food_random_sample_bytreatment.jpg", width = 7, height = 5)

d %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = interaction(treatment, replicate))) +
  geom_line(stat = "density", alpha = 0.1) +
  scale_color_viridis_d("individuals\nrandomly\nsampled from\n groups of:", option = "C") +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = c(0.2, 0.5)) +
  xlab("Latency to move")

ggsave("latency/images/typical_food_random_sample_bytreatment.jpg", width = 7, height = 5)

d %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = interaction(treatment, replicate))) +
  geom_line(stat = "density", alpha = 0.1) +
  scale_color_viridis_d("individuals\nrandomly\nsampled from\n groups of:", option = "C") +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  MCMsBasics::minimal_ggplot_theme() +
  scale_x_log10() +
  theme(legend.position = c(0.8, 0.8)) +
  xlab("Latency to move")

ggsave("latency/images/typical_food_random_sample_bytreatment_logx.jpg", width = 7, height = 5)

# I think the above graph is the strongest evidence for the effect of group size and demonstrating that the effect is not purely driven by random sampling. However, there is a different kind of sampling effect that may be driving the observed pattern. It is true that larger groups will have a higher probability of containing a fast individual, purely due to random sampling. If that individual then acts as a keystone individual, it may increase the speed of the rest of the group members. This is a mechanistic explanation for a true group size effect, not evidence of a group size effect being driven completely by random sampling.

# Fig X shows the distribution of latency values for random sets of 100 individuals sampled from each group size treatment. If group size had no effect on latency, then we would expect the overall distributions of latency values within each treatment, irrespective of group ID, to be similar. Therefore, if we were to randomly sample 100 individuals from each treatment, we would be equally likely to sample fast individuals, which may have been distributed unevenly through specific groups in the experiment. In Fig X, we repeat this process of randomly selecting 100 individuals per treatment 100 times, leading to 100 density curves per treatment. The curves differ significantly across treatments, with groups of 8 having 


# here we draw 300 random samples from the data, irrespective of treatment, and then randomly assign treatments. this is essentially saying "let's assume treatment doesn't make a difference and just grab 300 latency values and randomly group them into treatments"
resample_lat_no_treatment <- name <- function(rep, data, samples = 300) {
  data %>% 
    select(-treatment) %>% 
    slice_sample(n = samples) %>% 
    mutate(replicate = rep) %>% 
    mutate(treatment = sample(c(2,4,8), size = samples, replace = T))
}

# we repeat this 100 times, as above
dng <- map_dfr(.x = 1:100, .f = resample_lat_no_treatment, data = latency_typical_food %>% filter(!is.na(latency), latency > 0))

# as you can see, this plot shows the treatments overlapping completely, as we'd expect if groups did not differ
dng %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = interaction(treatment, replicate))) +
  geom_line(stat = "density", alpha = 0.1) +
  scale_color_viridis_d("individuals\nrandomly\nsampled from\n groups of:", option = "C") +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = c(0.2, 0.5)) +
  xlab("Latency to move")

ggsave("latency/images/typical_food_random_sample_remove_treatment.jpg", width = 7, height = 5)

# this is how many individuals didn't move at all during the trials
latency_typical_food %>% 
  filter(!is.na(latency)) %>% 
  group_by(treatment) %>%
  summarise(n_tot = n(),
            n_0 = sum(latency == 0, na.rm = T),
            prop_0 = n_0/n_tot)

latency_typical_food %>% 
  filter(!is.na(latency)) %>% 
  filter(latency = 0) %>% 
  group_by(treatment) %>% 
  tally()




# random sampling within treatments NOVEL FOOD ---------------------------------------


d <- map_dfr(.x = 1:100, .f = resample_lat, data = latency_novel_food %>% filter(!is.na(latency), latency > 0))

# I think this shows it pretty definitively. 
d %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = replicate)) +
  geom_line(stat = "density", alpha = 0.05) +
  facet_wrap(vars(treatment), labeller = labeller(treatment = function(x) paste("100 randomly sampled\nindividuals from\ngroups of ", x))) +
  scale_color_viridis_d(option = "C") +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = "none") +
  xlab("Latency to move")

ggsave("latency/images/novel_food_random_sample_bytreatment.jpg", width = 7, height = 5)

d %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = interaction(treatment, replicate))) +
  geom_line(stat = "density", alpha = 0.1) +
  scale_color_viridis_d("individuals\nrandomly\nsampled from\n groups of:", option = "C") +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = c(0.2, 0.5)) +
  xlab("Latency to move")

ggsave("latency/images/novel_food_random_sample_bytreatment.jpg", width = 7, height = 5)

# I think the above graph is the strongest evidence for the effect of group size and demonstrating that the effect is not purely driven by random sampling. However, there is a different kind of sampling effect that may be driving the observed pattern. It is true that larger groups will have a higher probability of containing a fast individual, purely due to random sampling. If that individual then acts as a keystone individual, it may increase the speed of the rest of the group members. This is a mechanistic explanation for a true group size effect, not evidence of a group size effect being driven completely by random sampling.

# Fig X shows the distribution of latency values for random sets of 100 individuals sampled from each group size treatment. If group size had no effect on latency, then we would expect the overall distributions of latency values within each treatment, irrespective of group ID, to be similar. Therefore, if we were to randomly sample 100 individuals from each treatment, we would be equally likely to sample fast individuals, which may have been distributed unevenly through specific groups in the experiment. In Fig X, we repeat this process of randomly selecting 100 individuals per treatment 100 times, leading to 100 density curves per treatment. The curves differ significantly across treatments, with groups of 8 having 


# here we draw 300 random samples from the data, irrespective of treatment, and then randomly assign treatments. this is essentially saying "let's assume treatment doesn't make a difference and just grab 300 latency values and randomly group them into treatments"
resample_lat_no_treatment <- name <- function(rep, data, samples = 300) {
  data %>% 
    select(-treatment) %>% 
    slice_sample(n = samples) %>% 
    mutate(replicate = rep) %>% 
    mutate(treatment = sample(c(2,4,8), size = samples, replace = T))
}

# we repeat this 100 times, as above
dng <- map_dfr(.x = 1:100, .f = resample_lat_no_treatment, data = latency_novel_food %>% filter(!is.na(latency), latency > 0))

# as you can see, this plot shows the treatments overlapping completely, as we'd expect if groups did not differ
dng %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = interaction(treatment, replicate))) +
  geom_line(stat = "density", alpha = 0.1) +
  scale_color_viridis_d("individuals\nrandomly\nsampled from\n groups of:", option = "C") +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = c(0.2, 0.5)) +
  xlab("Latency to move")

ggsave("latency/images/novel_food_random_sample_remove_treatment.jpg", width = 7, height = 5)


# this is how many individuals didn't move at all during the trials
latency_novel_food %>% 
  filter(!is.na(latency)) %>% 
  group_by(treatment) %>%
  summarise(n_tot = n(),
            n_0 = sum(latency == 0, na.rm = T),
            prop_0 = n_0/n_tot)


# random sampling within treatments PRED CUE ---------------------------------------


d <- map_dfr(.x = 1:100, .f = resample_lat, data = latency_pred_cue %>% filter(!is.na(latency), latency > 0))

# I think this shows it pretty definitively. 
d %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = replicate)) +
  geom_line(stat = "density", alpha = 0.05) +
  facet_grid(rows = vars(treatment), labeller = labeller(treatment = function(x) paste("100 randomly sampled\nindividuals from\ngroups of ", x))) +
  scale_color_viridis_d(option = "C") +
  MCMsBasics::minimal_ggplot_theme(gridlines = T) +
  theme(legend.position = "none", strip.text.y = element_text(angle = 0)) +
  xlab("Latency to move")

ggsave("latency/images/pred_cue_random_sample_bytreatment.jpg", width = 7, height = 5)

d %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = interaction(treatment, replicate))) +
  geom_line(stat = "density", alpha = 0.1) +
  scale_color_viridis_d("individuals\nrandomly\nsampled from\n groups of:", option = "C") +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = c(0.2, 0.5)) +
  xlab("Latency to move")

ggsave("latency/images/pred_cue_random_sample_bytreatment.jpg", width = 7, height = 5)

# I think the above graph is the strongest evidence for the effect of group size and demonstrating that the effect is not purely driven by random sampling. However, there is a different kind of sampling effect that may be driving the observed pattern. It is true that larger groups will have a higher probability of containing a fast individual, purely due to random sampling. If that individual then acts as a keystone individual, it may increase the speed of the rest of the group members. This is a mechanistic explanation for a true group size effect, not evidence of a group size effect being driven completely by random sampling.

# Fig X shows the distribution of latency values for random sets of 100 individuals sampled from each group size treatment. If group size had no effect on latency, then we would expect the overall distributions of latency values within each treatment, irrespective of group ID, to be similar. Therefore, if we were to randomly sample 100 individuals from each treatment, we would be equally likely to sample fast individuals, which may have been distributed unevenly through specific groups in the experiment. In Fig X, we repeat this process of randomly selecting 100 individuals per treatment 100 times, leading to 100 density curves per treatment. The curves differ significantly across treatments, with groups of 8 having 


# here we draw 300 random samples from the data, irrespective of treatment, and then randomly assign treatments. this is essentially saying "let's assume treatment doesn't make a difference and just grab 300 latency values and randomly group them into treatments"
resample_lat_no_treatment <- name <- function(rep, data, samples = 300) {
  data %>% 
    select(-treatment) %>% 
    slice_sample(n = samples) %>% 
    mutate(replicate = rep) %>% 
    mutate(treatment = sample(c(2,4,8), size = samples, replace = T))
}

# we repeat this 100 times, as above
dng <- map_dfr(.x = 1:100, .f = resample_lat_no_treatment, data = latency_pred_cue %>% filter(!is.na(latency), latency > 0))

# as you can see, this plot shows the treatments overlapping completely, as we'd expect if groups did not differ
dng %>% 
  ggplot(aes(x = latency, color = factor(treatment), group = interaction(treatment, replicate))) +
  geom_line(stat = "density", alpha = 0.1) +
  scale_color_viridis_d("individuals\nrandomly\nsampled from\n groups of:", option = "C", end = 0.9) +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  MCMsBasics::minimal_ggplot_theme() +
  theme(legend.position = c(0.2, 0.5)) +
  xlab("Latency to move")

ggsave("latency/images/pred_cue_random_sample_remove_treatment.jpg", width = 7, height = 5)


# this is how many individuals didn't move at all during the trials
latency_pred_cue %>% 
  filter(!is.na(latency)) %>% 
  group_by(treatment) %>%
  summarise(n_tot = n(),
            n_0 = sum(latency == 0, na.rm = T),
            prop_0 = n_0/n_tot)






# comparing fastest and slowest individuals per group ---------------------------------

data <- read_rds("latency/data/cleaned/latency_typical_food.rds") %>% 
  select(group_ID, trial, treatment, latency) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3))

data %>% 
  group_by(group_ID, treatment, trial) %>% 
  top_n(n = 1, wt = latency) %>% 
  arrange(treatment, trial, group_ID) %>% 
  ggplot(aes(x = treatment, y = latency)) +
  geom_violin(fill = NA) +
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



# comparing differences between first/second or 2nd last/last ----------------------

first2 <- read_rds("other_models/data/cleaned/known_first2_diff.rds")
last2 <- read_rds("other_models/data/cleaned/known_last2_diff.rds")
first2

first2 %>% 
  ggplot(aes(x = factor(treatment, ordered = T, levels = c("2", "4", "8")), y = diff)) +
  geom_violin() +
  geom_jitter(alpha = 0.1) +
  MCMsBasics::minimal_ggplot_theme() +
  xlab("Group Size") +
  ylab("Difference in Latency Between 1st and 2nd Individual")

last2 %>% 
  ggplot(aes(x = factor(treatment, ordered = T, levels = c("2", "4", "8")), y = diff)) +
  geom_violin() +
  geom_jitter(alpha = 0.1) +
  MCMsBasics::minimal_ggplot_theme() +
  xlab("Group Size") +
  ylab("Difference in Latency Between Slowest and 2nd Slowest Individual")



# plateau plots! ----------------------------------------------------------

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

starts <- data %>%
  distinct(trial, group_ID, treatment) %>% 
  mutate(latency = 0, cumu_n = 0)

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
  scale_x_log10() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = NA, color = "gray80"), panel.spacing = unit(0, "mm"))

bind_rows(data, starts) %>% 
  arrange(group_ID, trial, cumu_n) %>% 
  mutate(treatment = factor(treatment, ordered = T, levels = c(2,4,8)),
         trial = factor(trial, ordered = T, levels = 1:3)) %>% 
  ggplot(aes(x = latency+1, y = cumu_n, color = trial, 
             group = group_ID)) +
  geom_step(alpha = 0.5, direction = "hv") +
  scale_x_log10() +
  facet_grid(trial~treatment, labeller = labeller(
    trial = c(`1` = "Trial 1", `2` = "Trial 2", `3` = "Trial 3"),
    treatment = c(`2` = "Groups of 2", `4` = "Groups of 4", `8` = "Groups of 8")
  )) +
  ylab("# of Fish That Have Eaten") +
  xlab("Trial Time + 1 (s)") +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = NA, color = "gray80"), panel.spacing = unit(0, "mm"))





