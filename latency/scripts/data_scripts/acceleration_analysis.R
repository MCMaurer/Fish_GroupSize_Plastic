# calculating the "acceleration" of individuals in groups of 8
library(tidyverse)

data <- readRDS("latency/data/cleaned/latency_typical_food.rds")


# groups of 8 -------------------------------------------------------------

accel <- data %>% 
  filter(treatment == 8, latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(lat_diff = latency - lag(latency, 1), order = row_number())

accel %>% print(n=Inf)

accel %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point(aes(group = group_ID, color = as.factor(trial))) +
  geom_smooth()

# making random groups

# create array to hold data
trial_samples <- array(dim = c(8,1000,3))

# for each trial, create some data
for(j in 1:3){
  trial_data <- data %>%
    filter(treatment == 8, trial == j, !is.na(latency))

# then sample 1000 groups of 8, without replacement
for (i in 1:1000) {
  res <- sample(trial_data$latency, 8)
  trial_samples[,i,j] <- res
}
  
}


# make them into data frames and add a trial #
t1s <- as.data.frame(trial_samples[,,1]) %>% mutate(trial = 1)
t2s <- as.data.frame(trial_samples[,,2]) %>% mutate(trial = 2)
t3s <- as.data.frame(trial_samples[,,3]) %>% mutate(trial = 3)

# gather them
t1s <- t1s %>% gather(key = group_ID, value = latency, -trial)
t2s <- t2s %>% gather(key = group_ID, value = latency, -trial)
t3s <- t3s %>% gather(key = group_ID, value = latency, -trial)

# put all the trials together
trial_samples <- rbind(t1s, t2s, t3s)

# now calculate the latency differences
trial_samples <- trial_samples %>% 
  filter(latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(fastest = min(latency), keystone = latency - fastest) %>% 
  mutate(lat_diff = latency - lag(latency, 1), order = row_number())
  

# plot them both and give them smoothed curves
accel %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = trial_samples, color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency") +
  MCMsBasics::minimal_ggplot_theme()

ggsave("latency/images/acceleration_analysis_8.jpeg", width = 7, height = 7, dpi = 300)




accel %>% 
  filter(trial == 1) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==1), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 1")


accel %>% 
  filter(trial == 2) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==2), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 2")

accel %>% 
  filter(trial == 3) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==3), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 3")



# latency diff from FIRST individual

keystone <- data %>%
  filter(treatment == 8, latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(fastest = min(latency), keystone = latency - fastest, order = individual)
keystone %>% 
  print(n=Inf)


keystone %>% 
  ggplot(aes(x = order, y = keystone)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(data = trial_samples, color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6))



# groups of 4 -------------------------------------------------------------

accel <- data %>% 
  filter(treatment == 4, latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(lat_diff = latency - lag(latency, 1), order = row_number())

accel %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point(aes(group = group_ID, color = as.factor(trial))) +
  geom_smooth()


# making random groups

# create array to hold data
trial_samples <- array(dim = c(4,1000,3))

# for each trial, create some data
for(j in 1:3){
  trial_data <- data %>%
    filter(treatment == 4, trial == j, !is.na(latency))
  
  # then sample 1000 groups of 8, without replacement
  for (i in 1:1000) {
    res <- sample(trial_data$latency, 4)
    trial_samples[,i,j] <- res
  }
  
}


# make them into data frames and add a trial #
t1s <- as.data.frame(trial_samples[,,1]) %>% mutate(trial = 1)
t2s <- as.data.frame(trial_samples[,,2]) %>% mutate(trial = 2)
t3s <- as.data.frame(trial_samples[,,3]) %>% mutate(trial = 3)

# gather them
t1s <- t1s %>% gather(key = group_ID, value = latency, -trial)
t2s <- t2s %>% gather(key = group_ID, value = latency, -trial)
t3s <- t3s %>% gather(key = group_ID, value = latency, -trial)

# put all the trials together
trial_samples <- rbind(t1s, t2s, t3s)

# now calculate the latency differences
trial_samples <- trial_samples %>% 
  filter(latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(fastest = min(latency), keystone = latency - fastest) %>% 
  mutate(lat_diff = latency - lag(latency, 1), order = row_number())


# plot them both and give them smoothed curves
accel %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 3)) +
  geom_smooth(data = trial_samples, color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 3)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency") +
  MCMsBasics::minimal_ggplot_theme()

ggsave("latency/images/acceleration_analysis_4.jpeg", width = 7, height = 7, dpi = 300)


accel %>% 
  filter(trial == 1) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==1), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 1")


accel %>% 
  filter(trial == 2) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==2), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 2")

accel %>% 
  filter(trial == 3) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==3), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 3")



# latency diff from FIRST individual

keystone <- data %>%
  filter(treatment == 4, latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(fastest = min(latency), keystone = latency - fastest, order = individual)
keystone %>% 
  print(n=Inf)


keystone %>% 
  ggplot(aes(x = order, y = keystone)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(data = trial_samples, color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6))


# groups of 2 -------------------------------------------------------------

accel <- data %>% 
  filter(treatment == 2, latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(lat_diff = latency - lag(latency, 1), order = row_number())

accel %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point(aes(group = group_ID, color = as.factor(trial))) +
  geom_smooth()


# making random groups

# create array to hold data
trial_samples <- array(dim = c(2,1000,3))

# for each trial, create some data
for(j in 1:3){
  trial_data <- data %>%
    filter(treatment == 2, trial == j, !is.na(latency))
  
  # then sample 1000 groups of 8, without replacement
  for (i in 1:1000) {
    res <- sample(trial_data$latency, 2)
    trial_samples[,i,j] <- res
  }
  
}


# make them into data frames and add a trial #
t1s <- as.data.frame(trial_samples[,,1]) %>% mutate(trial = 1)
t2s <- as.data.frame(trial_samples[,,2]) %>% mutate(trial = 2)
t3s <- as.data.frame(trial_samples[,,3]) %>% mutate(trial = 3)

# gather them
t1s <- t1s %>% gather(key = group_ID, value = latency, -trial)
t2s <- t2s %>% gather(key = group_ID, value = latency, -trial)
t3s <- t3s %>% gather(key = group_ID, value = latency, -trial)

# put all the trials together
trial_samples <- rbind(t1s, t2s, t3s)

# now calculate the latency differences
trial_samples <- trial_samples %>% 
  filter(latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(fastest = min(latency), keystone = latency - fastest) %>% 
  mutate(lat_diff = latency - lag(latency, 1), order = row_number())


# plot them both and give them smoothed curves
accel %>% 
  ggplot(aes(x = lat_diff)) +
  geom_density(fill = "blue", alpha = 0.5, color = "transparent") +
  geom_density(data = trial_samples, aes(x = lat_diff), fill = "red", alpha = 0.5, color = "transparent")

ggsave("latency/images/acceleration_analysis_2.jpeg", width = 7, height = 7, dpi = 300)




accel %>% 
  filter(trial == 1) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==1), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 1")


accel %>% 
  filter(trial == 2) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==2), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 2")

accel %>% 
  filter(trial == 3) %>% 
  ggplot(aes(x = order, y = lat_diff)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  geom_smooth(data = filter(trial_samples, trial==3), color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6)) +
  ggtitle("Difference in Latency Between Each Individual,\n Ordered By Latency, For Trial 3")



# latency diff from FIRST individual

keystone <- data %>%
  filter(treatment == 2, latency != 0) %>% 
  group_by(group_ID, trial) %>% 
  arrange(trial, group_ID, latency) %>% 
  mutate(fastest = min(latency), keystone = latency - fastest, order = individual)
keystone %>% 
  print(n=Inf)


keystone %>% 
  ggplot(aes(x = order, y = keystone)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(data = trial_samples, color = "red", method = 'gam', formula = y ~ s(x, bs = "cs", k = 6))


