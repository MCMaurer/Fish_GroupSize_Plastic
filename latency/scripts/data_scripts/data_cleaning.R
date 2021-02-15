library(tidyverse)
# read data
d <- read_csv("latency/data/raw/latency_typical_food_group_size.csv")
d

d <- d[,1:15]

d

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 8:15)
d

# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "novel_bite", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))

# make some factors
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

# set all instances where fish did not move during trial to 0, this will be the hurdle
d$latency[d$latency == 300] <- 0

saveRDS(d, "latency/data/cleaned/latency_typical_food.rds")

#### novel food data now ####
d <- read_csv("latency/data/raw/full_latency_novel_food_group_size.csv")
d <- d[,1:17]

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 9:17)

# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "novel_bite", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(Group, 1, 1))) %>% 
  rename(tank = Tank, group_ID = Group, novel_food = `Novel Food`, trial = Trial) %>% 
  mutate(novel_food = factor(novel_food))

# make some factors
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

# set all instances where fish did not move during trial to 0, this will be the hurdle
d$latency[d$latency == 300] <- 0

d

d <- d %>% 
  mutate(novel_food = fct_relevel(novel_food, "Brine", "Bead", "Plastic"))

saveRDS(d, "latency/data/cleaned/latency_novel_food.rds")


#### predator cue latency data ####

d <- read_csv("latency/data/raw/GroupSizePredAssay_HalfDataSheet.csv")
d

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 6:13)
d


# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "move", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(group_ID, 1, 1)))

# make some factors
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

# set all instances where fish did not move during trial to 0, this will be the hurdle
d$latency[d$latency == 300] <- 0

saveRDS(d, "latency/data/cleaned/latency_pred_cue.rds")
d


# predator latency FULL data ----------------------------------------------

d <- read_csv("latency/data/raw/GroupSizePredAssay_FinalDataSheet .csv")
d

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 6:13)
d


# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "move", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(group_ID, 1, 1)))

# make some factors
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

# set all instances where fish did not move during trial to 0, this will be the hurdle
d$latency[d$latency == 300] <- 0

saveRDS(d, "latency/data/cleaned/latency_pred_cue_final.rds")
d
