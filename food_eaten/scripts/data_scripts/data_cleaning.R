library(tidyverse)
#### proportion food eaten data ####

d <- read_csv("food_eaten/data/raw/full_proportion_eaten_group_size.csv")

d$food_eaten <- d$`Food Input` - d$`Leftover Food`
d$novel_eaten <- d$`Novel Input` - d$`Leftover Novel`

d$food_eaten <- floor(d$food_eaten)
d$novel_eaten <- floor(d$novel_eaten)

d$food_eaten[d$food_eaten < 0] <- 0
d$novel_eaten[d$novel_eaten < 0] <- 0

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(Group, 1, 1)))

d$group_ID <- as.factor(d$Group)
d$tank <- as.factor(d$tank)

d <- d %>% 
  select(treatment, Trial, Tank, Group, `Novel Food`, food_eaten, novel_eaten, `Proportion Eaten Food`, `Proportion Eaten Novel`, `Food Input`, `Novel Input`) %>% 
  rename(trial = Trial)

d_typical <- d %>% 
  select(-c(`Novel Food`, novel_eaten, `Proportion Eaten Novel`))

d_novel <- d %>% 
  filter(`Novel Food` == "Brine") %>% 
  select(-c(food_eaten, `Proportion Eaten Food`))

d_typical <- d_typical %>% 
  mutate(censored = case_when(
    treatment == 2 & food_eaten == 4 ~ "right",
    treatment == 4 & food_eaten == 8 ~ "right",
    treatment == 8 & food_eaten == 16 ~ "right",
    TRUE ~ "none"
  ))

d_novel <- d_novel %>% 
  mutate(censored = case_when(
    treatment == 2 & novel_eaten == 4 ~ "right",
    treatment == 4 & novel_eaten == 8 ~ "right",
    treatment == 8 & novel_eaten == 16 ~ "right",
    TRUE ~ "none"
  ))

d_typical <- d_typical %>% rename(prop_eaten_food = `Proportion Eaten Food`, food_input = `Food Input`)
d_novel <- d_novel %>% rename(prop_eaten_novel = `Proportion Eaten Novel`, novel_input = `Novel Input`)

d_typical$prop_eaten_food[d_typical$prop_eaten_food < 0] <- 0
d_typical$prop_eaten_food[d_typical$prop_eaten_food > 1] <- 1

d_novel$prop_eaten_novel[d_novel$prop_eaten_novel < 0] <- 0
d_novel$prop_eaten_novel[d_novel$prop_eaten_novel > 1] <- 1


saveRDS(d_typical, "food_eaten/data/cleaned/typical_food_proportion_eaten_group_size.rds")

saveRDS(d_novel, "food_eaten/data/cleaned/novel_food_proportion_eaten_group_size.rds")

#### Joining food_eaten and chase data to look at food_eaten and aggression ####

food_eaten <- readRDS("food_eaten/data/cleaned/typical_food_proportion_eaten_group_size.rds") %>% 
  rename(group = Group, trial = Trial, tank = Tank)

chases <- readRDS("chases/data/cleaned/only_half_chase_group_size.rds") %>% 
  select(-timechases) %>% 
  spread(key = assay, value = numchases)


full_data <- full_join(x = food_eaten, y = chases, by = c("trial", "group", "treatment", "tank"))
full_data <- full_data %>% 
  select(treatment, trial, tank, group, food_eaten, food_input, activity, food, novel) %>% 
  rename(typical_chases = food)

full_data <- full_data %>% 
  group_by(group) %>% 
  mutate(act_chases_aggregate = sum(activity)) %>% 
  arrange(group, trial)

saveRDS(full_data, "food_eaten/data/cleaned/food_eaten_chases.rds")
