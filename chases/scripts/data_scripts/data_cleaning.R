library(tidyverse)

d <- read_csv("chases/data/raw/only_half_chase_group_size.csv")
d <- d[,1:12]

# gather the chase measurements together
# have to add the row ids as columns early on, otherwise the spread function doesn't work, which is stupid. Should make a reproducible example out of this since it seems silly
d <- d %>% rowid_to_column() %>% 
  gather(key = "assay_type", value = "value", 8:13) %>%
  separate(col = assay_type, into = c("assay", "measurement"), sep = "_") 

d <- d %>% 
  spread(key = measurement, value = value) %>% 
  rename(trial = Trial, group = Group, tank = Tank)

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(group, 1, 1)))

d %>% ggplot(aes(x=timechases)) + geom_histogram()

d$group_ID <- as.factor(d$group)
d$tank <- as.factor(d$tank)
d$percap_chases <- d$numchases / d$treatment

saveRDS(d, "chases/data/cleaned/only_half_chase_group_size.rds")



# trying again ------------------------------------------------------------


d <- read_csv("latency/data/raw/full_latency_novel_food_group_size.csv") 
d <- d %>% 
  select(-matches("^X.*"), -`Done?`, -`Video Folder`, -`Video #`) %>% 
  rename(novel_food = `Novel Food`, novel_start = `Novel Start`)
d %>% 
  select(Trial, Tank, Group, novel_food, activity_numchases, activity_timechases, novel_numchases, novel_timechases)

d <- d[,1:12]

# gather the chase measurements together
# have to add the row ids as columns early on, otherwise the spread function doesn't work, which is stupid. Should make a reproducible example out of this since it seems silly
d <- d %>% rowid_to_column() %>% 
  gather(key = "assay_type", value = "value", 8:13) %>%
  separate(col = assay_type, into = c("assay", "measurement"), sep = "_") 

d <- d %>% 
  spread(key = measurement, value = value) %>% 
  rename(trial = Trial, group = Group, tank = Tank)

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(group, 1, 1)))

d %>% ggplot(aes(x=timechases)) + geom_histogram()

d$group_ID <- as.factor(d$group)
d$tank <- as.factor(d$tank)
d$percap_chases <- d$numchases / d$treatment

saveRDS(d, "chases/data/cleaned/only_half_chase_group_size.rds")


# now with lea's full chases dataset --------------------------------------

d <- read_csv("chases/data/raw/full_chase_group_size.csv")
d
d <- d %>% 
  select_if(!str_detect(names(.), "^X[0-9]*")) %>% 
  select(-Date, -`Video Folder`, -`Video #`) %>% 
  rename(trial = Trial, tank = Tank, group_ID = Group) %>% 
  rowid_to_column() %>% 
  gather(key = "assay_type", value = "value", -trial, -tank, -group_ID, -rowid) %>%
  separate(col = assay_type, into = c("assay", "measurement"), sep = "_") %>% 
  select(-rowid) %>% 
  mutate(treatment = as.numeric(str_sub(group_ID, 1, 1))) %>% 
  select(treatment, trial, tank, group_ID, assay, measurement, value)

saveRDS(d, "chases/data/cleaned/full_chase_group_size.rds")
