library(tidyverse)
library(brms)
library(tidybayes)
library(kableExtra)
library(knitr)
library(pander)
library(emmeans)
library(patchwork)
theme_set(theme_minimal())
panderOptions("table.style", "grid")
source("figure_generating_functions.R")
source("icc_functions.R")

load("all_manuscript_models.rda")

pnb <- marginal_effects_plot(model = pred_fit, effects = c("treatment", "trial")) +
  xlab("Group size") + ylab("Latency to\nresume movement") + theme(axis.title.y = element_text(angle = 90)) +
  scale_color_viridis_d("Trial") + scale_fill_viridis_d("Trial")
ph <- marginal_effects_plot(model = pred_fit, effects = c("treatment", "trial"), dpar = "hu") +
  xlab("Group size") + ylab("Probability of\nresuming movement") + theme(axis.title.y = element_text(angle = 90)) +
  scale_color_viridis_d("Trial") + scale_fill_viridis_d("Trial")

knb <- marginal_effects_plot(model = known_fit, effects = c("treatment", "trial")) +
  xlab("Group size") + ylab("Latency to\nsample known food") + theme(axis.title.y = element_text(angle = 90)) +
  scale_color_viridis_d("Trial") + scale_fill_viridis_d("Trial")
kh <- marginal_effects_plot(model = known_fit, effects = c("treatment", "trial"), dpar = "hu") +
  xlab("Group size") + ylab("Probability of\nsampling known food") + theme(axis.title.y = element_text(angle = 90)) +
  scale_color_viridis_d("Trial") + scale_fill_viridis_d("Trial")

nnb <- marginal_effects_plot(model = nov_fit, effects = c("treatment", "novel_food")) +
  xlab("Group size") + ylab("Latency to\nsample novel food") + theme(axis.title.y = element_text(angle = 90)) +
  scale_color_viridis_d("Novel\nFood", labels = c("Brine\nShrimp", "Glass\nBead", "Micro-\nplastic")) +
  scale_fill_viridis_d("Novel\nFood", labels = c("Brine\nShrimp", "Glass\nBead", "Micro-\nplastic")) +
  theme(legend.key.height = unit(0.8, "cm"))
nh <- marginal_effects_plot(model = nov_fit, effects = c("treatment", "novel_food"), dpar = "hu") +
  xlab("Group size") + ylab("Probability of\nsampling novel food") + theme(axis.title.y = element_text(angle = 90)) +
  scale_color_viridis_d("Novel\nFood", labels = c("Brine\nShrimp", "Glass\nBead", "Micro-\nplastic")) +
  scale_fill_viridis_d("Novel\nFood", labels = c("Brine\nShrimp", "Glass\nBead", "Micro-\nplastic")) +
  theme(legend.key.height = unit(0.8, "cm"))

nb <- pnb / knb / nnb + plot_annotation(tag_levels = 'a') & 
  theme(text = element_text(size = 10))

ggsave("manuscript_and_talk_figs_results/manuscript_conditional_plots_all.tiff", 
       units = "mm", width = 173, height = 231)

h <- ph / kh / nh + plot_annotation(tag_levels = 'a') & 
  theme(text = element_text(size = 10))

ggsave("manuscript_and_talk_figs_results/manuscript_conditional_hurdle_plots_all.tiff", 
       units = "mm", width = 173, height = 231)


