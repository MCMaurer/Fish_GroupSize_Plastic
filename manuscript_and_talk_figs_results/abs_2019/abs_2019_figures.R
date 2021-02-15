library(tidyverse)
library(brms)
library(sjstats)
library(ggridges)
theme_set(MCMsBasics::minimal_ggplot_theme())

source("figure_generating_functions.R")

# typical food ------------------------------------------------------------

typ_hu <- readRDS("latency/fit_models/lat_typ_hurd_nbin_hu_fit.rds")

param_estimate_plot(typ_hu, num_params = 6)
ggsave("manuscript_and_talk_figs_results/abs_2019/typ_hu_params.tiff", width = 6, height = 4, dpi = 300)

marginal_effects_plot(typ_hu, effects = c("treatment", "trial"), ci_type = "both") + xlab("Group size") + ylab("Individual\nlatency to\neat known\nfood")
ggsave("manuscript_and_talk_figs_results/abs_2019/typ_hu_me.tiff", width = 6, height = 4, dpi = 300)

marginal_effects_plot(typ_hu, effects = c("treatment", "trial"), ci_type = "both", dpar = "hu") +
  ylab("Probability\nof eating\nknown food") + xlab("Group size")
ggsave("manuscript_and_talk_figs_results/abs_2019/typ_hu_me_hu.tiff", width = 6, height = 4, dpi = 300)


# novel food --------------------------------------------------------------

nov_hu <- readRDS("latency/fit_models/lat_nov_hurd_nbin_hu_fit.rds")

param_estimate_plot(nov_hu, num_params = 6)
ggsave("manuscript_and_talk_figs_results/abs_2019/nov_hu_params.tiff", width = 6, height = 4, dpi = 300)

marginal_effects_plot(nov_hu, effects = c("treatment", "trial"), ci_type = "both") + xlab("Group size") + ylab("Individual\nlatency to\neat novel\nfood") + scale_color_viridis_d(labels = c("1: brine", "2: bead", "3: plastic")) + scale_fill_viridis_d(labels = c("1: brine", "2: bead", "3: plastic"))
ggsave("manuscript_and_talk_figs_results/abs_2019/nov_hu_me.tiff", width = 6, height = 4, dpi = 300)

marginal_effects_plot(nov_hu, effects = c("treatment", "trial"), ci_type = "both", dpar = "hu") +
  ylab("Probability\nof eating\nnovel food") + xlab("Group size") + scale_color_viridis_d(labels = c("1: brine", "2: bead", "3: plastic")) + scale_fill_viridis_d(labels = c("1: brine", "2: bead", "3: plastic"))
ggsave("manuscript_and_talk_figs_results/abs_2019/nov_hu_me_hu.tiff", width = 6, height = 4, dpi = 300)


# predation latency -------------------------------------------------------

pred_hu <- readRDS("latency/fit_models/lat_pred_hurd_nbin_hu_fit.rds")

param_estimate_plot(pred_hu, num_params = 6)
ggsave("manuscript_and_talk_figs_results/abs_2019/pred_hu_params.tiff", width = 6, height = 4, dpi = 300)

marginal_effects_plot(pred_hu, effects = c("treatment", "trial"), ci_type = "both") + xlab("Group size") + ylab("Individual\nlatency to\nresume\nactivity")
ggsave("manuscript_and_talk_figs_results/abs_2019/pred_hu_me.tiff", width = 6, height = 4, dpi = 300)

marginal_effects_plot(pred_hu, effects = c("treatment", "trial"), ci_type = "both", dpar = "hu") +
  ylab("Probability\nof resuming\nactivity") + xlab("Group size")
ggsave("manuscript_and_talk_figs_results/abs_2019/pred_hu_me_hu.tiff", width = 6, height = 4, dpi = 300)


# ICC stuff ---------------------------------------------------------------

icc_typ_hu_result <- icc(x = typ_hu, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 + treatment + trial | group_ID))
icc_typ_hu_result

icc_nov_hu_result <- icc(x = nov_hu, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 + treatment + trial | group_ID))
icc_nov_hu_result

icc_pred_hu_result <- icc(x = pred_hu, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 + treatment + trial | group_ID))
icc_pred_hu_result

## now the varying intercept only models

typ_int <- readRDS("latency/fit_models/lat_typ_int_hurd_nbin_hu_fit.rds")
nov_int <- readRDS("latency/fit_models/lat_nov_int_hurd_nbin_hu_fit.rds")
pred_int <- readRDS("latency/fit_models/lat_pred_int_hurd_nbin_hu_fit.rds")

icc_typ_int_result <- icc(x = typ_int, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 | group_ID))
icc_typ_int_result

icc_nov_int_result <- icc(x = nov_int, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 | group_ID))
icc_nov_int_result

icc_pred_int_result <- icc(x = pred_int, adjusted = TRUE, ppd = TRUE, typical = "median", re.form = ~(1 | group_ID))
icc_pred_int_result

cat(capture.output(icc_typ_int_result), file = "manuscript_and_talk_figs_results/abs_2019/icc_typ_int.txt", sep = "\n")

cat(capture.output(icc_nov_int_result), file = "manuscript_and_talk_figs_results/abs_2019/icc_nov_int.txt", sep = "\n")

cat(capture.output(icc_pred_int_result), file = "manuscript_and_talk_figs_results/abs_2019/icc_pred_int.txt", sep = "\n")



# model comparison --------------------------------------------------------

typ_comp <- waic(typ_hu, typ_int)
typ_comp

nov_comp <- waic(nov_hu, nov_int)
nov_comp

pred_comp <- waic(pred_hu, pred_int)
pred_comp


# write out model structure -----------------------------------------------

cat(capture.output(typ_hu), file = "manuscript_and_talk_figs_results/abs_2019/typ_hu_out.txt", sep = "\n")

cat(capture.output(nov_hu), file = "manuscript_and_talk_figs_results/abs_2019/nov_hu_out.txt", sep = "\n")

cat(capture.output(pred_hu), file = "manuscript_and_talk_figs_results/abs_2019/pred_hu_out.txt", sep = "\n")
