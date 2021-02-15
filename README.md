# Bayesian Models For Fish Group Studies

These are some first stabs at Bayesian GLMMs for fish group personality data lovingly collected by [Lea Pollack](github.com/ljpollack), using the `brms` package, the UC Davis FARM cluster, and stuff learned from Richard McElreath's class.

# Repo Organization

## Organization To-Do

1) ~~pull all models down from FARM~~
2) compartmentalize by specific model to be fitted
3) make sure FARM and local relative paths for model scripts match so we can run the FARM `.R` scripts locally to test

### FARM Transfer

1) start with a single model
2) take file from FARM pulled folder, clean it up (put in new STDOUT, make sure directories look good, take out data cleaning elements and transfer them to data cleaning scripts)
3) when file is cleaned, put it into a new folder that will have the correct cleaned structure
4) when all the files are cleaned and put into new structure, you can take that whole new structure and replace the local repo's "files for farm" folder
5) from here, transfer the proper files from the local repo "files for farm **into the non-farm folders, so the models match between "files for farm** and everywhere else

**actually, should consider just making the files match exactly. one version on the local repo, push this to the FARM, anything that gets pulled back with changes gets pulled directly on top of the local file. keeps everything the same between both places**

# Fitted Models

Listed by response variable.

## Latency

### Known Food

#### *BEST MODEL:* Multi-level hurdle negative binomial model
```
family = hurdle_negbinomial,
            latency ~ 1 + treatment + trial +
              (1 + treatment + trial | group_ID) +
              (1 + treatment + trial | tank)
```
Model Info:

- File Location:
- Data Used:
- Post-Model Script:

### Novel Food

#### *BEST MODEL:* Multi-level hurdle negative binomial model
```
family = hurdle_negbinomial,
               latency ~ 1 + treatment + novel_food +
                 (1 + treatment + novel_food | group_ID) +
                 (1 + treatment + novel_food | tank)
```
Model Info:

- File Location:
- Data Used:
- Post-Model Script:

## Chases

### Number of Chases

#### *BEST MODEL:*
```
```
Model Info:

- File Location:
- Data Used:
- Post-Model Script:

## Food Eaten

# To-Do Models

1) In latency models, add explicit predictors for the hurdle portion
2) Latency post-predator cue
    - same structure as the latency for known food model
3) Look for correlation between aggression (chase count) and % food eaten
    - predictor would be aggression
    - so we'll add aggression into a % of food eaten model
    - there are 3x more aggression data points than food eaten data points, since there were 3 aggression trials but only 1 trial where food eaten was measured
        - do we use **all** the aggression data points or just the ones that correspond to the trial where food eaten was recorded? *need to think about this*
            - could maybe use 3 separate aggression predictors: one for each assay in which aggression was measured. so a "activity assay aggression" predictor, a "known food aggression" predictor (which is the one where % food was actually measured), and a "novel food aggression" predictor
4) for "number of chases" model, should run it once with a per-capita measure of # of chases
    - problem here is can't use nbinom model because we don't have integers any more
    - should plot these data and look at the per-capita rate
    - look into using something like `numchases | trials(group_size)`, like in the aggregated binomial model of % food eaten, this would be instead of a per-capita numchase value
4) Try to calculate repeatability value for within-group in chase and food eaten models, maybe latency too.
5) Want to try and look at **conformity**, or how closely you stick to your group members. So, for a given trial and *treatment*, are you closer to the individuals **in your group** than you are to all of the other fish in your *treatment*? So this would be something like comparing within-group variance to total treatment variance? Or something like that? Need to look into multilevel modeling comparisons between group vs. global effects maybe?
    - another way to phrase this would be: does knowing what group you're in tell you more than knowing the global value for your treatment?
        - there has to be info out there on this, comparing group-level and global effects
    - this is maybe a repeatability measure for a *group* instead of an *individual*, just like we'd calculate repeatability to determine personality for an individual. Look at Nakagawa 2016/17?
    - we could calculate this for a negative binomial model
6) ICC Stuff: look at `sjstats:::icc.brmsfit` code, also look into how to use `brms::predicted` with a single value for a predictor, ie how to use treatment = 2. Check [this link](https://github.com/paul-buerkner/brms/issues/82)

## To-Do Models as of 1/9/2020

(2) look at repeatability of sum of group latencies (three separate models for each trial type). I'm not sure what to do about individuals who never move or never eat? I guess leave them out entirely - or make them 500 seconds? Thoughts? We could just do a model where sum of latencies is predicted by treatment and trial, with random effects for tank and group ID and then calculator the variance ratio for group ID, right?

This might be too much work, and I'm sorry about that! Maybe we can start with variance ratios from predictions 12, 13, and 14?

Lastly, (3) could we calculate group repeatability for the per-capita chases model (Prediction 11 in the write up)? That should be easy, right, just doing the icc function on that model output. 

1. Repeatability of sum of group latencies
    - food
    - novel
    - pred
    - percap_chase
        - icc of `chases` model from Results rmd
    - problem with sum is that you can't really use the individuals where they never moved, but then you are also left with a messed up "sum of latencies" since the sum is dependent on how many even moved.
    - mean_lat ~ treatment + trial + (1 | group_ID) + (1 | tank)
        - do this for each assay
    - then do variance ratio
    - can use "other_models/data/cleaned/mean_group_lat_all_assays.rds" dataset for this
    
2. **REDO** all the mean latency models, I used the 0s in them and need to filter them out instead, and model a regular negbinom instead of a hurdle model

# Other To-Do

1) Figure out way to push large model `.rds` files to GitHub or look into other storage method
2) Finish plot-generating functions
3) Clean up pre-analysis data cleaning scripts
4) Make a **separate** post-analysis script for *each* model
5) Shade the parameter density plots with a 95% posterior density interval
