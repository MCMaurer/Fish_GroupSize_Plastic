
# this one needs some work, particularly in generating the initial conditions

marginal_effects_plot <- function(model, effects, color = effects[2], ci_type = "band", ...){
  initial_conditions = list()
  
  # loop through all of your listed effects, *inside* the model's data, and generate initial conditions based on the values in your data
  for (x in 1:length(effects)) {
      initial_conditions[[effects[x]]] = unique(model$data[[effects[x]]])
    }
  
  # if you just have one effect, just use its name for effects
   if(length(effects) == 1){
    e <- marginal_effects(model, effects = effects, int_conditions = initial_conditions, ... = ...)
  } else {
  # otherwise, you have to paste your effect names together
  e <- marginal_effects(model, effects = paste0(effects[1],":",effects[2]), int_conditions = initial_conditions, ... = ...)
  }
  
  response <- attributes(e[[1]])$response
  if(response == "hu"){
    response <- "probability of\ncrossing hurdle"
  }
  
  # pull out the dataframe from the marginal effects call
  e <- e[[1]]
  
  effects_factors <- e %>% 
    select(effects) %>% 
    select_if(is.factor) %>% 
    names()
  
  e <- e %>%
    mutate_at(effects_factors, fct_inorder)
  
  if(response == "probability of\ncrossing hurdle"){
    e <- e %>% 
      mutate(estimate__ = 1 - estimate__,
             lower__ = 1 - lower__,
             upper__ = 1 - upper__)
  }
  
  if(length(effects) == 1){
    e %>% 
      ggplot(aes(x= !!sym(effects[1]), y=estimate__), color= "black") +
      geom_line() +
      geom_ribbon(aes(ymin=lower__, ymax=upper__), fill = "black", 
                  color = "transparent", alpha = 0.1) +
      ylab(response)
      
  } else{
    
    p <- e %>% 
      ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
      geom_line(size = 1.5) +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      ylab(response) +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5), text = element_text(size = 16))
      
    if(ci_type == "both"){
       p +
        geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.1) +
        geom_line(aes(x = !!sym(effects[1]), y = lower__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.5) +
        geom_line(aes(x = !!sym(effects[1]), y = upper__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.5)
      } else{
        if(ci_type == "lines"){
      p + geom_line(aes(x = !!sym(effects[1]), y = lower__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.7) +
        geom_line(aes(x = !!sym(effects[1]), y = upper__, 
                      color = !!sym(color)), linetype = 2, alpha = 0.7)
    } else{
      p + geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.3)
    }
      }
  }
}


# marginal_effects_plot(typ_hu, effects = c("treatment", "trial"))
# marginal_effects_plot(typ_hu, effects = c("treatment", "trial"), ci_type = "both")
# marginal_effects_plot(typ_int, effects = c("treatment"), dpar = "hu")
# marginal_effects_plot(typ_int, effects = c("treatment"))


# # 2nd attempt at marginal effects plots -----------------------------------
# 
# model = typ_int
# effects = c("treatment", "trial")
# names(typ_int$data)[1]
# 
# initial_conditions = list()
# initial_conditions
# 
# length(effects)
# 
# # loop through all of your listed effects, *inside* the model's data, and generate initial conditions based on the values in your data
# for (x in 1:length(effects)) {
#   initial_conditions[[effects[x]]] = unique(model$data[[effects[x]]])
# }
# 
# initial_conditions
# 
# 
# # if you just have one effect, just use its name for effects
# if(length(effects) == 1){
#   e <- marginal_effects(model, effects = effects, int_conditions = initial_conditions, ... = ...)
# } else {
#   # otherwise, you have to paste your effect names together
#   e <- marginal_effects(model, effects = paste0(effects[1],":",effects[2]), int_conditions = initial_conditions)
# }
# 
# e
# 
# response <- attributes(e[[1]])$response
# 
# # pull out the dataframe from the marginal effects call
# e <- e[[1]]
# 
# if(length(effects) == 1){
#   e %>% 
#     ggplot(aes(x= !!sym(effects[1]), y=estimate__), color= "black") +
#     geom_line() +
#     geom_ribbon(aes(ymin=lower__, ymax=upper__), fill = "black", 
#                 color = "transparent", alpha = 0.1) +
#     ylab(response)
#   
# } else{
#   e %>% 
#     ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
#     geom_line() +
#     geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.1) +
#     geom_line(aes(x = !!sym(effects[1]), y = lower__, color = !!sym(color), linetype = 2)) +
#     scale_fill_viridis_d() +
#     scale_color_viridis_d() +
#     ylab(response)
# }
# 
# names(e)
# 
# e %>% 
#   ggplot(aes(x = treatment, y = latency)) +
#   geom_line(aes(color = trial))

# param estimate plots ----------------------------------------------------

param_estimate_plot <- function(model, num_params = 4){
  p <- posterior_summary(model) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    head(n=num_params)
  pp <- p %>% 
    mutate(variable = rownames(p)) %>% 
    ggplot(aes(y=Estimate, x = variable))+
    geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 3/5, shape = 20) +
    geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
    coord_flip() +
    ylab("Estimate with 95% credible interval")
  return(pp)
}


# model description tables ------------------------------------------------

make_model_table <- function(...){
  all_model_names <- lapply(substitute(list(...))[-1], deparse)
  models <- list(...)
  names(models) <- all_model_names
  
  model_table <- tibble(
    model_name = character(),
    distribution = character(),
    formula = character(),
    random_effects = character(),
    response = character(),
    predictors = character()
  )
  for (i in 1:length(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    distribution <- model$family$family
    formula <- as.character(model$formula$formula)
    formula <- paste(formula[2], formula[1], formula[3])
    if (length(model$formula$pforms) > 0) {
      formula2 <- as.character(model$formula$pforms[[1]])
      formula2 <- paste(formula2[2], formula2[1], formula2[3])
      formula <- paste(formula, formula2, sep = "\n")
    }
    random_effects <- paste(as.character(insight::find_formula(model)$random), sep = "\n", collapse = "\n")
    response <- insight::find_response(model)
    predictors <- paste(as.character(unlist(insight::find_predictors(model))), sep = "\n", collapse = "\n")
    
    new_model_table <- tibble(
      model_name = model_name,
      distribution = distribution,
      formula = formula,
      random_effects = random_effects,
      response = response,
      predictors = predictors)
    
    model_table <- rbind(model_table, new_model_table)
    
  }
  model_table <- model_table %>% 
    mutate_if(.predicate = str_detect(., "\n"), .funs = linebreak)
  return(model_table)
}


fitted_contrasts <- function(model, by, cov.keep = 4, re_formula = ~0, plot = FALSE, hurdle_sep = FALSE){
  if(hurdle_sep){
    d <- model %>% 
      emmeans::ref_grid(cov.keep = cov.keep) %>% 
      .@grid %>% 
      tidybayes::add_fitted_draws(model, re_formula = re_formula, dpar = TRUE) %>% 
      group_by(!!rlang::ensym(by), .draw) %>% 
      summarise(mu = mean(mu, na.rm = T),
                hu = mean(hu, na.rm = T))
    
    neworder <- d %>% 
      group_by(!!rlang::ensym(by)) %>% 
      summarise(mu = mean(mu)) %>% 
      arrange(desc(mu)) %>% 
      select(!!rlang::ensym(by)) %>% 
      unlist() %>% 
      as.character()
    
    d_mu <- d %>% 
      compare_levels(variable = mu, by = !!rlang::ensym(by), comparison = combn(neworder, 2, simplify = FALSE))
    
    print(d_mu)
    
    d_hu <- d %>% 
      compare_levels(variable = hu, by = !!rlang::ensym(by), comparison = combn(neworder, 2, simplify = FALSE))
    
    print(d_hu)
    
    if(plot){
      p_mu <- d_mu %>% 
        mutate(first_cat = str_extract(!!rlang::ensym(by), "([^\\s]+)") %>% 
                 factor(levels = neworder) %>% 
                 as.numeric()) %>%
        ggplot(aes(x = mu, y = reorder(!!rlang::ensym(by), desc(first_cat)))) +
        geom_halfeyeh() +
        ylab("Contrast Pair") +
        xlab("Difference in mean post-hurdle value")
      
      p_hu <- d_hu %>% 
        mutate(first_cat = str_extract(!!rlang::ensym(by), "([^\\s]+)") %>% 
                 factor(levels = neworder) %>% 
                 as.numeric()) %>%
        ggplot(aes(x = 1 - hu, y = reorder(!!rlang::ensym(by), desc(first_cat)))) +
        geom_halfeyeh() +
        ylab("Contrast Pair") +
        xlab("Difference in mean prob of crossing hurdle")
      
      return(list(post_hurdle = p_mu, hurdle = p_hu))
      
    } else{
      d_mu <- d_mu %>% 
        rename(param = mu)
      d_hu <- d_hu %>% 
        rename(param = hu)
      
      bind_rows(mu = d_mu, hu = d_hu, .id = "param")
    }
    
    
  } else {
    
    
    d <- model %>% 
      emmeans::ref_grid(cov.keep = cov.keep) %>% 
      .@grid %>% 
      tidybayes::add_fitted_draws(model, re_formula = re_formula) %>% 
      group_by(!!rlang::ensym(by), .draw) %>% 
      summarise(.value = mean(.value, na.rm = T))
    
    neworder <- d %>% 
      group_by(!!rlang::ensym(by)) %>% 
      summarise(.value = mean(.value)) %>% 
      arrange(desc(.value)) %>% 
      select(!!rlang::ensym(by)) %>% 
      unlist() %>% 
      as.character()
    
    d <- d %>% 
      compare_levels(variable = .value, by = !!rlang::ensym(by), comparison = combn(neworder, 2, simplify = FALSE))
    
    if(plot){
      d %>% 
        mutate(first_cat = str_extract(!!rlang::ensym(by), "([^\\s]+)") %>% 
                 factor(levels = neworder) %>% 
                 as.numeric()) %>%
        ggplot(aes(x = .value, y = reorder(!!rlang::ensym(by), desc(first_cat)))) +
        geom_halfeyeh() +
        ylab("Contrast Pair") +
        xlab("Difference in Fitted Mean Outcomes")
    } else{
      return(d)
    }
  }
  
}

# 
# make_model_table(lat_pred_int, lat_typ_int, lat_nov_int, food_eaten)
# 
# make_model_table(lat_pred_int, lat_typ_int, lat_nov_int)
# make_model_table(lat_pred_int)



# model = typ_hu
# effects = c("treatment", "trial")
# 
# 
# initial_conditions = list()
# 
# # loop through all of your listed effects, *inside* the model's data, and generate initial conditions based on the values in your data
# for (x in 1:length(effects)) {
#   initial_conditions[[effects[x]]] = unique(model$data[[effects[x]]])
# }
# 
# initial_conditions
# 
# # if you just have one effect, just use its name for effects
# if(length(effects) == 1){
#   e <- marginal_effects(model, effects = effects, int_conditions = initial_conditions)
# } else {
#   # otherwise, you have to paste your effect names together
#   e <- marginal_effects(model, effects = paste0(effects[1],":",effects[2]), int_conditions = initial_conditions)
# }
# 
# response <- attributes(e[[1]])$response
# if(response == "hu"){
#   response <- "probability of crossing hurdle"
# }
# 
# # pull out the dataframe from the marginal effects call
# e <- e[[1]]
# str(e)
# 
# e %>%
#   mutate(trial = fct_inorder(trial)) %>%
#   str()
# 
# e <- e %>%
#   mutate_if(is.factor, fct_inorder)

