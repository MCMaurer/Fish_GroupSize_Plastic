library(performance)

my_icc <- function (model, re.form = NULL, robust = TRUE, ci = 0.95, newdata = NULL, ...) 
{
  mi <- insight::model_info(model)
  if (insight::is_multivariate(model)) {
    resp <- insight::find_response(model)
    is.mixed <- sapply(resp, function(i) mi[[i]]$is_mixed, 
                       simplify = TRUE)
    if (!any(is.mixed)) {
      warning("'model' has no random effects.", call. = FALSE)
      return(NULL)
    }
  }
  else if (!mi$is_mixed) {
    warning("'model' has no random effects.", call. = FALSE)
    return(NULL)
  }
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package `brms` needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  PPD <- brms::posterior_predict(model, re.form = re.form, newdata = newdata, 
                                 summary = FALSE)
  var_total <- apply(PPD, MARGIN = 1, FUN = stats::var)
  PPD_0 <- brms::posterior_predict(model, re.form = NA, newdata = newdata, summary = FALSE)
  var_rand_intercept <- apply(PPD_0, MARGIN = 1, FUN = stats::var)
  if (robust) 
    fun <- get("median", asNamespace("stats"))
  else fun <- get("mean", asNamespace("base"))
  var_icc <- var_rand_intercept/var_total
  var_residual <- var_total - var_rand_intercept
  ci_icc <- rev(1 - stats::quantile(var_rand_intercept/var_total, 
                                    probs = c((1 - ci)/2, (1 + ci)/2)))
  result <- structure(class = "icc_decomposed", list(ICC_decomposed = 1 - 
                                                       fun(var_icc), ICC_CI = ci_icc))
  attr(result, "var_rand_intercept") <- fun(var_rand_intercept)
  attr(result, "var_residual") <- fun(var_residual)
  attr(result, "var_total") <- fun(var_total)
  attr(result, "ci.var_rand_intercept") <- bayestestR::ci(var_rand_intercept, 
                                                          ci = ci)
  attr(result, "ci.var_residual") <- bayestestR::ci(var_residual, 
                                                    ci = ci)
  attr(result, "ci.var_total") <- bayestestR::ci(var_total, 
                                                 ci = ci)
  attr(result, "ci") <- ci
  attr(result, "re.form") <- re.form
  attr(result, "ranef") <- model$ranef$group[1]
  attr(attr(result, "ci.var_rand_intercept"), "data") <- NULL
  attr(attr(result, "ci.var_residual"), "data") <- NULL
  attr(attr(result, "ci.var_total"), "data") <- NULL
  result
}


my_icc_tibble <- function(model, total_re.form, lesser_re.form, new_data = NULL, hurdle_sep = FALSE){
  
  is_multi <- insight:::is_multivariate(model)
  
  PPD <- posterior_predict(model, re.form = total_re.form, newdata = new_data)
  PPD_0 <- posterior_predict(model, re.form = lesser_re.form, newdata = new_data)
  
  if(is_multi){
    PPD_list <- array_branch(PPD, 3)
    PPD_0_list <- array_branch(PPD_0, 3)
    
    PPD_list <- map(.x = PPD_list, function(x)  apply(x, 1, FUN = stats::var, na.rm = T))
    PPD_0_list <- map(.x = PPD_0_list, function(x)  apply(x, 1, FUN = stats::var, na.rm = T))
    
    ICC_list <- map2(.x = PPD_0_list, .y = PPD_list, .f = function(x,y) 1 - (x/y))
    
    map_dfr(ICC_list, .f = function(x) x %>% median_hdi() %>% 
              rename(median_icc = y, ci_2.5 = ymin, ci_97.5 = ymax) %>% 
              select(median_icc, ci_2.5, ci_97.5), .id = "outcome")
  } else{
  
  if(hurdle_sep){
  
    # h is hurdle, d is other distribution
    PPD_h <- PPD
    PPD_h[PPD_h > 0] <- 1
    
    PPD_0_h <- PPD_0
    PPD_0_h[PPD_0_h > 0] <- 1
    
    PPD_d <- PPD
    PPD_d[PPD_d == 0] <- NA
    
    PPD_0_d <- PPD_0
    PPD_0_d[PPD_0_d == 0] <- NA
    
    vars_h <- apply(PPD_h, MARGIN = 1, FUN = var)
    vars_0_h <- apply(PPD_0_h, MARGIN = 1, FUN = var)
    
    vars_d <- apply(PPD_d, MARGIN = 1, FUN = var, na.rm = T)
    vars_0_d <- apply(PPD_0_d, MARGIN = 1, FUN = var, na.rm = T)
    
    icc_draws_h <- tibble(icc_draws = 1 - vars_0_h/vars_h)
    icc_draws_d <- tibble(icc_draws = 1 - vars_0_d/vars_d)

    model_name <- deparse(substitute(model))
    
    icc_draws_h <- icc_draws_h %>% 
      median_hdi() %>% 
      rename(median_icc = icc_draws, ci_2.5 = .lower, ci_97.5 = .upper) %>% 
      mutate(model_name = model_name, distribution = "hurdle") %>% 
      select(model_name, distribution, median_icc, ci_2.5, ci_97.5)
    
    icc_draws_d <- icc_draws_d %>% 
      median_hdi() %>% 
      rename(median_icc = icc_draws, ci_2.5 = .lower, ci_97.5 = .upper) %>% 
      mutate(model_name = model_name, distribution = "post-hurdle") %>% 
      select(model_name, distribution, median_icc, ci_2.5, ci_97.5)
    
    bind_rows(icc_draws_h, icc_draws_d)
    
  } else {
  
  vars <- apply(PPD, MARGIN = 1, FUN = var)
  vars_0 <- apply(PPD_0, MARGIN = 1, FUN = var)
  
  icc_draws <- tibble(icc_draws = 1 - (vars_0/vars))
  model_name <- deparse(substitute(model))
  icc_draws %>% 
    median_hdi() %>% 
    rename(median_icc = icc_draws, ci_2.5 = .lower, ci_97.5 = .upper) %>% 
    mutate(model_name = model_name) %>% 
    select(model_name, median_icc, ci_2.5, ci_97.5)
  }
  }
}

my_icc_plot <- function(model, total_re.form, lesser_re.form, plot_each_var = F, new_data = NULL){
  PPD <- posterior_predict(model, re.form = total_re.form, newdata = new_data)
  vars <- apply(PPD, MARGIN = 1, FUN = var)
  
  PPD_0 <- posterior_predict(model, re.form = lesser_re.form, newdata = new_data)
  vars_0 <- apply(PPD_0, MARGIN = 1, FUN = var)
  
  icc_draws <- tibble(icc_draws = 1 - (vars_0/vars))
  
  g <- icc_draws %>% 
    ggplot(aes(x = icc_draws)) +
    geom_histogram(bins = 100) +
    ggtitle(deparse(substitute(model)))
  
  if (plot_each_var == T) {
    g2 <- vars %>% 
      as.data.frame() %>% 
      ggplot(aes(x = vars)) +
      geom_histogram(bins = 100) +
      xlab("Variance Draws Conditional on Random Effects")
    
    g3 <- vars_0 %>% 
      as.data.frame() %>% 
      ggplot(aes(x = vars_0)) +
      geom_histogram(bins = 100) +
      xlab("Variance Draws Not Conditional on Random Effects")
    
    # exctracting axis limits from each plot
    g2x <- ggplot_build(g2)$layout$panel_scales_x[[1]]$range$range
    g3x <- ggplot_build(g3)$layout$panel_scales_x[[1]]$range$range
    
    # making the limits the same while fitting both sets of data
    g2 <- g2 + xlim(min(g2x, g3x), max(g2x, g3x))
    g3 <- g3 + xlim(min(g2x, g3x), max(g2x, g3x))
    
    g <- list(icc = g, conditional_variance = g2, unconditional_variance = g3)
  }
  return(g)
}
