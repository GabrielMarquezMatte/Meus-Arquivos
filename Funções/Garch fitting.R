find_best_arch_model <- function(x, 
                                 type_models = "sGARCH", 
                                 dist_to_use = "norm",
                                 max_lag_AR = 2,
                                 max_lag_MA = 2,
                                 max_lag_ARCH = 2,
                                 max_lag_GARCH = 2,
                                 min_ARCH = 0, min_GARCH = 0,
                                 min_AR = 0, min_MA = 0){
  
  require(tidyverse)
  
  df_grid <- expand_grid(type_models = type_models,
                         dist_to_use = dist_to_use,
                         arma_lag = min_AR:max_lag_AR,
                         ma_lag = min_MA:max_lag_MA,
                         arch_lag = min_ARCH:max_lag_ARCH,
                         garch_lag = min_GARCH:max_lag_GARCH)
  
  
  l_out <- pmap(.l = list(x = rep(list(x), nrow(df_grid)), 
                          type_model = df_grid$type_models,
                          type_dist = df_grid$dist_to_use,
                          lag_ar = df_grid$arma_lag,
                          lag_ma = df_grid$ma_lag,
                          lag_arch = df_grid$arch_lag,
                          lag_garch  = df_grid$garch_lag),
                do_single_garch)
  
  tab_out <- bind_rows(l_out)
  
  # find by AIC
  idx <- which.min(tab_out$AIC)
  best_aic <- tab_out[idx, ]
  
  # find by BIC
  idx <- which.min(tab_out$BIC)
  best_bic <- tab_out[idx, ]
  
  l_out <- list(best_aic = best_aic,
                best_bic = best_bic,
                tab_out = tab_out,
                ugspec_b = ugarchspec(variance.model = 
                                        list(model = best_bic$type_model,
                                             garchOrder = c(best_bic$lag_arch, best_bic$lag_garch)),
                                      mean.model = 
                                        list(armaOrder = c(best_bic$lag_ar, best_bic$lag_ma)),
                                      distribution.model = best_bic$type_dist),
                ugspec_a = ugarchspec(variance.model = 
                                        list(model = best_aic$type_model,
                                             garchOrder = c(best_aic$lag_arch, best_aic$lag_garch)),
                                      mean.model = 
                                        list(armaOrder = c(best_aic$lag_ar, best_aic$lag_ma)),
                                      distribution.model = best_aic$type_dist))
  
  return(l_out)
}
do_single_garch <- function(x, 
                            type_model, 
                            type_dist, 
                            lag_ar, 
                            lag_ma, 
                            lag_arch, 
                            lag_garch) {
  require(rugarch)
  
  
  spec = ugarchspec(variance.model = list(model =  type_model, 
                                          garchOrder = c(lag_arch, lag_garch)),
                    mean.model = list(armaOrder = c(lag_ar, lag_ma)),
                    distribution = type_dist)
  
  message('Estimating',names(x),' ARMA(',lag_ar, ',', lag_ma,')-',
          type_model, '(', lag_arch, ',', lag_garch, ')', 
          ' dist = ', type_dist,
          appendLF = FALSE)
  
  try({
    my_rugarch <- list()
    my_rugarch <- ugarchfit(spec = spec, data = x)
  }, silent = T)
  
  if (!is.null(coef(my_rugarch))) {
    message('\tDone')
    
    AIC <- rugarch::infocriteria(my_rugarch)[1]
    BIC <- rugarch::infocriteria(my_rugarch)[2]
  } else {
    message('\tEstimation failed..')
    
    AIC <- NA
    BIC <- NA
  }
  
  est_tab <- tibble(lag_ar, 
                    lag_ma,
                    lag_arch,
                    lag_garch,
                    AIC =  AIC,
                    BIC = BIC,
                    type_model = type_model,
                    type_dist,
                    model_name = paste0('ARMA(', lag_ar, ',', lag_ma, ')+',
                                        type_model, '(', lag_arch, ',', lag_garch, ') ',
                                        type_dist)) 
  
  return(est_tab)
}
do_single_dcc <- function(x, uspec, 
                          type_model = "DCC", 
                          type_dist = "mvnorm", 
                          order1, 
                          order2){
  require(rmgarch)
  require(tidyverse)
  
  
  spec = dccspec(uspec, dccOrder = c(order1, order2),
                 model = type_model, distribution = type_dist)
  
  message('Estimating DCC(',order1, ',', order2,')-',
          type_model,' dist = ', type_dist,
          appendLF = FALSE)
  
  try({my_rugarch <- list()
  my_rugarch <- dccfit(spec = spec, data = x)})
  if (!is.null(coef(my_rugarch))) {
    message('\tDone')
    AIC <- rugarch::infocriteria(my_rugarch)[1]
    BIC <- rugarch::infocriteria(my_rugarch)[2]
  }else{
    message('\tEstimation failed..')
    AIC <- NA
    BIC <- NA
  }
  est_tab <- tibble(order1 = order1,
                    order2 = order2,
                    AIC =  AIC,
                    BIC = BIC,
                    type_model = type_model,
                    type_dist = type_dist)
  return(est_tab)
}
find_best_dcc_model <- function(x,uspec, 
                                type_models = "DCC", 
                                dist_to_use = "mvnorm",
                                max_order1 = 2,
                                max_order2 = 2,
                                min_order1 = 1,
                                min_order2 = 1){
  
  require(tidyr)
  
  df_grid <- expand_grid(type_models = type_models,
                         dist_to_use = dist_to_use,
                         order1 = min_order1:max_order1,
                         order2 = min_order2:max_order2)
  
  
  l_out <- pmap(.l = list(x = rep(list(x), nrow(df_grid)),
                          uspec = rep(list(uspec), nrow(df_grid)),
                          type_model = df_grid$type_models,
                          type_dist = df_grid$dist_to_use,
                          order1 = df_grid$order1,
                          order2 = df_grid$order2),
                do_single_dcc)
  
  tab_out <- bind_rows(l_out)
  
  # find by AIC
  idx <- which.min(tab_out$AIC)
  best_aic <- tab_out[idx,]
  
  # find by BIC
  idx <- which.min(tab_out$BIC)
  best_bic <- tab_out[idx, ]
  
  l_out <- list(best_aic = best_aic,
                best_bic = best_bic,
                tab_out = tab_out,
                dccspec_b = dccspec(uspec,
                                    dccOrder = c(best_bic$order1, best_bic$order2),
                                    distribution = best_bic$type_dist,
                                    model = best_bic$type_model),
                dccspec_a = dccspec(uspec,
                                    dccOrder = c(best_aic$order1, best_aic$order2),
                                    distribution = best_aic$type_dist,
                                    model = best_aic$type_model))
  
  return(l_out)
}