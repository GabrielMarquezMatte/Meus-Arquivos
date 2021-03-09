do_single_msgarch <- function(x,
                              models,
                              distribution){
  require(tidyverse)
  require(MSGARCH)
  spec <- CreateSpec(variance.spec = list(model = models),
                     distribution.spec = list(distribution))
  message("Estimating model")
  try({fit <- NA
  fit <- FitML(spec,x)}, silent = T)
  if(is.na(fit)){
    message("\tEstimation failed")
    bic <- NA
  }else{
    message("\tDone")
    bic <- BIC(fit)
  }
  est_tab <- tibble(model = paste(models,collapse = " "),
                    dists = paste(distribution, collapse = " "),
                    BIC = bic)
  return(est_tab)
}
find_best_msgarch <- function(x,
                              models = "sGARCH",
                              permut_model = 1,
                              n_model = 1,
                              distribution = "norm",
                              permut_dist = 1,
                              Repeat = F){
  require(gtools)
  modelos <- permutations(permut_model,n_model,
                          models, repeats.allowed = Repeat) %>% 
    as.data.frame
  colnames(modelos) <- paste0("modelo",1:ncol(modelos))
  dists <- permutations(permut_dist,n_model,
                        distribution,repeats.allowed = Repeat) %>%
    as.data.frame()
  names(dists) <- paste0("dist",1:ncol(dists))
  df_grid <- expand_grid(modelos,dists)
  models1 <- names(df_grid)
  models1 <- grepl("modelo",models1)
  l_out <- replicate(nrow(df_grid),list)
  for(i in 1:nrow(df_grid)){
    l_out[[i]] <- do_single_msgarch(x,df_grid[i,models1] %>% unlist,
                                    df_grid[i,!models1] %>% unlist)
  }
  tab_out <- bind_rows(l_out)
  min_bic <- which.min(tab_out$BIC)
  modelo <- df_grid[min_bic,models1] %>% unlist
  dist <- df_grid[min_bic,!models1] %>% unlist
  spec <- CreateSpec(variance.spec = list(model = modelo),
                     distribution.spec = list(distribution = dist))
  l_out <- list(BIC = tab_out$BIC[min_bic],
                tab_out = tab_out,
                spec = spec)
  return(l_out)
}