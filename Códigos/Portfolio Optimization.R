options(warn = -1)
pacotes <- c("tidyverse","CVXR","quadprog","Rsolnp","GA","actuar")
sapply(pacotes,require,character.only = T)
rm(pacotes)
pesos_carteira <- function(n_acoes, n_sim, short = F, p_min = -2, p_max = 2){
  pesos <- matrix(0,n_acoes,n_sim)
  if(isFALSE(short)){
    pesos <- apply(pesos,2,rpareto, shape = 1.2, scale = 1)
  }else{
    pesos <- apply(pesos,2,rnorm)
    
  }
  pesos <- apply(pesos,2,function(x)x/sum(x))
  colsub <- apply(pesos,2,function(x)!any(x >= p_max| x <= p_min))
  pesos <- pesos[,colsub]
  return(pesos)
}
portfront <- function(ret,cov,p_ret,rf = NA,
                      short = T,min_w = 0,max_w = 1){
  fun <- function(ret,cov,p_ret,short = T,min_w,max_w,rf){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- 1e-16
      ret <- c(ret,rf)
      colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
    }
    ret <- unlist(ret)
    cov <- as.matrix(cov)
    p_ret <- unlist(p_ret)
    short <- unlist(short)
    if(short){
      A1 <- 2*cov
      A1 <- rbind(rbind(A1,ret),1)
      A1 <- cbind(A1,c(ret,rep(0,2)))
      A1 <- cbind(A1,c(rep(1,nrow(cov)), 0,0))
      b <- c(rep(0,length(ret)),p_ret,1)
      zb <- solve(A1) %*% b
      w <- c(head(zb,length(ret)))
      multi <- c(tail(zb,2))
    }else{
      Dmat <- 2*cov
      dvec <- rep(0,length(ret))
      Amat <- cbind(1,ret,diag(length(ret)),-diag(length(ret)))
      bvec <- c(1,p_ret,rep(min_w,length(ret)),rep(-max_w,length(ret)))
      result <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec, 
                                   Amat = Amat, bvec = bvec, meq = 2)
      w <- round(result$solution,8)
      multi <- result$Lagrangian
    }
    return <- c(w %*% ret)
    dsb <- sqrt(c(w %*% cov %*% w))
    names(w) <- colnames(cov)
    contri <- round(c(w %*% cov * w)/dsb^2,8)
    names(contri) <- names(w)
    ret_contr <- w*ret/return
    names(ret_contr) <- names(w)
    lista <- list(Return = return,Sd = dsb, Weights = w,
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multiplicadores = multi)
    return(lista)
  }
  f <- possibly(fun,NULL)
  pmap(list(p_ret = p_ret,short = short,rf = rf),
       f,cov = cov,ret = ret,min_w = min_w,max_w = max_w)
}
utility_optim <- function(ret,cov,delta = 5,rf = NA,short = T,
                          min_w = 0,max_w = 1){
  fun <- function(ret,cov,delta,short,min_w,max_w,rf){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- 1e-16
      ret <- c(ret,rf)
      colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
    }
    if(short){
      cova <- cbind(rbind(cov,1),1)
      cova[nrow(cova),ncol(cova)] <- 0
      coefs <- c(ret/delta,1)
      w <- solve(cova) %*% coefs
      w1 <- round(w[-length(w)],6)
      multiplier <- w[length(w)]
    }else{
      Dmat <- cov
      Amat <- cbind(1,diag(length(ret)),-diag(length(ret)))
      bvec <- c(1,rep(min_w,length(ret)),rep(-max_w,length(ret)))
      dvec <- ret/delta
      result <- solve.QP(Dmat,dvec,Amat,bvec,1)
      w1 <- round(result$solution,6)
      multiplier <- result$Lagrangian
    }
    w1 <- w1/sum(w1)
    names(w1) <- colnames(cov)
    sd <- sqrt(as.numeric(w1 %*% cov %*% w1))
    coef <- c(w1 %*% ret)
    utility <- coef-0.5*delta*sd^2
    contri <- round(c(w1 %*% cov * w1)/sd^2,6)
    names(contri) <- names(w1)
    ret_contr <- w1*ret/coef
    names(ret_contr) <- names(w1)
    lista <- list(Return = coef,
                  Sd = sd,
                  Weights = w1,
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  utility = utility,
                  Multiplicador = multiplier)
    return(lista)
  }
  pmap(list(delta = delta,
            short = short,
            rf = rf),
       fun,cov = cov,ret = ret,
       min_w = min_w,max_w = max_w)
}
sharpe_optim <- function(ret,cov,rf = 0,short = F){
  fun <- function(ret,cov,rf,short){
    if(short){
      inversa <- solve(cov)
      coefi <- ret-rf
      w <- c((inversa %*% coefi)/sum(inversa %*% coefi))
      multiplier <- 0
    }else{
      Dmat <- 2 * cov
      dvec <- rep(0,length(ret))
      er.excess <- ret - rf
      Amat <- cbind(er.excess,diag(length(ret)))
      bvec <- c(1, rep(0,length(ret)))
      result <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec, 
                                   Amat = Amat, bvec = bvec, meq = 1)
      w <- round(result$solution/sum(result$solution),8)
      multiplier <- result$Lagrangian
    }
    return <- c(w %*% ret)
    sd <- c(sqrt(w %*% cov %*% w))
    names(w) <- colnames(cov)
    contri <- round(c(w %*% cov * w)/sd^2,6)
    names(contri) <- names(w)
    ret_contr <- w*ret/return
    names(ret_contr) <- names(w)
    lista <- list(Return = return,
                  Sd = sd,
                  Weights = w,
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multiplicador = multiplier)
  }
  f <- possibly(fun,NULL)
  pmap(list(rf = rf,
            short = short),
       f,ret = ret,cov = cov)
}
port_min_risk <- function(ret,cov,short = T,
                          min_w = 0,max_w = 1){
  fun <- function(ret,cov,short,min_w,max_w){
    if(short){
      cova <- rbind(cbind(2*cov,1),1)
      cova[nrow(cova),ncol(cova)] <- 0
      inv_a <- solve(cova)
      b <- c(rep(0,length(ret)),1)
      w <- inv_a %*% b
      multi <- w[length(w)]
      w1 <- round(w[-length(w)]/sum(w[-length(w)]),6)
    }else{
      Dmat <- 2 * cov
      dvec <- rep.int(0, length(ret))
      Amat <- cbind(1,diag(length(ret)),-diag(length(ret)))
      bvec <- c(1,rep(min_w,length(ret)),rep(-max_w,length(ret)))
      result <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec, 
                                   Amat = Amat, bvec = bvec, meq = 1)
      w1 <- round(result$solution/sum(result$solution),6)
      multi <- result$Lagrangian
    }
    names(w1) <- colnames(cov)
    ret_m <- c(ret %*% w1)
    sd_m <- c(sqrt(w1 %*% cov %*% w1))
    contri <- round(c(w1 %*% cov * w1)/sd_m^2,6)
    names(contri) <- names(w1)
    ret_contr <- w1*ret/ret_m
    names(ret_contr) <- names(w1)
    lista <- list(Return = ret_m, Sd = sd_m,
                  Weights = w1, Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multiplicador = multi)
    return(lista)
  }
  pmap(list(short = short),
       fun,ret = ret,cov = cov,min_w = min_w,
       max_w = max_w)
}
kelly_port <- function(ret,cov,rf = 0,use_rf = F,
                       short = T,min_w = 0,max_w = 1){
  if(use_rf){
    nomes <- colnames(cov)
    cov <- cbind(rbind(cov,0),0)
    cov[length(cov)] <- 1e-16
    ret <- c(ret,rf)
    colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
  }
  fun <- function(ret,cov,rf,short,min_w,max_w){
    if(short){
      cova1 <- cbind(rbind(cov,1),1)
      cova1[nrow(cova1),ncol(cova1)] <- 0
      coefs <- c(ret-rf,1/(1+rf))
      w <- solve(cova1) %*% coefs
      w1 <- c((1+rf)*w[-length(w)])
      multiplier <- w[length(w)]
    }else{
      min_w <- min_w/(1+rf)
      max_w <- max_w/(1+rf)
      Dmat <- cov
      dvec <- ret-rf
      Amat <- cbind(1,diag(length(ret)),-diag(length(ret)))
      bvec <- c(1/(1+rf),rep(min_w,length(ret)),rep(-max_w,length(ret)))
      result <- solve.QP(Dmat,dvec,Amat,bvec,1)
      w1 <- round((1+rf)*result$solution,8)
      w1 <- w1/sum(w1)
      multiplier <- result$Lagrangian
    }
    return <- c(w1 %*% ret)
    sd <- c(sqrt(w1 %*% cov %*% w1))
    names(w1) <- colnames(cov)
    contri <- round(c(w1 %*% cov * w1)/sd^2,8)
    names(contri) <- names(w1)
    ret_contr <- w1*ret/return
    names(ret_contr) <- names(w1)
    lista <- list(Return = return,Sd = sd,Weights = w1,
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multiplicadores = multiplier)
    return(lista)
  }
  pmap(list(short = short,rf = rf),
       fun,ret = ret,cov = cov,min_w = min_w,max_w = max_w)
}
importance_portfolio <- function(ret,cov,lambda = 0.5,rf = NA,
                                 short = T,min_w = 0,max_w = 1){
  lambda <- lambda[lambda < 1 & lambda >= 0]
  if(length(lambda) == 0){
    return(NULL)
  }
  if(is.numeric(rf)){
    cov <- cbind(rbind(cov,0),0)
    cov[length(cov)] <- 1e-15
    ret <- c(ret,rf)
    colnames(cov) <- rownames(cov) <- names(ret) <- c(names(ret)[-length(ret)],"Rf")
  }
  fun <- function(ret,cov,lambda,short){
    if(short){
      sig <- cbind(rbind(cov,1),1)
      sig[length(sig)] <- 0
      coefs <- c(lambda*ret/(2*(1-lambda)),1)
      a <- solve(sig)%*%(coefs)
      w <- c(a[-length(a)])
      mult <- c(tail(a,1))
    }else{
      Dmat <- cov
      dvec <- lambda*ret/(2*(1-lambda))
      lb <- rep(min_w,ncol(cov))
      ub <- rep(-max_w,ncol(cov))
      Amat <- cbind(1,diag(ncol(cov)),-diag(ncol(cov)))
      bvec <- c(1,lb,ub)
      result <- solve.QP(Dmat,dvec,Amat,bvec,meq = 1)
      w <- round(result$solution/sum(result$solution),6)
      mult <- result$Lagrangian
    }
    names(w) <- colnames(cov)
    return <- c(w %*% ret)
    sd <- c(sqrt(w %*% cov %*% w))
    contri <- round(c(w %*% cov * w)/sd^2,6)
    ret_contr <- w*ret/return
    names(ret_contr) <- names(w)
    lista <- list(Return = return,
                  Sd = sd,
                  Weights = w,
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multipliers = mult)
    return(lista)
  }
  pmap(list(lambda = lambda,
            short = short),
       fun,ret = ret,cov = cov)
}
port_vol_target <- function(ret,cov,vol_target = min(sqrt(diag(cov))),
                            rf = NA,short = T,min_w = 0,max_w = 1){
  fun <- function(cov,ret,short,vol,min_w,max_w,rf){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- 1e-16
      ret <- c(ret,rf)
      colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
    }
    min_vol <- port_min_risk(ret,cov,short)[[1]]
    if(vol_target <= min_vol$Sd){
      return(min_vol)
    }else{
      vol_target <- vol_target^2
      pesos <- Variable(length(ret))
      risco <- quad_form(pesos,cov)
      objetivo <- Maximize(t(pesos) %*% ret)
      if(short){
        const <- list(sum(pesos) == 1,
                      risco <= vol_target)
      }else{
        const <- list(sum(pesos) == 1,
                      risco <= vol_target,
                      pesos >= min_w,
                      pesos <= max_w)
      }
      problema <- Problem(objetivo,const)
      solu <- solve(problema)
      w <- c(round(solu$getValue(pesos),6))
      w <- w/sum(w)
      names(w) <- colnames(cov)
      sd <- c(sqrt(w %*% cov %*% w))
      return <- c(w %*% ret)
      contri <- round(c(w %*% cov * w)/sd^2,6)
      names(contri) <- names(w)
      ret_contr <- w*ret/return
      names(ret_contr) <- names(w)
      lista <- list(Return = return,
                    Sd = sd,
                    Weights = w,
                    Var_contribution = contri,
                    Return_contribution = ret_contr)
      return(lista)
    }
  }
  pmap(list(vol = vol_target,
            short = short,rf = rf),
       fun,ret = ret,cov = cov,
       min_w = min_w,max_w = max_w)
}
max_diversification <- function(ret,cov,rf = NA,short = T,min_w = 0,max_w = 1){
  fun <- function(ret,cov,short,min_w,max_w,rf){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- 1e-16
      ret <- c(ret,rf)
      colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
    }
    vols <- sqrt(diag(cov))
    fun <- function(x){
      sqrt(x %*% cov %*% x)/(x %*% vols)
    }
    eq <- function(x){
      sum(x)
    }
    if(short){
      a <- solnp(rep(1/length(ret),length(ret)),fun = fun,eqfun = eq,eqB = 1,
                 control = list(trace = F))
    }else{
      a <- solnp(rep(1/length(ret),length(ret)),fun = fun,
                 eqfun = eq,eqB = 1,LB = rep(min_w,length(ret)),
                 UB = rep(max_w,length(ret)),control = list(trace = F))
    }
    w <- round(a$pars,6)
    w <- w/sum(w)
    return <- c(w %*% ret)
    sd <- c(sqrt(w %*% cov %*% w))
    diversification <- c((w %*% vols)/sd)
    names(w) <- colnames(cov)
    contri <- round(c(w %*% cov * w)/sd^2,6)
    names(contri) <- names(w)
    ret_contr <- w*ret/return
    names(ret_contr) <- names(w)
    lista <- list(Return = return,
                  Sd = sd,
                  Weights = w,
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Diversification = diversification,
                  Multiplier = c(a$lagrange))
    return(lista)
  }
  pmap(list(short = short,rf = rf),
       fun,ret = ret,cov = cov,
       min_w = min_w,max_w = max_w)
}
momentum_utility_portfolio <- function(ret,cov,returns,rf = NA,delta = 3,
                                       freq = 1/252,min_ret = -1,short = F,
                                       popSize = 100,maxiter = 1000,
                                       min_ret_tot = -0.6,optim = T,
                                       monitor = T,run = 50,elitism = 4){
  returns <- as.matrix(returns)
  if(is.numeric(rf)){
    rf1 <- (1+rf)^freq-1
    returns <- cbind(returns,rf1)
    cov <- cbind(rbind(cov,0),0)
    cov[length(cov)] <- 1e-16
    ret <- c(ret,rf)
    colnames(cov)[ncol(cov)] <- rownames(cov)[nrow(cov)] <- "Rf"
    names(ret)[length(ret)] <- "Rf"
  }
  func <- function(ret,cov,returns,delta,short = F){
    fun <- function(x){
      x <- x/sum(x)
      rets <- returns %*% x
      rets_tot <- cumprod(rets+1)-1
      if(min(rets) <= min_ret | min(rets_tot) <= min_ret_tot){
        return(-Inf)
      }
      mu <- c(x %*% ret)*freq
      sig <- sqrt(c(x %*% cov %*% x))*sqrt(freq)
      n <- nrow(returns)
      k <- sum((rets-mu)^4/(sig^2*(n-1)/n)^2)/n
      s <- sum((rets-mu)^3/sqrt(sig^2*(n-1)/n)^3)/n
      parte1 <- mu-0.5*delta*sig^2
      parte2 <- delta^2*sig^3*s/6
      parte3 <- -delta^3*sig^4*(k-3)/720
      (parte1+parte2+parte3)/freq
    }
    lb <- rep(0,length(ret))
    ub <- rep(1,length(ret))
    if(short){
      lb <- rep(-50,length(ret))
      ub <- rep(50,length(ret))
    }
    otimo <- utility_optim(medias,cova,delta,rf,short,min_w,max_w)[[1]]
    ga_res <- ga("real-valued",fitness = fun,lower = lb,upper = ub,
                 popSize = popSize,maxiter = maxiter,
                 run = run,optim = optim,names = colnames(cov),
                 elitism = elitism,monitor = monitor,suggestions = otimo$Weights)
    w <- summary(ga_res)$solution[1,]
    w <- round(w/sum(w),6)
    utility <- fun(w)
    modelo <- ga_res
    sd <- sqrt(c(w %*% cov %*% w))
    return <- c(w %*% ret)
    contri <- round(c(w %*% cov *w)/sd^2,6)
    names(w) <- names(contri) <- colnames(cov)
    ret_contr <- w*ret/return
    names(ret_contr) <- names(w)
    lista <- list(Return = return,
                  Sd = sd,
                  Weights = w,Utility = utility,
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Model = modelo)
    return(lista)
  }
  pmap(list(delta = delta,
            short = short),
       func,returns = returns,cov = cov,ret = ret)
}
equal_risk_cont <- function(ret,cov,p_ret = NA,rf = NA,
                            short = T,min_w = 0,max_w = 1,
                            expected_contr = NA){
  nomes <- colnames(cov)
  fun1 <- function(ret,cov,p_ret,short,min_w,max_w,expected_contr,rf){
    if(is.numeric(rf)){
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- 1e-16
      colnames(cov)[ncol(cov)] <- rownames(cov)[nrow(cov)] <- "Rf"
      nomes <- colnames(cov)
      ret <- c(ret,rf)
      names(ret) <- nomes
    }
    if(!is.numeric(expected_contr)){
      otimo <- 1/length(ret)
    }else{
      otimo <- expected_contr
    }
    pars <- rep(1/length(ret),length(ret))
    fun <- function(x){
      contr <- c(x %*% cov * x)
      covv <- c(x %*% cov %*% x)
      perc <- contr/covv
      sum((perc-otimo)^2)
    }
    lb <- NULL
    ub <- NULL
    if(!short){
      lb <- rep(min_w,length(ret))
      ub <- rep(max_w,length(ret))
    }
    if(!is.na(p_ret)){
      eq <- function(x){
        c(x %*% ret,sum(x))
      }
      a <- solnp(pars,fun,eqfun = eq,eqB = c(p_ret,1),
                 LB = lb,UB = ub,control = list(trace = F))
    }else{
      eq <- function(x){
        c(sum(x))
      }
      a <- solnp(pars,fun,eqfun = eq,eqB = 1,LB = lb,
                 UB = ub,control = list(trace = F))
    }
    w <- round(a$pars,6)
    contr <- c(w %*% cov * w)
    sd <- c(w %*% cov %*% w)
    contr <- round(contr/sd,6)
    sd <- sqrt(sd)
    retorno <- c(w %*% ret)
    names(w) <- nomes
    ret_contr <- w*ret/retorno
    names(ret_contr) <- names(w)
    lista <- list(Return = retorno,
                  Sd = sd,
                  Weights = w,
                  Var_contribution = contr,
                  Return_contribution = ret_contr)
    return(lista)
  }
  pmap(list(p_ret = p_ret,
            short = short,rf = rf),
       fun1,ret = ret,cov = cov,
       min_w = min_w,max_w = max_w,expected_contr = expected_contr)
}
garch_weights <- function(cova,returns,quantias,min_return = NA,
                          max_risk = NA,tx_livre = 0,short = T,
                          target_vol = T){
  data <- data.frame(min_return,max_risk,tx_livre,target_vol,
                     short)
  min_return <- data$min_return
  max_risk <- data$max_risk
  tx_livre <- data$tx_livre
  target_vol <- data$target_vol
  short <- data$short
  f <- function(quantias,cova,tx_livre,short,max_risk,
                min_return,target_vol,control){
    medias <- quantias
    covari <- cova
    otimo <- sharpe_optim(medias,covari,tx_livre, short = short)[[1]]
    if(otimo$Sd >= max_risk & is.numeric(max_risk)){
      otimo <- port_min_risk(quantias,covari,short = short)
      if(max_risk >= otimo$Sd & target_vol){
        otimo <- port_vol_target(medias,covari,max_risk,short)[[1]]
      }
    }
    if(otimo$Return < min_return & is.numeric(min_return)){
      otimo <- portfront(quantias,covari,min_re,short = short)[[1]]
    }
    pesos <- otimo$Weights
    names(pesos) <- names(quantias)
    sds <- otimo$Sd
    rets <- otimo$Return
    lista <- list(pesos = pesos,
                  sds = sds,
                  rets = rets)
    return(lista)
  }
  lista_cova <- replicate(dim(cova)[3],list())
  r <- replicate(dim(cova)[3],list())
  for(i in 1:length(lista_cova)){
    lista_cova[[i]] <- cova[,,i]
    r[[i]] <- returns[i,]
  }
  a <- pmap(list(cova = lista_cova,
                 tx_livre = tx_livre,
                 max_risk = max_risk,
                 min_return = min_return,
                 short = short,
                 target_vol = target_vol),
            f,quantias = quantias,control = control)
  pesos <- lapply(a,function(a)a$pesos)
  soma <- pmap(list(p = pesos,
                    r = r),
               function(p,r)sum(p*r)) %>%
    unlist
  pesos <- bind_rows(pesos)
  sd <- sapply(a,function(a)a$sds)
  expected <- sapply(a,function(a)a$rets)
  lista <- list(Weights = pesos,
                Return = soma,
                Sd = sd,
                Expected_ret = expected)
  return(lista)
}
portfolios <- function(ret,cov,returns,delta = 5,p_ret = 0.2,
                       rf = NA,use_rf = T,short = T,min_w = 0,max_w = 1,
                       lambda = 0.3,optimizer = c("sharpe","utility","min vol",
                                                  "kelly","frontier","importance")){
  returns <- as.matrix(returns)
  optimizer <- tolower(optimizer)
  fun <- function(ret,cov,returns,delta,p_ret,rf,use_rf,optimizer,short,min_w,max_w){
    if(length(dim(cov)) == 2){
      if(optimizer == "sharpe"){
        rf <- ifelse(!is.numeric(rf),0,rf)
        otimo <- sharpe_optim(ret,cov,rf = rf,short = short)[[1]]
      }else if(optimizer == "utility"){
        otimo <- utility_optim(ret,cov,delta,rf,short,min_w,max_w)[[1]]
      }else if(optimizer == "min vol"){
        otimo <- port_min_risk(ret,cov,short,min_w,max_w)[[1]]
      }else if(optimizer == "kelly"){
        rf <- ifelse(!is.numeric(rf),0,rf)
        otimo <- kelly_port(ret,cov,rf,use_rf,short,min_w,max_w)[[1]]
      }else if(optimizer == "frontier"){
        otimo <- portfront(ret,cov,p_ret,rf,short,min_w,max_w)[[1]]
      }else if(optimizer == "importance"){
        otimo <- importance_portfolio(ret,cov,lambda,rf,short,min_w,max_w)[[1]]
      }
      pesos <- otimo$Weights
      sds <- otimo$Sd
      if(length(pesos) == ncol(returns)+1){
        rf1 <- (1+rf)^(1/252)-1
        returns <- cbind(returns,Rf = rf1)
      }
      rets <- returns %*% pesos
      ret_e <- otimo$Return
    }else{
      if(optimizer == "sharpe"){
        rf <- ifelse(!is.numeric(rf),0,rf)
        otimo <- apply(cov,3,sharpe_optim,ret = ret,rf = rf,short = short)
      }else if(optimizer == "utility"){
        otimo <- apply(cov,3,utility_optim,ret = ret,
                       rf = rf,short = short,delta = delta,
                       min_w = min_w,max_w = max_w)
      }else if(optimizer == "min vol"){
        otimo <- apply(cov,3,port_min_risk,ret = ret,
                       short = short,min_w = min_w,max_w = max_w)
      }else if(optimizer == "kelly"){
        rf <- ifelse(!is.numeric(rf),0,rf)
        otimo <- apply(cov,3,kelly_port,ret = ret,rf = rf,short = short,
                       use_rf = use_rf,min_w = min_w,max_w = max_w)
      }else if(optimizer == "frontier"){
        otimo <- apply(cov,3,portfront,ret = ret,rf = rf,short = short,
                       min_w = min_w,max_w = max_w,p_ret = p_ret)
      }else if(optimizer == "importance"){
        otimo <- apply(cov,3,importance_portfolio,ret = ret,lambda = lambda,rf = rf,
                       short = short,min_w = min_w,max_w = max_w)
      }
      otimo <- lapply(otimo,function(x)x[[1]])
      pesos <- sapply(otimo,function(x)x$Weights) %>%
        t
      sds <- sapply(otimo,function(x)x$Sd)
      if(ncol(pesos) == ncol(returns)+1){
        rf1 <- (1+rf)^(1/252)-1
        returns <- cbind(returns,Rf = rf1)
      }
      rets <- sapply(1:nrow(returns),function(i)pesos[i,] %*% returns[i,])
      ret_e <- sapply(otimo,function(x)x$Return)
    }
    lista <- list(Weights = as.data.frame(pesos) %>% mutate(portfolio = optimizer),
                  Sd = sds,Return_Expec = ret_e,
                  Returns = rets)
    return(lista)
  }
  l <- pmap(list(delta = delta,optimizer = optimizer,
                 short = short,min_w = min_w,max_w = max_w,p_ret = p_ret),
            fun,ret = ret,cov = cov,returns = returns,rf = rf,use_rf = use_rf)
  names(l) <- optimizer
  return(l)
}