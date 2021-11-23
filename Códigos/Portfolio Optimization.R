options(warn = -1)
pacotes <- c("tidyverse","CVXR","quadprog","Rsolnp","GA","quadprogXT")
a <- sapply(pacotes,require,character.only = T)
if(!all(a)){
  nao_inst <- pacotes[!a]
  b <- readline(paste0("Download packages: ",nao_inst,"? [Y/N]"))
  b <- toupper(b)
  if(b == "Y"){
    install.packages(nao_inst)
    sapply(pacotes,require,character.only = T)
  }
}
rm(pacotes,a,b,nao_inst)
pesos_carteira <- function(n_acoes, n_sim, short = F, p_min = -2, p_max = 2){
  pesos <- matrix(0,n_acoes,n_sim)
  if(isFALSE(short)){
    pesos <- apply(pesos,2,rf, df1 = 1, df2 = 1)
  }else{
    pesos <- apply(pesos,2,rnorm)
    
  }
  pesos <- apply(pesos,2,function(x)x/sum(x))
  colsub <- apply(pesos,2,function(x)!any(x >= p_max| x <= p_min))
  pesos <- pesos[,colsub]
  return(pesos)
}
portfront <- function(ret,cov,p_ret,rf = NA,
                      short = T,min_w = 0,max_w = 1,
                      leverage = NA){
  fun <- function(ret,cov,p_ret,short = T,min_w,max_w,rf,leverage){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- .Machine$double.eps
      ret <- c(ret,rf)
      if(!is.null(nomes)){
        colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
      }else{
        colnames(cov) <- rownames(cov) <- names(ret) <- c(rep(NA,ncol(cov)-1),"Rf")
      }
    }
    if(short){
      if(is.numeric(leverage)){
        dmat <- 2*cov
        dvec <- rep(0,length(ret))
        Amat <- cbind(1,ret)
        bvec <- c(1,p_ret)
        AmatPos <- cbind(rep(-1,2*ncol(cov)))
        bvecPos <- -leverage
        res <- solveQPXT(Dmat = dmat,dvec = dvec,Amat = Amat,bvec = bvec,
                         AmatPosNeg = AmatPos,bvecPosNeg = bvecPos,meq = 2)
        w <- round(head(res$solution,length(ret)),8)
        multi <- res$Lagrangian
      }else{
        A1 <- 2*cov
        A1 <- rbind(rbind(A1,ret),1)
        A1 <- cbind(A1,c(ret,rep(0,2)))
        A1 <- cbind(A1,c(rep(1,nrow(cov)), 0,0))
        b <- c(rep(0,length(ret)),p_ret,1)
        zb <- solve(A1,b)
        w <- head(zb,length(ret))
        multi <- tail(zb,2)
      }
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
                  Leverage = sum(abs(w)),
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multiplicadores = multi)
    return(lista)
  }
  f <- possibly(fun,NULL)
  pmap(list(p_ret = p_ret,short = short,rf = rf,
            leverage = leverage),
       f,cov = cov,ret = ret,min_w = min_w,max_w = max_w)
}
utility_optim <- function(ret,cov,delta = 5,rf = NA,short = T,
                          min_w = 0,max_w = 1,leverage = NA){
  fun <- function(ret,cov,delta,short,min_w,max_w,rf,leverage){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- .Machine$double.eps
      ret <- c(ret,rf)
      if(!is.null(nomes)){
        colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
      }else{
        colnames(cov) <- rownames(cov) <- names(ret) <- c(rep(NA,ncol(cov)-1),"Rf")
      }
    }
    if(short){
      if(is.numeric(leverage)){
        Dmat <- cov
        Amat <- cbind(rep(1,ncol(cov)))
        bvec <- 1
        dvec <- ret/delta
        AmatPos <- cbind(rep(-1,2*ncol(cov)))
        bvecPos <- -leverage
        res <- solveQPXT(Dmat = Dmat,dvec = dvec,Amat = Amat,bvec = bvec,
                         AmatPosNeg = AmatPos,bvecPosNeg = bvecPos,meq = 1)
        w1 <- head(res$solution,ncol(cov))
        w1 <- round(w1,6)
        multiplier <- res$Lagrangian
      }else{
        cova <- cbind(rbind(cov,1),1)
        cova[length(cova)] <- 0
        coefs <- c(ret/delta,1)
        w <- solve(cova) %*% coefs
        w1 <- round(w[-length(w)],6)
        multiplier <- w[length(w)]
      }
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
                  Leverage = sum(abs(w1)),
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Utility = utility,
                  Multiplicador = multiplier)
    return(lista)
  }
  pmap(list(delta = delta,
            short = short,
            rf = rf,leverage = leverage),
       fun,cov = cov,ret = ret,
       min_w = min_w,max_w = max_w)
}
sharpe_optim <- function(ret,cov,rf = 0,short = F){
  fun <- function(ret,cov,rf,short){
    if(short){
      cov1 <- cbind(rbind(2*cov,ret-rf),ret-rf)
      cov1[length(cov1)] <- 0
      coefi <- c(rep(0,length(ret)),1)
      w1 <- solve(cov1,coefi)
      w <- w1[-length(w1)]/sum(w1[-length(w1)])
      multiplier <- -w1[length(w1)]
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
                  Leverage = sum(abs(w)),
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Sharpe = (return-rf)/sd,
                  Multiplicador = multiplier)
  }
  f <- possibly(fun,quiet = T,NULL)
  pmap(list(rf = rf,
            short = short),
       f,ret = ret,cov = cov)
}
port_min_risk <- function(ret,cov,short = T,
                          min_w = 0,max_w = 1,
                          leverage = NA){
  fun <- function(ret,cov,short,min_w,max_w,leverage){
    if(short){
      if(is.numeric(leverage)){
        Dmat <- 2*cov
        dvec <- rep(0, length(ret))
        Amat <- cbind(rep(1,ncol(cov)))
        bvec <- 1
        AmatPos <- cbind(rep(-1,2*ncol(cov)))
        bvecPos <- -leverage
        result <- solveQPXT(Dmat = Dmat, dvec = dvec, 
                            Amat = Amat, bvec = bvec, meq = 1,
                            AmatPosNeg = AmatPos,bvecPosNeg = bvecPos)
        w1 <- head(result$solution,ncol(cov))
        w1 <- round(w1/sum(w1),6)
        multi <- result$Lagrangian
      }else{
        cova <- rbind(cbind(2*cov,1),1)
        cova[nrow(cova),ncol(cova)] <- 0
        b <- c(rep(0,length(ret)),1)
        w <- solve(cova,b)
        multi <- w[length(w)]
        w1 <- head(w,ncol(cov))
        w1 <- round(w1/sum(w1),6)
      }
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
                  Weights = w1,
                  Leverage = sum(abs(w1)),
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multiplicador = multi)
    return(lista)
  }
  pmap(list(short = short,leverage = leverage),
       fun,ret = ret,cov = cov,min_w = min_w,
       max_w = max_w)
}
kelly_port <- function(ret,cov,rf = 0,use_rf = F,
                       short = T,min_w = 0,max_w = 1,
                       leverage = NA){
  if(use_rf){
    nomes <- colnames(cov)
    cov <- cbind(rbind(cov,0),0)
    cov[length(cov)] <- .Machine$double.eps
    ret <- c(ret,rf)
    if(!is.null(nomes)){
      colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
    }else{
      colnames(cov) <- rownames(cov) <- names(ret) <- c(rep(NA,ncol(cov)-1),"Rf")
    }
  }
  fun <- function(ret,cov,rf,short,min_w,max_w,leverage){
    if(short){
      if(is.numeric(leverage)){
        Dmat <- cov
        dvec <- ret-rf
        Amat <- cbind(rep(1,ncol(cov)))
        bvec <- 1/(1+rf)
        AmatPos <- cbind(rep(-1,2*ncol(cov)))
        bvecPos <- -leverage/(1+rf)
        result <- solveQPXT(Dmat = Dmat,dvec = dvec,Amat = Amat,bvec = bvec,meq = 1,
                            AmatPosNeg = AmatPos,bvecPosNeg = bvecPos)
        w1 <- head(result$solution,ncol(cov))
        w1 <- round((1+rf)*w1,8)
        w1 <- w1/sum(w1)
        multiplier <- result$Lagrangian
      }else{
        cova1 <- cbind(rbind(cov,1),1)
        cova1[nrow(cova1),ncol(cova1)] <- 0
        coefs <- c(ret-rf,1/(1+rf))
        w <- solve(cova1,coefs)
        w1 <- (1+rf)*w[-length(w)]
        multiplier <- w[length(w)]
      }
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
                  Leverage = sum(abs(w1)),
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Multiplicadores = multiplier)
    return(lista)
  }
  pmap(list(short = short,rf = rf,leverage = leverage),
       fun,ret = ret,cov = cov,min_w = min_w,max_w = max_w)
}
importance_portfolio <- function(ret,cov,lambda = 0.5,rf = NA,
                                 short = T,min_w = 0,max_w = 1,
                                 leverage = NA){
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
  fun <- function(ret,cov,lambda,short,leverage){
    if(short){
      if(is.numeric(leverage)){
        dmat <- cov
        dvec <- lambda*ret/(2*(1-lambda))
        amat <- rep(1,ncol(cov))
        bvec <- 1
        AmatPos <- cbind(rep(-1,2*ncol(cov)))
        bvecPos <- -leverage
        res <- solveQPXT(Dmat = dmat,dvec = dvec,Amat = cbind(amat),bvec = bvec,
                         AmatPosNeg = AmatPos,bvecPosNeg = bvecPos)
        w <- round(res$solution[1:ncol(cov)],6)
        mult <- res$Lagrangian
      }else{
        sig <- cbind(rbind(cov,1),1)
        sig[length(sig)] <- 0
        coefs <- c(lambda*ret/(2*(1-lambda)),1)
        a <- solve(sig,coefs)
        w <- round(a[-length(a)],6)
        mult <- tail(a,1)
      }
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
                  Leverage = sum(abs(w)),
                  Var_contribution = contri,
                  Return_contribution = ret_contr,
                  Importance = lambda*return-(1-lambda)*sd^2,
                  Multipliers = mult)
    return(lista)
  }
  pmap(list(lambda = lambda,
            short = short,leverage = leverage),
       fun,ret = ret,cov = cov)
}
min_track_error <- function(returns,benchmark,ret = NA,cov = NA,
                            p_ret = NA,short = T,min_w = 0,max_w = 1,
                            rf = NA,leverage = NA){
  returns <- as.matrix(returns)
  benchmark <- c(benchmark)
  if(!is.na(rf)){
    rf <- rf/252
    returns <- cbind(returns,rf)
  }
  n <- ncol(returns)
  if(all(is.na(ret))){
    ret <- colMeans(returns)*252
  }
  if(all(is.na(cov))){
    cov <- cov(returns)*252
  }
  if(!is.na(rf) & length(ret) != ncol(returns) & length(ret) == ncol(cov)){
    cov <- cbind(rbind(cov,0),0)
    cov[length(cov)] <- 1e-15
    ret <- c(ret,rf*252)
  }
  fun <- function(short,min_w,max_w,p_ret,leverage){
    vec <- c(t(benchmark) %*% returns)
    mat <- t(returns) %*% returns
    if(short){
      if(is.numeric(leverage)){
        Amat <- cbind(rep(1,n))
        bvec <- 1
        meq <- 1
        if(!is.na(p_ret)){
          Amat <- cbind(ret,Amat)
          bvec <- c(p_ret,bvec)
          meq <- 2
        }
        AmatPos <- cbind(rep(-1,2*ncol(mat)))
        bvecPos <- -leverage
        solu <- solveQPXT(Dmat = mat,dvec = vec,Amat = Amat,bvec = bvec,meq = meq,
                          AmatPosNeg = AmatPos,bvecPosNeg = bvecPos)
        w1 <- head(solu$solution,ncol(mat))
        multiplier <- solu$Lagrangian
      }
      vec <- c(vec,1)
      mat <- cbind(rbind(mat,1),1)
      mat[length(mat)] <- 0
      if(!is.na(p_ret)){
        vec <- c(vec,p_ret)
        mat <- cbind(rbind(mat,ret),ret)
        mat[n+(1:2),n+(1:2)] <- 0
      }
      w <- solve(mat) %*% vec
      multiplier <- -w[-c(1:n)]
      w1 <- head(w[-length(w)],n)
    }else{
      Amat <- cbind(1,diag(n),-diag(n))
      bvec <- c(1,rep(min_w,n),-rep(max_w,n))
      meq <- 1
      if(!is.na(p_ret)){
        Amat <- cbind(ret,Amat)
        bvec <- c(p_ret,bvec)
        meq <- 2
      }
      solu <- solve.QP(mat,vec,Amat,bvec,meq = meq)
      w1 <- solu$solution
      multiplier <- solu$Lagrangian
    }
    w1 <- round(w1/sum(w1),6)
    rets <- returns %*% w1
    erro <- sqrt(sum((rets-benchmark)^2)/(length(rets)-1))
    return <- c(w1 %*% ret)
    sd <- c(sqrt(w1 %*% cov %*% w1))
    contr <- c(w1 %*% cov * w1)/(sd^2)
    contr <- round(contr,8)
    ret_contr <- (w1*ret)/return
    names(w1) <- names(contr) <- colnames(returns)
    lista <- list(Return = return,
                  Sd = sd,
                  Weights = w1,
                  Leverage = sum(abs(w1)),
                  Var_contribution = contr,
                  Return_contribution = ret_contr,
                  Multiplicador = multiplier,
                  Tracking_error = erro)
    return(lista)
  }
  pmap(list(short = short,
            min_w = min_w,max_w = max_w,
            p_ret = p_ret,leverage = leverage),
       fun)
}
port_vol_target <- function(ret,cov,vol_target = min(sqrt(diag(cov))),
                            rf = NA,short = T,min_w = 0,max_w = 1,
                            leverage = NA){
  fun <- function(cov,ret,short,vol,min_w,max_w,rf,leverage){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- .Machine$double.eps
      ret <- c(ret,rf)
      if(!is.null(nomes)){
        colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
      }else{
        colnames(cov) <- rownames(cov) <- names(ret) <- c(rep(NA,ncol(cov)-1),"Rf")
      }
    }
    min_vol <- port_min_risk(ret,cov,short,min_w = min_w,
                             max_w = max_w,leverage = leverage)[[1]]
    if(vol_target <= min_vol$Sd){
      return(min_vol)
    }else{
      vol_target <- vol_target^2
      pesos <- Variable(length(ret))
      risco <- quad_form(pesos,cov)
      objetivo <- Maximize(t(pesos) %*% ret)
      if(short){
        if(is.numeric(leverage)){
          const <- list(sum(pesos) == 1,
                        risco <= vol_target,
                        norm(pesos) <= leverage)
        }else{
          const <- list(sum(pesos) == 1,
                        risco <= vol_target)
        }
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
                    Leverage = sum(abs(w)),
                    Var_contribution = contri,
                    Return_contribution = ret_contr)
      return(lista)
    }
  }
  pmap(list(vol = vol_target,
            short = short,rf = rf,leverage = leverage),
       fun,ret = ret,cov = cov,
       min_w = min_w,max_w = max_w)
}
max_diversification <- function(ret,cov,rf = NA,short = T,min_w = 0,max_w = 1){
  fun <- function(ret,cov,short,min_w,max_w,rf){
    if(is.numeric(rf)){
      nomes <- colnames(cov)
      cov <- cbind(rbind(cov,0),0)
      cov[length(cov)] <- .Machine$double.eps
      ret <- c(ret,rf)
      if(!is.null(nomes)){
        colnames(cov) <- rownames(cov) <- names(ret) <- c(nomes,"Rf")
      }else{
        colnames(cov) <- rownames(cov) <- names(ret) <- c(rep(NA,ncol(cov)-1),"Rf")
      }
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
                  Leverage = sum(abs(w)),
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
                                       monitor = T,run = 50,elitism = 4,
                                       log = T){
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
      rets_tot <- ifelse(log,exp(cumsum(rets)),cumprod(rets+1))
      draw <- rets_tot/cummax(rets_tot)-1
      rets_tot <- rets_tot-1
      if(min(rets) <= min_ret | min(draw) <= min_ret_tot){
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
                  Weights = w,Leverage = sum(abs(w)),
                  Utility = utility,
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
                            expected_contr = NA,p_sd = NA){
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
    if(!is.na(p_ret)|!is.na(p_sd)){
      if(!is.na(p_sd)&!is.na(p_ret)){
        eq <- function(x){
          c(x %*% ret,sum(x),x %*% cov %*% x)
        }
        a <- solnp(pars,fun,eqfun = eq,eqB = c(p_ret,1,p_sd^2),
                   LB = lb,UB = ub,control = list(trace = F))
      }else if(!is.na(p_sd)&is.na(p_ret)){
        eq <- function(x){
          c(sum(x),x %*% cov %*% x)
        }
        a <- solnp(pars,fun,eqfun = eq,eqB = c(1,p_sd^2),
                   LB = lb,UB = ub,control = list(trace = F))
      }else if(!is.na(p_ret)&is.na(p_sd)){
        eq <- function(x){
          c(sum(x),x %*% ret)
        }
        a <- solnp(pars,fun,eqfun = eq,eqB = c(1,p_ret),
                   LB = lb,UB = ub,control = list(trace = F))
      }
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
                  Leverage = sum(abs(w)),
                  Var_contribution = contr,
                  Return_contribution = ret_contr)
    return(lista)
  }
  pmap(list(p_ret = p_ret,
            short = short,rf = rf),
       fun1,ret = ret,cov = cov,
       min_w = min_w,max_w = max_w,expected_contr = expected_contr)
}