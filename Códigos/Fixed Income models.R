par_cir_estimate <- function(r){
  rss <- function(pars,r){
    a <- pars[1]
    b <- pars[2]
    rt <- numeric(length(r))
    rt[1] <- (r[1]-b)/sqrt(b)
    for(i in 2:length(rt)){
      rt[i] <- (r[i]-r[i-1]-a*(b-r[i-1]))/sqrt(r[i-1])
    }
    return(rt)
  }
  rss1 <- function(pars,r){
    sum(rss(pars,r)^2)
  }
  otimo <- optim(c(1,1),rss1,r = r)
  a <- otimo$par[1]
  b <- otimo$par[2]
  d <- sd(rss(c(a,b),r))
  parameters <- c(b,a,d)
  names(parameters) <- c("mu","lambda","sigma")
  return(parameters)
}
fit_cir <- function(r){
  pars <- par_cir_estimate(r)
  a <- pars["lambda"]
  b <- pars["mu"]
  sigma <- pars["sigma"]
  fit <- numeric(length(r))
  fit[1] <- b
  for(i in 2:length(r)){
    fit[i] <- r[i-1]+a*(b-r[i-1])+sigma*sqrt(r[i-1])
  }
  residuo <- r-fit
  lista <- list(coef = pars,
                fitted.values = fit,
                residuals = residuo)
  return(lista)
}
simchi <- function(n,nu,lambda){
  if(nu > 1){
    rchisq(1, nu - 1) + (rnorm(1) + sqrt(lambda))^2
  }else{
    rchisq(n, nu + 2 * rpois(1, 0.5 * lambda))
  }
}
sim_cir <- function(sigma = NA,lambda = NA,mu = NA,m,h,r0 = 0.01,n = 1,
                    r = NULL){
  f <- function(sigma,lambda,mu,m,h,r0,
                r){
    pars <- c(sigma,lambda,mu)
    if(all(!is.numeric(pars))){
      pars <- fit_cir(r)$coef
      sigma <- pars["sigma"]
      lambda <- pars["lambda"]
      mu <- pars["mu"]
    }
    sigma2 <- sigma^2
    nu <- 4*lambda*mu/sigma2
    phi <- exp(-lambda*m)
    if(length(phi) == 1){
      phi <- rep(phi,h)
    }
    omega <- sigma2*(1-phi)/(4*lambda)
    r <- mat.or.vec(h, 1)
    r[1] <- r0
    for(t in 2:h){
      x <- r[t-1]/omega[t]
      D <- x*phi[t]
      tt <- simchi(1,nu, D)
      r[t] <- omega[t]*tt
    }
    return(r)
  }
  pars <- c(sigma,lambda,mu)
  if(all(!is.numeric(pars))){
    pars <- fit_cir(r)$coef
    sigma <- pars["sigma"]
    lambda <- pars["lambda"]
    mu <- pars["mu"]
  }
  sim <- replicate(n,f(sigma,lambda,mu,m,h,r0))
  return(sim)
}
vasicek_fit <- function(r){
  fit <- function(pars,r){
    fi <- numeric(length(r))
    mu <- pars[1]
    lambda <- pars[2]
    fi[1] <- mu
    for(i in 2:length(fi)){
      fi[i] <- r[i-1]+lambda*(mu-r[i-1])
    }
    return(fi)
  }
  f <- function(pars){
    fi <- fit(pars,r)
    residual <- r-fi
    return(mean(residual^2))
  }
  pars <- optim(c(1,1),f)
  mu <- pars$par[1]
  lambda <- pars$par[2]
  fitted <- fit(pars$par,r)
  residuos <- fitted-r
  sigma <- sd(residuos)
  par <- c(mu = mu,lambda = lambda,sigma = sigma)
  lista <- list(coef = par,
                fitted.values = fitted,
                residuals = residuos,
                hessian = pars$hessian)
  return(lista)
}
sim_vasicek <- function(r = NULL,h = 100,n = 1,r0 = NULL,
                        sigma = NULL,mu = NULL,lambda = NULL,
                        pars = c(sigma,mu,lambda)){
  f <- function(r,h,r0,sigma,mu,lambda,pars = c(sigma,mu,lambda)){
    if(any(is.null(pars))){
      pars <- vasicek_fit(r)
      mu <- pars$coef["mu"]
      lambda <- pars$coef["lambda"]
      sigma <- pars$coef["sigma"]
      sim <- numeric(h)
      sim[1] <- ifelse(is.null(r0),tail(r,1),r0)
      erro <- rnorm(h,sd = sigma)
      for(i in 2:h){
        sim[i] <- sim[i-1]+lambda*(mu-sim[i-1])+erro[i]
      }
      return(sim)
    }else{
      sim <- numeric(h)
      sim[1] <- ifelse(is.null(r0),tail(r,1),r0)
      erro <- rnorm(h,sd = sigma)
      for(i in 2:h){
        sim[i] <- sim[i-1]+lambda*(mu-sim[i-1])+erro[i]
      }
      return(sim)
    }
  }
  pars <- vasicek_fit(r)
  sigma <- pars$coef["sigma"]
  mu <- pars$coef["mu"]
  lambda <- pars$coef["lambda"]
  pars <- c(sigma,mu,lambda)
  mat <- replicate(n,f(r,h,r0,sigma,mu,lambda,pars))
  return(mat)
}