equacoes <- function(x,y){
  cofator <- function(matrix,i,j){
    cofator <- (-1)^(i+j)*det(as.matrix(matrix[-i,-j]))
    return(cofator)
  }
  matriz_cofatores <- function(matrix){
    matriz <- matrix(nrow = nrow(matrix), ncol = ncol(matrix))
    for(i in 1:nrow(matrix)){
      for(j in 1:ncol(matrix)){
        matriz[i,j] <- cofator(matrix,i,j)
      }
    }
    return(matriz)
  }
  inversa <- function(matrix){
    cof <- t(matriz_cofatores(matrix))
    dete <- det(matrix)
    inversa <- cof/dete
    colnames(inversa) <- colnames(matrix)
    rownames(inversa) <- rownames(matrix)
    return(inversa)
  }
  order <- length(y)-1
  mat <- matrix(NA,nrow = order+1,ncol = order+1)
  for(i in 1:ncol(mat)){
    mat[i,] <- x[i]^(order:0)
  }
  pars <- NA
  try(pars <- as.numeric(solve(mat) %*% y))
  if(all(is.na(pars))){
    pars <- as.numeric(inversa(mat) %*% y)
  }
  names(pars) <- paste0("x^",order:0)
  fun <- function(x){
    fun1 <- function(x){
      sum(pars*x^(order:0))
    }
    sapply(x,fun1)
  }
  list(Fun = fun,
       parameters = pars,
       error_estimation = mean(abs(fun(x)-y)))
}
newton_raphson <- function(fun,index = 1,deriv = NA,x0 = 1,n = 100,tol = 1e-8,...){
  fun <- match.fun(fun)
  start <- Sys.time()
  if(is.na(deriv)){
    deriv <- Deriv::Deriv(fun)
  }else{
    deriv <- match.fun(deriv)
  }
  xs <- numeric(n)
  xs[1] <- x0
  der <- numeric(n)
  der[1] <- Inf
  for(i in 2:n){
    der[i] <- deriv(xs[i-1],...)[index]
    xs[i] <- xs[i-1]-(fun(xs[i-1],...)[index]/der[i])
    y <- fun(xs[i],...)[index]
    y1 <- fun(xs[i-1],...)[index]
    if(abs(y) <= tol | y == y1){
      der <- der[-((i+1):n)]
      xs <- xs[-((i+1):n)]
      xs <- na.omit(xs)
      end <- Sys.time()
      dife <- end-start
      lista <- list(root = tail(xs,1),
                    n_iter = i,
                    iter = xs,
                    f_x = fun(tail(xs,1),...)[index],
                    der = der,
                    time = dife)
      return(lista)
    }
  }
  xx <- xs[which.min(abs(der))]
  der <- der[-1]
  end <- Sys.time()
  dife <- end-start
  lista <- list(root = tail(xs,1),
                n_iter = i,
                iter = xs,
                f_x = fun(tail(xs,1),...)[index],
                der = der,
                time = dife)
  return(lista)
}
bisection <- function(fun,interval,n = 100, tol = 1e-8){
  start <- Sys.time()
  xmin <- min(interval)
  xmax <- max(interval)
  fmin <- fun(xmin)
  fmax <- fun(xmax)
  fval <- c(fmin,fmax)
  if(sign(fmin) == sign(fmax)){
    stop("Signs must be oposite")
  }
  N <- 2
  cc <- rep(NA,n)
  cc[1] <- (xmin+xmax)/2
  while(N <= n){
    cc[N] <- (xmin+xmax)/2
    f_c <- fun(cc[N])
    if(f_c == 0 | (xmax-xmin)/2 < tol){
      if(N != n){
        cc <- cc[-((N+1):n)]
      }
      end <- Sys.time()
      dife <- end-start
      lista <- list(root = tail(cc,1),
                    n_iter = N-1,
                    iters = cc[-1],
                    f.root = f_c,
                    time = dife)
      return(lista)
    }
    if(sign(f_c) == sign(xmin)){
      xmin <- cc[N]
    }else{
      xmax <- cc[N]
    }
    N <- N+1
  }
  end <- Sys.time()
  dife <- end-start
  lista <- list(root = tail(cc,1),
                n_iter = N-1,
                iters = cc[-1],
                f.root = f_c,
                time = dife)
  return(lista)
}
integral <- function(fun = NA,interval = NA,n = 1e5,...,x = c(),y = c()){
  l_x <- length(x)
  l_y <- length(y)
  b <- max(interval)
  a <- min(interval)
  if(!any(is.infinite(interval))){
    if(n >= 100){
      nn <- 2:n-1
      soma <- sum(fun(a+nn*((b-a)/n),...))+fun(b,...)/2
      tot <- (b-a)/n*soma
      return(tot)
    }else{
      (b-a)/6*(fun(a,...)+4*fun(mean(interval),...)+fun(b,...))
    }
  }else{
    integrate(fun,lower = a, upper = b,... = ...)$value
  }
}
gradient_descent <- function(fun,pars,n_max = 1000,
                             tol = 1e-3,f = `-`,
                             method = "Richardson",
                             gradiente = NULL,
                             l_rate = 0.01,...){
  if(length(pars) == 1){
    return(newton_raphson(fun,x0 = pars,n = n_max, tol = tol,... = ...))
  }
  if(!is.null(gradiente)){
    grad1 <- function(vec,...){gradiente(vec,...)}
  }
  library(numDeriv)
  pars1 <- matrix(ncol = length(pars),nrow = n_max)
  pars1[1,] <- pars
  start <- Sys.time()
  dif <- 1
  for(i in 2:n_max){
    length(dif) <- i-1
    if(!is.null(gradiente)){
      grads <- grad1(pars1[i-1,],... = ...)
    }else{
      grads <- numDeriv::grad(fun,pars1[i-1,],... = ...,method = method)
    }
    dif[i] <- sum(abs(grads))
    pars1[i,] <- f(pars1[i-1,],l_rate*grads)
    if(is.na(dif[i])){
      stop("The function diverges")
    }else{
      if(dif[i] < tol){
        end <- Sys.time()
        dife <- end-start
        lista <- list(f_v = fun(pars1[i,],...),
                      coeff = pars1[i,],
                      n_iters = i-1,
                      iters = pars1[1:i,],
                      differencial = dif[-1],
                      time = dife)
        return(lista)
      }
    }
  }
  mini <- which.min(dif[-1])+1
  end <- Sys.time()
  dife <- end-start
  lista <- list(f_v = fun(pars1[mini,],...),
                coeff = pars1[mini,],
                n_iters = i-1,
                iters = pars1,
                differencial = dif[-1],
                time = dife)
  return(lista)
}
polynomial_reg <- function(x,y,degree){
  suppressPackageStartupMessages(library(tidyverse))
  fun <- function(x,y,degree){
    x <- unlist(x)
    y <- unlist(y)
    degree <- unlist(degree)
    phi <- matrix(x,nrow = length(x), ncol = degree)
    for(i in 1:ncol(phi)){
      phi[,i] <- phi[,i]^(i-1)
    }
    a <- NA
    a <- as.numeric(solve(t(phi) %*% phi) %*% t(phi) %*% y)
    z <- sapply(x,function(x)sum(a*x^(0:(degree-1))))
    error <- mean((y-z)^2)
    lista <- list(coef = a,
                  fit = z,
                  mean_error = error)
    return(lista)
  }
  f <- possibly(fun,NULL,quiet = F)
  listas <- pmap(list(x = list(x),
                      y = list(y),
                      degree = degree),
                 f)
  len <- sapply(listas,function(x)length(x) == 3)
  listas[len]
}
deriv_num <- function(fun,x,h = 3e-9,...){
  f <- function(x,h,...){
    (fun(x+h,...)-fun(x,...))/h
  }
  unlist(purrr::map(x,f,h = h,... = ...))
}
weierstrass <- function(x,n,a = 2){
  f <- function(x,n,a){
    n <- 1:n
    y <- sum(sin(pi*(n^a)*x)/(pi*(n^a)))
  }
  sapply(x,f,n = n,a = a)
}