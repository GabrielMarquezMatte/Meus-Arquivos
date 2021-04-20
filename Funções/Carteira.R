source("https://raw.githubusercontent.com/GabrielMarquezMatte/Meus-Arquivos/master/Fun%C3%A7%C3%B5es/Coleta%20de%20dados.R",encoding = "UTF-8")
apply.roll <- function(x,w = 100,FUN = sum){
  n <- numeric(length(x))
  for(i in 1:length(x)){
    if(i >= (w+1)){
      n[i] <- FUN(x[(i-w):i])
    }else{
      n[i] <- FUN(x[1:i])
    }
  }
  return(n)
}
retorno_carteira <- function(valor,aporte,
                             compras,dividendos = 0,
                             ret_dividendos = T){
  if(length(dividendos) == 1)dividendos <- rep(dividendos,length(valor))
  if(length(aporte) == 1)aporte <- rep(aporte,length(valor))
  if(length(compras) == 1)compras <- rep(compras,length(valor))
  dividendos[is.na(dividendos)] <- 0
  if(!ret_dividendos)dividendos <- rep(0,length(valor))
  retorno <- numeric(length(valor))
  if(length(valor) > 1){
    for(i in 2:length(retorno)){
      if(round(valor[i],100) <= 0 & aporte[i] == 0){
        retorno[i] <- 0
      }else if(aporte[i] < 0 & round(valor[i],20) == 0 & round(valor[i-1],20) > 0){
        retorno[i] <- (-aporte[i]+dividendos[i])/valor[i-1]-1
      }else if(aporte[i] != 0 & round(valor[i],20) == 0&
               round(valor[i-1],20) == 0 & compras[i] != 0){
        retorno[i] <- -((aporte[i]-dividendos[i])/compras[i])
      }else if(aporte[i] == 0 & valor[i] > 0 & valor[i-1] < 0){
        retorno[i] <- 0
      }else{
        retorno[i] <- (valor[i]-valor[i-1]-aporte[i]+dividendos[i])/
          (valor[i-1]+aporte[i])
      }
      if(retorno[i] <= -1){
        retorno[i] <- (aporte[i]+valor[i])/valor[i-1]
      }
    }
  }
  if(round(valor[1],100) == 0 & aporte[1] != 0 & compras[1] != 0){
    retorno[1] <- -(aporte[1]/compras[1])
  }else{
    retorno[1] <- (valor[1]+dividendos[1])/aporte[1]-1
  }
  return(retorno)
}
preco_medio <- function(n_acoes,price){
  n_acoes1 <- cumsum(n_acoes)
  n_acoes2 <- as.numeric(n_acoes)
  n_acoes3 <- ifelse(n_acoes2 > 0, 0, -n_acoes2)
  n_acoes2 <- ifelse(n_acoes2 < 0, 0, n_acoes2)
  precos <- as.numeric(price)
  pm_compra <- cumsum(precos*n_acoes2)/cumsum(n_acoes2)
  pm_venda <- cumsum(precos*n_acoes3)/cumsum(n_acoes3)
  zero <- which(n_acoes1 == 0)
  if(length(zero) > 1){
    for(i in 2:length(zero)){
      zero1 <- zero[i-1]
      pm_compra[-(1:zero1)] <- cumsum(n_acoes2[-(1:zero1)]*precos[-(1:zero1)])/
        cumsum(n_acoes2[-(1:zero1)])
      pm_venda[-(1:zero1)] <- cumsum(n_acoes3[-(1:zero1)]*precos[-(1:zero1)])/
        cumsum(n_acoes3[-(1:zero1)])
    }
  }
  pm_compra[is.na(pm_compra)] <- 0
  pm_venda[is.na(pm_venda)] <- 0
  cbind(pm_compra,pm_venda)
}
lucro_realizado <- function(price,preco_med,n_acoes,aporte,valor_tot){
  aporte1 <- ifelse(aporte < 0,aporte,0)
  lucro_realizado = -(cumsum(n_acoes)*(price-preco_med))/
    valor_tot*aporte1
  if(length(n_acoes) > 1){
    for(i in 2:length(n_acoes)){
      if(sum(n_acoes[1:i]) == 0&aporte[i] != 0){
        lucro_realizado[i] <- (price[i]-preco_med[i])*
          cumsum(n_acoes[1:i])[i-1]
      }else{
        lucro_realizado[i] <- lucro_realizado[i]
      }
    }
  }
  return(lucro_realizado)
}
ajustar <- function(data.frame,last_split = NA){
  data <- data.frame %>%
    mutate(n_acoes = as.numeric(n_acoes),
           price = as.numeric(price),
           date = as.Date(date)) %>%
    arrange(date)
  if(is.na(last_split) & is.null(data$ajustado)){
    last_split <- tail(data$date,1)
  }else if(is.na(last_split) & !is.null(data$ajustado)){
    last_split <- tail(data$ajustado,1)
  }
  splits <- tq_get(head(data$symbol,1),get = "splits",from = last_split)
  if(!is.na(splits)){
    if(tail(splits$date,1) < last_split){
      splits <- NA
    }
  }
  if(is.na(splits)){
    return(data)
  }else{
    data <- data %>%
      mutate(n_acoes = ifelse(date <= last_split, n_acoes/prod(splits$value),n_acoes),
             price = ifelse(date <= last_split, price*prod(splits$value),price),
             ajustado = tail(splits$date,1)+1)
    return(data)
  }
}
clean <- function(val){
  if(!all(val$date != unique(val$date))){
    val <- val %>%
      group_by(date) %>%
      summarise(date = dplyr::last(date),symbol = dplyr::last(symbol),
                close = dplyr::last(close),n_acoes = sum(n_acoes),
                dividendos = sum(dividendos),aporte = sum(aporte),
                compras = sum(compras),vendas = sum(vendas),
                valor_tot = dplyr::last(valor_tot),
                preco_med = dplyr::last(preco_med),
                lucro = tail(lucro,1),
                lucro_realizado = tail(lucro_realizado,1),
                retorno = prod(retorno+1)-1) %>%
      suppressMessages()
  }
  return(val)
}
carteira <- function(data.framee,Quandl_api_key = NA,
                     valores_atual = T, be.quiet = T,
                     do_cache = T,bench = "^BVSP",
                     ret_dividendos = T){
  #data.frame(symbol,date,price,n_acoes,moeda = NA ou moeda de preferência)
  options(warn = -1)
  pacotes <- c("tidyverse","BatchGetSymbols","GetTDData",
               "Quandl","glue","httr","rvest","tidyquant","rbcb")
  instalados <- pacman::p_isinstalled(pacotes)
  faltam <- pacotes[!instalados]
  if(length(faltam) != 0){
    install.packages(faltam)
  }
  for(i in pacotes){
    suppressPackageStartupMessages(require(i,character.only = T))
  }
  if(!is.null(data.framee$symbol[1])){
    message(glue("Consolidando {data.framee$symbol[1]}"))
  }else if(!is.null(data.framee$cnpj[1])){
    message(glue("Consolidando {data.framee$cnpj[1]}"))
  }else{
    message(glue("Consolidando {data.framee$option[1]}"))
  }
  val <- get_precos(data.framee,Quandl_api_key = Quandl_api_key,
                    valores_atual = valores_atual,be.quiet = be.quiet,
                    do_cache = do_cache,bench = bench)
  val <- val %>%
    mutate(value = zoo::na.fill(value,0) %>% as.numeric*moeda,
           n_acoes = zoo::na.fill(n_acoes,0) %>% as.numeric,
           value = value*cumsum(n_acoes),
           close = close*moeda,
           price = zoo::na.fill(price,0) %>% as.numeric,
           price = ifelse(is.na(price_moeda),price*moeda,price*price_moeda),
           aporte = price*n_acoes,
           aporte = zoo::na.fill(aporte,0) %>% as.numeric,
           compras = ifelse(aporte > 0, aporte, 0),
           vendas = ifelse(aporte < 0, aporte, 0),
           valor_tot = cumsum(n_acoes)*close,
           moeda = NULL,
           preco_med = preco_medio(n_acoes,price)[,1],
           lucro = cumsum(n_acoes)*(close-preco_med),
           lucro_realizado = lucro_realizado(price,preco_med,
                                             n_acoes,aporte,valor_tot),
           retorno = retorno_carteira(valor_tot,aporte,compras,value,ret_dividendos)) %>%
    select(symbol,date,close,n_acoes,dividendos = value,
           aporte,compras,vendas,valor_tot,preco_med,lucro,lucro_realizado,
           price,retorno) %>%
    clean
  val <- val %>%
    na.omit() %>%
    mutate(retorno = retorno,
           retorno_tot = cumprod(retorno+1)-1,
           n_acoes = cumsum(n_acoes),
           lucro_realizado = zoo::na.fill(lucro_realizado,0),
           lucro_realizado = cumsum(lucro_realizado)+cumsum(dividendos),
           lucro = lucro)
  return(val %>%
           select(symbol,date,close,n_acoes,dividendos,
                  aporte,compras,vendas,valor_tot,preco_med,lucro,lucro_realizado,
                  retorno,retorno_tot))
}
carteira_tot <- function(lista, Quandl_api_key = NA, 
                         valores_atual = T,be.quiet = T,
                         do_cache = T,bench = "^BVSP",
                         ret_dividendos = T){
  options(warn = -1)
  start <- Sys.time()
  cart <- lapply(lista,carteira,Quandl_api_key = Quandl_api_key, 
                 valores_atual = valores_atual,be.quiet = be.quiet,
                 do_cache = do_cache,ret_dividendos = ret_dividendos)
  tot <- bind_rows(cart)
  total <- tot %>%
    group_by(date) %>%
    summarise(aporte = sum(aporte),
              compras = sum(compras),
              vendas = sum(vendas),
              valor_tot = sum(valor_tot),
              dividendos = sum(dividendos),
              lucro = sum(lucro, na.rm = T),
              lucro_realizado = sum(lucro_realizado))
  total <- total %>%
    mutate(retorno = retorno_carteira(valor_tot,aporte,
                                      compras,dividendos,ret_dividendos),
           retorno_tot = cumprod(retorno+1)-1)
  pesos <- tot %>%
    group_by(date) %>%
    summarise(symbol = symbol,pesos = valor_tot/sum(valor_tot),
              lucro,lucro_realizado,
              valor_tot, n_acoes,dividendos,close,
              aporte,compras,vendas,preco_med) %>%
    group_by(symbol, date) %>%
    summarise(symbol = dplyr::last(symbol), 
              pesos = sum(pesos),
              valor_tot = sum(valor_tot),
              lucro = sum(lucro),
              n_acoes = sum(n_acoes),
              dividendos = sum(dividendos),
              aporte = sum(aporte),
              compras = sum(compras),
              vendas = sum(vendas),
              preco_med = sum(preco_med*n_acoes)/sum(n_acoes),
              close = max(close),
              lucro_realizado = sum(lucro_realizado)) %>%
    group_by(symbol) %>%
    summarise(date,pesos,valor_tot,
              n_acoes = n_acoes,
              dividendos,
              aporte,compras,vendas,
              preco_med,close,
              retorno = retorno_carteira(valor_tot,aporte,
                                         compras,dividendos,ret_dividendos),
              retorno_tot = cumprod(retorno+1)-1,
              lucro, lucro_realizado,
              retorno_ativo = close/close[1]-1) %>%
    group_by(date) %>%
    mutate(retorno_na_carteira = pesos/(1+retorno),
           retorno_na_carteira = retorno_na_carteira/sum(retorno_na_carteira),
           retorno_na_carteira = (retorno_na_carteira*retorno)) %>%
    suppressMessages()
  total <- total %>%
    left_join(pesos %>%
                group_by(date) %>%
                summarise(diversificacao = 1-sum(pesos^2)),
              "date") %>%
    mutate(drawdown = (c(1,retorno_tot+1)/cummax(c(1,retorno_tot+1))-1)[-1],
           cotas = valor_tot/(retorno_tot+1))
  pesos <- pesos
  end <- Sys.time()
  dife <- difftime(end,start,units = "secs")/length(lista)
  print(glue("Tempo medio de consolidacao: {dife} segundos"))
  listaa <- list(retornos = total %>%
                   mutate(lucro_tot = lucro+lucro_realizado,
                          dividend_yield = apply.roll(dividendos,365)/valor_tot),
                 pesos = pesos %>%
                   group_by(symbol) %>%
                   mutate(lucro_tot = lucro+lucro_realizado,
                          dividend_yield = apply.roll(dividendos,365)/valor_tot),
                 tempo = dife)
  return(listaa)
}