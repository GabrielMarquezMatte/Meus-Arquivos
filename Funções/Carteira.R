source("https://raw.githubusercontent.com/GabrielMarquezMatte/Meus-Arquivos/master/Fun%C3%A7%C3%B5es/Coleta%20de%20dados.R",encoding = "UTF-8")
completar <- function(x){
  xx <- numeric(length(x))
  xx[1] <- na.omit(x)[1]
  xx[-1] <- x[-1]
  if(length(x) > 1){
    for(i in 2:length(x)){
      if(is.na(xx[i])){
        xx[i] <- xx[i-1]
      }
    }
  }
  return(xx)
}
retorno_carteira <- function(valor,aporte,compras){
  retorno <- numeric(length(valor))
  if(length(valor) > 1){
    for(i in 2:length(retorno)){
      if(round(valor[i],3) == 0 & aporte[i] == 0){
        retorno[i] <- 0
      }else if(aporte[i] < 0 & round(valor[i],3) == 0 & round(valor[i-1],3) != 0){
        retorno[i] <- (-aporte[i])/valor[i-1]-1
      }else if(aporte[i] != 0 & round(valor[i],3) == 0 &
               round(valor[i-1],3) == 0 & compras[i] != 0){
        retorno[i] <- -(aporte[i]/compras[i])
      }else{
        retorno[i] <- (valor[i]-valor[i-1]-aporte[i])/
          (valor[i-1]+aporte[i])
      }
    }
  }
  if(round(valor[1],3) == 0 & aporte[1] != 0 & compras[1] != 0){
    retorno[1] <- -(aporte[1]/compras[1])
  }else{
    retorno[1] <- valor[1]/aporte[1]-1
  }
  return(retorno)
}
preco_medio <- function(n_acoes,price){
  n_acoes1 <- cumsum(n_acoes)
  n_acoes2 <- as.numeric(n_acoes)
  n_acoes2 <- ifelse(n_acoes2 < 0, 0, n_acoes2)
  precos <- as.numeric(price)
  pm <- cumsum(precos*n_acoes2)/cumsum(n_acoes2)
  tem <- T
  zero <- which(n_acoes1 == 0)
  try(tem <- zero == length(n_acoes),silent = T)
  if(length(zero) > 1){
    for(i in 2:length(zero)){
      zero1 <- zero[i-1]
      pm[-(1:zero1)] <- cumsum(n_acoes2[-(1:zero1)]*precos[-(1:zero1)])/
        cumsum(n_acoes2[-(1:zero1)])
    }
  }
  pm
}
carteira <- function(data.framee,Quandl_api_key = NA,
                     valores_atual = T, be.quiet = T){
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
  val <- get_precos(data.framee,Quandl_api_key,valores_atual,be.quiet = be.quiet)
  val <- val %>%
    mutate(value = zoo::na.fill(value,0) %>% as.numeric*moeda,
           n_acoes = zoo::na.fill(n_acoes,0) %>% as.numeric,
           value = value*cumsum(n_acoes),
           close = close*moeda,
           price = zoo::na.fill(price,0) %>% as.numeric*moeda,
           aporte = price*n_acoes,
           aporte = zoo::na.fill(aporte,0) %>% as.numeric,
           compras = ifelse(aporte > 0, aporte, 0),
           vendas = ifelse(aporte < 0, aporte, 0),
           valor_tot = cumsum(n_acoes)*close,
           moeda = NULL,
           preco_med = preco_medio(n_acoes,price),
           lucro = cumsum(n_acoes)*(close-preco_med),
           aporte1 = ifelse(aporte <0, aporte, 0),
           lucro_realizado = -(cumsum(n_acoes)*(price-preco_med))/
             valor_tot*aporte1,
           retorno = retorno_carteira(valor_tot,aporte,compras)) %>%
    select(symbol,date,close,n_acoes,dividendos = value,
           aporte,compras,vendas,valor_tot,preco_med,lucro,lucro_realizado,
           price,retorno)
  if(nrow(val) > 1){
    for(i in 2:nrow(val)){
      if(sum(val$n_acoes[1:i]) == 0 & 
         val$aporte[i] != 0){
        val$lucro_realizado[i] <- (val$price[i]-val$preco_med[i])*
          cumsum(val$n_acoes[1:i])[i-1]
      }else{
        val$lucro_realizado[i] <- val$lucro_realizado[i]
      }
    }
  }
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
  val <- val %>%
    na.omit() %>%
    mutate(retorno = retorno,
           retorno_tot = cumprod(retorno+1)-1,
           n_acoes = cumsum(n_acoes),
           lucro_realizado = zoo::na.fill(lucro_realizado,0),
           lucro_realizado = cumsum(lucro_realizado),
           lucro = lucro+cumsum(dividendos))
  return(val %>%
           select(symbol,date,close,n_acoes,dividendos,
                  aporte,compras,vendas,valor_tot,preco_med,lucro,lucro_realizado,
                  retorno,retorno_tot))
}
carteira_tot <- function(lista, Quandl_api_key = NA, valores_atual = T,be.quiet = T){
  options(warn = -1)
  start <- Sys.time()
  cart <- lapply(lista,carteira,Quandl_api_key, 
                 valores_atual = valores_atual,be.quiet = be.quiet)
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
    mutate(retorno = retorno_carteira(valor_tot,aporte,compras),
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
              retorno = retorno_carteira(valor_tot,aporte,compras),
              retorno_tot = cumprod(retorno+1)-1,
              lucro, lucro_realizado,
              retorno_ativo = close/close[1]-1,
              retorno_na_carteira = retorno*pesos) %>%
    suppressMessages()
  total <- total %>%
    left_join(pesos %>%
                group_by(date) %>%
                summarise(diversificacao = 1-sum(pesos^2)),
              "date") %>%
    mutate(drawdown = (c(1,retorno_tot+1)/cummax(c(1,retorno_tot+1))-1)[-1],
           cotas = valor_tot/(retorno_tot+1))
  pesos <- pesos %>%
    filter(pesos != 0)
  end <- Sys.time()
  dife <- (end-start)/length(lista)
  print(glue("Tempo medio de consolidacao:{round(dife,5)} segundos"))
  listaa <- list(retornos = total,
                 pesos = pesos,
                 tempo = dife)
  return(listaa)
}