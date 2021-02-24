get_atual <- function(x){
  pacotes <- c("tidyverse", "glue","rvest")
  for(i in pacotes){
    suppressPackageStartupMessages(require(i,character.only = T))
  }
  funcao <- function(x){
    url <- glue("https://finance.yahoo.com/quote/{x}/history?p={x}")
    ht <- read_html(url)
    tabela <- html_table(ht)[[1]][1,]
    tabela <- tabela %>%
      mutate(Open = gsub(",","",Open, fixed = T),
             `Close*` = gsub(",","",`Close*`, fixed = T),
             High = gsub(",","",High, fixed = T),
             Low = gsub(",","",Low, fixed = T),
             `Adj Close**` = gsub(",","",`Adj Close**`, fixed = T),
             Open = gsub("-",NA,Open, fixed = T),
             `Close*` = gsub("-",NA,`Close*`, fixed = T),
             High = gsub("-",NA,High, fixed = T),
             Low = gsub("-",NA,Low, fixed = T),
             `Adj Close**` = gsub("-",NA,`Adj Close**`, fixed = T),
             Volume = gsub("-",NA,`Adj Close**`, fixed = T))
    tabela1 <- tq_get(x,from = Sys.Date()-5)
    try(if(nrow(tabela1) >= 2){
      for(i in 2:nrow(tabela1)){
        if(is.na(tabela1$close[i])){
          tabela1$close[i] <- tabela1$close[i-1]
        }
      }
    },silent = T)
    tabela <- tabela %>%
      mutate(symbol = x,
             Open = as.numeric(Open),
             `Close*` = as.numeric(`Close*`),
             High = as.numeric(High),
             Low = as.numeric(Low),
             `Adj Close**` = as.numeric(`Adj Close**`)) %>%
      dplyr::select(symbol,Date,Open,High,Low,`Close*`,Volume,`Adj Close**`)
    colnames(tabela) <- c("symbol","date","open","high","low",
                          "close","volume","adjusted")
    decisao <- T
    try(decisao <- round(tabela1$close[nrow(tabela1)],2)
        != tabela$close,silent = T)
    if(is.na(decisao)){decisao <- F}
    if(decisao){
      tabela <- tabela  %>%
        mutate(volume = NA, date = Sys.Date())
      return(tabela)
    }else{
      return()
    }
  }
  data <- data.frame()
  for(i in 1:length(x)){
    fun <- funcao(x[i])
    data <- rbind(fun,data)
  }
  return(data)
}
informacoes_fundo <- function(cnpj){
  pacotes <- c("httr","rvest","tidyverse")
  for(i in pacotes){
    suppressPackageStartupMessages(require(i,character.only = T))
  }
  cnpj1 <- gsub("\\D","",cnpj)
  url <- paste0("https://www.okanebox.com.br/api/fundoinvestimento/info/",cnpj1,"/")
  g <- GET(url)
  cont <- content(g)[[1]]
  return(cont)
}
valor_cota <- function(cnpj, data_inicio = Sys.Date()-365, data_fim = Sys.Date()){
  pacotes <- c("httr","rvest","tidyverse","lubridate")
  for(i in pacotes){
    suppressPackageStartupMessages(require(i,character.only = T))
  }
  vl_cota <- function(cnpj,data_inicio = Sys.Date()-365, data_fim = Sys.Date()){
    cnpj1 <- gsub("\\D","",cnpj,fixed = F)
    url <- paste0("https://www.okanebox.com.br/api/fundoinvestimento/hist/",cnpj1)
    data_inicio <- as.Date(data_inicio)
    data_inicio <- format(data_inicio,"%Y%m%d")
    data_fim <- as.Date(data_fim)
    data_fim <- format(data_fim,"%Y%m%d")
    url <- paste0(url,"/",data_inicio,"/",data_fim)
    g <- GET(url)
    cont <- content(g)
    cota <- matrix(nrow = length(cont), ncol = 6)
    for(i in 1:nrow(cota)){
      cota[i,1] <- as.Date(cont[[i]]$DT_COMPTC)
      cota[i,2] <- as.numeric(cont[[i]]$VL_TOTAL)
      cota[i,3] <- as.numeric(cont[[i]]$VL_QUOTA)
      cota[i,4] <- as.numeric(cont[[i]]$VL_PATRIM_LIQ)
      cota[i,5] <- as.numeric(cont[[i]]$CAPTC_DIA)
      cota[i,6] <- as.numeric(cont[[i]]$RESG_DIA)
    }
    cota <- apply(cota,2,as.numeric)
    nome <- informacoes_fundo(cnpj)$DENOM_SOCIAL
    cota <- data.frame(cnpj = cnpj,
                       nome = nome,
                       date = as_date(cota[,1]),
                       vl_total = as.numeric(cota[,2]),
                       vl_quota = as.numeric(cota[,3]),
                       vl_patrim_liq = as.numeric(cota[,4]),
                       captc_dia = as.numeric(cota[,5]),
                       resg_dia = as.numeric(cota[,6]))
  }
  cota <- vl_cota(cnpj[1], data_inicio, data_fim)
  if(length(cnpj) > 1){
    for(i in 2:length(cnpj)){
      cota1 <- vl_cota(cnpj[i], data_inicio, data_fim)
      cota <- rbind(cota,cota1)
    }
  }
  return(cota)
}
retorno_carteira <- function(valor,aporte){
  retorno <- numeric(length(valor))
  for(i in 2:length(retorno)){
    if(round(valor[i],2) == 0 & aporte[i] == 0){
      retorno[i] <- 0
    }else if(aporte[i] < 0 & round(valor[i],2) == 0){
      retorno[i] <- (-aporte[i])/valor[i-1]-1
    }else{
      retorno[i] <- (valor[i]-valor[i-1]-aporte[i])/
        (valor[i-1]+aporte[i])
    }
  }
  retorno[1] <- valor[1]/aporte[1]-1
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
carteira <- function(data.framee){
  #data.frame(symbol,date,price,n_acoes,moeda = NA ou moeda de preferência)
  options(warn = -1)
  pacotes <- c("tidyverse","tidyquant","GetTDData","Quandl")
  for(i in pacotes){
    suppressPackageStartupMessages(require(i,character.only = T))
  }
  if(is.null(data.framee$moeda)){
    data.framee$moeda <- NA
  }
  if(is.null(data.framee$vencimento)){
    data.framee$vencimento <- NA
  }
  if(is.null(data.framee$benchmark)){
    data.framee$benchmark <- 1
  }
  if(is.null(data.framee$symbol)){
    data.framee$symbol <- NA
  }
  if(is.null(data.framee$cnpj)){
    data.framee$cnpj <- NA
  }
  data.framee$vencimento <- as.Date(data.framee$vencimento)
  data.framee$date <- as.Date(data.framee$date)
  dataa <- data.framee[1,]
  tota <- data.frame(date = seq(dataa$date,
                                Sys.Date(),"1 day"))
  if(is.na(dataa$symbol)){
    message(glue::glue("Consolidando {dataa$cnpj}"))
  }else{
    message(glue::glue("Consolidando {dataa$symbol}"))
  }
  if(!(dataa$symbol %in% c("NTN-B","NTN-B Principal","LTN","LFT","NTN-F","CDI")) &
     !is.na(dataa$symbol)){
    valores <- tq_get(dplyr::first(data.framee$symbol), 
                      from = dplyr::first(data.framee$date)-5)
    atual <- get_atual(dataa$symbol)
    valores <- rbind(valores,atual)
    valores <- left_join(tota,valores,"date") %>%
      mutate(symbol = dataa$symbol)
    for(j in 2:ncol(valores)){
      for(i in 2:nrow(valores)){
        if(is.na(valores[i,j])){
          valores[i,j] <- valores[i-1,j]
        }
      }
    }
    dividendos <- tq_get(dplyr::first(data.framee$symbol), 
                         from = dplyr::first(data.framee$date)-5,
                         get = "dividends")
    if(!is.na(dataa$moeda)){
      moeda <- tq_get(dplyr::first(data.framee$moeda),
                      from = dplyr::first(data.framee$date)-5)
      moeda_atual <- get_atual(dataa$moeda)
      moeda <- rbind(moeda,moeda_atual)
      moeda <- moeda %>%
        select(date,close)
      moeda <- left_join(tota,moeda,"date")
      for(i in 2:nrow(moeda)){
        if(is.na(moeda[i,2])){
          moeda[i,2] <- moeda[i-1,2]
        }
      }
    }else{
      moeda <- data.frame(date = valores$date,
                          moeda = 1)
    }
    colnames(moeda) <- c("date","moeda")
    if(is.null(dim(dividendos))| any(dim(dividendos) == 0)){
      dividendos <- data.frame(symbol = data.framee$symbol,
                               date = data.framee$date,
                               value = 0,
                               price = data.framee$price,
                               n_acoes = data.framee$n_acoes)
      val <- left_join(valores,dividendos,c("symbol","date"))
      val <- left_join(val,moeda,c("date"))
    }else{
      val <- valores %>%
        left_join(data.framee %>%
                    select(symbol,date,price,n_acoes),by = c("symbol","date")) %>%
        left_join(dividendos, by = c("symbol","date")) %>%
        left_join(moeda,"date")
    }
  }else if(dataa$symbol %in% c("NTN-B","NTN-B Principal","LTN","LFT","NTN-F")){
    download.TD.data(asset.codes = dataa$symbol)
    valores <- read.TD.files(asset.codes = dataa$symbol,
                             maturity = format(dataa$vencimento,"%d%m%y")) %>%
      filter(ref.date >= dataa$date-2) %>%
      select(ref.date,price.bid)
    colnames(valores) <- c("date","close")
    valores <- left_join(tota,valores,"date")
    for(i in 2:nrow(valores)){
      if(is.na(valores[i,2])){
        valores[i,2] <- valores[i-1,2]
      }
    }
    val <- valores %>%
      left_join(data.framee %>%
                  select(symbol, date, price, n_acoes,moeda), "date") %>%
      mutate(value = 0,
             moeda = 1,
             symbol = dataa$symbol)
  }
  try(if(dataa$symbol == "CDI"){
    Quandl.api_key("sPcpUeyLAs8vUHiAZpdc")
    valores <- Quandl("BCB/12", start_date = dataa$date)
    valores <- valores[nrow(valores):1,]  
    valores <- valores %>%
      summarise(date = Date,
                close = cumprod((Value/100)*dataa$benchmark+1)*1000) %>%
      suppressMessages()
    valores <- left_join(tota,valores,"date")
    for(i in 2:nrow(valores)){
      if(is.na(valores[i,2])){
        valores[i,2] <- valores[i-1,2]
      }
    }
    val <- valores %>%
      left_join(data.framee %>%
                  select(symbol, date, price, n_acoes,moeda), "date") %>%
      mutate(value = 0,
             moeda = 1,
             symbol = dataa$symbol)
  }, silent = T)
  if(!is.na(dataa$cnpj)){
    valores <- valor_cota(dataa$cnpj, dataa$date) %>%
      select(date,cnpj,vl_quota) %>%
      rename(close = vl_quota)
    valores <- left_join(tota,valores,"date")
    for(i in 2:nrow(valores)){
      for(j in 2:ncol(valores)){
        if(is.na(valores[i,j])){
          valores[i,j] <- valores[i-1,j]
        }
      }
    }
    val <- valores %>%
      left_join(data.framee %>%
                  select(cnpj, date, price, n_acoes, moeda),c("date","cnpj")) %>%
      mutate(value = 0,
             moeda = 1) %>%
      rename(symbol = cnpj)
  }
  val <- val %>%
    mutate(value = zoo::na.fill(value,0) %>% as.numeric*moeda,
           n_acoes = zoo::na.fill(n_acoes,0) %>% as.numeric,
           value = value*cumsum(n_acoes),
           close = close*moeda,
           price = zoo::na.fill(price,0) %>% as.numeric*moeda,
           aporte = price*n_acoes,
           aporte = zoo::na.fill(aporte,0) %>% as.numeric,
           valor_tot = cumsum(n_acoes)*close,
           moeda = NULL,
           preco_med = preco_medio(n_acoes,price),
           lucro = cumsum(n_acoes)*(close-preco_med),
           aporte1 = ifelse(aporte <0, aporte, 0),
           lucro_realizado = -(cumsum(n_acoes)*(price-preco_med))/
             valor_tot*aporte1) %>%
    select(symbol,date,close,n_acoes,dividendos = value,
           aporte,valor_tot,preco_med,lucro,lucro_realizado,
           price)
  for(i in 2:nrow(val)){
    if(sum(val$n_acoes[1:i]) == 0 & 
       val$aporte[i] != 0){
      val$lucro_realizado[i] <- (val$price[i]-val$preco_med[i])*
        cumsum(val$n_acoes[1:i])[i-1]
    }else{
      val$lucro_realizado[i] <- val$lucro_realizado[i]
    }
  }
  if(!all(val$date != unique(val$date))){
    val <- val %>%
      group_by(date) %>%
      summarise(date = dplyr::last(date),symbol = dplyr::last(symbol),
                close = dplyr::last(close),n_acoes = sum(n_acoes),
                dividendos = sum(dividendos),aporte = sum(aporte),
                valor_tot = dplyr::last(valor_tot),
                preco_med = dplyr::last(preco_med),
                lucro = tail(lucro,1),
                lucro_realizado = tail(lucro_realizado,1)) %>%
      suppressMessages()
  }
  val <- val %>%
    mutate(retorno = retorno_carteira(valor_tot,aporte),
           retorno_tot = cumprod(retorno+1)-1,
           n_acoes = cumsum(n_acoes),
           lucro_realizado = zoo::na.fill(lucro_realizado,0),
           lucro_realizado = cumsum(lucro_realizado))
  return(val %>%
           select(symbol,date,close,n_acoes,dividendos,
                  aporte,valor_tot,preco_med,lucro,lucro_realizado,
                  retorno,retorno_tot))
}
carteira_tot <- function(lista){
  options(warn = -1)
  cart <- lapply(lista,carteira)
  tot <- bind_rows(cart)
  total <- tot %>%
    group_by(date) %>%
    summarise(aporte = sum(aporte),
              valor_tot = sum(valor_tot),
              dividendos = sum(dividendos),
              lucro = sum(lucro, na.rm = T),
              lucro_realizado = sum(lucro_realizado))
  total <- total %>%
    mutate(retorno = retorno_carteira(valor_tot,aporte),
           retorno_tot = cumprod(retorno+1)-1,
           drawdown = (retorno_tot+1)/cummax(retorno_tot+1)-1)
  pesos <- tot %>%
    group_by(date) %>%
    summarise(symbol = symbol,pesos = valor_tot/sum(valor_tot),
              lucro,lucro_realizado,
              valor_tot, n_acoes,dividendos,close,
              aporte, preco_med) %>%
    group_by(symbol, date) %>%
    summarise(symbol = dplyr::last(symbol), 
              pesos = sum(pesos),
              valor_tot = sum(valor_tot),
              lucro = sum(lucro),
              n_acoes = sum(n_acoes),
              dividendos = sum(dividendos),
              aporte = sum(aporte),
              preco_med = sum(preco_med*n_acoes)/sum(n_acoes),
              close = max(close),
              lucro_realizado = sum(lucro_realizado)) %>%
    group_by(symbol) %>%
    summarise(date,pesos,valor_tot,
              n_acoes = n_acoes,
              dividendos,
              aporte,preco_med,close,
              retorno = retorno_carteira(valor_tot,aporte),
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
    mutate(drawdown = (retorno_tot+1)/cummax(retorno_tot+1)-1,
           cotas = valor_tot/(retorno_tot+1))
  pesos <- pesos %>%
    filter(pesos != 0)
  listaa <- list(retornos = total,
                 pesos = pesos)
  return(listaa)
}