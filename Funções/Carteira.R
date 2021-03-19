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
get_atual <- function(x){
  pacotes <- c("tidyverse","quantmod")
  for(i in pacotes){
    suppressPackageStartupMessages(require(i,character.only = T))
  }
  funcao <- function(x){
    preco <- getQuote(x)
    tabela <- data.frame(symbol = x,
                         date = Sys.Date(),
                         open = preco$Open,
                         high = preco$High,
                         low = preco$Low,
                         close = preco$Last,
                         volume = preco$Volume,
                         adjusted = preco$Last)
    return(tabela)
  }
  data <- lapply(x,funcao) %>% bind_rows
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
carteira <- function(data.framee,Quandl_api_key = NA){
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
  #Inputando parâmetros faltantes
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
  data.framee$benchmark <- as.numeric(data.framee$benchmark)
  dataa <- data.framee[1,]
  tota <- data.frame(date = seq(dataa$date,
                                Sys.Date(),"1 day"))
  if(is.na(dataa$symbol)){
    message(glue::glue("Consolidando {dataa$cnpj}"))
  }else{
    message(glue::glue("Consolidando {dataa$symbol}"))
  }
  #Coletando preços#
  #Ações e criptos
  if(!(dataa$symbol %in% c("NTN-B","NTN-B Principal","LTN","LFT","NTN-F","CDI")) &
     !is.na(dataa$symbol)){
    valores <- BatchGetSymbols(dplyr::first(data.framee$symbol), 
                               first.date = dplyr::first(data.framee$date)-5,
                               bench.ticker = dataa$symbol,be.quiet = T,
                               cache.folder = glue("{getwd()}/BGS_Cache"))$df.tickers %>%
      select(symbol = ticker,
             date = ref.date,
             open = price.open,
             high = price.high,
             low = price.low,
             close = price.close,
             volume,adjusted = price.adjusted) %>%
      suppressMessages()
    atual <- get_atual(dataa$symbol)
    if(atual$date == valores$date[nrow(valores)]){
      atual <- NULL
    }
    valores <- rbind(valores,atual)
    valores <- left_join(tota,valores,"date") %>%
      mutate(symbol = dataa$symbol)
    for(j in 2:ncol(valores)){
      valores[,j] <- completar(valores[,j])
    }
    dividendos <- tq_get(dplyr::first(data.framee$symbol), 
                         from = dplyr::first(data.framee$date)-5,
                         get = "dividends")
    #Ativos estrangeiros
    if(!is.na(dataa$moeda)){
      moeda <- BatchGetSymbols(dplyr::first(data.framee$moeda),
                               first.date = dplyr::first(data.framee$date)-5,
                               bench.ticker = dataa$moeda,be.quiet = T,
                               cache.folder = glue("{getwd()}/BGS_Cache"))$df.tickers %>%
        select(symbol = ticker,
               date = ref.date,
               open = price.open,
               high = price.high,
               low = price.low,
               close = price.close,
               volume,adjusted = price.adjusted) %>%
        suppressMessages()
      moeda_atual <- get_atual(dataa$moeda)
      moeda <- rbind(moeda,moeda_atual)
      moeda <- moeda %>%
        select(date,close)
      moeda <- left_join(tota,moeda,"date") %>%
        mutate(close = completar(close))
      moeda <- moeda[!duplicated(moeda$date),]
    }else{ #Ativos nacionais
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
    download.TD.data(asset.codes = dataa$symbol) #Tesouro Direto
    valores <- read.TD.files(asset.codes = dataa$symbol,
                             maturity = format(dataa$vencimento,"%d%m%y")) %>%
      filter(ref.date >= dataa$date-2) %>%
      select(ref.date,price.bid)
    colnames(valores) <- c("date","close")
    valores <- left_join(tota,valores,"date")
    valores[,2] <- completar(valores[,2])
    val <- valores %>%
      left_join(data.framee %>%
                  select(symbol, date, price, n_acoes,moeda), "date") %>%
      mutate(value = 0,
             moeda = 1,
             symbol = paste(dataa$symbol,dataa$vencimento))
  }
  try(if(dataa$symbol == "CDI"){ #Renda Fixa Pós-fixada
    if(is.character(Quandl_api_key)){
      Quandl.api_key(Quandl_api_key)
      valores <- Quandl("BCB/12", start_date = dataa$date) %>%
        arrange(Date) %>%
        summarise(date = Date,
                  close = cumprod((Value/100)*dataa$benchmark+1)*1000) %>%
        suppressMessages()
    }else{
      valores <- get_series(c(CDI = 12), start_date = dataa$date) %>%
        arrange(date) %>%
        summarise(date,close = cumprod((CDI/100)*dataa$benchmark+1)*1000)
    }
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
  if(!is.na(dataa$cnpj)){ #Fundos de investimentos brasileiros
    valores <- valor_cota(dataa$cnpj, dataa$date) %>%
      select(date,cnpj,vl_quota) %>%
      rename(close = vl_quota)
    valores <- left_join(tota,valores,"date")
    for(j in 2:ncol(valores)){
      valores[,j] <- completar(valores[,j])
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
carteira_tot <- function(lista, Quandl_api_key = NA){
  options(warn = -1)
  cart <- lapply(lista,carteira,Quandl_api_key)
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
  listaa <- list(retornos = total,
                 pesos = pesos)
  return(listaa)
}