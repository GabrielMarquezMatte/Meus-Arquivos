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
carteira <- function(data.framee){
  #data.frame(symbol,date,price,n_acoes,moeda = NA ou moeda de preferência)
  options(warn = -1)
  pacotes <- c("tidyverse","tidyquant","bizdays","GetTDData","rbcb")
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
    data.framee$benchmark <- NA
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
  if(!(dataa$symbol %in% c("NTN-B","NTN-B Principal","LTN","LFT","NTN-F","CDI")) &
     !is.na(dataa$symbol)){
    valores <- tq_get(dplyr::first(data.framee$symbol), 
                      from = dplyr::first(data.framee$date)-5)
    dividendos <- tq_get(dplyr::first(data.framee$symbol), 
                         from = dplyr::first(data.framee$date)-5,
                         get = "dividends")
    if(!is.na(data.framee$moeda)){
      moeda <- tq_get(dplyr::first(data.framee$moeda),
                      from = dplyr::first(data.framee$date)-5) %>%
        select(date,close)
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
      val <- valores %>%
        left_join(dividendos, by = c("symbol","date")) %>%
        left_join(moeda,"date")
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
    val <- valores %>%
      left_join(data.framee %>%
                  select(symbol, date, price, n_acoes,moeda), "date") %>%
      mutate(value = 0,
             moeda = 1,
             symbol = dataa$symbol)
  }
  try(if(dataa$symbol == "CDI"){
    valores <- get_series(c(CDI = 12), start_date = dataa$date) %>%
      summarise(date,close = cumprod((CDI/100)*dataa$benchmark+1)*1000)
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
    val <- valores %>%
      left_join(data.framee %>%
                  select(cnpj, date, price, n_acoes, moeda),c("date","cnpj")) %>%
      mutate(value = 0,
             moeda = 1) %>%
      rename(symbol = cnpj)
  }
  val <- val %>%
    mutate(value = zoo::na.fill(value,0),
           n_acoes = zoo::na.fill(n_acoes,0) %>% as.numeric,
           value = value*cumsum(n_acoes),
           close = close*moeda,
           price = zoo::na.fill(price,0) %>% as.numeric,
           aporte = price*n_acoes*moeda,
           aporte = zoo::na.fill(aporte,0),
           valor_tot = cumsum(n_acoes)*close,
           price = NULL,
           moeda = NULL) %>%
    select(symbol,date,close,n_acoes,value,aporte,valor_tot)
  tota <- data.frame(date = seq(dplyr::first(data.framee$date),
                                Sys.Date(),"1 day"))
  tota <- left_join(tota,val,"date") %>%
    mutate(aporte = na.fill(aporte,0),
           value = na.fill(value,0),
           n_acoes = na.fill(n_acoes,0))
  for(j in c(2,3,7)){
    for(i in 2:nrow(tota)){
      if(is.na(tota[i,j])){
        tota[i,j] <- tota[i-1,j]
      }
    }
  }
  if(!all(tota$date != unique(tota$date))){
    tota <- tota %>%
      group_by(date) %>%
      summarise(date = dplyr::last(date),symbol = dplyr::last(symbol),
                close = dplyr::last(close),n_acoes = sum(n_acoes),
                value = sum(value),aporte = sum(aporte),
                valor_tot = dplyr::last(valor_tot))
  }
  tota <- tota %>%
    mutate(retorno = c(0,
                       valor_tot[2:length(valor_tot)]-
                         aporte[2:length(valor_tot)]-valor_tot[2:length(valor_tot)-1]),
           retorno = c(0,retorno[2:length(valor_tot)]/
                         (valor_tot[2:length(valor_tot)-1]+aporte[2:length(valor_tot)])))
  tota$retorno[1] <- tota$valor_tot[1]/tota$aporte[1]-1
  return(tota)
}
carteira_tot <- function(lista){
  options(warn = -1)
  cart <- lapply(lista,carteira)
  tot <- data.frame()
  for(i in 1:length(cart)){
    tot <- rbind(tot,cart[[i]])
  }
  total <- tot %>%
    group_by(date) %>%
    summarise(aporte = sum(aporte),
              valor_tot = sum(valor_tot),
              value = sum(value))
  total <- total %>%
    mutate(retorno = c(0,valor_tot[2:length(valor_tot)]-
                         valor_tot[2:length(valor_tot)-1]-aporte[2:length(valor_tot)]),
           retorno = c(0,retorno[2:length(valor_tot)]/
                         (valor_tot[2:length(valor_tot)-1]+aporte[2:length(valor_tot)])))
  total$retorno[1] <- total$valor_tot[1]/total$aporte[1]-1
  total$retorno_tot <- cumprod(total$retorno+1)-1
  pesos <- tot %>%
    group_by(date) %>%
    summarise(symbol,pesos = valor_tot/sum(valor_tot), valor_tot) %>%
    group_by(date,symbol) %>%
    summarise(pesos = sum(pesos), valor_tot = sum(valor_tot)) %>%
    filter(pesos != 0)
  listaa <- list(retornos = total,
                 pesos = pesos)
  return(listaa)
}