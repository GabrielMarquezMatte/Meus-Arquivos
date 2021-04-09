exercicio <- function(symbol){
  url <- glue("https://www.investindo.net/opcao/{toupper(symbol)}")
  ht <- read_html(url)
  texto <- ht %>%
    html_nodes(".value") %>%
    html_text %>%
    .[1]
  texto <- gsub("R$","",texto,fixed = T)
  exer <- str_split(texto,",")[[1]] %>%
    paste0(collapse = "-")
  exer <- gsub(" ","",exer, fixed = T)
  return(exer)
}
get_opcoes <- function(symbol,from = Sys.Date()-30,to = Sys.Date()){
  library(tidyverse)
  library(rvest)
  library(bizdays)
  from <- as.Date(from)
  to <- as.Date(to)
  urls <- bizdays(from,to,"Brazil/ANBIMA")/68
  if(urls < 1){
    urls <- floor(urls)
  }else{
    urls <- ceiling(urls)
  }
  from <- from %>%
    format("%d/%m/%y")
  to <- to %>%
    format("%d/%m/%y")
  exercicio <- exercicio(symbol)
  url <- glue("https://br.advfn.com/bolsa-de-valores/bovespa/{tolower(symbol)}-ex-{exercicio}-{toupper(symbol)}/historico/mais-dados-historicos?current={0:urls}&Date1={from}&Date2={to}")
  func <- function(url){
    ht <- read_html(url)
    mudar <- function(x){
      x <- gsub("%","",x,fixed = T)
      x <- gsub(".","",x,fixed = T)
      x <- gsub(",",".",x,fixed = T)
      return(as.numeric(x))
    }
    tabela <- html_table(ht)[[2]]
    colnames(tabela) <- c("Data","Fechamento",
                          "Variacao","Variacao_perc",
                          "Abertura","Maxima","Minima","Volume")
    tabela <- tabela %>%
      mutate(Abertura = mudar(Abertura),
             Maxima = mudar(Maxima),
             Minima = mudar(Minima),
             Fechamento = mudar(Fechamento),
             Volume = mudar(Volume),
             Variacao_perc = mudar(Variacao_perc),
             Variacao_perc = Variacao_perc/100,
             Data = as.Date(Data,"%d %B %Y")) %>%
      select(date = Data,open = Abertura,high = Maxima,
             low = Minima,close = Fechamento,volume = Volume,
             adjusted = Fechamento,daily.return = Variacao_perc)
    return(tabela)
  }
  lapply(url,func) %>% bind_rows %>%
    mutate(symbol = symbol) %>%
    select(symbol,date,open,high,low,close,volume,adjusted,daily.return) %>%
    arrange(date)
}
my_options <- function(ticker,first.date = Sys.Date()-365,
                       last.date = Sys.Date(),do.cache = T, be.quiet = F,
                       cache.folder = "Options"){
  first.date <- as.Date(first.date)
  last.date <- as.Date(last.date)
  if(do.cache){
    my.cache.files <- list.files(cache.folder, full.names = TRUE)
    if(length(my.cache.files) > 0){
      l.out <- stringr::str_split(tools::file_path_sans_ext(basename(my.cache.files)), 
                                  "_")
      df.cache.files <- dplyr::tibble(f.name = my.cache.files, 
                                      ticker = sapply(l.out, function(x) x[1]),
                                      first.date = as.Date(sapply(l.out,function(x) x[2])),
                                      last.date = as.Date(sapply(l.out,function(x) x[3])))
    }else{
      df.cache.files <- dplyr::tibble(f.name = "", ticker = "",
                                      first.date = first.date,
                                      last.date = last.date)
    }
    fixed.ticker <- fix.ticker.name(ticker)
    temp.cache <- dplyr::filter(df.cache.files, ticker == 
                                  fixed.ticker)
    if(nrow(temp.cache) > 1){
      stop(paste0("Found more than one file in cache for ", 
                  ticker, "\nYou must manually remove one of \n\n", 
                  paste0(temp.cache$f.name, collapse = "\n")))
    }
    if(nrow(temp.cache) != 0){
      df.cache <- data.frame()
      flag.dates <- TRUE
      if(!be.quiet){
        message(ticker," | Found cache file", appendLF = FALSE)
      }
      df.cache <- readRDS(temp.cache$f.name)
      max.diff.dates <- 0
      flag.dates <- ((first.date - temp.cache$first.date) < 
                       -max.diff.dates) | ((last.date - temp.cache$last.date) > 
                                             max.diff.dates)
      df.out <- data.frame()
      if(flag.dates){
        if(!be.quiet){
          message(" | Need new data", appendLF = FALSE)
        }
        flag.date.bef <- ((first.date - temp.cache$first.date) < 
                            -max.diff.dates)
        df.out.bef <- data.frame()
        if(flag.date.bef){
          df.out.bef <- get_opcoes(ticker,first.date, temp.cache$first.date)
        }
        flag.date.aft <- ((last.date - temp.cache$last.date) > 
                            max.diff.dates)
        df.out.aft <- data.frame()
        if(flag.date.aft){
          df.out.aft <- get_opcoes(ticker,temp.cache$last.date, last.date)
        }
        df.out <- rbind(df.out.bef, df.out.aft)
      }
      df.out <- unique(rbind(df.cache, df.out))
      if(nrow(df.out) > 0){
        idx <- order(df.out$symbol, df.out$date)
        df.out <- df.out[idx, ]
      }
      file.remove(temp.cache$f.name)
      my.f.out <- paste0(fixed.ticker, "_", 
                         min(c(temp.cache$first.date, first.date)), "_", 
                         max(c(temp.cache$last.date, last.date)), ".rds")
      saveRDS(df.out, file = file.path(cache.folder, my.f.out))
      date <- NULL
      df.out <- dplyr::filter(df.out, date >= first.date, 
                              date <= last.date)
    }else{
      if(!be.quiet){
        message(" | Not Cached", appendLF = FALSE)
      }
      my.f.out <- paste0(fixed.ticker, "_", 
                         first.date, "_", last.date, ".rds")
      df.out <- get_opcoes(ticker, first.date, 
                           last.date)
      if(nrow(df.out) >= 1){
        if(!be.quiet){
          message(" | Saving cache", appendLF = FALSE)
        }
        if(dir.exists(cache.folder)){
          saveRDS(df.out, file = file.path(cache.folder, 
                                           my.f.out))
        }else{
          dir.create(cache.folder)
          saveRDS(df.out, file = file.path(cache.folder, 
                                           my.f.out))
        }
      }
    }
  }else{
    df.out <- get_opcoes(ticker, first.date, last.date)
  }
  if(nrow(df.out) == 0){
    download.status = "NOT OK"
    total.obs = 0
    perc.benchmark.dates = 0
    threshold.decision = "OUT"
    df.out <- data.frame()
    if(!be.quiet){
      message(" - Error in download..", appendLF = FALSE)
    }
  }else{
    morale.boost <- c(rep(c("OK!", "Got it!", "Nice!", "Good stuff!", 
                            "Looking good!", "Good job!", "Well done!", "Feels good!", 
                            "You got it!", "Youre doing good!"), 10), "Boa!", 
                      "Mas bah tche, que coisa linda!", "Mais contente que cusco de cozinheira!", 
                      "Feliz que nem lambari de sanga!", "Mais faceiro que guri de bombacha nova!")
    df.control <- tibble::tibble(ticker = ticker,download.status = "OK",
                                 total.obs = "100%", 
                                 threshold.decision = "KEEP")
    l.out <- list(df.tickers = df.out, df.control = df.control)
    return(l.out)
  }
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
dividendos <- function(tickers, src = "yahoo", first.date, last.date){
  df.out <- data.frame()
  suppressMessages({
    suppressWarnings({
      try(df.out <- tidyquant::tq_get(x = tickers,get = "dividends",
                                      from = first.date, to = last.date),
          silent = T)
    })
  })
  return(df.out)
}
my_dividends <- function(ticker, i.ticker = ticker, length.tickers = length(ticker), 
                         src = "yahoo", first.date = "2010-01-01",
                         last.date = Sys.Date(), do.cache = TRUE, 
                         cache.folder = file.path("BGS_Dividends"),
                         df.bench = NULL, be.quiet = FALSE, thresh.bad.data = 0){
  first.date <- as.Date(first.date)
  last.date <- as.Date(last.date)
  if(!be.quiet){
    message(paste0("\n", ticker, " | ", src, " (", i.ticker, 
                   "|", length.tickers, ")"), appendLF = FALSE)
  }
  if((do.cache)){
    my.cache.files <- list.files(cache.folder, full.names = TRUE)
    if(length(my.cache.files) > 0){
      l.out <- stringr::str_split(tools::file_path_sans_ext(basename(my.cache.files)), 
                                  "_")
      df.cache.files <- dplyr::tibble(f.name = my.cache.files, 
                                      ticker = sapply(l.out, function(x) x[1]), src = sapply(l.out, 
                                                                                             function(x) x[2]), first.date = as.Date(sapply(l.out, 
                                                                                                                                            function(x) x[3])), last.date = as.Date(sapply(l.out, 
                                                                                                                                                                                           function(x) x[4])))
    }else{
      df.cache.files <- dplyr::tibble(f.name = "", ticker = "", 
                                      src = "", first.date = first.date, last.date = last.date)
    }
    fixed.ticker <- fix.ticker.name(ticker)
    temp.cache <- dplyr::filter(df.cache.files, ticker == 
                                  fixed.ticker, src == src)
    if(nrow(temp.cache) > 1){
      stop(paste0("Found more than one file in cache for ", 
                  ticker, "\nYou must manually remove one of \n\n", 
                  paste0(temp.cache$f.name, collapse = "\n")))
    }
    if(nrow(temp.cache) != 0){
      df.cache <- data.frame()
      flag.dates <- TRUE
      if(!be.quiet){
        message(" | Found cache file", appendLF = FALSE)
      }
      df.cache <- readRDS(temp.cache$f.name)
      max.diff.dates <- 0
      flag.dates <- ((first.date - temp.cache$first.date) < 
                       -max.diff.dates) | ((last.date - temp.cache$last.date) > 
                                             max.diff.dates)
      df.out <- data.frame()
      if(flag.dates){
        if(!be.quiet){
          message(" | Need new data", appendLF = FALSE)
        }
        flag.date.bef <- ((first.date - temp.cache$first.date) < 
                            -max.diff.dates)
        df.out.bef <- data.frame()
        if(flag.date.bef){
          df.out.bef <- dividendos(ticker, src, 
                                   first.date, temp.cache$first.date)
        }
        flag.date.aft <- ((last.date - temp.cache$last.date) > 
                            max.diff.dates)
        df.out.aft <- data.frame()
        if(flag.date.aft){
          df.out.aft <- dividendos(ticker, src,temp.cache$last.date, last.date)
        }
        df.out <- rbind(df.out.bef, df.out.aft)
      }
      df.out <- unique(rbind(df.cache, df.out))
      if(nrow(df.out) > 0){
        idx <- order(df.out$symbol, df.out$date)
        df.out <- df.out[idx, ]
      }
      file.remove(temp.cache$f.name)
      my.f.out <- paste0(fixed.ticker, "_", src, "_", 
                         min(c(temp.cache$first.date, first.date)), "_", 
                         max(c(temp.cache$last.date, last.date)), ".rds")
      saveRDS(df.out, file = file.path(cache.folder, my.f.out))
      date <- NULL
      df.out <- dplyr::filter(df.out, date >= first.date, 
                              date <= last.date)
    }else{
      if(!be.quiet){
        message(" | Not Cached", appendLF = FALSE)
      }
      my.f.out <- paste0(fixed.ticker, "_", src, "_", 
                         first.date, "_", last.date, ".rds")
      df.out <- dividendos(ticker, src, first.date, 
                           last.date)
      if(nrow(df.out) >= 1){
        if(!be.quiet){
          message(" | Saving cache", appendLF = FALSE)
        }
        if(dir.exists(cache.folder)){
          saveRDS(df.out, file = file.path(cache.folder, 
                                           my.f.out))
        }else{
          dir.create(cache.folder)
          saveRDS(df.out, file = file.path(cache.folder, 
                                           my.f.out))
        }
      }
    }
  }else{
    df.out <- dividendos(ticker, src, first.date, last.date)
  }
  if(nrow(df.out) == 0){
    download.status = "NOT OK"
    total.obs = 0
    perc.benchmark.dates = 0
    threshold.decision = "OUT"
    df.out <- data.frame()
    if (!be.quiet) {
      message(" - Error in download..", appendLF = FALSE)
    }
  }
  else{
    if(is.null(df.bench)) 
      return(df.out)
    download.status = "OK"
    total.obs = nrow(df.out)
    perc.benchmark.dates = sum(df.out$date %in% df.bench$date)/length(df.bench$date)
    if (perc.benchmark.dates >= thresh.bad.data) {
      threshold.decision = "KEEP"
    }
    else {
      threshold.decision = "OUT"
    }
    morale.boost <- c(rep(c("OK!", "Got it!", "Nice!", "Good stuff!", 
                            "Looking good!", "Good job!", "Well done!", "Feels good!", 
                            "You got it!", "Youre doing good!"), 10), "Boa!", 
                      "Mas bah tche, que coisa linda!", "Mais contente que cusco de cozinheira!", 
                      "Feliz que nem lambari de sanga!", "Mais faceiro que guri de bombacha nova!")
    if (!be.quiet) {
      if (threshold.decision == "KEEP") {
        message(paste0(" - ", "Got ", scales::percent(perc.benchmark.dates), 
                       " of valid prices | ", sample(morale.boost, 
                                                     1)), appendLF = FALSE)
      }
      else {
        message(paste0(" - ", "Got ", scales::percent(perc.benchmark.dates), 
                       " of valid prices | ", "OUT: not enough data (thresh.bad.data = ", 
                       scales::percent(thresh.bad.data), ")"), appendLF = FALSE)
      }
    }
    df.control <- tibble::tibble(ticker = ticker, src = src, 
                                 download.status, total.obs, perc.benchmark.dates, 
                                 threshold.decision)
    l.out <- list(df.tickers = df.out, df.control = df.control)
    return(l.out)
  }
}
get_precos <- function(data.framee,Quandl_api_key,
                       valores_atual,be.quiet = T,
                       do_cache = T){
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
  if(is.null(data.framee$option)){
    data.framee$option <- NA
  }
  data.framee <- data.framee %>%
    mutate(vencimento = as.Date(vencimento),
           date = as.Date(date),
           benchmark = as.numeric(benchmark),
           n_acoes = as.numeric(n_acoes)) %>%
    arrange(date)
  dataa <- data.framee[1,]
  valores_atual <- T
  data_f <- Sys.Date()
  if(sum(data.framee$n_acoes) == 0){
    data_f <- tail(data.framee$date,1)
    valores_atual <- F
  }
  tota <- data.frame(date = seq(dataa$date,
                                data_f,"1 day"))
  if(!(dataa$symbol %in% c("NTN-B","NTN-B Principal","LTN","LFT","NTN-F","CDI")) &
     !is.na(dataa$symbol)){
    if(grepl("USD",dataa$symbol)){
      if(data_f == Sys.Date())data_f <- data_f-1
    }
    valores <- BatchGetSymbols(dplyr::first(data.framee$symbol), 
                               first.date = dplyr::first(data.framee$date)-5,
                               last.date = data_f,do.cache = do_cache,
                               bench.ticker = dataa$symbol,be.quiet = be.quiet,
                               cache.folder = glue("{getwd()}/BGS_Cache"))$df.tickers %>%
      select(symbol = ticker,
             date = ref.date,
             open = price.open,
             high = price.high,
             low = price.low,
             close = price.close,
             volume,adjusted = price.adjusted)
    if(valores_atual){
      atual <- get_atual(dataa$symbol)
      if(atual$date == valores$date[nrow(valores)]){
        valores$close[nrow(valores)] <- atual$close
        valores$adjusted[nrow(valores)] <- atual$adjusted
        atual <- NULL
      }
    }else{
      atual <- NULL
    }
    valores <- rbind(valores,atual)
    valores <- left_join(tota,valores,"date") %>%
      mutate(symbol = dataa$symbol)
    for(j in 2:ncol(valores)){
      valores[,j] <- completar(valores[,j])
    }
    dividendos <- my_dividends(dataa$symbol,first.date = dataa$date-5,
                               last.date = data_f,do.cache = do_cache,
                               cache.folder = glue("{getwd()}/BGS_Dividends"),
                               be.quiet = be.quiet)
    #Ativos estrangeiros
    if(!is.na(dataa$moeda)){
      if(data_f == Sys.Date())data_f <- data_f-1
      moeda <- BatchGetSymbols(dplyr::first(data.framee$moeda),
                               first.date = dplyr::first(data.framee$date)-5,
                               last.date = data_f,do.cache = do_cache,
                               bench.ticker = dataa$moeda,be.quiet = be.quiet,
                               cache.folder = glue("{getwd()}/BGS_Cache"))$df.tickers %>%
        select(symbol = ticker,
               date = ref.date,
               open = price.open,
               high = price.high,
               low = price.low,
               close = price.close,
               volume,adjusted = price.adjusted) %>%
        suppressMessages()
      if(valores_atual){
        moeda_atual <- get_atual(dataa$moeda)
      }else{
        moeda_atual <- NULL
      }
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
    ano_i <- lubridate::year(dataa$date)
    dif_anos <- (year(Sys.Date())-ano_i)+1
    download.TD.data(asset.codes = dataa$symbol,n.dl = dif_anos) #Tesouro Direto
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
      valores <- Quandl("BCB/12", start_date = dataa$date,end_date = data_f) %>%
        arrange(Date) %>%
        summarise(date = Date,
                  close = cumprod((Value/100)*dataa$benchmark+1)*1000) %>%
        suppressMessages()
    }else{
      valores <- get_series(c(CDI = 12),start_date = dataa$date,end_date = data_f) %>%
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
    valores <- valor_cota(dataa$cnpj,data_inicio = dataa$date,data_fim = data_f) %>%
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
  if(!is.na(dataa$option)){
    valores <- my_options(dataa$option, 
                          first.date = dataa$date, 
                          last.date = data_f, 
                          be.quiet = be.quiet,do.cache = do_cache)$df.tickers %>%
      select(date,symbol,close)
    valores <- left_join(tota,valores,"date")
    for(i in 2:ncol(valores)){
      valores[,i] <- completar(valores[,i])
    }
    val <- valores %>%
      left_join(data.framee %>%
                  select(symbol = option,date,
                         price,n_acoes,moeda),c("date","symbol")) %>%
      mutate(value = 0,
             moeda = 1)
  }
  return(val)
}