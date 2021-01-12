carteira <- function(data.framee){
  #data.frame(symbol,date,price,n_acoes,moeda = NA ou moeda de preferência)
  options(warn = -1)
  pacotes <- c("tidyverse","tidyquant","bizdays")
  for(i in pacotes){
    suppressPackageStartupMessages(require(i,character.only = T))
  }
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
  val <- val %>%
    mutate(value = na.fill(value,0),
           n_acoes = na.fill(n_acoes,0),
           value = value*cumsum(n_acoes),
           close = close*moeda,
           aporte = price*n_acoes*moeda,
           aporte = na.fill(aporte,0),
           valor_tot = cumsum(n_acoes)*close+cumsum(value)*cumsum(n_acoes),
           retorno = c(0,
                       valor_tot[2:length(valor_tot)]-
                         aporte[2:length(valor_tot)]-valor_tot[2:length(valor_tot)-1]),
           retorno = c(0,retorno[2:length(valor_tot)]/
                         (valor_tot[2:length(valor_tot)-1]+aporte[2:length(valor_tot)])),
           price = NULL,
           moeda = NULL)
  val$retorno[1] <- val$valor_tot[1]/val$aporte[1]-1
  return(val)
}
carteira_tot <- function(lista){
  options(warn = -1)
  data <- grepl("date",names(unlist(lista)))
  data <- min(unlist(lista)[data]) %>%
    as.numeric() %>%
    as.Date()
  ibov <- tq_get("^BVSP", from = data) %>%
    na.omit()
  datas <- ibov$date
  cart <- lapply(lista,carteira)
  tot <- data.frame()
  for(i in 1:length(cart)){
    tot <- rbind(tot,cart[[i]])
  }
  total <- tot %>%
    filter(date %in% datas) %>%
    group_by(date) %>%
    summarise(aporte = sum(aporte),
              valor_tot = dplyr::last(valor_tot),
              value = sum(value))
  total <- total %>%
    mutate(retorno = c(0,valor_tot[2:length(valor_tot)]-
                         valor_tot[2:length(valor_tot)-1]-aporte[2:length(valor_tot)]),
           retorno = c(0,retorno[2:length(valor_tot)]/
                         (valor_tot[2:length(valor_tot)-1]+aporte[2:length(valor_tot)])))
  total$retorno[1] <- total$valor_tot[1]/total$aporte[1]-1
  total <- total %>%
    filter(retorno > -1)%>%
    mutate(retorno = c(0,valor_tot[2:length(valor_tot)]-
                         valor_tot[2:length(valor_tot)-1]-aporte[2:length(valor_tot)]),
           retorno = c(0,retorno[2:length(valor_tot)]/
                         (valor_tot[2:length(valor_tot)-1]+aporte[2:length(valor_tot)])))
  total$retorno[1] <- total$valor_tot[1]/total$aporte[1]-1
  return(total)
}