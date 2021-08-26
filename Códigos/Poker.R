library(poker)
baralho <- function(){
  matriz <- matrix(c(1:52),nrow = 13,ncol = 4)
  rownames(matriz) <- c(2:10,"Valete","Rainha","Rei","Ás")
  colnames(matriz) <- c("Espada","Paus","Coração","Diamante")
  return(matriz)
}
sim_poker <- function(cartas_jogador,mesa = c(),n_players = 2,
                      cartas_inimigo = list()){
  suppressPackageStartupMessages(library(poker))
  cartas_jogador <- unlist(cartas_jogador)
  mesa <- unlist(mesa)
  cartas <- 1:52
  cartas_disp <- cartas[!(cartas %in% c(cartas_jogador,mesa,unlist(cartas_inimigo)))]
  player <- list()
  length(player) <- n_players
  if(length(n_players) == 2){
    player[[1]] <- cartas_jogador
  }else{
    player[[1]] <- c(cartas_jogador,sample(cartas_disp,2-length(cartas_jogador)))
  }
  cartas_tot <- c(cartas_jogador,mesa)
  if(length(cartas_inimigo) != 0 & is.list(cartas_inimigo)){
    for(i in 1:length(cartas_inimigo)+1){
      if(length(cartas_inimigo[[i-1]]) == 2){
        player[[i]] <- cartas_inimigo[[i-1]]
      }
    }
    play <- unlist(player)
    cartas_tot <- c(play,mesa)
    cartas_disp <- 1:52
    cartas_disp <- cartas_disp[!(cartas_disp %in% cartas_tot)]
    tot <- unlist(lapply(player,function(x)is.null(x)))
    tot <- which(tot)
    if(length(tot) != 0){
      for(i in tot){
        player[[i]] <- sample(cartas_disp,size = length(tot)*2)
      }
    }
  }else{
    cartas_disp <- 1:52
    cartas_disp <- cartas_disp[!(cartas_disp %in% cartas_tot)]
    for(i in 2:n_players){
      player[[i]] <- sample(cartas_disp,size = 2)
      play <- unlist(player)
      cartas_tot <- c(play,mesa)
      cartas_disp <- 1:52
      cartas_disp <- cartas_disp[!(cartas_disp %in% cartas_tot)]
    }
    play <- unlist(player)
  }
  cartas_tot <- c(play,mesa)
  cartas_disp <- 1:52
  cartas_disp <- cartas_disp[!(cartas_disp %in% cartas_tot)]
  mesa1 <- c(mesa,sample(cartas_disp,5-length(mesa)))
  y <- c(play,mesa1)
  player1 <- matrix(play,ncol = 2,byrow = T)
  board <- mesa1
  cards <- hand(player1,board)
  score <- showdown(cards)
  winner <- tiebreaker(n_players,cards,score)
  vencedor <- winner
  return(vencedor)
}
simulacao_poker <- function(cartas_jogador,mesa = c(),n_players = 2, n_sim = 1000,
                            cartas_inimigo = list()){
  l_cartas <- length(cartas_jogador)
  l_mesa <- length(mesa)
  l_inimigo <- sapply(cartas_inimigo,length)
  l_lista <- length(cartas_inimigo)
  t1 <- all(c(l_cartas,l_inimigo) == 2)
  t2 <- l_mesa == 5
  t3 <- l_lista == n_players-1
  if(t1&t2&t3){
    n_sim <- 1
  }
  vencedor <- replicate(n_sim,list)
  for(i in 1:n_sim){
    vencedor[[i]] <- sim_poker(cartas_jogador,mesa,n_players,cartas_inimigo)
  }
  fun <- function(lista,i = 1){
    if(any(lista == i) & length(lista) > 1){
      return(1)
    }else if(length(lista) == 1 & all(lista == i)){
      return(2)
    }else{
      return(0)
    }
  }
  todos <- replicate(n_players,list)
  names(todos) <- paste("Player",1:n_players)
  for(i in 1:n_players){
    todos[[i]] <- unlist(lapply(vencedor,fun,i = i))
  }
  vencer <- lapply(todos,function(x)mean(x == 2))
  empatar <- lapply(todos,function(x)mean(x == 1))
  perder <- lapply(todos,function(x)mean(x == 0))
  lista <- replicate(n_players,list)
  names(lista) <- paste("Player",1:n_players)
  for(i in 1:n_players){
    lista[[i]] <- c(vencer[[i]],empatar[[i]],perder[[i]])
    names(lista[[i]]) <- c("%Vencer","%Empatar","%Perder")
  }
  return(lista)
}
transformar_num <- function(string){
  library(poker)
  options(warn = -1)
  fun <- function(string){
    if(is.na(string)){
      return(NA)
    }
    if(string == ""){
      return(NULL)
    }
    string <- gsub("10","t",string,fixed = T)
    str_s <- unlist(stringr::str_split(string,""))
    rank <- as.numeric(str_s[1])
    if(length(rank) != 0 & is.na(rank)){
      str_s[1] <- toupper(str_s[1])
      if(length(str_s) != 0 & str_s[1] == "T"){
        rank <- 10
      }else if(str_s[1] == "J"){
        rank <- 11
      }else if(str_s[1] == "Q"){
        rank <- 12
      }else if(str_s[1] == "K"){
        rank <- 13
      }else{
        rank <- 14
      }
    }
    suit <- tolower(str_s[2])
    if(length(suit) != 0){
      if(suit == "s"){
        suit <- 1
      }else if(suit == "c"){
        suit <- 2
      }else if(suit == "h"){
        suit <- 3
      }else if(suit == "d"){
        suit <- 4
      }
    }
    dotTransformToNumber(rank,suit)
  }
  sapply(string,fun)
}
transformar_str <- function(num){
  library(poker)
  options(warn = -1)
  fun <- function(num){
    rank <- dotTransformToRank(num)
    suit <- dotTransformToSuit(num)
    numero <- dotTransformToNumber(rank,suit)
    bara <- baralho()
    linha <- apply(bara,1,function(x)any(x == numero))
    linha <- names(linha[linha])
    coluna <- apply(bara,2,function(x)any(x == numero))
    coluna <- names(coluna[coluna])
    nome <- paste(linha,"de",coluna)
  }
  nome <- sapply(num,fun)
  return(nome)
}