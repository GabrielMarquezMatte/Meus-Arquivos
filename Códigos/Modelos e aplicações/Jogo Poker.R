source("Meus Arquivos/Códigos/Poker.R",encoding = "UTF-8")
library(poker)
library(tidyverse)
cartas_jogador <- transformar_num(c("ac","ad")) %>% unlist #Cartas do jogador
mesa <- transformar_num(c("")) %>% unlist #Cartas na mesa
cartas <- transformar_num(c("")) %>% unlist() #Cartas dos adversários
if(length(cartas) != 0){
  cartas_inimigo <- list(cartas)
}else{
  cartas_inimigo <- list()
}
n_players <- 2
n_sim <- 1000
prob <- simulacao_poker(cartas_jogador = cartas_jogador,mesa = mesa,
                        n_players = n_players,
                        n_sim = n_sim,cartas_inimigo = cartas_inimigo)[1]
prob
1/n_players

