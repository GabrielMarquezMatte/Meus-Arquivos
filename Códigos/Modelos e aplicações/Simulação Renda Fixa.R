source("Meus Arquivos/Códigos/Fixed Income models.R")
pacotes <- c("tidyverse","GetTDData","bizdays","glue")
sapply(pacotes,require,character.only = T)
vencimento <- "2021-01-01" %>% #Vencimento do título
  as.Date()
vencimento1 <- format(vencimento,"%d%m%y")
#download.TD.data(asset.codes = "LTN") #Baixando os valores de LTNs
tb2 <- read.TD.files(asset.codes = "LTN",maturity = vencimento1) #
tb1 <- tb2 %>%
  group_by(date = ref.date) %>%
  summarise(yield.bid = mean(yield.bid),
            price.bid = mean(price.bid),
            matur.date)
tb <- tb1 %>%
  filter(date <= max(date)-500)
#Fazendo o fit dos modelos
fit <- fit_cir(tb$yield.bid)
fit_vas <- vasicek_fit(tb$yield.bid)
parametros <- fit$coef
sigma <- parametros["sigma"]
lambda <- parametros["lambda"]
mu <- parametros["mu"]
mat <- bizdays::bizdays(tail(tb$date,1),tail(tb$matur.date,1),"Brazil/ANBIMA")
r0 <- tail(tb$yield.bid,1) #Taxa de juros inicial
p0 <- tail(tb$price.bid,1) #Preço inicial
h <- 90 #Horizonte de simulação
m <- mat:(mat-h)/252 #Vetor de diferença de dias até o vencimento
m <- m[m >= 0]
h <- length(m)-1
cir <- F #Modelo CIR ou Vasicek
n <- 5000 #Número de simulações
if(cir){
  simula <- sim_cir(lambda = lambda, mu = mu,
                    sigma = sigma, h = h,
                    n = n,m = m,r0 = r0)
}else{
  simula <- sim_vasicek(r = tb$yield.bid,h = h,n = n)
}
prices <- apply(simula,2,function(x,m)1000/(1+x)^m,m = m[-1])
retornos <- apply(prices,2,function(x)c(x[1]/p0-1,diff(x)/x[-length(x)]))
sds <- apply(retornos[-1,],1,sd)
var <- apply(retornos[-1,],1,quantile,c(0.01,0.05,0.1,0.5,0.9,0.95,0.99)) %>%
  t
acertos <- apply(retornos[-1,],1,function(x)mean(x > 0))
prob <- prices %>% #Probabilidade de retorno positivo desde o início da simulação
  as.data.frame() %>%
  mutate(date = add.bizdays(tail(tb$date,1),1:h-1,"Brazil/ANBIMA")) %>%
  gather(key = "sim", value = price,-date) %>%
  group_by(date) %>%
  summarise(prob = mean(price >= tail(tb$price.bid,1),na.rm = T))
quantias <- apply(simula,1,quantile,c(0.05,0.1,0.3,0.5,0.7,0.9,0.95),na.rm = T) %>% #Quartis da simulação
  t %>%
  as.data.frame() %>%
  mutate(date = add.bizdays(tail(tb$date,1),1:h-1,"Brazil/ANBIMA")) %>%
  gather(key = "quantile", value = yield,-date) %>%
  group_by(quantile,date) %>%
  mutate(matur = tail(tb$matur.date,1),
         matur = as.Date(matur),
         dife = bizdays::bizdays(date,matur,"Brazil/ANBIMA")/252,
         price = 1000/(1+yield)^dife) %>%
  gather(key = index, value = value, -c(date,quantile,dife,matur)) %>%
  rbind(data.frame(date = tb$date,
                   quantile = "Real",
                   yield = tb$yield.bid,
                   price = tb$price.bid,
                   matur = tb$matur.date,
                   dife = bizdays::bizdays(tb$date,tb$matur.date,"Brazil/ANBIMA")) %>%
          gather(key = index,value = value, -c(date,quantile,dife,matur)))
label <- function(x)ifelse(x > 1,
                           scales::dollar(x,prefix = "R$",
                                                 big.mark = ".",decimal.mark = ","),
                           scales::percent(x))
fatores <- c("Real","95%","90%","70%","50%","30%","10%","5%")
ggplot(quantias %>%
         filter(date >= tail(tb$date,1)-100),
       aes(x = date, y = value, col = quantile))+
  geom_line(size = 1)+
  facet_wrap(~index,nrow = 2, scales = "free")+
  labs(x = "", y = "", title = glue("LTN {vencimento}"),
       subtitle = ifelse(cir,"Modelo CIR","Modelo Vasicek"))+
  scale_color_manual("Percentil", values = c(1:length(fatores)),breaks = fatores)+
  scale_y_continuous(labels = label, n.breaks = 10)
ggplot(prob[-1,], aes(x = date,y  = prob))+
  geom_line()+
  scale_y_continuous(labels = scales::percent,n.breaks = 13)+
  labs(x = "", y = "", 
       title = glue("Probabilidade de retorno positivo LTN {vencimento}"),
       subtitle = ifelse(cir,"Modelo CIR","Modelo Vasicek"))
#Comparando a simulação com os dados reais
if(tail(tb$date,1) < tail(tb1$date,1)){
  real <- rbind(tb1 %>%
                  dplyr::select(date,yield = yield.bid,price = price.bid) %>%
                  mutate(quantile = "Real") %>%
                  gather(key = index, value = value,-c(date,quantile)),quantias %>%
                  dplyr::select(date,quantile,index,value)) %>%
    filter(date >= max(tb$date)-100 & date <= max(quantias$date))
  fatores1 <- c("Real","95%","90%","70%","50%","30%","10%","5%")
  ggplot(real %>%
           mutate(index = gsub("price","Preço",index),
                  index = gsub("yield","Juros",index)), aes(x = date, y = value, col = quantile))+
    geom_line()+
    facet_wrap(~index,nrow = 2, scales = "free")+
    scale_color_manual("Percentis simulados",
                       values = 1:length(fatores1),breaks = fatores1)+
    labs(x = "", y = "", title =  glue("Previsão LTN {vencimento}"),
         caption = "Fonte:Tesouro Nacional \n Elaboração:Gabriel Matte",
         subtitle = ifelse(cir,"Modelo CIR","Modelo Vasicek"))+
    scale_y_continuous(labels = label, n.breaks = 13)
}
