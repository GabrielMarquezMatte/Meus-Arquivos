pacotes <- c("tidyverse","Quandl","tidyquant",
             "lubridate","BatchGetSymbols","fANCOVA")
sapply(pacotes,require,character.only = T)
source("Meus Arquivos/Códigos/Carteira.R",encoding = "UTF-8")
source("Meus Arquivos/Códigos/Funções.R",encoding = "UTF-8")
Quandl.api_key("<API>")
total_btc <- Quandl("BCHAIN/TOTBC") %>%
  arrange(Date) %>%
  rename(date = Date, bitcoins = Value) %>%
  mutate(new_btc = c(bitcoins[1],diff(bitcoins)),
         month = month(date),
         week = week(date),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarise(date = date,
            new_btc = mean(new_btc),
            bitcoins = tail(bitcoins,1),
            sf = bitcoins/new_btc/12) %>%
  arrange(date) %>%
  ungroup %>%
  mutate(year = NULL,
         month = NULL,
         week = NULL)
price_month = Quandl("BCHAIN/MKPRU") %>%
  arrange(Date) %>%
  summarise(date = Date,
            price = Value)
dolar <- BatchGetSymbols("USDBRL=X", first.date = head(price_month$date,1)-10,
                         cache.folder = "BGS_Cache",
                         bench.ticker = "USDBRL=X",
                         do.cache = F)$df.tickers %>%
  select(date = ref.date,close = price.close)
total <- data.frame(date = seq(head(dolar$date,1),tail(dolar$date,1),"1 day"))
total <- left_join(total,dolar,"date")
total <- total %>%
  mutate(close = completar(close))
tot <- left_join(total_btc,price_month,"date") %>%
  left_join(total,"date") %>%
  filter(price != 0)
linhas <- 1:round(nrow(tot)/1.5)
linhas1 <- 1:nrow(tot)
linhas1 <- linhas1[!(linhas1 %in% linhas)]
reg <- lm(log(price) ~ log(sf), data = tot,subset = linhas)
pred <- predict(reg,tot)
estimate <- pred
tot <- tot %>%
  mutate(close = 1,
         estimate = exp(reg$coefficients[1])*sf^reg$coefficients[2]*close,
         price = price*close,
         close = NULL,
         erro = estimate/price-1)
loess_price <- loess.as(tot$date,tot$estimate)
span <- loess_price$pars$span
ggplot(tot %>%
         filter(date >= "2010-01-01" &
                  date <= Sys.Date()), aes(x = date))+
  geom_line(aes(y = price, col = "Price"))+
  geom_line(aes(y = estimate, col = "Estimate"), size = 1)+
  scale_y_log10(labels = scales::dollar_format(prefix = "$",big.mark = ".",
                                               decimal.mark = ","),
                n.breaks = 13)+
  scale_color_manual("", values = c("red","black"))+
  labs(x = "", y = "Price", title = "Stock-to-Flow model")+
  geom_smooth(aes(x = date, y = estimate), method = "loess", formula = y ~ x,
              span = span)
