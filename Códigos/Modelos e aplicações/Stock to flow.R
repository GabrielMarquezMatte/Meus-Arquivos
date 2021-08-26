#Carregando os pacotes
pacotes <- c("tidyverse","Quandl","tidyquant",
             "lubridate","fANCOVA")
sapply(pacotes,require,character.only = T)
Quandl.api_key("<API>") #API Quandl
total_btc <- Quandl("BCHAIN/TOTBC") %>% #Calculando o número de bitcoins adicionados
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
price_month = Quandl("BCHAIN/MKPRU") %>% #Coletando o preço do Bitcoin
  arrange(Date) %>%
  summarise(date = Date,
            price = Value)
tot <- left_join(total_btc,price_month,"date") %>%
  filter(price != 0)
linhas <- 1:round(nrow(tot)/1.5)
linhas1 <- 1:nrow(tot)
linhas1 <- linhas1[!(linhas1 %in% linhas)]
reg <- lm(log(price) ~ log(sf), data = tot,subset = linhas)
pred <- predict(reg,tot)
estimate <- pred
tot <- tot %>%
  mutate(estimate = exp(reg$coefficients[1])*sf^reg$coefficients[2],
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