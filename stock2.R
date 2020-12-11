install.packages("TTR")
install.packages("dygraphs")
install.packages("htmlwidgets")

library(htmlwidgets)
library(dygraphs)
library(quantmod)
library(tseries)
library(timeSeries)
library(xts)
library(PerformanceAnalytics)
library(xts)
library(htmltools)
library(dygraphs)

stockID<-"DBK"
stdata <- getSymbols(stockID, auto.assign = FALSE)
aa<-chartSeries(stdata, subset = '20100401/20201212', type = 'bars', theme = chartTheme("black", up.col='green', down.col='red'),
                TA = c(addADX(), addMACD(), addSMI(), addEVWMA(), addRSI(), addSMA()))


stdata4col <- na.omit(stdata[,c(4)])
volume <- stdata[,c(5)]
adxtable <- ADX(HLC(stdata), n=26, maType=EMA, wilder=TRUE) #ADX data retrive
smitable <- SMI(HLC(stdata), n=2,nFast = 5, nSlow = 8, nSig = 5) #ADX data retrive
adx3col <- adxtable[,c(1,2,4)] #retrive 3 columns from adx
lagtable <- (adxtable[,c(4)] - lag(adxtable[,c(4)])) #calculate adx lag column deduction from adx column
dlplagtable <- (adxtable[,c(1)] - lag(adxtable[,c(1)])) #calculate adx lag column deduction from adx column
bindadx <- cbind(stdata4col, adx3col, lagtable, dlplagtable)
bindEMA <- cbind(ematbl, ematbl22)
adxemalag <- EMA(lagtable)
macdtbl <- MACD(stdata4col, nFast = 12, nSlow = 26, nSig = 9, maType=WMA, percent = TRUE)
stoch <- stoch(stdata4col)
ematbl <- WMA(stdata4col)
ematbl22 <- EMA(stdata4col, n=5)
emalag <- roctbl-lag(roctbl)
gmmatbl<- GMMA(stdata4col)
roctbl<- VHF(stdata4col)
nulltbl <- stdata4col*0
lagtblwithnull<-cbind(lagtable, nulltbl)
almatbl<- ALMA(stdata4col)
lagalmatbl <- almatbl - lag(almatbl)
bind.alma <- cbind(stdata4col, almatbl)

dvitbl <- DVI(stdata4col, n = 252, wts = c(0.8, 0.2), smooth = 3, magnitude = c(5, 100, 5), stretch = c(10, 100, 2), exact.multiplier = 1)



data(stdata)
m <- tail(OHLC(stdata), n = 120)
dygraph(m) %>%
  dyCandlestick()



dygraph(stdata)
dygraph(smitable)
dygraph(bindadx)
dygraph(lagtable)
dygraph(adx3col)
dygraph(clagtable)
dygraph(lagalmatbl)


#--------------------------------------syncfronisation


dy_graph <- list(
  dygraphs::dygraph(adx3col, main = "adx", group = "strat"),
  dygraphs::dygraph(macdtbl, main = "macd", group = "strat"),
  dygraphs::dygraph(lagtblwithnull, main = "lag", group = "strat"),
  dygraphs::dygraph(clagtable1, main = "clagtable1", group = "strat"),
  dygraphs::dygraph(gmmatbl, main = "gmma", group = "strat"),
  dygraphs::dygraph(bind.alma, main = "almaprice", group = "strat"),
  dygraphs::dygraph(stoch, main = "stoch", group = "strat")
)
htmltools::browsable(htmltools::tagList(dy_graph))
#-------------------------------------------
####   
#   (ADX.lag < 0) && (ADX > 16) && (Dln > Dlp)
#   (ADX.lag > 0) && (ADX > 16) && (Dln > Dlp)
#   
#   
#   
####


summary(stdata4col)
lm(stdata4col)
sd(stdata4col)


#########################################

clagtable555 <- na.omit(clagtable)

for (i in 1:nrow(clagtable555)) {
  if(clagtable555[i]>0){
    clagtable555[i] <- clagtable555[i]*0
  }
  else {
    clagtable555[i] <- clagtable555[i]*0+1
  }
}

#########################################


clagtable <- c((adxtable[,c(4)] - lag(adxtable[,c(4)])))  #calculate adx lag column deduction from adx column







clagtable1 <- ifelse(clagtable > 0 ,0.5,1)#######
dygraph(clagtable1)####





