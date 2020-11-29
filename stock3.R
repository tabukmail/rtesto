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

# Sample data
library(quantmod)
stockID<-"AAPL"
stdata <- getSymbols(stockID, from="2019-02-01", auto.assign = FALSE)

#-----------------------------------------------------------------------------------------
stockID<-"DBK.DE"
#stdata <- na.omit(getSymbols(stockID, auto.assign = FALSE))
aa<-chartSeries(stdata, subset = '20100401/20201006', type = 'bars', theme = chartTheme("black", up.col='green', down.col='red'),
                TA = c(addADX(), addMACD(), addSMI(), addEVWMA(), addRSI(), addSMA()))


stdata4col <- na.omit(stdata[,c(4)])
volume <- stdata[,c(5)]
adxtable <- ADX(HLC(stdata), n=26, maType=EMA, wilder=TRUE) #ADX data retrive
smitable <- SMI(HLC(stdata), n=13,nFast = 2, nSlow = 25, nSig = 9, bounded = TRUE) #
rsitable <- RSI(stdata4col, n=14, maType="WMA", wts=volume)
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



clagtable <- c((adxtable[,c(4)] - lag(adxtable[,c(4)])))
clagtable1 <- ifelse(clagtable > 0 ,0.5,1)



dy_graph <- list(
  dygraphs::dygraph(adx3col, main = "adx", group = "strat"),
  dygraphs::dygraph(macdtbl, main = "macd", group = "strat"),
  dygraphs::dygraph(lagtblwithnull, main = "lag", group = "strat"),
  dygraphs::dygraph(clagtable1, main = "clagtable1", group = "strat"),
  dygraphs::dygraph(gmmatbl, main = "gmma", group = "strat"),
  dygraphs::dygraph(bind.alma, main = "almaprice", group = "strat"),
  dygraphs::dygraph(smitable, main = "SMI", group = "strat"),
  dygraphs::dygraph(rsitable, main = "RSI", group = "strat"),
  dygraphs::dygraph(stoch, main = "stoch", group = "strat")
)
htmltools::browsable(htmltools::tagList(dy_graph))

# Sample data
library(quantmod)
stockID<-"DBK.DE"
stdata2 <- getSymbols(stockID, from="2019-02-01", auto.assign = FALSE)

#-----------------------------------------------------------------------------------------
# Rolling regression (unweighted), with prediction intervals
x <- rollapplyr( 
  as.zoo(Ad(stdata2)), 
  width=300, by.column = FALSE, 
  FUN = function(x) {
    r <- lm( x ~ index(x) )
    tail(predict(r, interval="prediction"),1)
  } 
)

# Plots
plot( index(stdata2), Ad(stdata2), type="l", lwd=3, las=1 )
lines( index(x), x$fit, col="purple", lwd=3 )
lines( index(x), x$lwr, col="purple", lwd=3, lty=3 )
lines( index(x), x$upr, col="purple", lwd=3, lty=3 )
abline( lm( Ad(stdata2) ~ index(stdata2) ), col="light blue", lwd=3 )  
