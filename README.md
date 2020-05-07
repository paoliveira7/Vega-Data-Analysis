# Vega-Data-Analysis
install.packages("data.table")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("TTR")
library(data.table)
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
install.packages("dplyr")
install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)
library(dplyr)

tickers <- c("ITSA4.SA", "EGIE3.SA", "WEGE3.SA", "KLBN11.SA")
weights <- c(.25, .25, .25, .25)

#Segundo passo é puxar os dados pela fonte de dados
portfolioprices <- NULL
for(Ticker in tickers)
  portfolioprices <- cbind(portfolioprices,
                           getSymbols.yahoo(Ticker,from = "2010-01-01",to="2020-05-06",periodicity = "monthly", auto.assign=FALSE)[,4])
portfolioreturns <- na.omit(ROC(portfolioprices, type = "discrete"))

#Calcular o índice de referêcia para fazer a comparação

benchmarkvalue <- getSymbols.yahoo("BOVA11.SA",from = "2010-01-01",to="2020-05-06",periodicity = "monthly", auto.assign=FALSE)[,4]
benchmarkreturn <- na.omit(ROC(benchmarkvalue, type = "discrete"))


#Calcular retorno do portfólio e plotando gráficos modafocas 

Carteiraretorno <- Return.portfolio(portfolioreturns)

CAPM.beta(portfolioreturns, benchmarkreturn, 5/12)
CAPM.jensenAlpha(portfolioreturns,benchmarkreturn,5/12)

table.AnnualizedReturns(portfolioreturns)
table.CalendarReturns(portfolioreturns)
charts.PerformanceSummary(Carteiraretorno,Rf= 4/12, main = "Carteira By Pablo", geometric = TRUE, methods = "none", ylog = FALSE)
