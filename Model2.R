if(TRUE){
  #install.packages("rvest")
  library(rvest)
  #language = en
  Tickers <- c("MCD","IBM","GE")
  
  #Create array of dates
  url <- paste('https://finance.yahoo.com/quote/',Tickers[1],'/history?period1=1393020000&period2=1550786400&interval=1mo&filter=history&frequency=1mo',sep = "")
  webpage <- read_html(url)
  webData <- webpage %>% html_node("[data-test='historical-prices']") %>% html_table()
  
  #Remove NON NUMERIC
  webData[,2] <- sapply(webData[,2], as.numeric)
  webData <- webData[!is.na(webData[,2]),]
  HistoricReturns <- webData[1]
  
  for(i in Tickers){
    print(i)
    Ticker <- i
    
    url <- paste('https://finance.yahoo.com/quote/',i,'/history?period1=1393020000&period2=1550786400&interval=1mo&filter=history&frequency=1mo',sep = "")
    webpage <- read_html(url)
    webData <- webpage %>% html_node("[data-test='historical-prices']") %>% html_table()
    
    #Adjusted Prices Only
    closeData <- webData[,c(1,6)]
    #add header (ticker)
    names(closeData)[2] <- Ticker
    #convert to numeric
    closeData[,2] <- sapply(closeData[,2], as.numeric)
    #remove Non-numeric
    closeData <- closeData[!is.na(closeData[,2]),]
    
    #merge
    HistoricReturns <- merge(HistoricReturns,closeData, by.x = "Date", by.y = "Date", all.x = TRUE)  
    
  }
}

if (1=1){
  
  ##Basic Stats
  MeanReturns <- colMeans(HistoricReturns)
  SigmaReturns <- apply(HistoricReturns, 2,sd)
  CovMat <- cov(HistoricReturns)
  
  ## Finding 2 efficient portfolios
  #Assuming Risk Free Rate
  RfOne <- 0.000
  RfTwo <- 0.001
  
  #Calculating risk premium
  PremiumA <- MeanReturns-RfOne
  PremiumB <- MeanReturns-RfTwo
  
  #Calculating assets weights in efficient portfolios
  Port.A.Z <- solve(CovMat) %*% PremiumA
  Port.B.Z <- solve(CovMat) %*% PremiumB
  
  Port.A.Zsum <- sum(Port.A.Z[,1])
  Port.B.Zsum <- sum(Port.B.Z[,1])
  
  Port.A.W <- Port.A.Z/Port.A.Zsum
  Port.B.W <- Port.B.Z/Port.B.Zsum
  
  #Efficient portfolios stats
  VarA <- t(Port.A.W) %*% CovMat %*% Port.A.W
  VarB <- t(Port.B.W) %*% CovMat %*% Port.B.W
  CovA.B <- t(Port.B.W) %*% CovMat %*% Port.A.W
  
  SigmaA <-sqrt(VarA)
  SigmaB <-sqrt(VarB)
  
  ReturnA <- MeanReturns %*% Port.A.W
  ReturnB <- MeanReturns %*% Port.B.W
  
  #RhoA.B <- CovA.B/(SigmaA*SigmaB)
  
  ##drawing the efficient frontier
  #Define axes
  Xa <- c(-7)
  factorX = Xa
  
  for (i in 1:50){
    
    factorX = factorX + 0.4
    Xa <- append(Xa,factorX)
    #print(factorX)
  }
  
  Xb <- 1-Xa
  
  #Calculate sigma and expected return
  FrontierVariance <- (Xa^2)*c(VarA)+(Xb^2)*c(VarB)+2*Xa*Xb*c(CovA.B)
  FrontierSigma <- sqrt(FrontierVariance)
  FrontierReturn <- Xa*c(ReturnA) + Xb*c(ReturnB)
  
  #Plot
  plot(FrontierSigma,FrontierReturn)
  
  ##CAPM
  #Explenation: we found the market portfolio assuming Rf as Port.B calculation
  Rf <- RfTwo
  Xrf <- Xa
  
  CMLSigma <- Xb*c(SigmaB)
  CMLReturn <- Xb*c(ReturnB)+Xrf*c(Rf)
  
  plot(CMLSigma,CMLReturn)
}

