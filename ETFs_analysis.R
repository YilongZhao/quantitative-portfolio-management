library(quadprog)
library(tseries)
library(readr)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)


# ====================================================================================================================

# load data # time period can be changed
{
  fxe <- get.hist.quote(instrument = "fxe", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  ewj <- get.hist.quote(instrument = "ewj", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  gld <- get.hist.quote(instrument = "gld", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  qqq <- get.hist.quote(instrument = "qqq", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  spy <- get.hist.quote(instrument = "spy", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  shv <- get.hist.quote(instrument = "shv", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  dba <- get.hist.quote(instrument = "dba", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  uso <- get.hist.quote(instrument = "uso", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  xbi <- get.hist.quote(instrument = "xbi", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  ilf <- get.hist.quote(instrument = "ilf", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  gaf <- get.hist.quote(instrument = "gaf", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  epp <- get.hist.quote(instrument = "epp", start = "2007-08-15", end = "2007-10-02", quote = "Close")
  fez <- get.hist.quote(instrument = "fez", start = "2007-08-15", end = "2007-10-02", quote = "Close")
}

fama3 <- read_csv("E:/NYU/course/3rdSM/portfolio/final/fama.csv")
head(fama3)
tail(fama3)
length(fama3$Date)

{
  r_fxe <- double(189)
  r_ewj <- double(189)
  r_gld <- double(189)
  r_qqq <- double(189)
  r_spy <- double(189)
  r_shv <- double(189)
  r_dba <- double(189)
  r_uso <- double(189)
  r_xbi <- double(189)
  r_ilf <- double(189)
  r_gaf <- double(189)
  r_epp <- double(189)
  r_fez <- double(189)
  r_MktLessRF <- double(189)
  r_SMB <- double(189)
  r_HML <- double(189)
}

i <- 2
for( i in 2:189){
  r_fxe[i] <- coredata(fxe)[i]/coredata(fxe)[i-1] - 1
  r_ewj[i] <- coredata(ewj)[i]/coredata(ewj)[i-1] - 1
  r_gld[i] <- coredata(gld)[i]/coredata(gld)[i-1] - 1
  r_qqq[i] <- coredata(qqq)[i]/coredata(qqq)[i-1] - 1
  r_spy[i] <- coredata(spy)[i]/coredata(spy)[i-1] - 1
  r_shv[i] <- coredata(shv)[i]/coredata(shv)[i-1] - 1
  r_dba[i] <- coredata(dba)[i]/coredata(dba)[i-1] - 1 
  r_uso[i] <- coredata(uso)[i]/coredata(uso)[i-1] - 1
  r_xbi[i] <- coredata(xbi)[i]/coredata(xbi)[i-1] - 1
  r_ilf[i] <- coredata(ilf)[i]/coredata(ilf)[i-1] - 1
  r_gaf[i] <- coredata(gaf)[i]/coredata(gaf)[i-1] - 1
  r_epp[i] <- coredata(epp)[i]/coredata(epp)[i-1] - 1
  r_fez[i] <- coredata(fez)[i]/coredata(fez)[i-1] - 1
  
  i <- i+1
}

cumreturn <- (c(coredata(fxe)[length(fxe)]/coredata(fxe)[1],
               coredata(ewj)[length(ewj)]/coredata(ewj)[1],
               coredata(gld)[length(gld)]/coredata(gld)[1],
               coredata(qqq)[length(qqq)]/coredata(qqq)[1],
               coredata(spy)[length(spy)]/coredata(spy)[1],
               coredata(shv)[length(shv)]/coredata(shv)[1],
               coredata(dba)[length(dba)]/coredata(dba)[1],
               coredata(uso)[length(uso)]/coredata(uso)[1],
               coredata(xbi)[length(xbi)]/coredata(xbi)[1],
               coredata(ilf)[length(ilf)]/coredata(ilf)[1],
               coredata(gaf)[length(gaf)]/coredata(gaf)[1],
               coredata(epp)[length(epp)]/coredata(epp)[1],
               coredata(fez)[length(fez)]/coredata(fez)[1])-1)*100

Daily_mean <- c(mean(r_fxe, na.rm = T),
                mean(r_ewj, na.rm = T),
                mean(r_gld, na.rm = T),
                mean(r_qqq, na.rm = T),
                mean(r_spy, na.rm = T),
                mean(r_shv, na.rm = T),
                mean(r_dba, na.rm = T),
                mean(r_uso, na.rm = T),
                mean(r_xbi, na.rm = T), 
                mean(r_ilf, na.rm = T), 
                mean(r_gaf, na.rm = T), 
                mean(r_epp, na.rm = T), 
                mean(r_fez, na.rm = T))*100

Daily_min <- c(min(r_fxe, na.rm = T),
                min(r_ewj, na.rm = T),
                min(r_gld, na.rm = T),
                min(r_qqq, na.rm = T),
                min(r_spy, na.rm = T),
                min(r_shv, na.rm = T),
                min(r_dba, na.rm = T),
                min(r_uso, na.rm = T),
                min(r_xbi, na.rm = T), 
                min(r_ilf, na.rm = T), 
                min(r_gaf, na.rm = T), 
                min(r_epp, na.rm = T), 
                min(r_fez, na.rm = T))*100

Max_DD <- c(maxdrawdown(fxe)$maxdrawdown,
            maxdrawdown(ewj)$maxdrawdown,
            maxdrawdown(gld)$maxdrawdown,
            maxdrawdown(qqq)$maxdrawdown,
            maxdrawdown(spy)$maxdrawdown,
            maxdrawdown(shv)$maxdrawdown,
            maxdrawdown(dba)$maxdrawdown,
            maxdrawdown(uso)$maxdrawdown,
            maxdrawdown(xbi)$maxdrawdown, 
            maxdrawdown(ilf)$maxdrawdown, 
            maxdrawdown(gaf)$maxdrawdown, 
            maxdrawdown(epp)$maxdrawdown, 
            maxdrawdown(fez)$maxdrawdown)

Volatility <- c(sd(r_fxe, na.rm = T),
                sd(r_ewj, na.rm = T),
                sd(r_gld, na.rm = T),
                sd(r_qqq, na.rm = T),
                sd(r_spy, na.rm = T),
                sd(r_shv, na.rm = T),
                sd(r_dba, na.rm = T),
                sd(r_uso, na.rm = T),
                sd(r_xbi, na.rm = T), 
                sd(r_ilf, na.rm = T), 
                sd(r_gaf, na.rm = T), 
                sd(r_epp, na.rm = T), 
                sd(r_fez, na.rm = T))*100

sharpe_ratio <- c(sharpe(fxe),
                  sharpe(ewj),
                  sharpe(gld),
                  sharpe(qqq),
                  sharpe(spy),
                  sharpe(shv),
                  sharpe(dba),
                  sharpe(uso),
                  sharpe(xbi), 
                  sharpe(ilf), 
                  sharpe(gaf), 
                  sharpe(epp), 
                  sharpe(fez))

output <- data.frame(ETF = c("fxe", "ewj", "gld", "qqq", "spy", "shv", "dba", "uso", "xbi", "ilf", "gaf", "epp", "fez"), cum_return = cumreturn, Daily_mean = Daily_mean, Daily_min = Daily_min, Max_Drawdown = Max_DD, volatility = Volatility, sharpe_ratio = sharpe_ratio)
write.csv(t(rbind(output, output_pf)), file = "PortfolioResults.csv")

