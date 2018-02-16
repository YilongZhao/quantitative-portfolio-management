library(quadprog)
library(tseries)
library(readr)
library(zoo)
library(quantmod)



# ====================================================================================================================
# get optimal

# load data # time period can be changed
{
fxe <- get.hist.quote(instrument = "fxe", start = "2008-08-13", end = "2008-09-30", quote = "Close")
ewj <- get.hist.quote(instrument = "ewj", start = "2008-08-13", end = "2008-09-30", quote = "Close")
gld <- get.hist.quote(instrument = "gld", start = "2008-08-13", end = "2008-09-30", quote = "Close")
qqq <- get.hist.quote(instrument = "qqq", start = "2008-08-13", end = "2008-09-30", quote = "Close")
spy <- get.hist.quote(instrument = "spy", start = "2008-08-13", end = "2008-09-30", quote = "Close")
shv <- get.hist.quote(instrument = "shv", start = "2008-08-13", end = "2008-09-30", quote = "Close")
dba <- get.hist.quote(instrument = "dba", start = "2008-08-13", end = "2008-09-30", quote = "Close")
uso <- get.hist.quote(instrument = "uso", start = "2008-08-13", end = "2008-09-30", quote = "Close")
xbi <- get.hist.quote(instrument = "xbi", start = "2008-08-13", end = "2008-09-30", quote = "Close")
ilf <- get.hist.quote(instrument = "ilf", start = "2008-08-13", end = "2008-09-30", quote = "Close")
gaf <- get.hist.quote(instrument = "gaf", start = "2008-08-13", end = "2008-09-30", quote = "Close")
epp <- get.hist.quote(instrument = "epp", start = "2008-08-13", end = "2008-09-30", quote = "Close")
fez <- get.hist.quote(instrument = "fez", start = "2008-08-13", end = "2008-09-30", quote = "Close")
}

# as.vector
{
fxe1 <- as.vector(fxe)
ewj1 <- as.vector(ewj)
gld1 <- as.vector(gld)
qqq1 <- as.vector(qqq)
spy1 <- as.vector(spy)
shv1 <- as.vector(shv)
dba1 <- as.vector(dba)
uso1 <- as.vector(uso)
xbi1 <- as.vector(xbi)
ilf1 <- as.vector(ilf)
gaf1 <- as.vector(gaf)
epp1 <- as.vector(epp)
fez1 <- as.vector(fez)
}

fama3 <- read_csv("E:/NYU/course/3rdSM/portfolio/final/fama5.csv")
head(fama3)
tail(fama3)
length(fama3$Date)
length(fxe)
# [1] 2456
# [1] 189 for starts from 1/1/2007
# [1] 34 for starts from 8/15/2007

# state r_fxe, it is return less risk-free rate
{
r_fxe <- double(34)
r_ewj <- double(34)
r_gld <- double(34)
r_qqq <- double(34)
r_spy <- double(34)
r_shv <- double(34)
r_dba <- double(34)
r_uso <- double(34)
r_xbi <- double(34)
r_ilf <- double(34)
r_gaf <- double(34)
r_epp <- double(34)
r_fez <- double(34)
r_MktLessRF <- double(34)
r_SMB <- double(34)
r_HML <- double(34)
}
# calculate r:
i <- 2
for( i in 2:34){
  r_fxe[i] <- fxe1[i]/fxe1[i-1] - 1 - fama3$RF[i]/100
  r_ewj[i] <- ewj1[i]/ewj1[i-1] - 1 - fama3$RF[i]/100
  r_gld[i] <- gld1[i]/gld1[i-1] - 1 - fama3$RF[i]/100
  r_qqq[i] <- qqq1[i]/qqq1[i-1] - 1 - fama3$RF[i]/100
  r_spy[i] <- spy1[i]/spy1[i-1] - 1 - fama3$RF[i]/100
  r_shv[i] <- shv1[i]/shv1[i-1] - 1 - fama3$RF[i]/100
  r_dba[i] <- dba1[i]/dba1[i-1] - 1 - fama3$RF[i]/100
  r_uso[i] <- uso1[i]/uso1[i-1] - 1 - fama3$RF[i]/100
  r_xbi[i] <- xbi1[i]/xbi1[i-1] - 1 - fama3$RF[i]/100
  r_ilf[i] <- ilf1[i]/ilf1[i-1] - 1 - fama3$RF[i]/100
  r_gaf[i] <- gaf1[i]/gaf1[i-1] - 1 - fama3$RF[i]/100
  r_epp[i] <- epp1[i]/epp1[i-1] - 1 - fama3$RF[i]/100
  r_fez[i] <- fez1[i]/fez1[i-1] - 1 - fama3$RF[i]/100
  r_MktLessRF[i] <- (1 + fama3$`Mkt-RF`[i]/100)/(1 + fama3$`Mkt-RF`[i-1]/100) - 1
  r_SMB[i] <- (1 + fama3$SMB[i]/100)/(1 + fama3$SMB[i-1]/100)  - 1
  r_HML[i] <- (1 + fama3$HML[i]/100)/(1 + fama3$HML[i-1]/100)  - 1
  
  i <- i+1
}
tail(r_fxe)
tail(r_gaf)

# adjust for infinite value in fama3 factors
r_MktLessRF[which(is.infinite(r_MktLessRF))] <- NA
r_SMB[which(is.infinite(r_SMB))] <- NA
r_HML[which(is.infinite(r_HML))] <- NA

#linear regression to get lm_fxe
alldata <- data.frame('r_fxe' = r_fxe,
                      'r_ewj' = r_ewj,
                      'r_gld' = r_gld,
                      'r_qqq' = r_qqq,
                      'r_spy' = r_spy,
                      'r_shv' = r_shv,
                      'r_dba' = r_dba,
                      'r_uso' = r_uso,
                      'r_xbi' = r_xbi,
                      'r_ilf' = r_ilf,
                      'r_gaf' = r_gaf,
                      'r_epp' = r_epp,
                      'r_fez' = r_fez,
                      'r_MktLessRF' = r_MktLessRF,
                      'r_SMB' = r_SMB,
                      'r_HML' = r_HML)
{
lm_fxe <- lm(r_fxe ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.omit)
lm_ewj <- lm(r_ewj ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_gld <- lm(r_gld ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_qqq <- lm(r_qqq ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_spy <- lm(r_spy ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_shv <- lm(r_shv ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_dba <- lm(r_dba ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_uso <- lm(r_uso ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_xbi <- lm(r_xbi ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_ilf <- lm(r_ilf ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_gaf <- lm(r_gaf ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_epp <- lm(r_epp ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
lm_fez <- lm(r_fez ~ r_MktLessRF + r_SMB + r_HML, data = alldata, na.action = na.exclude)
}
# result of market beta: MktBeta
beta <-  list(fxe = coefficients(lm_fxe),
                 ewj = coefficients(lm_ewj), 
                 gld = coefficients(lm_gld), 
                 qqq = coefficients(lm_qqq),
                 spy = coefficients(lm_spy),
                 shv = coefficients(lm_shv),
                 dba = coefficients(lm_dba),
                 uso = coefficients(lm_uso),
                 xbi = coefficients(lm_xbi),
                 ilf = coefficients(lm_ilf),
                 gaf = coefficients(lm_gaf),
                 epp = coefficients(lm_epp),
                 fez = coefficients(lm_fez))
beta

# rho(dvec) of 13 ETFs: rho 
{
rho_fxe <- as.numeric(beta$fxe[1] + beta$fxe[2]*fama3$`Mkt-RF`[length(fama3)] + beta$fxe[3]*fama3$SMB[length(fama3)] + beta$fxe[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_ewj <- as.numeric(beta$ewj[1] + beta$ewj[2]*fama3$`Mkt-RF`[length(fama3)] + beta$ewj[3]*fama3$SMB[length(fama3)] + beta$ewj[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_gld <- as.numeric(beta$gld[1] + beta$gld[2]*fama3$`Mkt-RF`[length(fama3)] + beta$gld[3]*fama3$SMB[length(fama3)] + beta$gld[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_qqq <- as.numeric(beta$qqq[1] + beta$qqq[2]*fama3$`Mkt-RF`[length(fama3)] + beta$qqq[3]*fama3$SMB[length(fama3)] + beta$qqq[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_spy <- as.numeric(beta$spy[1] + beta$spy[2]*fama3$`Mkt-RF`[length(fama3)] + beta$spy[3]*fama3$SMB[length(fama3)] + beta$spy[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_shv <- as.numeric(beta$shv[1] + beta$shv[2]*fama3$`Mkt-RF`[length(fama3)] + beta$shv[3]*fama3$SMB[length(fama3)] + beta$shv[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_dba <- as.numeric(beta$dba[1] + beta$dba[2]*fama3$`Mkt-RF`[length(fama3)] + beta$dba[3]*fama3$SMB[length(fama3)] + beta$dba[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_uso <- as.numeric(beta$uso[1] + beta$uso[2]*fama3$`Mkt-RF`[length(fama3)] + beta$uso[3]*fama3$SMB[length(fama3)] + beta$uso[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_xbi <- as.numeric(beta$xbi[1] + beta$xbi[2]*fama3$`Mkt-RF`[length(fama3)] + beta$xbi[3]*fama3$SMB[length(fama3)] + beta$xbi[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_ilf <- as.numeric(beta$ilf[1] + beta$ilf[2]*fama3$`Mkt-RF`[length(fama3)] + beta$ilf[3]*fama3$SMB[length(fama3)] + beta$ilf[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_gaf <- as.numeric(beta$gaf[1] + beta$gaf[2]*fama3$`Mkt-RF`[length(fama3)] + beta$gaf[3]*fama3$SMB[length(fama3)] + beta$gaf[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_epp <- as.numeric(beta$epp[1] + beta$epp[2]*fama3$`Mkt-RF`[length(fama3)] + beta$epp[3]*fama3$SMB[length(fama3)] + beta$epp[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho_fez <- as.numeric(beta$fez[1] + beta$fez[2]*fama3$`Mkt-RF`[length(fama3)] + beta$fez[3]*fama3$SMB[length(fama3)] + beta$fez[4]*fama3$HML[length(fama3)] + fama3$RF[length(fama3)])
rho <- c(fxe = rho_fxe, ewj = rho_ewj, gld = rho_gld, qqq = rho_qqq, spy = rho_spy, shv = rho_shv, dba = rho_dba, uso = rho_uso, xbi = rho_xbi, ilf = rho_ilf, gaf = rho_gaf, epp = rho_epp, fez = rho_fez)
}
as.vector(rho)

# coveriance matrix(Dmat): cov.mat
ETFs <- as.matrix(alldata, na.omit = T)[ ,c(1:13)]
sum(is.na(ETFs))
cov.mat <- cov(ETFs, use = "complete.obs")
diag(cov.mat) <- 1
sum(is.na(cov.mat))
Lamda <- 1 # Lamda can be changed
Dmat <- cov.mat * Lamda
Dmat
# Amat
Amat <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1, beta$fxe[2], beta$ewj[2], beta$gld[2], beta$qqq[2], beta$spy[2], beta$shv[2], beta$dba[2], beta$uso[2], beta$xbi[2], beta$ilf[2], beta$gaf[2], beta$epp[2], beta$fez[2]), 13, 2, byrow = F)
Amat
# bvec
TargetBeta <- 0.5 # target beta can be changed
bvec <- c(1, TargetBeta)

# Solve qp. 
result <- solve.QP(Dmat = cov.mat, dvec = as.vector(rho), Amat = Amat, bvec = bvec, meq = 2)
result
output.value <- c(Amat[,1] %*% result$solution, Amat[,2] %*% result$solution)
output.value

# ==============================================================================================================================
# results analysis

portfolio <- fxe*result$solution[1]+ewj*result$solution[2] + gld*result$solution[3]+qqq*result$solution[4]+spy*result$solution[5]+shv*result$solution[6]+dba*result$solution[7]+uso*result$solution[8]+xbi*result$solution[9]+ilf*result$solution[10]+gaf*result$solution[11]+epp*result$solution[12]+fez*result$solution[13]
head(portfolio)
portfolio

i <- 2
r_portfolio <- double(34)
for( i in 2:34){
  r_portfolio[i] <- coredata(portfolio)[i]/coredata(portfolio)[i-1] - 1
  i <- i +1
}
cumreturn_pf <- (coredata(portfolio)[length(portfolio)]/coredata(portfolio)[1] - 1)*100
Daily_pf <- mean(r_portfolio, na.rm = T)*100
min_pf <- min(r_portfolio, na.rm = T)*100
MaxDD_pf <- maxdrawdown(portfolio)$maxdrawdown
Volatility_pf <- sd(r_portfolio, na.rm = T)*100
sharpe_pf <- sharpe(portfolio)
                  
output_pf <- data.frame(ETF = "portfolio", cum_return = cumreturn_pf, Daily_mean = Daily_pf, Daily_min = min_pf, Max_Drawdown = MaxDD_pf, volatility = Volatility_pf, sharpe_ratio = sharpe_pf)


write.csv(t(rbind(output, output_pf)), file = "PortfolioResults.csv") # output is created by ETFs_analysis.R , containing all ETFs output

# ====================================================================================================================================
# change different beta, compare with S&P 500

TargetBeta <- 1.5 # target beta can be changed
bvec <- c(1, TargetBeta)

result <- solve.QP(Dmat = cov.mat, dvec = as.vector(rho), Amat = Amat, bvec = bvec, meq = 2)
result
output.value <- c(Amat[,1] %*% result$solution, Amat[,2] %*% result$solution)
output.value

{ 
  portfolio <- fxe*result$solution[1]+ewj*result$solution[2] + gld*result$solution[3]+qqq*result$solution[4]+spy*result$solution[5]+shv*result$solution[6]+dba*result$solution[7]+uso*result$solution[8]+xbi*result$solution[9]+ilf*result$solution[10]+gaf*result$solution[11]+epp*result$solution[12]+fez*result$solution[13]
  head(portfolio)
  portfolio
  
  i <- 2
  r_portfolio <- double(34)
  for( i in 2:34){
    r_portfolio[i] <- coredata(portfolio)[i]/coredata(portfolio)[i-1] - 1
    i <- i +1
  }
 
  cumreturn_pf <- (coredata(portfolio)[length(portfolio)]/coredata(portfolio)[1] - 1)*100
  Daily_pf <- mean(r_portfolio, na.rm = T)*100
  min_pf <- min(r_portfolio, na.rm = T)*100
  MaxDD_pf <- maxdrawdown(portfolio)$maxdrawdown
  Volatility_pf <- sd(r_portfolio, na.rm = T)*100
  sharpe_pf <- sharpe(portfolio)
  
  output_pf <- data.frame(ETF = "pf beta = 1.5", cum_return = cumreturn_pf, Daily_mean = Daily_pf, Daily_min = min_pf, Max_Drawdown = MaxDD_pf, volatility = Volatility_pf, sharpe_ratio = sharpe_pf)
}

{
cumreturn_spy <- (coredata(spy)[length(spy)]/coredata(spy)[1] - 1)*100
Daily_spy <- mean(r_spy, na.rm = T)*100
min_spy <- min(r_spy, na.rm = T)*100
MaxDD_spy <- maxdrawdown(spy)$maxdrawdown
Volatility_spy <- sd(r_spy, na.rm = T)*100
sharpe_spy <- sharpe(spy)

output_spy <- data.frame(ETF = "spy", cum_return = cumreturn_spy, Daily_mean = Daily_spy, Daily_min = min_spy, Max_Drawdown = MaxDD_spy, volatility = Volatility_spy, sharpe_ratio = sharpe_spy)
}

output_pf
output_spy
# output_bybeta <-rbind(output_pf, output_spy) 
output_bybeta <-rbind(output_pf, output_bybeta) 
output_bybeta

write.csv(t(output_bybeta), file = "betaScenario.csv")

# =====================================================================================================================================
# change different lamda to get the best sharpe ratio

Lamda <- 2 # Lamda can be changed
Dmat <- cov.mat * Lamda
Dmat

result <- solve.QP(Dmat = cov.mat, dvec = as.vector(rho), Amat = Amat, bvec = bvec, meq = 2)
result
output.value <- c(Amat[,1] %*% result$solution, Amat[,2] %*% result$solution)
output.value

{ 
  portfolio <- fxe*result$solution[1]+ewj*result$solution[2] + gld*result$solution[3]+qqq*result$solution[4]+spy*result$solution[5]+shv*result$solution[6]+dba*result$solution[7]+uso*result$solution[8]+xbi*result$solution[9]+ilf*result$solution[10]+gaf*result$solution[11]+epp*result$solution[12]+fez*result$solution[13]
  head(portfolio)
  portfolio
  
  i <- 2
  r_portfolio <- double(34)
  for( i in 2:34){
    r_portfolio[i] <- coredata(portfolio)[i]/coredata(portfolio)[i-1] - 1
    i <- i +1
  }
  
  cumreturn_pf <- (coredata(portfolio)[length(portfolio)]/coredata(portfolio)[1] - 1)*100
  Daily_pf <- mean(r_portfolio, na.rm = T)*100
  min_pf <- min(r_portfolio, na.rm = T)*100
  MaxDD_pf <- maxdrawdown(portfolio)$maxdrawdown
  Volatility_pf <- sd(r_portfolio, na.rm = T)*100
  sharpe_pf <- sharpe(portfolio)
  
  output_pf <- data.frame(ETF = "pf lamda = 2", cum_return = cumreturn_pf, Daily_mean = Daily_pf, Daily_min = min_pf, Max_Drawdown = MaxDD_pf, volatility = Volatility_pf, sharpe_ratio = sharpe_pf)
}

# output_bylamda <- output_pf
output_bylamda <- rbind(output_pf, output_bylamda) 
output_bylamda

write.csv(t(output_bylamda), file = "lamdaScenario.csv")


