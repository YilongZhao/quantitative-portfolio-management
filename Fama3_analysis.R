library(quadprog)
library(tseries)
library(readr)
library(zoo)
library(quantmod)
library(quadprog)


# ====================================================================================================================

fama3 <- read_csv("E:/NYU/course/3rdSM/portfolio/final/FAMA3.csv")
which(fama3$Date == "9/30/2016")
# [1] 1
which(fama3$Date == "8/15/2016")
# [1] 34
which(fama3$Date == "1/4/2016")
# [1] 189
which(fama3$Date == "1/3/2007")
# [1] 2455
fama3$Date[2266] # 2455-189
# [1] "10/3/2007"
fama3$Date[2300] # 2455-189+34
# [1] "8/15/2007"

# after recession, short-term / long-term
fama3_aftrc_short <- fama3[1:34, ]
fama3_aftrc_long <- fama3[1:189, ]

# before recession, short-term / long-term
fama3_befrc_short <- fama3[2266:2300, ]
fama3_befrc_long <- fama3[2266:2455, ]



# ====================================================================================================================
# after-recession short-term
{
r_MktLessRF <- vector(mode = "numeric", length = length(fama3_aftrc_short$Date))
r_SMB <- vector(mode = "numeric", length = length(fama3_aftrc_short$Date))
r_HML <- vector(mode = "numeric", length = length(fama3_aftrc_short$Date))

i <- 1
for( i in 1:33){
  r_MktLessRF[i] <- (1 + fama3_aftrc_short$`Mkt-RF`[i]/100)/(1 + fama3_aftrc_short$`Mkt-RF`[i+1]/100)
  r_SMB[i] <- (1 + fama3_aftrc_short$SMB[i]/100)/(1 + fama3_aftrc_short$SMB[i+1]/100)
  r_HML[i] <- (1 + fama3_aftrc_short$HML[i]/100)/(1 + fama3_aftrc_short$HML[i+1]/100)
  
  i <- i+1
}
# geo-average return
cumprod(r_MktLessRF[1:33])[33]^(1/33)
cumprod(r_SMB[1:33])[33]^(1/33)
cumprod(r_HML[1:33])[33]^(1/33)
}

# =====================================================================================================================
# after-recession long-term
{
r_MktLessRF <- vector(mode = "numeric", length = length(fama3_aftrc_long$Date))
r_SMB <- vector(mode = "numeric", length = length(fama3_aftrc_long$Date))
r_HML <- vector(mode = "numeric", length = length(fama3_aftrc_long$Date))

i <- 1
for( i in 1:188){
  r_MktLessRF[i] <- (1 + fama3_aftrc_long$`Mkt-RF`[i]/100)/(1 + fama3_aftrc_long$`Mkt-RF`[i+1]/100)
  r_SMB[i] <- (1 + fama3_aftrc_long$SMB[i]/100)/(1 + fama3_aftrc_long$SMB[i+1]/100)
  r_HML[i] <- (1 + fama3_aftrc_long$HML[i]/100)/(1 + fama3_aftrc_long$HML[i+1]/100)
  
  i <- i+1
}
# geo-average return
cumprod(r_MktLessRF[1:188])[188]^(1/188)
cumprod(r_SMB[1:188])[188]^(1/188)
cumprod(r_HML[1:188])[188]^(1/188)
}

# =====================================================================================================================
# before-recession short-term
{
  r_MktLessRF <- vector(mode = "numeric", length = length(fama3_befrc_short$Date))
  r_SMB <- vector(mode = "numeric", length = length(fama3_befrc_short$Date))
  r_HML <- vector(mode = "numeric", length = length(fama3_befrc_short$Date))
  
  i <- 1
  for( i in 1:33){
    r_MktLessRF[i] <- (1 + fama3_befrc_short$`Mkt-RF`[i]/100)/(1 + fama3_befrc_short$`Mkt-RF`[i+1]/100)
    r_SMB[i] <- (1 + fama3_befrc_short$SMB[i]/100)/(1 + fama3_befrc_short$SMB[i+1]/100)
    r_HML[i] <- (1 + fama3_befrc_short$HML[i]/100)/(1 + fama3_befrc_short$HML[i+1]/100)
    
    i <- i+1
  }
  # geo-average return
  cumprod(r_MktLessRF[1:33])[33]^(1/33)
  cumprod(r_SMB[1:33])[33]^(1/33)
  cumprod(r_HML[1:33])[33]^(1/33)
}

# =====================================================================================================================
# before-recession long-term
{
  r_MktLessRF <- vector(mode = "numeric", length = length(fama3_befrc_long$Date))
  r_SMB <- vector(mode = "numeric", length = length(fama3_befrc_long$Date))
  r_HML <- vector(mode = "numeric", length = length(fama3_befrc_long$Date))
  
  i <- 1
  for( i in 1:188){
    r_MktLessRF[i] <- (1 + fama3_befrc_long$`Mkt-RF`[i]/100)/(1 + fama3_befrc_long$`Mkt-RF`[i+1]/100)
    r_SMB[i] <- (1 + fama3_befrc_long$SMB[i]/100)/(1 + fama3_befrc_long$SMB[i+1]/100)
    r_HML[i] <- (1 + fama3_befrc_long$HML[i]/100)/(1 + fama3_befrc_long$HML[i+1]/100)
    
    i <- i+1
  }
  # geo-average return
  cumprod(r_MktLessRF[1:188])[188]^(1/188)
  cumprod(r_SMB[1:188])[188]^(1/188)
  cumprod(r_HML[1:188])[188]^(1/188)
}
