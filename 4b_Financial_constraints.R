library(dplyr)
library(lubridate)
library(readstata13)
invest <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4a_INVESTMENT_data.csv",
                    header = T,
                    sep = ";",
                    dec = ",",
                    na.strings = "",
                    stringsAsFactors = F)

# Ticker clean ------------------------------------------------------------
#invest <- filter(invest,
#                     ticker != "AELP3" &
#                       ticker != "AGIN3" &
#                       ticker != "BHGR3" &
#                       ticker != "BISA3" &
#                       ticker != "CCIM3" &
#                       ticker != "COCE3" &
#                       ticker != "CRDE3" &
#                       ticker != "CVCB3" &
#                       ticker != "DSUL3" &
#                       ticker != "ELPL4" &
#                       ticker != "EVEN3" &
#                       ticker != "GFSA3" &
#                       ticker != "HBOR3" &
#                       ticker != "JFEN3" &
#                       ticker != "MRVE3" &
#                       ticker != "ODPV3" &
#                       ticker != "PDGR3" &
#                       ticker != "QUAL3" &
#                       ticker != "RDNI3" &
#                       ticker != "TCSA3" &
#                       ticker != "TEND3" &
#                       ticker != "TMCP3" &
#                       ticker != "TRIS3" &
#                       ticker != "UNIP3")

# KZ Index --------------------------------------------------------------
#invest$CF <- invest$cash_net_change_CF
#invest$CF[is.na(invest$CF)] <- invest$cash_BS_variation[is.na(invest$CF)]
#invest$CF[is.na(invest$CF)] <- invest$ebitda_IS[is.na(invest$CF)]

invest$KZ <- ( - 1.001909*(invest$cash_BS_variation/invest$Klag)
               + 3.139193*(invest$D_lt_BS/(invest$Dstlt_BS+invest$equity_BS))
               - 39.3678*(-invest$tdividends/(4*invest$Klag))
               - 1.314759*(invest$cash/invest$Klag)
               + (0.2826389*invest$Q))
invest[invest$KZ==-Inf & !is.na(invest$KZ),"KZ"] <- NA



#KZ not dynamic
invest$KZND <- rep(NULL,nrow(invest))
for (i in unique(invest$ticker)) {
      kz <- invest[invest$ticker==i,]
      kz <- 
      (- 1.001909*(mean(kz$cash_BS_variation/kz$Klag, na.rm = T))
      + 0.2826389*mean(kz$Q, na.rm = T)
      + 3.139193*(mean(kz$D_lt_BS/(kz$Dstlt_BS+kz$equity_BS), na.rm = T))
      - 39.3678*(-mean(kz$tdividends/kz$Klag, na.rm = T))
      - 1.314759*(mean(kz$cash/kz$Klag, na.rm = T)))
      invest[invest$ticker==i,"KZND"] <- kz
      rm(kz)
}

      #Check de valores KZ
      x <- invest[!is.na(invest$KZ),c("ticker","quarter","KZ","Klag",
                                    "Q","D_lt_BS","Dstlt_BS","equity_BS","tdividends","cash")]
      x <- arrange(x,KZ)
      
      y <- head(x,200)
      arrange(as.data.frame(table(y$ticker),stringsAsFactors = F),Freq)
      z <- tail(x,200)
      arrange(as.data.frame(table(z$ticker),stringsAsFactors = F),Freq)
      
      a <- group_by(x,ticker)
      a <- summarise(a, KZ = mean(KZ,na.rm = T))
      a <- ungroup(a)
      a <- as.data.frame(a,stringsAsFactors=F)
      arrange(a,KZ)
      rm(x,y,z,a)

invest <- filter(invest,
                 ticker != "EQTL3" &
                       ticker != "OGXP3" &
                       ticker != "RSID3" &
                       ticker != "BRAP3" &
                       ticker != "EZTC3" &
                       ticker != "GETI3" &
                       ticker != "LAME3" &
                       ticker != "LUPA3" &
                       ticker != "DTCY3" &
                       ticker != "BAHI3" &
                       ticker != "IENG3" &
                       ticker != "CPNY3" &
                       ticker != "CCXC3" &
                       ticker != "KSSA3" &
                       ticker != "JSLG3" &
                       ticker != "NETC4" &
                       ticker != "PTBL3")

x <- unique(select(invest, ticker,KZND))
arrange(x,KZND)
invest <- filter(invest,
                 ticker != "SNSL3" &
                       ticker != "ARTR3" &
                       ticker != "RDCD3" &
                       ticker != "CIEL3" &
                       ticker != "CYRE3" &
                       ticker != "BEMA3" &
                       ticker != "TEMP3" &
                       ticker != "AGRO3" &
                       ticker != "TOTS3" &
                       ticker != "DIRR3" &
                       ticker != "ECOR3" &
                       ticker != "IDNT3" &
                       ticker != "ACES3" &
                       ticker != "IMCH3" &
                       ticker != "MGLU3" &
                       ticker != "WHRL3")
rm(x)


#KZ Index = -1.001909 x (Cash Flows / K) 
#           + 0.2826389 x Q
#           + 3.139193 x Debt / Total Capital 
#           - 39.3678 x Dividends / K
#           - 1.314759 x Cash / K
#Cash Flows = (Income Before Extraordinary Itemst + Total Depreciation and Amortizationt)
      #EBITDA
#K = PP&Et-1
      #Fixed assets (t - 1)
#Q = Tobin's Q
      # (Market Capitalizationt
      # + Total Shareholder's Equityt
      # - Book Value of Common Equityt
      # - Deferred Tax Assetst) 
      # / Total Shareholder's Equityt
#Debt = Total Long Term Debtt 
#           + Notes Payablet
#           + Current Portion of Long Term Debtt
#Dividends = Total Cash Dividends Paidt (common and preferred)
#Cash = Cash and Short-Term Investmentst

# Industry revenues growth ------------------------------------------------
#COMPUTE SECTOR REVENUES GROWTH
reg <- read.csv2("D:\\$database\\ownCloud\\database\\rawdata\\economatica_registry\\BRA_BMFBOVESPA.txt",
                 sep = ";",
                 dec =",",
                 na.strings = "-",
                 skip = 4,
                 stringsAsFactors = F)

##Junção das tabelas de dados e cadastro
reg <- select(reg, 
              Ticker,
              Sector.NAICS.Level.1,
              Sector.NAICS.Level.2)
colnames(reg) <- c("ticker",
                   "naicslvl1",
                   "naicslvl12")
invest <- merge.data.frame(invest, reg, 
                           by.x = "ticker", 
                           by.y = "ticker", 
                           all.x = T, 
                           all.y = F)
rm(reg)

sector <- select(invest,
                 ticker,
                 quarter,
                 naicslvl1,
                 naicslvl12,
                 revenues_net_operating_IS,
                 revenues_net_operating_growth_IS,
                 market_capitalization)
library(stringi)
sector$proc <- stri_join(sector$quarter,sector$naicslvl1,sep = "_")
x <- aggregate(sector$market_capitalization,
               by = list(sector$proc),
               FUN = sum, na.rm = T)
colnames(x) <- c("proc","marketI")
sector <- merge.data.frame(x, sector, 
                           by.x = "proc", 
                           by.y = "proc", 
                           all.x = T, 
                           all.y = F)
rm(x)
sector$weight <- sector$market_capitalization/sector$marketI
sector$revenues_industry <- sector$weight*sector$revenues_net_operating_growth_IS/100
sector <- aggregate(sector$revenues_industry,
                    by = list(sector$proc),
                    FUN = sum, na.rm = T)
colnames(sector) <- c("proc","revenues_growth_industry")

invest$proc <- stri_join(invest$quarter,invest$naicslvl1,sep = "_")
invest <- merge.data.frame(invest, sector, 
                           by.x = "proc", 
                           by.y = "proc", 
                           all.x = T, 
                           all.y = F)

# WW Index ----------------------------------------------------------------


      invest$WW = ((-0.091*(invest$cash_BS_variation/invest$assets_total_BS))
             + (-0.062*invest$ddividends)
             + (0.021*invest$D_lt_BS/invest$assets_total_BS)
             + (-0.044*log(invest$assets_total_BS))
             + (-0.035*invest$revenues_net_operating_growth_IS/100)
             + (0.102*invest$revenues_growth_industry))

#WW not dynamic
invest$WWND <- rep(NULL,nrow(invest))
for (i in unique(invest$ticker)) {
      ww <- invest[invest$ticker==i,]
      ww <- (
            (-0.091*(mean(ww$cash_BS_variation/ww$assets_total_BS, na.rm = T)))
            + (-0.062*mean(ww$ddividends, na.rm = T))
            + (0.021*mean(ww$D_lt_BS/ww$assets_total_BS, na.rm = T))
            + (-0.044*mean(log(ww$assets_total_BS), na.rm = T))
            + (-0.035*mean(ww$revenues_net_operating_growth_IS/100, na.rm = T))
            + (0.102*mean(invest$revenues_growth_industry, na.rm = T)))
      invest[invest$ticker==i,"WWND"] <- ww
      rm(ww)
}

#WW = - 0.091CF - 0.062DIVPOS + 0.021*TLTD - 0.044*LNTA + 0.102*ISG - 0.035*SG

#TLTD: the ratio of the long-term debt to total assets;
#DIVPOS: an indicator that takes the value of one if the firm pays cash dividends;
#SG: firm sales growth;
#LNTA: the natural log of total assets;
#ISG: the firm's three-digit industry sales growth;
#CASH: the ratio of liquid assets to total assets;
#CF: the ratio of cash flow to total assets;
      #(see Whited and Wu. Financial Constraints Risk. 2006)

# Compute constraints -----------------------------------------------------

#1: restritas
#0: não restritas
#2: drop da amostra

#KZ Tercil
KZ3 <- as.data.frame(NULL, stringsAsFactor=F)
invest$KZ3 <- rep(2,nrow(invest))

for (i in unique(invest$quarter)) {
      data <- filter(invest, quarter == i)
      data <- select(data, KZ)
      low <- quantile(data$KZ, na.rm = T, probs = 1/3)
      upper <-  quantile(data$KZ, na.rm = T, probs = 2/3)
      KZ3 <- rbind(KZ3,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
      invest[invest$quarter == i &
                   !is.na(invest$KZ) &
                   invest$KZ >=upper,
             "KZ3"] <- 1
      invest[invest$quarter == i &
                   !is.na(invest$KZ) &
                   invest$KZ<=low,
             "KZ3"] <- 0
      rm(low,upper,data)
} ; rm(i)


      #KZ Tercil com dados agregados
      KZ3agg <- as.data.frame(NULL, stringsAsFactor=F)
      invest$KZ3agg <- rep(2,nrow(invest))
      
      for (i in unique(invest$quarter)) {
            data <- filter(invest, quarter <= i)
            data <- select(data, KZ)
            low <- quantile(data$KZ, na.rm = T, probs = 1/3)
            upper <-  quantile(data$KZ, na.rm = T, probs = 2/3)
            KZ3agg <- rbind(KZ3agg,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
            invest[invest$quarter == i &
                        !is.na(invest$KZ) &
                        invest$KZ >=upper,
                  "KZ3agg"] <- 1
            invest[invest$quarter == i &
                        !is.na(invest$KZ) &
                        invest$KZ<=low,
                  "KZ3agg"] <- 0
            rm(low,upper,data)
      } ; rm(i)


      #KZ Tercil full-sample
      KZ3fs <- as.data.frame(NULL, stringsAsFactor=F)
      invest$KZ3fs <- rep(2,nrow(invest))
      
      low <- quantile(invest$KZ, na.rm = T, probs = 1/3)
      upper <-  quantile(invest$KZ, na.rm = T, probs = 2/3)
      invest[!is.na(invest$KZ) & invest$KZ >=upper,"KZ3fs"] <- 1
      invest[!is.na(invest$KZ) & invest$KZ <=low,"KZ3fs"] <- 0
      
      KZ3fs <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
      KZ3fs <- cbind(KZ3fs,invest$quarter)
      KZ3fs <- unique(KZ3fs)
      KZ3fs$KZ3fslower <- low
      KZ3fs$KZ3fsupper <- upper
      rm(low,upper)


#KZ Quintil
KZ5 <- as.data.frame(NULL, stringsAsFactor=F)
invest$KZ5 <- rep(2,nrow(invest))

for (i in unique(invest$quarter)) {
      data <- filter(invest, quarter == i)
      data <- select(data, KZ)
      low <- quantile(data$KZ, na.rm = T, probs = 2/5)
      upper <-  quantile(data$KZ, na.rm = T, probs = 3/5)
      KZ5 <- rbind(KZ5,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
      invest[invest$quarter == i &
                   !is.na(invest$KZ) &
                   invest$KZ >=upper,
             "KZ5"] <- 1
      invest[invest$quarter == i &
                   !is.na(invest$KZ) &
                   invest$KZ<=low,
             "KZ5"] <- 0
      rm(low,upper,data)
} ; rm(i)
      
      
      #KZ Quintil com dados agregados
      KZ5agg <- as.data.frame(NULL, stringsAsFactor=F)
      invest$KZ5agg <- rep(2,nrow(invest))
      
      for (i in unique(invest$quarter)) {
            data <- filter(invest, quarter <= i)
            data <- select(data, KZ)
            low <- quantile(data$KZ, na.rm = T, probs = 2/5)
            upper <-  quantile(data$KZ, na.rm = T, probs = 3/5)
            KZ5agg <- rbind(KZ5agg,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
            invest[invest$quarter == i &
                         !is.na(invest$KZ) &
                         invest$KZ >=upper,
                   "KZ5agg"] <- 1
            invest[invest$quarter == i &
                         !is.na(invest$KZ) &
                         invest$KZ<=low,
                   "KZ5agg"] <- 0
            rm(low,upper,data)
      } ; rm(i)
      
      
      #KZ Quintil full-sample
      KZ5fs <- as.data.frame(NULL, stringsAsFactor=F)
      invest$KZ5fs <- rep(2,nrow(invest))
      
      low <- quantile(invest$KZ, na.rm = T, probs = 2/5)
      upper <-  quantile(invest$KZ, na.rm = T, probs = 3/5)
      invest[!is.na(invest$KZ) & invest$KZ >=upper,"KZ5fs"] <- 1
      invest[!is.na(invest$KZ) & invest$KZ <=low,"KZ5fs"] <- 0

      KZ5fs <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
      KZ5fs <- cbind(KZ5fs,invest$quarter)
      KZ5fs <- unique(KZ5fs)
      KZ5fs$KZ5fslower <- low
      KZ5fs$KZ5fsupper <- upper
      rm(low,upper)
      


#WW Tercil
WW3 <- as.data.frame(NULL, stringsAsFactor=F)
invest$WW3 <- rep(2,nrow(invest))

for (i in unique(invest$quarter)) {
      data <- filter(invest, quarter == i)
      data <- select(data, WW)
      low <- quantile(data$WW, na.rm = T, probs = 1/3)
      upper <-  quantile(data$WW, na.rm = T, probs = 2/3)
      WW3 <- rbind(WW3,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
      invest[invest$quarter == i &
                   !is.na(invest$WW) &
                   invest$WW >=upper,
             "WW3"] <- 1
      invest[invest$quarter == i &
                   !is.na(invest$WW) &
                   invest$WW<=low,
             "WW3"] <- 0
      rm(low,upper,data)
} ; rm(i)


      #WW Tercil com dados agregados
      WW3agg <- as.data.frame(NULL, stringsAsFactor=F)
      invest$WW3agg <- rep(2,nrow(invest))
      
      for (i in unique(invest$quarter)) {
            data <- filter(invest, quarter <= i)
            data <- select(data, WW)
            low <- quantile(data$WW, na.rm = T, probs = 1/3)
            upper <-  quantile(data$WW, na.rm = T, probs = 2/3)
            WW3agg <- rbind(WW3agg,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
            invest[invest$quarter == i &
                         !is.na(invest$WW) &
                         invest$WW >=upper,
                   "WW3agg"] <- 1
            invest[invest$quarter == i &
                         !is.na(invest$WW) &
                         invest$WW<=low,
                   "WW3agg"] <- 0
            rm(low,upper,data)
      } ; rm(i)
      
      
      #WW Tercil full-sample
      invest$WW3fs <- rep(2,nrow(invest))
      
      low <- quantile(invest$WW, na.rm = T, probs = 1/3)
      upper <-  quantile(invest$WW, na.rm = T, probs = 2/3)
      invest[!is.na(invest$WW) & invest$WW >=upper,"WW3fs"] <- 1
      invest[!is.na(invest$WW) & invest$WW <=low,"WW3fs"] <- 0

      WW3fs <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
      WW3fs <- cbind(WW3fs,invest$quarter)
      WW3fs <- unique(WW3fs)
      WW3fs$WW3fslower <- low
      WW3fs$WW3fsupper <- upper
      rm(low,upper)


#WW Quintil
WW5 <- as.data.frame(NULL, stringsAsFactor=F)
invest$WW5 <- rep(2,nrow(invest))

for (i in unique(invest$quarter)) {
      data <- filter(invest, quarter == i)
      data <- select(data, WW)
      low <- quantile(data$WW, na.rm = T, probs = 2/5)
      upper <-  quantile(data$WW, na.rm = T, probs = 3/5)
      WW5 <- rbind(WW5,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
      invest[invest$quarter == i &
                   !is.na(invest$WW) &
                   invest$WW >=upper,
             "WW5"] <- 1
      invest[invest$quarter == i &
                   !is.na(invest$WW) &
                   invest$WW<=low,
             "WW5"] <- 0
      rm(low,upper,data)
} ; rm(i)
      
      
      #WW Quintil com dados agregados
      WW5agg <- as.data.frame(NULL, stringsAsFactor=F)
      invest$WW5agg <- rep(2,nrow(invest))
      
      for (i in unique(invest$quarter)) {
            data <- filter(invest, quarter <= i)
            data <- select(data, WW)
            low <- quantile(data$WW, na.rm = T, probs = 2/5)
            upper <-  quantile(data$WW, na.rm = T, probs = 3/5)
            WW5agg <- rbind(WW5agg,as.data.frame(t(c(i,low,upper)),stringsAsFactors = F))
            invest[invest$quarter == i &
                         !is.na(invest$WW) &
                         invest$WW >=upper,
                   "WW5agg"] <- 1
            invest[invest$quarter == i &
                         !is.na(invest$WW) &
                         invest$WW<=low,
                   "WW5agg"] <- 0
            rm(low,upper,data)
      } ; rm(i)
      
      #WW Quintil full-sample
      invest$WW5fs <- rep(2,nrow(invest))
      low <- quantile(invest$WW, na.rm = T, probs = 2/5)
      upper <-  quantile(invest$WW, na.rm = T, probs = 3/5)
      invest[!is.na(invest$WW) & invest$WW >=upper,"WW5fs"] <- 1
      invest[!is.na(invest$WW) & invest$WW <=low,"WW5fs"] <- 0
      
      WW5fs <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
      WW5fs <- cbind(WW5fs,invest$quarter)
      WW5fs <- unique(WW5fs)
      WW5fs$WW5fslower <- low
      WW5fs$WW5fsupper <- upper
      rm(low,upper)


colnames(KZ3) <- c("quarter","KZ3lower","KZ3upper")
colnames(KZ3agg) <- c("quarter","KZ3agglower","KZ3aggupper")
colnames(KZ5) <- c("quarter","KZ5lower","KZ5upper")
colnames(KZ5agg) <- c("quarter","KZ5agglower","KZ5aggupper")
colnames(WW3) <- c("quarter","WW3lower","WW3upper")
colnames(WW3agg) <- c("quarter","WW3agglower","WW3aggupper")
colnames(WW5) <- c("quarter","WW5lower","WW5upper")
colnames(WW5agg) <- c("quarter","WW5agglower","WW5aggupper")

cons_thresh <- merge(WW5fs,merge(WW5agg,merge(WW5,merge(WW3fs,merge(WW3agg,merge(WW3,merge(KZ5fs,merge(KZ5agg,merge(KZ5,merge(KZ3fs,merge(KZ3,KZ3agg)))))))))))
rm(KZ3,KZ3agg,KZ5,KZ5agg,WW3,WW3agg,WW5,WW5agg,WW3fs,WW5fs,KZ5fs,KZ3fs)


#Not dynamic index

#KZ non dynamic Tercil
invest$KZND3 <- rep(2,nrow(invest))
low <- quantile(invest$KZND, na.rm = T, probs = 1/3)
upper <-  quantile(invest$KZND, na.rm = T, probs = 2/3)
invest[!is.na(invest$KZND) & invest$KZND >=upper,"KZND3"] <- 1
invest[!is.na(invest$KZND) & invest$KZND <=low,"KZND3"] <- 0

KZND3 <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
KZND3 <- cbind(KZND3,invest$quarter)
KZND3 <- unique(KZND3)
KZND3$KZND3lower <- low
KZND3$KZND3upper <- upper
rm(low,upper)

      #KZ non dynamic Quintil
      invest$KZND5 <- rep(2,nrow(invest))
      low <- quantile(invest$KZND, na.rm = T, probs = 2/5)
      upper <-  quantile(invest$KZND, na.rm = T, probs = 3/5)
      invest[!is.na(invest$KZND) & invest$KZND >=upper,"KZND5"] <- 1
      invest[!is.na(invest$KZND) & invest$KZND <=low,"KZND5"] <- 0
      
      KZND5 <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
      KZND5 <- cbind(KZND5,invest$quarter)
      KZND5 <- unique(KZND5)
      KZND5$KZND5lower <- low
      KZND5$KZND5upper <- upper
      rm(low,upper)

      
#WW non dynamic Tercil
invest$WWND3 <- rep(2,nrow(invest))
low <- quantile(invest$WWND, na.rm = T, probs = 1/3)
upper <-  quantile(invest$WWND, na.rm = T, probs = 2/3)
invest[!is.na(invest$WWND) & invest$WWND >=upper,"WWND3"] <- 1
invest[!is.na(invest$WWND) & invest$WWND <=low,"WWND3"] <- 0
      
WWND3 <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
WWND3 <- cbind(WWND3,invest$quarter)
WWND3 <- unique(WWND3)
WWND3$WWND3lower <- low
WWND3$WWND3upper <- upper
rm(low,upper)
      
      #WW non dynamic Quintil
      invest$WWND5 <- rep(2,nrow(invest))
      low <- quantile(invest$WWND, na.rm = T, probs = 2/5)
      upper <-  quantile(invest$WWND, na.rm = T, probs = 3/5)
      invest[!is.na(invest$WWND) & invest$WWND >=upper,"WWND5"] <- 1
      invest[!is.na(invest$WWND) & invest$WWND <=low,"WWND5"] <- 0
      
      WWND5 <- as.data.frame(rep(2,nrow(invest)), stringsAsFactor=F)
      WWND5 <- cbind(WWND5,invest$quarter)
      WWND5 <- unique(WWND5)
      WWND5$WWND5lower <- low
      WWND5$WWND5upper <- upper
      rm(low,upper)

#Clean
invest <- filter(invest, KZ >= mean(invest$KZ,na.rm=T)-2*sd(invest$KZ,na.rm = T)
                 & KZ <= mean(invest$KZ,na.rm=T)+2*sd(invest$KZ,na.rm = T)
                 & WW >= mean(invest$WW,na.rm=T)-2*sd(invest$WW,na.rm = T)
                 & WW <= mean(invest$WW,na.rm=T)+2*sd(invest$WW,na.rm = T))

a <- filter(invest, KZ3fs == 1 & (WW3fs == 1 | WW3fs == 2))
b <- filter(invest, KZ3fs == 0 & (WW3fs == 0 | WW3fs == 2))
c <- filter(invest, WW3fs == 1 & (KZ3fs == 1 | KZ3fs == 2))
d <- filter(invest, WW3fs == 0 & (KZ3fs == 0 | KZ3fs == 2))
invest <- rbind(a,b,c,d)
rm(a,b,c,d)
invest <- unique(invest)
invest <- arrange(invest,ticker,tseries)

# Write tables ------------------------------------------------------------
invest <- arrange(invest, ticker, tseries)
write.csv2(colnames(invest),
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4b_variables.csv",
           row.names = F,
           na = "")

write.csv2(invest,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4b_investmentFINALDATA.csv",
           row.names = F,
           na = "")

write.csv2(cons_thresh,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4b_constraint_index.csv",
           row.names = F,
           na = "")
rm(cons_thresh)


#Stata dta
for (i in colnames(invest)) {
      invest[is.na(invest[,i]),i] <- ""
} ; rm(i)

save.dta13(data = invest,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4b_INVESTMENT_data.dta",
           convert.dates = T
)
rm(invest)
