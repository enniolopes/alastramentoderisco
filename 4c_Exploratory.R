library(dplyr)

invest <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4b_investmentFULLDATA_clean.csv",
                    header = T,
                    na.strings = "",
                    stringsAsFactors = F)

attach(invest)
invest$IK = aquisition/Klag
invest$IK2 = invest$IK^2
invest$SK = revenues_net_operating_IS/Klag
invest$CFK = cash_BS_variation/Klag
invest$DK = D_lt_BS/Klag
invest$SPLL = pbCCX_fs*spillover
invest$iSPLL = invest$SPLL*invest$CFK
detach(invest)


y <- group_by(select(invest,
                     quarter,
                     ticker,
                     market_capitalization,
                     equity_BS),
              quarter)





x <- group_by(invest, ticker)
write.csv2(
summarize(x,
          KZ = mean(KZ, na.rm=T),
          WW = mean(WW, na.rm = T),
          TobinQ = mean(Q,na.rm = T),
          I = mean(aquisition, na.rm=T),
          K = mean(K, na.rm=T),
          S = mean(revenues_net_operating_IS, na.rm=T),
          CF = mean(cash_BS_variation, na.rm=T),
          D = mean(D_lt_BS, na.rm=T),
          IK = mean(IK, na.rm=T),
          IK2 = mean(IK2, na.rm=T),
          SK = mean(SK, na.rm=T),
          CFK = mean(CFK, na.rm=T),
          DK = mean(DK, na.rm=T),
          SPILLOVER = mean(SPLL, na.rm=T)),
file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4c_VARIABLES_mean.csv",
row.names = F,
na = "")

summarize(x,
          KZ = mean(KZ, na.rm=T))
filter(invest,ticker=="WHRL3")