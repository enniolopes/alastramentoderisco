#Spillover verification
library(dplyr)

# Leitura dos dados -------------------------------------------------------

spillover <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3d_spillover[min-var].csv",
                       header = T,
                       sep = ";",
                       dec = ",",
                       na.strings = "",
                       stringsAsFactors = F)
ccx <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3a_CCX[min-var].csv",
                       header = T,
                       sep = ";",
                       dec = ",",
                       na.strings = "",
                       stringsAsFactors = F)
reg <- read.csv2("D:\\$database\\ownCloud\\database\\rawdata\\economatica_registry\\BRA_BMFBOVESPA.txt",
                 sep = ";",dec =",",na.strings = "-", skip = 4,stringsAsFactors = F)
reg <- select(reg,Company.ID,Ticker)
colnames(reg) <- c("companyid","ticker")

ccx <- merge(ccx,reg,
             by.x = "ticker",
             by.y = "ticker",
             all.x = T,
             all.y = F)
spilldt <- merge(ccx,spillover)
rm(ccx,spillover,reg)

spilldt$pbspill_ccxVAR <- spilldt$pbCCX*spilldt$spillover

write.csv2(x = spilldt,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3e_spillover_data[min-var].csv",
           row.names = F,
           na = "")
rm(spilldt)