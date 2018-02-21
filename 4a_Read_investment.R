library(dplyr)
library(readr)
library(timeDate)
library(stringi)
library(readstata13)

# Leitura dos Dados -------------------------------------------------------

#Definição dos tickers
zipfile <- "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\Dados Investimento\\BRA_dadosinvestimento.zip"
file_list <- unzip(zipfile = zipfile, list = T)["Name"]
file_list <- as.character(file_list[1:nrow(file_list),"Name"])
name <- data.frame(file_list,stringsAsFactors = F)
name$id <- file_list %>%
      sub(pattern = strtrim(file_list, 38)[1], replacement = "") %>%
      sub(pattern = ".txt",replacement = "")

ticker <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3e_spillover_data[min-var].csv",
                    header = T,
                    sep = ";",
                    dec = ",",
                    na.strings = "",
                    stringsAsFactors = F)
ticker <- unique(ticker[,c("ticker","companyid")])
name <- merge(ticker,name,
              by.x = "ticker",
              by.y = "id",
              all.x = T,
              all.y = F)
colnames(name) <- c("id", "companyid", "file_list")

#Missing:
name[is.na(name$file_list),]
name <- name[!is.na(name$file_list),]
file_list <- name[,"file_list"]


#Leitura:
for (file in file_list) {
      if (!exists("stat")) {
            stat <-
                  mutate(mutate(
                        as.data.frame(
                              read_csv2(unz(zipfile,file),
                                        na = "-", skip = 4,
                                        col_types = "cccccccccccccccccccccccccccccc",
                                        n_max = 0
                              )
                        )
                        , id = name$id[name$file_list == file])
                        , ticker = as.character(read_csv2(unz(zipfile,file),
                                                          na = "-", skip = 4)[1,2])
                  )
      }
      
      if (exists("stat")) {
            temp_stat <-
                  mutate(mutate(
                        as.data.frame(
                              read_csv2(unz(zipfile,file),
                                        na = "-", skip = 4,
                                        col_types = "cccccccccccccccccccccccccccccc"
                              )[-1,]
                        )
                        , id = name$id[name$file_list == file])
                        , ticker = as.character(read_csv2(unz(zipfile,file),
                                                          na = "-", skip = 4)[1,2])
                  )
            stat <- rbind(stat, temp_stat)
            rm(temp_stat)
      }
} ; rm(file)


#Check tickers:
if (length(unique(stat$ticker[stat$id != stat$ticker])) == 0) {
      stat <- select(stat, -id)
      print("Ticker and ID are the same, id column was removed")
} else {
      print(paste(c("Check the diferences between ticker and id columns: ",
                    unique(stat$ticker[stat$id != stat$ticker]))))
}


#Renomear cabeçalhos:
cname <- c(
      "Date",
      "PrPIEN|customized|consolid:yes*",
      "SalPerAst|customized|of 3 months|consolid:yes*",
      "DepAmortDepl|customized|of 3 months|consolid:yes*",
      "Pu fix dif|customized|of 3 months|consolid:yes*",
      "Sale Perm|customized|of 3 months|consolid:yes*",
      "Capex|customized|of 3 months|consolid:yes*",
      "Invest Cap $|customized|consolid:yes*",
      "TtCaOA|customized|of 3 months|consolid:yes*",
      "NtChCs|customized|of 3 months|consolid:yes*",
      "Cash&EqCa|customized|consolid:yes*",
      "ChSTIn|customized|consolid:yes*",
      "EBITDA|customized|of 3 months|consolid:yes*",
      "Tt Debt Nt|customized|consolid:yes*",
      "Tt Debt Gr|customized|consolid:yes*",
      "StockEqTot|customized|consolid:yes*",
      "Stck Eq Par|customized|consolid:yes*",
      "Tot Assets|customized|consolid:yes*",
      "stDebt|customized|consolid:yes*",
      "ltDebt|customized|consolid:yes*",
      "DebtLT|customized|consolid:yes*",
      "Revenues|customized|of 3 months|consolid:yes*",
      "Revenues|customized|of 3 months|consolid:yes*|3mo variation",
      "AddPrPlEq|customized|of 3 months|consolid:yes*",
      "DepAmo|customized|of 3 months|consolid:yes*",
      "Impairm|customized|of 3 months|consolid:yes*",
      "SaPrPl&Eq|customized|of 3 months|consolid:yes*",
      "DividPaid|customized|of 3 months|consolid:yes*",
      "DivPai|customized|of 3 months|consolid:yes*",
      "Market Capitaliz|of the company|customized",
      "ticker")
tidyname <- c(
      "quarter",
      "fixed_asset_net_BS",
      "fixed_asset_net_gain_sale_IS_BRA",
      "fixed_asset_depreciation_period_IS_BRA",
      "fixed_asset_purchase_CF_BRA",
      "fixed_asset_sale_CF_BRA",
      "capex",
      "invested_capital_BS",
      "cash_operating_activity_CF",
      "cash_net_change_CF",
      "cash_equivalent_BS_IFRS_BRA_ARG_MEX",
      "cash_stinvestment_BS_LS_BRA_US_ARG_MEX",
      "ebitda_IS",
      "debt_net_BS",
      "debt_gross_BS",
      "equity_total_BS",
      "equity_parent_BS_LS",
      "assets_total_BS",
      "debt_st_BS",
      "debt_lt_BS_exUSA",
      "debt_lt_BS_USA",
      "revenues_net_operating_IS",
      "revenues_net_operating_growth_IS",
      "fixed_asset_purchase_CF_exBRA",
      "fixed_asset_depreciation_period_CF_exBRA",
      "impairment_BRA",
      "fixed_asset_sale_CF_exBRA",
      "dividends_CF_MEX",
      "dividends_CF_exMEX",
      "market_capitalization",
      "ticker")
#for (i in cname) {
#      if (!exists("ref")){
#            ref <- as.data.frame(NULL,stringsAsFactors=F)
#      }
#      if (exists("ref")){
#            temp_ref <- as.data.frame(grep(pattern = i, x = names(stat), fixed = T))
#            ref <- rbind(ref, temp_ref)
#            rm(temp_ref)
#      }
#}
#tidyname <- as.character(arrange(as.data.frame(tidyname), ref)[,1])
colnames(stat) <- tidyname
#rm(ref,i,tidyname,cname)
rm(tidyname,cname)


#Missing Values:
stat[stat == "-"] = NA

#End month date:
stat$date <- as.Date(
      timeLastDayInMonth(as.Date(
            stri_join(substr(stat$quarter,3,6),
                      "-0",substr(stat$quarter,1,1),
                      "-01"))))

#Quarter
stat$quarter <- stri_join(substr(stat$quarter,3,6),
                      "Q",substr(stat$quarter,1,1))

#Resumo dos dados:
stat <- arrange(stat,ticker,date)
str(stat)
rm(file_list,name,ticker,zipfile)
write.csv2(stat,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4a_Balancesheet_data.csv",
           row.names = F,
           na = "")
rm(stat)

# Compute Variables -------------------------------------------------------
stat <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4a_Balancesheet_data.csv",
                  header = T,
                  sep = ";",
                  dec = ",",
                  na.strings = "",
                  stringsAsFactors = F)

#Classe dos dados
nm <- select(stat[1,],-quarter,-ticker,-date)
nm <- colnames(nm)
for (i in nm) {
      stat[,i] <- as.numeric(stat[,i])
} ; rm(i,nm)



## Aquisições pelo BP ##
#ATIVO IMOBILIZADO líquido:
FA <- stat$fixed_asset_net_BS

#BAIXAS líquidas na DRE:
SALES <- stat$fixed_asset_net_gain_sale_IS_BRA
      #Se for missing usar vendas do FLCX
      SALES[is.na(SALES)] <- stat$fixed_asset_sale_CF_BRA[is.na(SALES)]
      #Se ainda for missing usar vendas do FLCX para diferentes do BRA
      SALES[is.na(SALES)] <- stat$fixed_asset_sale_CF_exBRA[is.na(SALES)]

#DEPRECIACAO:
DEPRECIATION <- stat$fixed_asset_depreciation_period_IS_BRA
      #Se missing usar diferentes do BRA
      DEPRECIATION[is.na(DEPRECIATION)] <- stat$fixed_asset_depreciation_period_CF_exBRA[is.na(DEPRECIATION)]

#IMPAIRMENT:
IMPAIRMENT <- stat$impairment_BRA

#Compute aquisition:
FAlag <- as.data.frame(NULL)
for (i in unique(stat$ticker)) {
      temp <- as.data.frame(lag(FA[stat$ticker == i],1),
                            stringsAsFactors = F)
      FAlag <- rbind(FAlag,temp)
      rm(temp)
} ; rm(i)
stat$aquisition <- FA - FAlag[,1] + SALES + DEPRECIATION + IMPAIRMENT
rm(FA, SALES, DEPRECIATION, IMPAIRMENT)
FAlag <- as.data.frame(NULL)
for (i in unique(stat$ticker)) {
      temp <- as.data.frame(lag(stat$fixed_asset_net_BS[stat$ticker == i],1),
                            stringsAsFactors = F)
      FAlag <- rbind(FAlag,temp)
      rm(temp)
} ; rm(i)
stat$aquisition[is.na(stat$aquisition)] <- stat$fixed_asset_net_BS[is.na(stat$aquisition)] - FAlag[is.na(stat$aquisition),1]
rm(FAlag)


## Investimento em Capital ##
INVlag <- as.data.frame(NULL)
for (i in unique(stat$ticker)) {
      temp <- as.data.frame(lag(stat$invested_capital_BS[stat$ticker == i],1),
                            stringsAsFactors = F)
      INVlag <- rbind(INVlag,temp)
      rm(temp)
} ; rm(i)

stat$invested_capital <- stat$invested_capital_BS - INVlag[,1]
rm(INVlag)


## Capital K ##
stat$K <- stat$fixed_asset_net_BS

Klag <- as.data.frame(NULL)
for (i in unique(stat$ticker)) {
      temp <- as.data.frame(lag(stat$fixed_asset_net_BS[stat$ticker == i],1),
                            stringsAsFactors = F)
      Klag <- rbind(Klag,temp)
      rm(temp)
} ; rm(i)

stat$Klag <- Klag[,1]
rm(Klag)
stat$Kmean <- (stat$K + stat$Klag)/2


## Total Caixa ##
stat$cash <- stat$cash_equivalent_BS_IFRS_BRA_ARG_MEX
stat$cash[is.na(stat$cash)] <- stat$cash_stinvestment_BS_LS_BRA_US_ARG_MEX[is.na(stat$cash)]


## Variação de caixa e Equivalentes ##
cashlag <- as.data.frame(NULL)
for (i in unique(stat$ticker)) {
      temp <- as.data.frame(lag(stat$cash_equivalent_BS_IFRS_BRA_ARG_MEX[stat$ticker == i],1),
                            stringsAsFactors = F)
      cashlag <- rbind(cashlag,temp)
      rm(temp)
} ; rm(i)
stat$cash_BS_variation <- stat$cash_equivalent_BS_IFRS_BRA_ARG_MEX - cashlag[,1]
cashlag <- as.data.frame(NULL)
for (i in unique(stat$ticker)) {
      temp <- as.data.frame(lag(stat$cash_stinvestment_BS_LS_BRA_US_ARG_MEX[stat$ticker == i],1),
                            stringsAsFactors = F)
      cashlag <- rbind(cashlag,temp)
      rm(temp)
} ; rm(i)
stat$cash_BS_variation[is.na(stat$cash_BS_variation)] <-
      (stat$cash_stinvestment_BS_LS_BRA_US_ARG_MEX -
             cashlag[,1])[is.na(stat$cash_BS_variation)]
rm(cashlag)


## Total Debt st+lt ##
stat$D_lt_BS <- stat$debt_lt_BS_exUSA
stat$D_lt_BS[is.na(stat$D_lt_BS)] <- stat$debt_lt_BS_USA[is.na(stat$D_lt_BS)]

stat$D_st_BS <- stat$debt_st_BS

stat$Dstlt_BS <- rep(NA,nrow(stat))

for (i in 1:nrow(stat)) {
      if (!all(is.na(stat$D_st_BS[i]), is.na(stat$D_lt_BS[i]))) {
            
            if(is.na(stat$D_st_BS[i])) {st=0} else{
                  st=stat$D_st_BS[i]
            }
            
            if(is.na(stat$D_lt_BS[i])) {lt=0} else{
                  lt=stat$D_lt_BS[i]
            }
            
            stat$Dstlt_BS[i] <- st + lt
            rm(st,lt)
      }
} ; rm(i)


## Total Capital ##
stat$totcap_BS <- stat$equity_total_BS
stat$totcap_BS[is.na(stat$totcap_BS)] <- stat$equity_parent_BS_LS[is.na(stat$totcap_BS)]
stat$equity_BS <- stat$totcap_BS
stat$totcap_BS <- stat$totcap_BS + stat$Dstlt_BS


## total cash dividends ##
stat$tdividends <- stat$dividends_CF_exMEX
stat$tdividends[is.na(stat$tdividends)] <- stat$dividends_CF_MEX[is.na(stat$tdividends)]
stat[is.na(stat$tdividends),"tdividends"] <- 0

## Dummy dividendos ##
stat$ddividends[stat$tdividends == 0] <- 0
stat$ddividends[stat$tdividends != 0] <- 1

## Tobin's Q
stat$Q <- stat$market_capitalization/stat$equity_BS

### Base de dados função de investimento ###
investment <- stat[,c("date","quarter","ticker",
                      "K",
                      "Klag",
                      "Kmean",
                      "aquisition",
                      "capex",
                      "invested_capital",
                      "fixed_asset_purchase_CF_BRA",
                      "cash",
                      "cash_BS_variation",
                      "cash_operating_activity_CF",
                      "cash_net_change_CF",
                      "ebitda_IS",
                      "Dstlt_BS",
                      "D_lt_BS",
                      "D_st_BS",
                      "debt_net_BS",
                      "debt_gross_BS",
                      "totcap_BS",
                      "equity_BS",
                      "assets_total_BS",
                      "revenues_net_operating_IS",
                      "revenues_net_operating_growth_IS",
                      "tdividends",
                      "ddividends",
                      "market_capitalization",
                      "Q")]
rm(stat)

## GDPg ##
GDPg <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\GDPg_data.csv",
                  header = T,
                  sep = ";",
                  dec = ",",
                  na.strings = "NA",
                  stringsAsFactors = F)
GDPg <- GDPg[,c("quarter","GDPg_BRA")]

investment <- merge(investment,GDPg,
                    by_x = "quarter",
                    by_y = "quarter",
                    all_x = T,
                    all_y = F)
rm(GDPg)


## Risk Spillover ##
spillover <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3e_spillover_data[min-var].csv",
                       header = T,
                       stringsAsFactors = F)
spillover <- select(spillover,-p.test_robust,-p.test,-jb_residuals)
spillover <- arrange(spillover, ticker, quarter)

spillover$proc <- stri_join(spillover$ticker,spillover$quarter,sep="_")
spillover <- select(spillover,-ticker,-quarter)

investment$proc <- stri_join(investment$ticker,investment$quarter,sep="_")

investment <- merge(investment,spillover,
                    by_x = "proc",
                    by_y = "proc",
                    all_x = T,
                    all_y = T)
rm(spillover)


# Risk Spillover [size]
spillover <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3e_spillover_data[size].csv",
                       header = T,
                       stringsAsFactors = F)
spillover <- select(spillover,-p.test_robust,-p.test,-jb_residuals,-companyid)
colnames(spillover) <- stri_join(colnames(spillover),"_size")
spillover <- arrange(spillover, ticker_size, quarter_size)
spillover$proc <- stri_join(spillover$ticker_size,spillover$quarter_size,sep="_")
spillover <- select(spillover,-ticker_size,-quarter_size)

investment <- merge(investment,spillover,
                    by_x = "proc",
                    by_y = "proc",
                    all_x = T,
                    all_y = T)
rm(spillover)
investment <- select(investment,-proc)


#Crisis dummies
investment$ruscrisis <- rep(0,nrow(investment))
investment[investment$quarter=="1998Q3" | 
                 investment$quarter=="1998Q4" |
                 investment$quarter=="1999Q1" |
                 investment$quarter=="1999Q2","ruscrisis"] <- 1
investment$subprimecrisis <- rep(0,nrow(investment))
investment[investment$quarter=="2008Q3" | 
                 investment$quarter=="2008Q4" |
                 investment$quarter=="2009Q1",
       "subprimecrisis"] <- 1

# Clean data --------------------------------------------------------------
qt <- unique(investment[,c("quarter","date")])
qt <- arrange(qt,date)
qt$tseries <- seq(1:nrow(qt))
investment <- merge(investment,qt[,-1],
                    by_x = "date",
                    by_y = "date",
                    all_x = T,
                    all_y = F)
rm(qt)
investment$date <- as.Date(investment$date)
investment <- arrange(investment, ticker, date)


#Revenues check
filter(investment, revenues_net_operating_IS < 0)
investment <- filter(investment, ticker != "IGBR3"
                     & ticker != "LIPR3"
                     & ticker != "PRML3")
investment <- filter(investment,
                     revenues_net_operating_IS >= 0 
                     | is.na(revenues_net_operating_IS))


#Capital (K) check
unique(filter(investment, K == 0)[,"ticker"])
capital <- filter(investment,
                  ticker == "PTIP4",
                  date >= "2002-04-30")
investment <- filter(investment,
                     ticker != "ARTE4" &
                           ticker != "ATOM3" &
                           ticker != "BMTO4" &
                           ticker != "CGAS3" &
                           ticker != "CTPC3" &
                           ticker != "ENMA3B" &
                           ticker != "HETA4" &
                           ticker != "LATS3" &
                           ticker != "MAPT4" &
                           ticker != "MPLU3" &
                           ticker != "PMET6" &
                           ticker != "PTIP4" &
                           ticker != "RDTR3")
investment <- rbind(investment,capital)
rm(capital)
investment <- investment[!is.na(investment$K),]
investment <- arrange(investment, ticker, date)

#s-t Debt check
investment <- filter(investment, D_st_BS >= 0 | is.na(D_st_BS))

#Tobin's Q check
investment <- filter(investment, equity_BS >= 0 | is.na(equity_BS))

#Obs frequency clean
x <- as.data.frame(table(investment$ticker), stringsAsFactors = F)
x <- x[x$Freq>=8,]
investment <- merge(investment,x,
                    by.x = "ticker",
                    by.y = "Var1",
                    all.x = F,
                    all.y = T)
investment <- select(investment, -Freq)

# Write final data --------------------------------------------------------
investment <- arrange(investment, ticker, date)
write.csv2(investment,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\4a_INVESTMENT_data.csv",
           row.names = F,
           na = "")
rm(investment)


#reg <- read.csv2("D:\\$database\\ownCloud\\database\\rawdata\\economatica_registry\\BRA_BMFBOVESPA_txt",
#                 sep = ";",
#                 dec =",",
#                 na.strings = "-",
#                 skip = 4,
#                 stringsAsFactors = F)
