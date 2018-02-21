## VAR-GARCH 2 estágios ##
library(urca)
library(lubridate)
library(tseries)
library(vars)
library(stringi)
library(dplyr)
library(RCurl)
# import the function for robust test in summary() function from repository:
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_robust)


# Leitura dos dados -------------------------------------------------------

#Setor Financeiro
fin <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\2_BRAfin_size.csv",
                 header = T, 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "", 
                 stringsAsFactors = F)
colnames(fin) <- c("date","returnInf")
fin$date <- as.Date(fin$date)
fin <- filter(fin, date >= "1996-01-01")
fin$quarter <- stri_join(year(fin$date),quarters.Date(fin$date))
#Retorno continuo
fin$ccreturnInf <- log(1 + fin$returnInf)

#Empresas
rtn <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\1_BRAnaofinanceiro.csv", 
                 header = T, 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "", 
                 stringsAsFactors = F)
rtn <- filter(rtn, ticker != "PTQS4")
rtn$date <- as.Date(rtn$date)
rtn <- filter(rtn, date >= "1996-01-01")
rtn$quarter <- stri_join(year(rtn$date),quarters.Date(rtn$date))
rtn$returnInf <- rtn$returnInf/100
rtn <- rtn[,c("ticker","companyid","name","date","quarter","returnInf")]
rtn[is.na(rtn$returnInf),"returnInf"] <- 0
rtn$ccreturnInf <- log(1 + rtn$returnInf)

# 1. Teste de Raiz Unitária -----------------------------------------------

## Augmented Dickey-Fuller (ADF) ##
#lags selection using bayesian information criterion

#Financial sector
urt <- ur.df(fin$returnInf,
             type = "none",
             selectlags = "BIC")
ADF <- as.data.frame(summary(urt)@teststat,stringsAsFactors=F)
colnames(ADF) <- "tau-stat"
ADF$company <- "00000"

#Industry
for (i in unique(rtn$companyid)) {
      urt <- ur.df(rtn[rtn$companyid == i,"returnInf"],
                         type = "none",
                         selectlags = "BIC")
            x <- summary(urt)@teststat
            x <- t(as.data.frame(c(x,i),stringsAsFactors = F))
            colnames(x) <- c("tau-stat","company")
            ADF <- rbind(ADF,x)
            rm(urt,x)
} ; rm(i)
row.names(ADF) <- NULL
registry <- unique(rtn[,c("companyid", "name", "ticker")])
finst <- as.data.frame(t(c("00000","Financial Sector","FINST")),stringsAsFactors = F)
colnames(finst) <- c("companyid", "name", "ticker")
registry <- rbind(finst,registry) ; rm(finst)

ADF <- merge(registry, ADF,
             by.x = "companyid",
             by.y = "company",
             all.x = F,
             all.y = T)
rm(registry)
ADF$`tau-stat` <- as.numeric(ADF$`tau-stat`)


## Phillips-Perron ##

#Financial sector
PP <- as.data.frame(NULL)
PP <- as.data.frame(pp.test(fin$returnInf)$p.value,
                    stringsAsFactors = F) # no unit-root (stationary): p-value <= 0.05
colnames(PP) <- "p-value"
PP$company <- "00000"

#Industry
for (i in unique(rtn$companyid)) {
      x <- pp.test(rtn[rtn$companyid == i,"returnInf"])$p.value
      x <- t(as.data.frame(c(x,i),stringsAsFactors = F))
      colnames(x) <- c("p-value","company")
      PP <- rbind(PP,x)
      rm(x)
} ; rm(i)
row.names(PP) <- NULL

registry <- unique(rtn[,c("companyid", "name", "ticker")])
finst <- as.data.frame(t(c("00000","Financial Sector","FINST")),stringsAsFactors = F)
colnames(finst) <- c("companyid", "name", "ticker")
registry <- rbind(finst,registry) ; rm(finst)

PP <- merge(registry, PP,
            by.x = "companyid",
            by.y = "company",
            all.x = F,
            all.y = T)
rm(registry)
PP$`p-value` <- as.numeric(PP$`p-value`)

# Relatório dos resultados
urt <- merge(ADF,PP)
rm(ADF,PP)
notstationary <- urt[urt$`p-value` > 0.05 | urt$`tau-stat` > -1.95,]

print(notstationary)
rm(notstationary)
write.csv2(x = urt,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3d_unitroottest[size].csv",
           row.names = F,
           na = "")
rm(urt)

# 2. Modelo VAR(p) --------------------------------------------------------
#r_FIN.t e r_i.t (resíduos do modelo VAR)

#Lag selection
sel <- as.data.frame(NULL)
for (i in unique(rtn$companyid)) {
            dta <- rtn[rtn$companyid == i,]
            dta <- dta[,c("date","returnInf")]
            dta <- merge(dta, fin[,c("date","returnInf")],
                         by.x = "date",
                         by.y = "date",
                         all.x = T,
                         all.y = F)
            colnames(dta) <- c("date","returnInf_ind","returnInf_fin")
            dta <- arrange(dta, date)
            dta <- as.ts(dta[,-1], class = "mts")
            temp <- VARselect(dta, lag.max = 10, type = "none")$selection
            temp <- as.data.frame(t(c(i,temp[c("AIC(n)","SC(n)")])),stringsAsFactors=F)
            sel <- rbind(sel,temp)
            rm(dta,temp)
} ; rm(i)
colnames(sel) <- c("companyid","AIC","SC")
sel$AIC <- as.numeric(sel$AIC)
sel$SC <- as.numeric(sel$SC)
sum(sel$AIC)
sum(sel$SC)
#Used Schwarz Criterion to select the lags
write.csv2(x = sel,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3d_lagselection[size].csv",
           row.names = F,
           na = "")
rm(sel)


#VAR model
r_JB <- as.data.frame(NULL)
risknormality <- as.data.frame(NULL)

for (i in unique(rtn$companyid)) {
            #Data:
            dta <- rtn[rtn$companyid == i,]
            dta <- dta[,c("date","returnInf")]
            dta <- merge(dta, fin[,c("date","returnInf")],
                         by.x = "date",
                         by.y = "date",
                         all.x = T,
                         all.y = F)
            colnames(dta) <- c("date","returnInf_ind","returnInf_fin")
            dta <- arrange(na.omit(dta), date)
            
            #VAR:
            v.arg <- VAR(dta[,-1], lag.max = 10, ic = "SC", type = "none")
            r_ind <- v.arg$varresult$returnInf_ind$residuals
            r_fin <- v.arg$varresult$returnInf_fin$residuals
            rm(v.arg)
            rnm <- dta$date[(1+nrow(dta) - length(r_ind)):nrow(dta)]

            #GARCH(1,1):
            g_fin <- garch(r_fin, order = c(1,1))
            e_fin <- as.numeric(r_fin/g_fin$fitted.values[,1])
            rm(g_fin)
            e_fin <- cbind(as.data.frame(rnm, stringsAsFactors = F),
                           as.data.frame(e_fin, stringsAsFactors = F))
            e_fin <- na.omit(e_fin)
            colnames(e_fin) <- c("date","e_fin")
            e_fin$companyid <- i
            e_fin$quarter <- stri_join(year(e_fin$date),"Q",quarter(e_fin$date))
            
            #Jarque-Bera normality test
            #(p-value < 0.05 reject the hypothesis that there is normality)
            tempjb <- as.data.frame(t(c(i,
                                        jarque.bera.test(r_ind)$p.value,
                                        jarque.bera.test(r_fin)$p.value,
                                      jarque.bera.test(e_fin[,2])$p.value)),
                                    stringsAsFactors = F)
            rm(r_fin)
            colnames(tempjb) <- c("companyid","JB.pvalue_ind","JB.pvalue_fin","JB.pvalue_efin")
            r_JB <- rbind(r_JB,tempjb)
            rm(tempjb)
            
            #Data for contagion:
            r_ind <- cbind(as.data.frame(rnm, stringsAsFactors = F),
                           as.data.frame(r_ind, stringsAsFactors = F))
            rm(rnm)
            dta <- na.omit(merge(e_fin,r_ind,
                                 by.x = "date",
                                 by.y = "rnm",
                                 all.x = T,
                                 all.y = T))
            rm(r_ind,e_fin)
            risknormality <- rbind(risknormality,dta)
            rm(dta)
} ; rm(i,rtn,fin)



## Fazer relatório do teste de normalidade ##

write.csv2(x = r_JB,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3d_VARGARCHresiduals_JBtest[size].csv",
           row.names = F,
           na = "")
rm(r_JB)

# 3. Volatility spillover -------------------------------------------------------------

spilltest <- as.data.frame(NULL)

for (i in unique(risknormality$companyid)) {
      for (j in unique(risknormality$quarter[risknormality$companyid == i])) {
            dta <- risknormality[(risknormality$companyid == i & risknormality$quarter == j),c("r_ind","e_fin")]
            dta$sigma2_ind <- (dta$r_ind - mean(dta$r_ind))^2
            reg <- lm(sigma2_ind ~ 1 + lag(r_ind^2,1) + lag(sigma2_ind,1) + lag(e_fin^2,1), data = dta)
            rm(dta)
            test <- as.data.frame(t(c(
                  i,j,
                  summary(reg,robust=T)$coefficients["lag(e_fin^2, 1)","Estimate"],
                  summary(reg,robust=T)$coefficients["lag(e_fin^2, 1)","Pr(>|t|)"],
                  summary(reg, data = dta)$coefficients["lag(e_fin^2, 1)","Pr(>|t|)"],
                  jarque.bera.test(reg$residuals)$p.value)),
                  stringsAsFactors = F)
            rm(reg)
            colnames(test) <- c("companyid","quarter","gama_risk","p.test_robust","p.test","jb_residuals")
            spilltest <- rbind(spilltest,test)
            rm(test)
      }
} ; rm(i,j)
spilltest$gama_risk <- as.numeric(spilltest$gama_risk)
spilltest$p.test_robust <- round(as.numeric(spilltest$p.test_robust),4)
spilltest$p.test <- round(as.numeric(spilltest$p.test),4)
spilltest$jb_residuals <- round(as.numeric(spilltest$jb_residuals),4)
spilltest$spillover <- as.numeric(spilltest$p.test_robust <= 0.05)

#Report
spilltest[spilltest$spillover == 1,]

#Write files
write.csv2(x = risknormality,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3d_risknormalized[size].csv",
           row.names = F,
           na = "")

write.csv2(x = spilltest,
           file = "D:\\$database\\ownCloud\\dissertação\\data\\BRA\\3d_spillover[size].csv",
           row.names = F,
           na = "")

rm(risknormality,spilltest,summary.lm)
