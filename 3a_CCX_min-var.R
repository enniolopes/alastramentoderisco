## Cálculo do indicador de Contágio CCX ##
library(dplyr)
library(stringi)
library(lubridate)

## CARTEIRA SETOR FINANCEIRO E LIMIAR DE RETORNO EXTREMO ---------------------------------------------
fin <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\2_BRAfin_min-var.csv",
                 header = T, 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "", 
                 stringsAsFactors = F)
fin$date <- as.Date(fin$date)
fin$quarter <- stri_join(year(fin$date),quarters.Date(fin$date))

#Estatísticas descritivas
summary(fin)

      #Histograma
      windows()
      hist(fin$returnInf,
           breaks = 35,
           col = "darkgrey",
           main = "Histograma - retornos do setor financeiro",
           xlab = "Retorno dos preços ajustado pela inflação",
           ylab = "Frequência")
      
#Desvio padrão trimestral
fin$sd <- rep(NA, nrow(fin))
for (q in unique(fin$quarter)) {
      data <- filter(fin, quarter == q)
      data <- select(data, returnInf)
      fin$sd[fin$quarter == q] <- -sd(data[,1], na.rm = T)
      rm(data)
} ; rm(q)

#Desvio padrão acumulado
fin$aggsd <- rep(NA, nrow(fin))
for (q in unique(fin$quarter)) {
      dt <- filter(fin, quarter == q)
      dt <- max(unique(select(dt, date))[,1])
      data <- filter(fin, date <= dt)
      data <- select(data, returnInf)
      fin$aggsd[fin$quarter == q] <- -sd(data[,1], na.rm = T)
      rm(data)
} ; rm(q)

#Retorno continuo
fin$ccreturnInf <- log(1 + fin$returnInf)

# Sample Threshold quartely --------------------------------------------------------

#Limiar de 5% (threshold)
fin$threshold <- rep(NA, nrow(fin))
for (q in unique(fin$quarter)) {
      data <- filter(fin, quarter == q)
      data <- select(data, returnInf)
      fin$threshold[fin$quarter == q] <- quantile(data[,1], probs = .05, na.rm = T)
      rm(data)
} ; rm(q)

      #Limiar de 1% (threshold_1)
      fin$threshold_1 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            data <- filter(fin, quarter == q)
            data <- select(data, returnInf)
            fin$threshold_1[fin$quarter == q] <- quantile(data[,1], probs = .01, na.rm = T)
            rm(data)
      } ; rm(q)


      #Limiar de 10% (threshold_10)
      fin$threshold_10 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            data <- filter(fin, quarter == q)
            data <- select(data, returnInf)
            fin$threshold_10[fin$quarter == q] <- quantile(data[,1], probs = .1, na.rm = T)
            rm(data)
      } ; rm(q)


#Dados do conjunto do limiar
thresh <- select(fin,quarter,threshold, threshold_1, threshold_10, sd)
thresh <- unique(thresh)


windows()
plot(thresh$threshold,
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "Limiar e desvio padrão",
     main = c("Limiar extremo e desvio padrão dos retornos
da carteira do setor financeiro"),
     col = "red",
     lwd = 1.8
     )
lines(thresh$sd,col="black",lwd = 1.8)
axis(side = 1,at = 1:nrow(thresh) ,labels = thresh$quarter)
legend(x = 2,
       y = min(thresh$threshold+.006),
       col = c("red","black"),
       lwd = c(1.8,1.8),
       legend = c("Limiar","Desvio-padrão"))

#Computar os indicadores de retorno abaixo o limiar (Ic)
fin$Ic <- fin$returnInf < fin$threshold
indicator <- select(fin, quarter, Ic)
indicator$Ic <- as.numeric(indicator$Ic)
indicator <- as.data.frame(table(indicator), stringsAsFactors = F)
indicator <- indicator[indicator$Ic==1,c("quarter","Freq")]
colnames(indicator) <- c("quarter","Ic")

# Log-normal Threshold quartely ----------------------------------------------------
#Limiar de 5% log-normal (ccthreshold)
fin$ccthreshold <- rep(NA, nrow(fin))
for (q in unique(fin$quarter)) {
      dt <- fin[fin$quarter == q,]
      mu <- mean(dt$ccreturnInf)
      sd <- sd(dt$ccreturnInf)
      fin$ccthreshold[fin$quarter == q] <- qlnorm(.05,meanlog = mu, sdlog = sd) - 1
      rm(dt,mu,sd)
} ; rm(q)


      #Limiar de 1% log-normal (ccthreshold_1)
      fin$ccthreshold_1 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            dt <- fin[fin$quarter == q,]
            mu <- mean(dt$ccreturnInf)
            sd <- sd(dt$ccreturnInf)
            fin$ccthreshold_1[fin$quarter == q] <- qlnorm(.01,meanlog = mu, sdlog = sd) - 1
            rm(dt,mu,sd)
      } ; rm(q)


      #Limiar de 10% log-normal (ccthreshold_10)
      fin$ccthreshold_10 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            dt <- fin[fin$quarter == q,]
            mu <- mean(dt$ccreturnInf)
            sd <- sd(dt$ccreturnInf)
            fin$ccthreshold_10[fin$quarter == q] <- qlnorm(.1,meanlog = mu, sdlog = sd) - 1
            rm(dt,mu,sd)
      } ; rm(q)

      
      
#Dados do conjunto do limiar log-normal
ccthresh <- select(fin,quarter,ccthreshold, ccthreshold_1, ccthreshold_10, sd)
ccthresh <- unique(ccthresh)

windows()
plot(ccthresh$ccthreshold,
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "Limiar e desvio padrão",
     main = c("Limiar extremo e desvio padrão dos retornos
da carteira do setor financeiro - distribuição log-normal"),
     col = "red",
     lwd = 1.8
     )
lines(ccthresh$sd,col="black",lwd = 1.8)
axis(side = 1,at = 1:nrow(ccthresh) ,labels = ccthresh$quarter)
legend(x = 2,
       y = min(ccthresh$ccthreshold+.006),
       col = c("red","black"),
       lwd = c(1.8,1.8),
       legend = c("Limiar (log-normal)","Desvio-padrão"))


#Computar os indicadores de retorno abaixo o limiar (Ic_ln)
fin$Ic_ln <- fin$returnInf < fin$ccthreshold
ccindicator <- select(fin, quarter, Ic_ln)
ccindicator$Ic_ln <- as.numeric(ccindicator$Ic_ln)
ccindicator <- as.data.frame(table(ccindicator), stringsAsFactors = F)
ccindicator <- ccindicator[ccindicator$Ic_ln==1,c("quarter","Freq")]
colnames(ccindicator) <- c("quarter","Ic_ln")


#Gráficos do limiar log-normal e do desvio-padrão
windows()
plot(ccthresh$ccthreshold,
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "Limiar e desvio padrão",
     main = c("Limiar extremo e desvio padrão dos retornos
da carteira do setor financeiro"),
     col = "red",
     lwd = 1.5
     )
lines(ccthresh$sd,col="black",lwd = 1.5)
lines(thresh$threshold,col="blue",lwd = 1.5)
axis(side = 1,at = 1:nrow(ccthresh) ,labels = ccthresh$quarter)
legend(x = 2,
       y = min(ccthresh$ccthreshold+.005),
       col = c("red","black","blue"),
       lwd = c(1.5,1.5,1.5),
       legend = c("Limiar","Desvio-padrão","Limiar (log-normal)"))
      

# Sample Threshold full-sample --------------------------------------------

#Limiar de 5% (fsthreshold)
fin$fsthreshold <- quantile(fin$returnInf, probs = .05, na.rm = T)

      #Limiar de 1% (fsthreshold_1)
      fin$fsthreshold_1 <- quantile(fin$returnInf, probs = .01, na.rm = T)

      #Limiar de 10% (fsthreshold_10)
      fin$fsthreshold_10 <- quantile(fin$returnInf, probs = .1, na.rm = T)

#Dados do conjunto do limiar
fsthresh <- select(fin,quarter,fsthreshold, fsthreshold_1, fsthreshold_10, sd)
fsthresh <- unique(fsthresh)

#Gráfico do limiar fsthreshold e do desvio-padrão
windows()
plot(fsthresh$sd,
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "Limiar e desvio padrão",
     main = c("Limiar extremo usando toda a amostra e desvio padrão
               dos retornos da carteira do setor financeiro"),
     col = "black",
     lwd = 1.8
     )
lines(fsthresh$fsthreshold,col="red",lwd = 1.8)
axis(side = 1,at = 1:nrow(thresh) ,labels = fsthresh$quarter)
legend(x = 2,
       y = min(fsthresh$sd +.002),
       col = c("red","black"),
       lwd = c(1.8,1.8),
       legend = c("Limiar","Desvio-padrão"))

#Computar os indicadores de retorno abaixo o limiar (Ic)
fin$Icfs <- fin$returnInf < fin$fsthreshold
fsindicator <- select(fin, quarter, Icfs)
fsindicator$Icfs <- as.numeric(fsindicator$Icfs)
fsindicator <- as.data.frame(table(fsindicator), stringsAsFactors = F)
fsindicator <- fsindicator[fsindicator$Icfs==1,c("quarter","Freq")]
colnames(fsindicator) <- c("quarter","Icfs")

# Log-normal Threshold full-sample --------------------------------------------
mu <- mean(fin$ccreturnInf)
sd <- sd(fin$ccreturnInf)

#Limiar de 5% (fsccthreshold)
fin$ccfsthreshold <- qlnorm(.05,meanlog = mu, sdlog = sd) - 1

      #Limiar de 1% (fsccthreshold_1)
      fin$ccfsthreshold_1 <- qlnorm(.01,meanlog = mu, sdlog = sd) - 1

      #Limiar de 10% (fsccthreshold_10)
      fin$ccfsthreshold_10 <- qlnorm(.1,meanlog = mu, sdlog = sd) - 1

rm(mu,sd)

#Dados do conjunto do limiar
ccfsthresh <- select(fin,quarter,ccfsthreshold, ccfsthreshold_1, ccfsthreshold_10, sd)
ccfsthresh <- unique(ccfsthresh)

#Gráfico do limiar fsthreshold e do desvio-padrão
windows()
plot(ccfsthresh$sd,
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "Limiar e desvio padrão",
     main = c("Limiar extremo usando toda a amostra para uma distribuição log-normal
            e desvio padrão dos retornos da carteira do setor financeiro"),
     col = "black",
     lwd = 1.8
)
lines(ccfsthresh$ccfsthreshold,col="red",lwd = 1.8)
axis(side = 1,at = 1:nrow(thresh) ,labels = ccfsthresh$quarter)
legend(x = 2,
       y = min(ccfsthresh$sd +.002),
       col = c("red","black"),
       lwd = c(1.8,1.8),
       legend = c("Limiar","Desvio-padrão"))

#Computar os indicadores de retorno abaixo o limiar (Ic)
fin$Icfs_ln <- fin$returnInf < fin$ccfsthreshold
ccfsindicator <- select(fin, quarter, Icfs_ln)
ccfsindicator$Icfs_ln <- as.numeric(ccfsindicator$Icfs_ln)
ccfsindicator <- as.data.frame(table(ccfsindicator), stringsAsFactors = F)
ccfsindicator <- ccfsindicator[ccfsindicator$Icfs_ln==1,c("quarter","Freq")]
colnames(ccfsindicator) <- c("quarter","Icfs_ln")

# Aggregate Sample Threshold ----------------------------------------------

#Limiar de 5% (aggthreshold)
fin$aggthreshold <- rep(NA, nrow(fin))
for (q in unique(fin$quarter)) {
      dt <- filter(fin, quarter == q)
      dt <- max(unique(select(dt, date))[,1])
      data <- filter(fin, date <= dt)
      data <- select(data, returnInf)
      fin$aggthreshold[fin$quarter == q] <- quantile(data[,1], probs = .05, na.rm = T)
      rm(data,dt)
} ; rm(q)

      #Limiar de 1% (aggthreshold_1)
      fin$aggthreshold_1 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            dt <- filter(fin, quarter == q)
            dt <- max(unique(select(dt, date))[,1])
            data <- filter(fin, date <= dt)
            data <- select(data, returnInf)
            fin$aggthreshold_1[fin$quarter == q] <- quantile(data[,1], probs = .01, na.rm = T)
            rm(data,dt)
      } ; rm(q)

      #Limiar de 10% (aggthreshold_10)
      fin$aggthreshold_10 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            dt <- filter(fin, quarter == q)
            dt <- max(unique(select(dt, date))[,1])
            data <- filter(fin, date <= dt)
            data <- select(data, returnInf)
            fin$aggthreshold_10[fin$quarter == q] <- quantile(data[,1], probs = .1, na.rm = T)
            rm(data,dt)
      } ; rm(q)


#Dados do conjunto do limiar
aggthresh <- select(fin,quarter,aggthreshold, aggthreshold_1, aggthreshold_10, aggsd)
aggthresh <- unique(aggthresh)

windows()
plot(aggthresh$aggthreshold,
     ylim = c(min(c(min(aggthresh$aggthreshold),min(aggthresh$aggsd))),
              max(c(max(aggthresh$aggthreshold),max(aggthresh$aggsd)))),
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "Limiar e desvio padrão",
     main = c("Limiar extremo da série acumulada e desvio padrão
              dos retornos da carteira do setor financeiro"),
     col = "red",
     lwd = 1.8
     )
lines(aggthresh$aggsd,col="black",lwd = 1.8)
axis(side = 1,at = 1:nrow(aggthresh) ,labels = aggthresh$quarter)
legend(x = 2,
       y = min(aggthresh$aggthreshold+.002),
       col = c("red","black"),
       lwd = c(1.8,1.8),
       legend = c("Limiar da série acumulada","Desvio-padrão série acumulada"))

#Computar os indicadores de retorno abaixo o limiar (Ic)
fin$Icagg <- fin$returnInf < fin$aggthreshold
aggindicator <- select(fin, quarter, Icagg)
aggindicator$Icagg <- as.numeric(aggindicator$Icagg)
aggindicator <- as.data.frame(table(aggindicator), stringsAsFactors = F)
aggindicator <- aggindicator[aggindicator$Icagg==1,c("quarter","Freq")]
colnames(aggindicator) <- c("quarter","Icagg")

# Log-normal Aggregate Sample Threshold ----------------------------------------------

#Limiar de 5% (aggccthreshold)
fin$aggccthreshold <- rep(NA, nrow(fin))
for (q in unique(fin$quarter)) {
      dt <- filter(fin, quarter == q)
      dt <- max(unique(select(dt, date))[,1])
      data <- filter(fin, date <= dt)
      data <- select(data, ccreturnInf)
      mu <- mean(data$ccreturnInf)
      sd <- sd(data$ccreturnInf)
      fin$aggccthreshold[fin$quarter == q] <- qlnorm(.05,meanlog = mu, sdlog = sd) - 1
      rm(data,dt,mu,sd)
} ; rm(q)

      #Limiar de 1% (aggccthreshold_1)
      fin$aggccthreshold_1 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            dt <- filter(fin, quarter == q)
            dt <- max(unique(select(dt, date))[,1])
            data <- filter(fin, date <= dt)
            data <- select(data, ccreturnInf)
            mu <- mean(data$ccreturnInf)
            sd <- sd(data$ccreturnInf)
            fin$aggccthreshold_1[fin$quarter == q] <- qlnorm(.01,meanlog = mu, sdlog = sd) - 1
            rm(data,dt,mu,sd)
      } ; rm(q)

      #Limiar de 10% (aggccthreshold_10)
      fin$aggccthreshold_10 <- rep(NA, nrow(fin))
      for (q in unique(fin$quarter)) {
            dt <- filter(fin, quarter == q)
            dt <- max(unique(select(dt, date))[,1])
            data <- filter(fin, date <= dt)
            data <- select(data, ccreturnInf)
            mu <- mean(data$ccreturnInf)
            sd <- sd(data$ccreturnInf)
            fin$aggccthreshold_10[fin$quarter == q] <- qlnorm(.1,meanlog = mu, sdlog = sd) - 1
            rm(data,dt,mu,sd)
      } ; rm(q)


#Dados do conjunto do limiar
aggccthresh <- select(fin,quarter,aggccthreshold, aggccthreshold_1, aggccthreshold_10, aggsd)
aggccthresh <- unique(aggccthresh)

windows()
plot(aggccthresh$aggccthreshold,
     ylim = c(min(c(min(aggccthresh$aggccthreshold),min(aggccthresh$aggsd))),
              max(c(max(aggccthresh$aggccthreshold),max(aggccthresh$aggsd)))),
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "Limiar e desvio padrão",
     main = c("Limiar extremo da série acumulada para uma distribuição log-normal
              e desvio padrão dos retornos da carteira do setor financeiro"),
     col = "red",
     lwd = 1.8
     )
lines(aggccthresh$aggsd,col="black",lwd = 1.8)
axis(side = 1,at = 1:nrow(aggccthresh) ,labels = aggccthresh$quarter)
legend(x = 2,
       y = min(aggccthresh$aggccthreshold+.002),
       col = c("red","black"),
       lwd = c(1.8,1.8),
       legend = c("Limiar log-normal da série acumulada","Desvio-padrão série acumulada"))

#Computar os indicadores de retorno abaixo o limiar (Ic)
fin$Icagg_ln <- fin$returnInf < fin$aggccthreshold
aggccindicator <- select(fin, quarter, Icagg_ln)
aggccindicator$Icagg_ln <- as.numeric(aggccindicator$Icagg_ln)
aggccindicator <- as.data.frame(table(aggccindicator), stringsAsFactors = F)
aggccindicator <- aggccindicator[aggccindicator$Icagg_ln==1,c("quarter","Freq")]
colnames(aggccindicator) <- c("quarter","Icagg_ln")

# 3 SD Threshold full-sample --------------------------------------------
fin$sdthreshold <- -3*sd(fin$returnInf, na.rm = T)

#Dados do conjunto do limiar
sdthresh <- select(fin,quarter,sdthreshold, sd)
sdthresh <- unique(sdthresh)

#Computar os indicadores de retorno abaixo o limiar (Icsd)
fin$Icsd <- fin$returnInf < fin$sdthreshold
sdindicator <- select(fin, quarter, Icsd)
sdindicator$Icsd <- as.numeric(sdindicator$Icsd)
sdindicator <- as.data.frame(table(sdindicator), stringsAsFactors = F)
sdindicator <- sdindicator[sdindicator$Icsd==1,c("quarter","Freq")]
colnames(sdindicator) <- c("quarter","Icsd")



## RETORNO EMPRESAS -----------------------------------------------------
rtn <- read.csv2("D:\\$database\\ownCloud\\dissertação\\data\\BRA\\1_BRAnaofinanceiro.csv", 
                 header = T, 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "", 
                 stringsAsFactors = F)
rtn <- filter(rtn, ticker != "PTQS4")
rtn$date <- as.Date(rtn$date)
rtn$quarter <- stri_join(year(rtn$date),quarters.Date(rtn$date))
rtn$returnInf <- rtn$returnInf/100
rtn <- select(rtn,
              ticker,
              companyid,
              name,
              date,
              quarter,
              returnInf)
rtn <- na.omit(rtn)

##Filtro dados contábeis:
#tkt <- c("ABEV3","ABYA3","ACGU3","AEDU3","ALLL3",
#"ALPA3","AMAR3","AMIL3","ANIM3","ARCE3",
#"ARCZ3","ARZZ3","ASTA4","AUTM3","AVIL3",
#"BARB4","BDLL4","BEEF3","BOBR4","BPHA3",
#"BRDT4","BRFS3","BRKM3","BRTP3","BSUL5",
#"BTOW3","CAMB4","CARD3","CBEE3","CCRO3",
#"CEEB3","CEPE5","CESP3","CEVA4","CGRA3",
#"CLSC4","CMET4","CMIG3","CNFB4","CPFE3",
#"CPLE3","CPRE3","CPSL3","CREM3","CRGT5",
#"CRTP3","CRUZ3","CSAN3","CSMG3","CSNA3",
#"CSTB4","CTAX3","CTKA4","CTSA3","DASA3",
#"DPPI4","DTEX3","DXTG4","EALT4","EBEN4",
#"EBTP3","ELET3","ELEV3","EMBR3","ENBR3",
#"ENEV3","EPTE4","ESTC3","ETER3","EUCA4",
#"FESA4","FFTL4","FHER3","FIBR3","FJTA3",
#"FLCL5","FLRY3","FRIO3","FRTA3","FTSU4",
#"GGBR3","GOAU3","GOLL4","GPCP3","GRND3",
#"GUAR3","GVTT3","HBTS5","HGTX3","HYPE3",
#"IMBI4","INEP3","ITEC3","ITSA3","JBSS3",
#"JHSF3","KEPL3","KLBN4","KROT3","LEVE3",
#"LIGT3","LINX3","LOGN3","LREN3","MAGG3",
#"MAGS5","MAHS4","MDIA3","MEDI3","MGEL4",
#"MILS3","MLFT4","MMXM3","MNDL3","MRFG3",
#"MRSL4","MTIG4","MYPK3","NATU3","OIBR3",
#"OSXB3","PALF3","PCAR4","PEFX5","PETR3",
#"PFRM3","PLAS3","PMAM3","PNVL3","POMO3",
#"POSI3","PRIO3","PRVI3","PTIP4","PTNT4",
#"QGEP3","RAPT3","RHDS3","ROMI3","RPSA4",
#"RSIP3","SAPR4","SBSP3","SDIA3","SEER3",
#"SGAS4","SGPS3","SHAP4","SHOW3","SHUL4",
#"SLCE3","SLED4","SMTO3","SPRI3","SULT4",
#"SUZB5","SZPQ4","TBCP4","TCNO3","TCOC3",
#"TDBH3","TECN3","TELB3-old","TERI3","TGMA3",
#"TIMP3","TLCP3","TMAR3","TMGR3","TNCP3",
#"TNEP3","TNLP3","TPEC6B","TPIS3","TPRC3",
#"TRFO4","TRJC6","TSEP3","TUPY3","UCAS3",
#"UGPA3","UOLL4","USIM3","VAGR3","VALE3",
#"VGOR4","VIVO3","VIVR3","VIVT3","VLID3",
#"VVAR3","WEGE3","WHMT3")
#tkt <- as.data.frame(tkt, stringsAsFactors = F)

#rtn <- merge(rtn,tkt,
#             by.x = "ticker",
#             by.y = "tkt",
#             all.x = F,
#             all.y = T)
#rm(tkt)


#Informações dos dados
unique(rtn$ticker)
length(unique(rtn$returnInf))
length(unique(rtn$ticker))
dataf <- as.data.frame(table(unique(rtn[,c("quarter","ticker")])), stringsAsFactors = F)
colnames(dataf) <- c("quarter","ticker","dummy_obs")

#Estatísticas descritivas
summary(rtn)
mu <- aggregate(rtn$returnInf, by = list(rtn$companyid), FUN = mean, na.rm=T)
colnames(mu) <- c("companyid","mean")
md <- aggregate(rtn$returnInf, by = list(rtn$companyid), FUN = median, na.rm=T)
colnames(md) <- c("companyid","median")
sd <- aggregate(rtn$returnInf, by = list(rtn$companyid), FUN = sd, na.rm=T)
colnames(sd) <- c("companyid","standard deviation")
mn <- aggregate(rtn$returnInf, by = list(rtn$companyid), FUN = min, na.rm=T)
colnames(mn) <- c("companyid","minimum")
mx <- aggregate(rtn$returnInf, by = list(rtn$companyid), FUN = max, na.rm=T)
colnames(mx) <- c("companyid","maximum")
obs <- aggregate(!is.na(rtn$returnInf), by = list(rtn$companyid), FUN = sum, na.rm=T)
colnames(obs) <- c("companyid","observations")
mindate <- aggregate(rtn$date, by = list(rtn$companyid), FUN = min, na.rm=T)
colnames(mindate) <- c("companyid","min_date")
maxdate <- aggregate(rtn$date, by = list(rtn$companyid), FUN = max, na.rm=T)
colnames(maxdate) <- c("companyid","max_date")
desc <- merge(merge(merge(merge(merge(merge(merge(merge(mu,unique(select(rtn,companyid, name, ticker))),md),sd),mn),mx),obs),mindate),maxdate)
rm(mu,md,sd,mn,mx,obs,mindate,maxdate)

#Histograma
setwd("D:\\$database\\ownCloud\\dissertação\\reports\\BRA\\Histogramas retornos empresas")
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      nm <- stri_join("3_histograma_",i,".bmp",sep="")
      compan <- unique(rtn2$name)
      
      bmp(filename = nm)
      hist(rtn2$returnInf,
           breaks = 35,
           col = "darkgrey",
           main = stri_join("Histograma - retornos",compan,sep=" "),
           xlab = "Retorno dos preços ajustado pela inflação",
           ylab = "Frequência")
      dev.off()
      rm(rtn2,nm,compan)
} ; rm(i)


#Desvio padrão trimestral
rtn$sd <- rep(NA, nrow(rtn))

for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      for (q in unique(rtn2$quarter)) {
            data <- filter(rtn2, quarter == q)
            data <- select(data, returnInf)
            rtn$sd[rtn$quarter == q & rtn$ticker == i] <- sd(data[,1], na.rm = T)
            rm(data)
      }
      rm(rtn2)
} ; rm(i,q)

#Desvio padrão acumulado
rtn$aggsd <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      for (q in unique(rtn2$quarter)) {
            dt <- filter(rtn2, quarter == q)
            dt <- max(unique(select(dt, date))[,1])
            data <- filter(rtn2, date <= dt)
            data <- select(data, returnInf)
            rtn$aggsd[rtn$quarter == q & rtn$ticker == i] <- sd(data[,1], na.rm = T)
            rm(data,dt)
      }
      rm(rtn2)
} ; rm(i,q)

#Retorno continuo
rtn$ccreturnInf <- log(1 + rtn$returnInf)

# Sample Threshold quartely --------------------------------------------------------

#Limiar de 5% (threshold)
rtn$threshold <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      for (q in unique(rtn2$quarter)) {
            data <- filter(rtn2, quarter == q)
            data <- select(data, returnInf)
            rtn$threshold[rtn$quarter == q & rtn$ticker == i] <- quantile(data[,1], probs = .05, na.rm = T)
            rm(data)
            }
      rm(rtn2)
} ; rm(i,q)

      #Limiar de 1% (threshold_1)
      rtn$threshold_1 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  data <- filter(rtn2, quarter == q)
                  data <- select(data, returnInf)
                  rtn$threshold_1[rtn$quarter == q & rtn$ticker == i] <- quantile(data[,1], probs = .01, na.rm = T)
                  rm(data)
            }
            rm(rtn2)
      } ; rm(i,q)

      #Limiar de 10% (threshold_10)
      rtn$threshold_10 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  data <- filter(rtn2, quarter == q)
                  data <- select(data, returnInf)
                  rtn$threshold_10[rtn$quarter == q & rtn$ticker == i] <- quantile(data[,1], probs = .1, na.rm = T)
                  rm(data)
            }
            rm(rtn2)
      } ; rm(i,q)


#Dados do conjunto do limiar
thresh_rtn <- select(rtn,companyid,quarter,threshold,threshold_1,threshold_10,sd)
thresh_rtn <- unique(thresh_rtn)

#Computar os indicadores de retorno abaixo o limiar (Ic)
rtn$Ic <- rtn$returnInf < rtn$threshold
indicator_rtn <- select(rtn, companyid, quarter, Ic)
indicator_rtn$Ic <- as.numeric(indicator_rtn$Ic)
indicator_rtn <- as.data.frame(table(indicator_rtn), stringsAsFactors = F)
indicator_rtn <- indicator_rtn[indicator_rtn$Ic==1,c("companyid","quarter","Freq")]
colnames(indicator_rtn) <- c("companyid","quarter","Ic")

# Log-normal threshold quartely ----------------------------------------------------

#Limiar de 5% log-normal (ccthreshold)
rtn$ccthreshold <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      for (q in unique(rtn2$quarter)) {
            dt <- rtn2[rtn2$quarter == q,]
            mu <- mean(dt$ccreturnInf)
            sd <- sd(dt$ccreturnInf)
            rtn$ccthreshold[rtn$quarter == q & rtn$ticker == i] <- qlnorm(.05,meanlog = mu, sdlog = sd) - 1
            rm(dt,mu,sd)
      }
      rm(rtn2)
} ; rm(i,q)

      #Limiar de 1% log-normal (ccthreshold_1)
      rtn$ccthreshold_1 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  dt <- rtn2[rtn2$quarter == q,]
                  mu <- mean(dt$ccreturnInf)
                  sd <- sd(dt$ccreturnInf)
                  rtn$ccthreshold_1[rtn$quarter == q & rtn$ticker == i] <- qlnorm(.01,meanlog = mu, sdlog = sd) - 1
                  rm(dt,mu,sd)
            }
            rm(rtn2)
      } ; rm(i,q)

      #Limiar de 10% log-normal (ccthreshold_10)
      rtn$ccthreshold_10 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  dt <- rtn2[rtn2$quarter == q,]
                  mu <- mean(dt$ccreturnInf)
                  sd <- sd(dt$ccreturnInf)
                  rtn$ccthreshold_10[rtn$quarter == q & rtn$ticker == i] <- qlnorm(.1,meanlog = mu, sdlog = sd) - 1
                  rm(dt,mu,sd)
            }
            rm(rtn2)
      } ; rm(i,q)


#Dados do conjunto do limiar log-normal
ccthresh_rtn <- select(rtn,companyid,quarter,ccthreshold, ccthreshold_1, ccthreshold_10, sd)
ccthresh_rtn <- unique(ccthresh_rtn)

#Computar os indicadores de retorno abaixo o limiar (Ic_ln)
rtn$Ic_ln <- rtn$returnInf < rtn$ccthreshold
ccindicator_rtn <- select(rtn, companyid, quarter, Ic_ln)
ccindicator_rtn$Ic_ln <- as.numeric(ccindicator_rtn$Ic_ln)
ccindicator_rtn <- as.data.frame(table(ccindicator_rtn), stringsAsFactors = F)
ccindicator_rtn <- ccindicator_rtn[ccindicator_rtn$Ic_ln==1,c("companyid","quarter","Freq")]
colnames(ccindicator_rtn) <- c("companyid","quarter","Ic_ln")

# Sample Threshold full-sample --------------------------------------------

#Limiar de 5% (fsthreshold)
rtn$fsthreshold <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      rtn$fsthreshold[rtn$ticker == i] <- quantile(rtn2$returnInf, probs = .05, na.rm = T)
      rm(rtn2)
} ; rm(i)

      #Limiar de 1% (fsthreshold_1)
      rtn$fsthreshold_1 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            rtn$fsthreshold_1[rtn$ticker == i] <- quantile(rtn2$returnInf, probs = .01, na.rm = T)
            rm(rtn2)
      } ; rm(i)

      #Limiar de 10% (fsthreshold_10)
      rtn$fsthreshold_10 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            rtn$fsthreshold_10[rtn$ticker == i] <- quantile(rtn2$returnInf, probs = .1, na.rm = T)
            rm(rtn2)
      } ; rm(i)


#Dados do conjunto do limiar
fsthresh_rtn <- select(rtn,companyid,quarter,fsthreshold,fsthreshold_1,fsthreshold_10,sd)
fsthresh_rtn <- unique(fsthresh_rtn)

#Computar os indicadores de retorno abaixo o limiar (Icfs)
rtn$Icfs <- rtn$returnInf < rtn$fsthreshold
fsindicator_rtn <- select(rtn, companyid, quarter, Icfs)
fsindicator_rtn$Icfs <- as.numeric(fsindicator_rtn$Icfs)
fsindicator_rtn <- as.data.frame(table(fsindicator_rtn), stringsAsFactors = F)
fsindicator_rtn <- fsindicator_rtn[fsindicator_rtn$Icfs==1,c("companyid","quarter","Freq")]
colnames(fsindicator_rtn) <- c("companyid","quarter","Icfs")

# Log-normal Threshold full-sample --------------------------------------------

#Limiar de 5% (fsccthreshold)
rtn$ccfsthreshold <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      mu <- mean(rtn2$ccreturnInf, na.rm = T)
      sd <- sd(rtn2$ccreturnInf, na.rm = T)
      rtn$ccfsthreshold[rtn$ticker == i] <- qlnorm(.05,meanlog = mu, sdlog = sd) - 1
      rm(rtn2,mu,sd)
} ; rm(i)

      #Limiar de 1% (fsccthreshold_1)
      rtn$ccfsthreshold_1 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            mu <- mean(rtn2$ccreturnInf, na.rm = T)
            sd <- sd(rtn2$ccreturnInf, na.rm = T)
            rtn$ccfsthreshold_1[rtn$ticker == i] <- qlnorm(.01,meanlog = mu, sdlog = sd) - 1
            rm(rtn2,mu,sd)
      } ; rm(i)

      #Limiar de 10% (fsccthreshold_10)
      rtn$ccfsthreshold_10 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            mu <- mean(rtn2$ccreturnInf, na.rm = T)
            sd <- sd(rtn2$ccreturnInf, na.rm = T)
            rtn$ccfsthreshold_10[rtn$ticker == i] <- qlnorm(.1,meanlog = mu, sdlog = sd) - 1
            rm(rtn2,mu,sd)
      } ; rm(i)


#Dados do conjunto do limiar
ccfsthresh_rtn <- select(rtn,companyid,quarter,ccfsthreshold,ccfsthreshold_1,ccfsthreshold_10,sd)
ccfsthresh_rtn <- unique(ccfsthresh_rtn)

#Computar os indicadores de retorno abaixo o limiar (Icfs_ln)
rtn$Icfs_ln <- rtn$returnInf < rtn$ccfsthreshold
ccfsindicator_rtn <- select(rtn,companyid,quarter,Icfs_ln)
ccfsindicator_rtn$Icfs_ln <- as.numeric(ccfsindicator_rtn$Icfs_ln)
ccfsindicator_rtn <- as.data.frame(table(ccfsindicator_rtn), stringsAsFactors = F)
ccfsindicator_rtn <- ccfsindicator_rtn[ccfsindicator_rtn$Icfs_ln==1,c("companyid","quarter","Freq")]
colnames(ccfsindicator_rtn) <- c("companyid","quarter","Icfs_ln")

# Aggregate Sample Threshold ----------------------------------------------

#Limiar de 5% (aggthreshold)
rtn$aggthreshold <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      for (q in unique(rtn2$quarter)) {
            dt <- filter(rtn2, quarter == q)
            dt <- max(unique(select(dt, date))[,1])
            data <- filter(rtn2, date <= dt)
            data <- select(data, returnInf)
            rtn$aggthreshold[rtn$quarter == q & rtn$ticker == i] <- quantile(data[,1], probs = .05, na.rm = T)
            rm(data,dt)
      }
      rm(rtn2)
} ; rm(i,q)

      #Limiar de 1% (aggthreshold_1)
      rtn$aggthreshold_1 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  dt <- filter(rtn2, quarter == q)
                  dt <- max(unique(select(dt, date))[,1])
                  data <- filter(rtn2, date <= dt)
                  data <- select(data, returnInf)
                  rtn$aggthreshold_1[rtn$quarter == q & rtn$ticker == i] <- quantile(data[,1], probs = .05, na.rm = T)
                  rm(data,dt)
            }
            rm(rtn2)
      } ; rm(i,q)

      #Limiar de 10% (aggthreshold_10)
      rtn$aggthreshold_10 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  dt <- filter(rtn2, quarter == q)
                  dt <- max(unique(select(dt, date))[,1])
                  data <- filter(rtn2, date <= dt)
                  data <- select(data, returnInf)
                  rtn$aggthreshold_10[rtn$quarter == q & rtn$ticker == i] <- quantile(data[,1], probs = .05, na.rm = T)
                  rm(data,dt)
            }
            rm(rtn2)
      } ; rm(i,q)
      
      
#Dados do conjunto do limiar
aggthresh_rtn <- select(rtn,companyid,quarter,aggthreshold,aggthreshold_1,aggthreshold_10,aggsd)
aggthresh_rtn <- unique(aggthresh_rtn)

#Computar os indicadores de retorno abaixo o limiar (Icagg)
rtn$Icagg <- rtn$returnInf < rtn$aggthreshold
aggindicator_rtn <- select(rtn,companyid,quarter,Icagg)
aggindicator_rtn$Icagg <- as.numeric(aggindicator_rtn$Icagg)
aggindicator_rtn <- as.data.frame(table(aggindicator_rtn), stringsAsFactors = F)
aggindicator_rtn <- aggindicator_rtn[aggindicator_rtn$Icagg==1,c("companyid","quarter","Freq")]
colnames(aggindicator_rtn) <- c("companyid","quarter","Icagg")

# Log-normal Aggregate Sample Threshold ----------------------------------------------

#Limiar de 5% log-normal (aggccthreshold)
rtn$aggccthreshold <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker==i,]
      for (q in unique(rtn2$quarter)) {
            dt <- filter(rtn2, quarter == q)
            dt <- max(unique(select(dt, date))[,1])
            data <- filter(rtn2, date <= dt)
            mu <- mean(data$ccreturnInf)
            sd <- sd(data$ccreturnInf)
            rtn$aggccthreshold[rtn$quarter == q & rtn$ticker == i] <- qlnorm(.05,meanlog = mu, sdlog = sd) - 1
            rm(dt,mu,sd,data)
      }
      rm(rtn2)
} ; rm(i,q)

      #Limiar de 1% log-normal (aggccthreshold_1)
      rtn$aggccthreshold_1 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  dt <- filter(rtn2, quarter == q)
                  dt <- max(unique(select(dt, date))[,1])
                  data <- filter(rtn2, date <= dt)
                  mu <- mean(data$ccreturnInf)
                  sd <- sd(data$ccreturnInf)
                  rtn$aggccthreshold_1[rtn$quarter == q & rtn$ticker == i] <- qlnorm(.1,meanlog = mu, sdlog = sd) - 1
                  rm(dt,mu,sd,data)
            }
            rm(rtn2)
      } ; rm(i,q)

      #Limiar de 10% log-normal (aggccthreshold_10)
      rtn$aggccthreshold_10 <- rep(NA, nrow(rtn))
      for (i in unique(rtn$ticker)) {
            rtn2 <- rtn[rtn$ticker==i,]
            for (q in unique(rtn2$quarter)) {
                  dt <- filter(rtn2, quarter == q)
                  dt <- max(unique(select(dt, date))[,1])
                  data <- filter(rtn2, date <= dt)
                  mu <- mean(data$ccreturnInf)
                  sd <- sd(data$ccreturnInf)
                  rtn$aggccthreshold_10[rtn$quarter == q & rtn$ticker == i] <- qlnorm(.1,meanlog = mu, sdlog = sd) - 1
                  rm(dt,mu,sd,data)
            }
            rm(rtn2)
      } ; rm(i,q)


#Dados do conjunto do limiar
aggccthresh_rtn <- select(rtn,companyid,quarter,aggccthreshold,aggccthreshold_1,aggccthreshold_10,aggsd)
aggccthresh_rtn <- unique(aggccthresh_rtn)

#Computar os indicadores de retorno abaixo o limiar (Icagg_ln)
rtn$Icagg_ln <- rtn$returnInf < rtn$aggccthreshold
aggccindicator_rtn <- select(rtn,companyid,quarter,Icagg_ln)
aggccindicator_rtn$Icagg_ln <- as.numeric(aggccindicator_rtn$Icagg_ln)
aggccindicator_rtn <- as.data.frame(table(aggccindicator_rtn), stringsAsFactors = F)
aggccindicator_rtn <- aggccindicator_rtn[aggccindicator_rtn$Icagg_ln==1,c("companyid","quarter","Freq")]
colnames(aggccindicator_rtn) <- c("companyid","quarter","Icagg_ln")

# 3 SD Threshold full-sample --------------------------------------------
rtn$sdthreshold <- rep(NA, nrow(rtn))
for (i in unique(rtn$ticker)) {
      rtn$sdthreshold[rtn$ticker == i] <- -3*sd(rtn[rtn$ticker==i,"returnInf"], na.rm = T)
} ; rm(i)

#Dados do conjunto do limiar
sdthresh_rtn <- select(rtn,companyid,quarter,sdthreshold,sd)
sdthresh_rtn <- unique(sdthresh_rtn)

#Computar os indicadores de retorno abaixo o limiar (Icsd)
rtn$Icsd <- rtn$returnInf < rtn$sdthreshold
sdindicator_rtn <- select(rtn, companyid, quarter, Icsd)
sdindicator_rtn$Icsd <- as.numeric(sdindicator_rtn$Icsd)
sdindicator_rtn <- as.data.frame(table(sdindicator_rtn), stringsAsFactors = F)
sdindicator_rtn <- sdindicator_rtn[sdindicator_rtn$Icsd==1,c("companyid","quarter","Freq")]
colnames(sdindicator_rtn) <- c("companyid","quarter","Icsd")



# CCX ---------------------------------------------------------------------
finthresh <- select(fin,date,Ic,Ic_ln,Icfs,Icfs_ln,Icagg,Icagg_ln,Icsd)
colnames(finthresh) <- c("date","Ic_FIN","Ic_ln_FIN","Icfs_FIN","Icfs_ln_FIN","Icagg_FIN","Icagg_ln_FIN","Icsd_FIN")
rtn <- filter(rtn, date >= "1996-01-01")
rtn <- merge(rtn, finthresh,
             by.x = "date",
             by.y = "date",
             all.x = T,
             all.y = F)
rm(finthresh)
rtn$CCX <- rtn$Ic_FIN * rtn$Ic
rtn$CCX_ln <- rtn$Ic_ln_FIN * rtn$Ic_ln
rtn$CCX_fs <- rtn$Icfs_FIN * rtn$Icfs
rtn$CCX_fsln <- rtn$Icfs_ln_FIN * rtn$Icfs_ln
rtn$CCX_agg <- rtn$Icagg_FIN * rtn$Icagg
rtn$CCX_aggln <- rtn$Icagg_ln_FIN * rtn$Icagg_ln
rtn$CCX_sd <- rtn$Icsd_FIN * rtn$Icsd

ccx <- as.data.frame(NULL)
for (i in unique(rtn$ticker)) {
      rtn2 <- rtn[rtn$ticker == i,]
      rtn2 <- select(rtn2,quarter,CCX,CCX_ln,CCX_fs,CCX_fsln,CCX_agg,CCX_aggln,CCX_sd)
      rtn2 <- aggregate(x = select(rtn2,-quarter),
                        by = list(rtn2$quarter),
                        FUN = sum,
                        na.rm=T)
      colnames(rtn2) <- c("quarter","CCX","CCX_ln","CCX_fs","CCX_fsln","CCX_agg","CCX_aggln","CCX_sd")
      pb <- aggregate(x = rtn[rtn$ticker == i,"returnInf"],
                      by = list(rtn[rtn$ticker == i,"quarter"]),
                      FUN = length)
      colnames(pb) <- c("quarter","nt")
      rtn2 <- merge(rtn2,pb,by.x = "quarter",by.y = "quarter")
      rm(pb)
      rtn2$pbCCX <- rtn2$CCX/rtn2$nt
      rtn2$pbCCX_ln <- rtn2$CCX_ln/rtn2$nt
      rtn2$pbCCX_fs <- rtn2$CCX_fs/rtn2$nt
      rtn2$pbCCX_fsln <- rtn2$CCX_fsln/rtn2$nt
      rtn2$pbCCX_agg <- rtn2$CCX_agg/rtn2$nt
      rtn2$pbCCX_aggln <- rtn2$CCX_aggln/rtn2$nt
      rtn2$pbCCX_sd <- rtn2$CCX_sd/rtn2$nt
      rtn2$ticker = i
      ccx <- rbind(ccx,rtn2)
      rm(rtn2)
} ; rm(i)


# Write files -------------------------------------------------------------
setwd("D:\\$database\\ownCloud\\dissertação\\data\\BRA")

#Gravar arquivos dos dados finais do CCX
write.csv2(rtn,"3a_nonfinCCXdata[min-var].csv",row.names = F,na = "")
rm(rtn)
write.csv2(fin,"3a_finCCXdata[min-var].csv",row.names = F,na = "")
rm(fin)
write.csv2(ccx,"3a_CCX[min-var].csv",row.names = F,na = "")
rm(ccx)

#Gravar arquivo com os limiares e desvio-padrão do setor financeiro
indicator <- merge(merge(merge(merge(merge(merge(indicator,ccindicator),fsindicator),ccfsindicator),aggindicator),aggccindicator),sdindicator)
rm(ccindicator,fsindicator,ccfsindicator,aggindicator,aggccindicator,sdindicator)
write.csv2(indicator,"3a_fin_indicator[min-var].csv",row.names = F,na = "")
rm(indicator)

indicator_rtn <- merge(merge(merge(merge(merge(merge(indicator_rtn,ccindicator_rtn),fsindicator_rtn),ccfsindicator_rtn),aggindicator_rtn),aggccindicator_rtn),sdindicator_rtn)
rm(ccindicator_rtn,fsindicator_rtn,ccfsindicator_rtn,aggindicator_rtn,aggccindicator_rtn,sdindicator_rtn)
write.csv2(indicator_rtn,"3a_nonfin_indicator[min-var].csv",row.names = F,na = "")
rm(indicator_rtn)

thresh <- merge(merge(merge(merge(merge(merge(thresh,ccthresh),fsthresh),ccfsthresh),aggccthresh),aggthresh),sdthresh)
rm(ccthresh,fsthresh,ccfsthresh,aggccthresh,aggthresh,sdthresh)
write.csv2(thresh,"3a_fin_threshold[min-var].csv",row.names = F,na = "")
rm(thresh)

thresh_rtn <- merge(merge(merge(merge(merge(merge(thresh_rtn,ccthresh_rtn),fsthresh_rtn),ccfsthresh_rtn),aggthresh_rtn),aggccthresh_rtn),sdthresh_rtn)
rm(ccthresh_rtn,fsthresh_rtn,ccfsthresh_rtn,aggthresh_rtn,aggccthresh_rtn,sdthresh_rtn)
write.csv2(thresh_rtn,"3a_nonfin_threshold[min-var].csv",row.names = F,na = "")
rm(thresh_rtn)

#Gravar arquivo com a descrição dos dados das empresas setor não financeiro
write.csv2(desc,"3_descriptive.csv",row.names = F,na = "")
rm(desc)
write.csv2(dataf,"3_companieslist.csv",row.names = F,na = "")
rm(dataf)
