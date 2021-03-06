##    Limpeza base dos dados de cota��es do Setor N�o Financeiro  ##
## BRA ##



##Leitura dos dados de cota��o do setor n�o financeiro:
##[dados]()
rtn <- read.csv2(choose.files(), 
                 header = T, 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "", 
                 stringsAsFactors = F)
library("dplyr")
rtn <- select(rtn, 
              ticker,
              origcurrency, 
              active, 
              liquidityy, 
              assettype, 
              date, 
              close, 
              closeInf, 
              returnInf,
              ticker,
              companyid,
              name,
              class)
rtn$date <- as.Date(rtn$date, "%Y-%m-%d")

      ###Relat�rio: resumo dos dados
      str(rtn)

##Seleciona apenas a��es ordin�rias e preferenciais. Portanto � realizado o corte de BDRs, ETFs,
##units, a��es nominativas, fracionadas, mercado de balc�o, e outros tipos.

      ###Relat�rio: tickers exclu�dos
      unique(select(filter(rtn, assettype != "Stock"),ticker,class,origcurrency,active))
      rtn <- filter(rtn, assettype == "Stock")



##Primeiro � realizado uma pre sele��o das observa��es com a premissa de que data final das 
#observa��es para um determinado ticker � a data da �ltima cota��o do pre�o de fechamento dispon�vel

for (t in unique(rtn$ticker)) {
      a <- filter(rtn, ticker == t)
      a <- max(a[!is.na(a$close),"date"])
      temp <- filter(rtn, ticker == t, date <=a)
      rtn <- filter(rtn, ticker != t)
      rtn <- rbind(rtn, temp)
      rm(temp,a)
}



##Drop de quantidade de observa��es:
library(data.table)
library(stringi)

##Data frame com a rela��o dos tickers e anos
rtn$procindex <- stri_join(rtn$ticker, year(rtn$date), month(rtn$date), sep="_")
rtn$nonmissing <- !is.na(rtn$close)
du <- as.data.frame(aggregate(rtn$nonmissing,
                by=list("procindex"=rtn$procindex), 
                FUN=sum))
##Rela��o do inicio da s�rie das observa��es:
ds <- as.data.frame(aggregate(rtn$date,
                        by=list("ticker"=rtn$ticker), 
                        FUN=min))
      #Fun��o para determinar a data do final do m�s:
      library(timeDate)

ds$d <- as.Date(alignMonthly(
      as.Date(stri_join(year(ds$x),
                        "-",
                        month(ds$x),
                        "-10"),
              format="%Y-%m-%d")))



#Registra a quantidade de dias no m�s de in�cio da cota��o da s�rie ('ipoy')
du$ipoy <- as.numeric(rep(NA,nrow(du)))
colnames(du) <- c("procindex","nobs","ipoy")
for (i in unique(rtn$ticker)) {
      a <- filter(ds, ticker == i)[,"d"]
      du[du$procindex == stri_join(i,year(a),month(a),sep="_"),"ipoy"] <- 
            nrow(filter(rtn, ticker == i, date <= a))
} ; rm(a,i,ds)

##Rela��o dos retornos discretos:
#Preenchimento dos missing de dados de fechamento com a �ltima observa��o dispon�vel
rtn <- arrange(rtn, ticker, date)
library(zoo)
close0 <- select(rtn, ticker, close)
close0 <- as.data.frame(close0 %>% group_by(ticker) %>% na.locf %>% ungroup)
rtn$close0 <- as.numeric(close0[,"close"])
rm(close0)

#C�lculo dos retornos a partir dos fechamentos sem missing values
#(� considerado que os missing values ocorreram pois n�o houve cota��o no dia, portanto o valor 
#da a��o � o mesmo da �ltima cota��o)
rtn$return0 <- as.numeric(x = rep(NA,nrow(rtn)))
for (j in unique(rtn$ticker)) {
      temp <- c(NA,
                round((filter(
                      rtn, ticker == j)[2:nrow(
                            filter(rtn, ticker == j)),
                            "close0"]
                      /filter(
                            rtn, ticker == j)[1:(nrow(
                                  filter(rtn, ticker == j))-1),
                                  "close0"]
                ) - 1,6))
      rtn[rtn$ticker==j,"return0"] <- temp
      rm(temp)
} ; rm(j)

#Observa��es de retornos diferentes de zero
rtn$rt0 <- rtn$return0 != 0
du <- merge(x = du,
            y = as.data.frame(aggregate(rtn$rt0,
                                        by=list("procindex"=rtn$procindex),
                                        FUN=sum,
                                        na.rm=T)),
            by.x = "procindex",
            by.y = "procindex",
            all.x = T,
            all.y = T)
colnames(du) <- c("procindex","nobs","ipoy","rt0")

#Data frame de filtro:
      #vARI�VEIS:
      #nobs: quantidade de observa��es no m�s sem missing value
      #ipoy: quantidade total de observa��es no m�s de inicio da s�rie
      #rt0: quantidade de retornos diferentes de zero

#Vari�vel dummy dos meses a serem exclu�dos dos tickers
      #Crit�rios para exclus�o:
      #1. Quantidade de observa��es menor que 2/3 dos dias �teis do m�s (m�s com 20 du);
      #2. Metade dos retornos da s�rie iguais a zero.
du$sel <- as.numeric(rep(NA,nrow(du)))
for(i in unique(du$procindex)) {
      t <- 
      if (is.na(filter(du, procindex == i)[,"ipoy"])) {
            if (all(
                  filter(du, procindex == i)[,"nobs"] >= round(20*2/3,0),
                  filter(du, procindex == i)[,"rt0"] >= round(20/2,0))) {1}else{0}
      } else {
            if (all(
                  filter(du, procindex == i)[,"nobs"] >= 
                  round(filter(du, procindex == i)[,"ipoy"]*2/3,0),
                  filter(du, procindex == i)[,"rt0"] >= 
                  round(filter(du, procindex == i)[,"ipoy"]/2,0))) {1}else{0}
      }
      du[du$procindex==i,"sel"] <- t
      rm(t)
} ; rm(i)
      #Relat�rio dos tickers e ano que ser�o exclu�dos e gr�fico da frequ�ncia das observa��es:
      filter(du,sel==0)
      hist(filter(du,sel==1)[,"nobs"],
           col = "red",
           main = "Frequ�ncia total observa��es por ano",
           xlab = "Observa��es por ano do pre�o de fechamento",
           ylab = "Frequ�ncia das observa��es"
      )
#Limpeza das observa��es com baixa quantidade de observa��es
n <- nrow(rtn)
rtn <- merge(select(filter(du,sel==1),procindex),
             rtn,
             by.x = "procindex",
             by.y = "procindex",
             all.x = T,
             all.y = F)

      ###Relat�rio: Quantidade de observa��es exclu�das:
      print(n - nrow(rtn))
      print((1-(nrow(rtn)/n))*100) #em porcentagem: 52%
rm(n,du)



## Limpar ticker que cont�m menos de 12 meses de observa��es (anteriores a 2015)
library(lubridate)
s <- as.data.frame(table(unique(select(rtn,procindex,ticker))[,"ticker"]))
colnames(s) <- c("ticker","months")
s$months <- s$months - 1

s <- as.data.frame(merge(s,
           y = aggregate(rtn$date,
                         by=list("ticker"=rtn$ticker),
                         FUN = min),
           by.x = "ticker",
           by.y = "ticker"))
s$ticker <- as.character(s$ticker)
colnames(s) <- c("ticker","months","stdate")

      ###Relat�rio: Ticker a serem exclu�dos:
      filter(s, months <= 12, stdate < "2015-01-01")[,"ticker"]
      n <- nrow(rtn)
      
for (i in unique(filter(s, months < 12, stdate < "2015-01-01")[,"ticker"])) {
rtn <- filter(rtn, ticker != i)
} ; rm(i)
s <- filter(s, months >= 12 | stdate >= "2015-01-01")
      ###Relat�rio: total de observa��es exclu�das
      print(n - nrow(rtn)) ; rm(n)
      s



      ###Relat�rio: Quantidade de observa��es por ticker:
      r <- cbind(table(select(rtn, ticker, nonmissing)),
                 round((
                       filter(as.data.frame(table(select(rtn,
                                                         ticker,
                                                         nonmissing))),
                              nonmissing == "TRUE")[,3]
                       /(filter(as.data.frame(table(select(rtn,
                                                           ticker,
                                                           nonmissing))),
                                nonmissing == "TRUE")[,3]
                         +filter(as.data.frame(table(select(rtn,
                                                            ticker,
                                                            nonmissing))),
                                 nonmissing == "FALSE")[,3]))*100,1))
      print(r)
      r <- cbind(as.data.frame(r),rownames(r))
      colnames(r) <- c("missing","non missing","freq non missing","ticker")
      r <- merge(r,s[,c(1,2)],
                 by.x ="ticker",
                 by.y ="ticker",
                 all.x = T)
      r$du <- r$months*21
      r$freq_total <- round((r$`non missing`/r$du)*100,1)
      r
      filter(r, freq_total < 50)
      rm(r,s)

      rtn <- select(rtn,-procindex,-nonmissing,-close0,-return0,-rt0)

#Verifica��o das classes dos ativos
#Crit�rios: ficar com as ON de preferencia, caso n�o tenha as PNA,B,C,D... na sequencia
#e exclus�o dos pa�ses de origem diferentes do Brasil:
cid <- unique(select(rtn, companyid, ticker, class))
      #[Arquivo de cadastro das empresas no Econom�tica](.\ownCloud\database\rawdata\economatica_registry)
      reg <- read.csv2(choose.files(),
                       sep = ";",
                       dec =",",
                       na.strings = "-",
                       skip = 4,
                       stringsAsFactors = F)
      
      ##Jun��o das tabelas de dados e cadastro
      reg <- select(reg, 
                    Ticker,
                    Country.of.Origin)
      colnames(reg) <- c("ticker",
                         "countryorigin")
      cid <- merge.data.frame(cid, reg, 
                              by.x = "ticker", 
                              by.y = "ticker", 
                              all.x = T, 
                              all.y = F)
      rm(reg)
#Empresas com mais de 1 ticker:
arrange(cid[duplicated(cid$companyid),],companyid)

      #Exclus�o pa�s diferente BR
      cid <- filter(cid, countryorigin == "BR")
      for(i in grep(pattern = "[+]",cid$ticker,value = T)) {
            cid <- filter(cid, ticker != i)
      }
      
      #Sele��o das ON e PN
      on <- filter(cid, class == "ON" | class == "ON A")
            #exclus�o ON do restante
      non <- filter(cid, class != "ON" & class != "ON A")
      non <- merge(non, select(on, companyid, ticker),
                   by.x = "companyid",
                   by.y = "companyid",
                   all.x = T,
                   all.y = F)
      colnames(non) <- c("companyid","ticker","class","countryorigin","tickerON")
      non <- non[is.na(non$tickerON),]
            #exclus�o UNT N2
      non <- filter(non, class != "UNT N2")
      
            #sele��o e exclus�o PN do restante
      pnpn <- filter(non, class=="PN")
      non <- merge(non, select(pnpn, companyid, ticker),
                   by.x = "companyid",
                   by.y = "companyid",
                   all.x = T,
                   all.y = F)
      colnames(non) <- c("companyid","ticker","class","countryorigin","tickerON","tickerPN")
      non <- non[is.na(non$tickerPN),]
            #sele��o e exclus�o PNA do restante
      pnpna <- filter(non, class=="PNA")
      non <- merge(non, select(pnpna, companyid, ticker),
                   by.x = "companyid",
                   by.y = "companyid",
                   all.x = T,
                   all.y = F)
      colnames(non) <- c("companyid","ticker","class","countryorigin","tickerON","tickerPN","tickerPNA")
      non <- non[is.na(non$tickerPNA),]
            #sele��o e exclus�o PNB do restante
      pnpnb <- filter(non, class=="PNB")
      non <- merge(non, select(pnpnb, companyid, ticker),
                   by.x = "companyid",
                   by.y = "companyid",
                   all.x = T,
                   all.y = F)
      colnames(non) <- c("companyid","ticker","class","countryorigin","tickerON","tickerPN","tickerPNA","tickerPNB")
      non <- non[is.na(non$tickerPNB),]
      
      #ticker finais restantes:
      pnpn <- select(pnpn, companyid, ticker, class, countryorigin)
      pnpna <- select(pnpna, companyid, ticker, class, countryorigin)
      pnpnb <- select(pnpnb, companyid, ticker, class, countryorigin)
      finalt <- rbind(on,rbind(pnpn,rbind(pnpna,pnpnb)))
      if (nrow(finalt) == length(unique(finalt$companyid))) {
            print("Existe apenas um ticker por empresa")
      } else {
            print("Empresas ainda com mais de 1 ticker:")
            arrange(finalt[duplicated(finalt$companyid),],companyid)
      }
      rm(non,on,pnpn,pnpna,pnpnb,i)
      
      #Tickers inclu�dos
      arrange(finalt, ticker)
      
      #Tickers exclu�dos
      cid[is.na(merge(cid, select(finalt, companyid, ticker),
            by.x = "ticker",
            by.y = "ticker",
            all.x = T,
            all.y = F)[,"companyid.y"]),]
      rm(cid)
      
      rtn <- merge(rtn, select(finalt,ticker,countryorigin),
                   by.x = "ticker",
                   by.y = "ticker",
                   all.x = F,
                   all.y = T)
      rm(cid,finalt)
      
#Verificar mais de 1 ticker por empresa:
rtn[duplicated(unique(select(rtn,ticker,companyid))[,"companyid"]),]
rtn <- select(rtn, -countryorigin)
rtn <- arrange(rtn,ticker,date)

#Gravar arquivos
#[dados](ownCloud\disserta��o\tidydata\"BRAnaofinanceiro.csv")
getwd()
write.csv2(rtn,"1_BRAnaofinanceiro.csv",row.names = F,na = "")

#Limpeza arquivo final
rtn <- read.csv2("D:\\$database\\ownCloud\\disserta��o\\data\\BRA\\1_BRAnaofinanceiro.csv", stringsAsFactors = F,na.strings = "")
rtn <- filter(rtn, ticker != "PTQS4")
rtn <- filter(rtn, ticker != "BIOM4")

library(data.table)
library(stringi)
x <- select(rtn, date, ticker, returnInf)
x$date <- as.Date(x$date)
x$notmissing <- !is.na(x$returnInf)
x$quarter <- stri_join(year(x$date),"Q",quarter(x$date))
x$aggregate <- stri_join(x$ticker,x$quarter,sep="_")

missreport <- aggregate(x$notmissing,by = list(x$aggregate), FUN = sum)
rm(x)
colnames(missreport) <- c("aggregate","obs")
missreport$freq <- missreport$obs/60
missreport <- filter(missreport, freq >= .5)
rtn$aggregate <- stri_join(rtn$ticker,
                           stri_join(year(rtn$date),"Q",quarter(rtn$date))
                           ,sep="_")
n <- nrow(rtn)
rtn <- merge(select(missreport,aggregate,freq), rtn,
             by.x = "aggregate",
             by.y = "aggregate",
             all.x = T,
             all.y = F)
n - nrow(rtn)
(n - nrow(rtn))/n
rtn <- select(rtn, -aggregate, -freq)
write.csv2(rtn,
           "D:\\$database\\ownCloud\\disserta��o\\data\\BRA\\1_BRAnaofinanceiro.csv",
           row.names = F,
           na = "")