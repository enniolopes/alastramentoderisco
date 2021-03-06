##    Limpeza base dos dados de cota��es do Setor Financeiro  ##



##Leitura dos dados de cota��o do setor financeiro:
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
      print(1-(nrow(rtn)/n))*100 #em porcentagem
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

#Gravar arquivos
#[dados](ownCloud\disserta��o\tidydata\"BRAsetorfinanceiro.csv")
getwd()
write.csv2(rtn,"BRAsetorfinanceiro.csv",row.names = F,na = "")