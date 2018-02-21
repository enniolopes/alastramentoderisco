##    Limpeza base dos dados de cotações do Setor Financeiro  ##



##Leitura dos dados de cotação do setor financeiro:
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

      ###Relatório: resumo dos dados
      str(rtn)

##Seleciona apenas ações ordinárias e preferenciais. Portanto é realizado o corte de BDRs, ETFs,
##units, ações nominativas, fracionadas, mercado de balcão, e outros tipos.

      ###Relatório: tickers excluídos
      unique(select(filter(rtn, assettype != "Stock"),ticker,class,origcurrency,active))
rtn <- filter(rtn, assettype == "Stock")



##Primeiro é realizado uma pre seleção das observações com a premissa de que data final das 
#observações para um determinado ticker é a data da última cotação do preço de fechamento disponível

for (t in unique(rtn$ticker)) {
      a <- filter(rtn, ticker == t)
      a <- max(a[!is.na(a$close),"date"])
      temp <- filter(rtn, ticker == t, date <=a)
      rtn <- filter(rtn, ticker != t)
      rtn <- rbind(rtn, temp)
      rm(temp,a)
}



##Drop de quantidade de observações:
library(data.table)
library(stringi)

##Data frame com a relação dos tickers e anos
rtn$procindex <- stri_join(rtn$ticker, year(rtn$date), month(rtn$date), sep="_")
rtn$nonmissing <- !is.na(rtn$close)
du <- as.data.frame(aggregate(rtn$nonmissing,
                by=list("procindex"=rtn$procindex), 
                FUN=sum))
##Relação do inicio da série das observações:
ds <- as.data.frame(aggregate(rtn$date,
                        by=list("ticker"=rtn$ticker), 
                        FUN=min))
      #Função para determinar a data do final do mês:
      library(timeDate)

ds$d <- as.Date(alignMonthly(
      as.Date(stri_join(year(ds$x),
                        "-",
                        month(ds$x),
                        "-10"),
              format="%Y-%m-%d")))



#Registra a quantidade de dias no mês de início da cotação da série ('ipoy')
du$ipoy <- as.numeric(rep(NA,nrow(du)))
colnames(du) <- c("procindex","nobs","ipoy")
for (i in unique(rtn$ticker)) {
      a <- filter(ds, ticker == i)[,"d"]
      du[du$procindex == stri_join(i,year(a),month(a),sep="_"),"ipoy"] <- 
            nrow(filter(rtn, ticker == i, date <= a))
} ; rm(a,i,ds)

##Relação dos retornos discretos:
#Preenchimento dos missing de dados de fechamento com a última observação disponível
rtn <- arrange(rtn, ticker, date)
library(zoo)
close0 <- select(rtn, ticker, close)
close0 <- as.data.frame(close0 %>% group_by(ticker) %>% na.locf %>% ungroup)
rtn$close0 <- as.numeric(close0[,"close"])
rm(close0)

#Cálculo dos retornos a partir dos fechamentos sem missing values
#(é considerado que os missing values ocorreram pois não houve cotação no dia, portanto o valor 
#da ação é o mesmo da última cotação)
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

#Observações de retornos diferentes de zero
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
      #vARIÁVEIS:
      #nobs: quantidade de observações no mês sem missing value
      #ipoy: quantidade total de observações no mês de inicio da série
      #rt0: quantidade de retornos diferentes de zero

#Variável dummy dos meses a serem excluídos dos tickers
      #Critérios para exclusão:
      #1. Quantidade de observações menor que 2/3 dos dias úteis do mês (mês com 20 du);
      #2. Metade dos retornos da série iguais a zero.
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
      #Relatório dos tickers e ano que serão excluídos e gráfico da frequência das observações:
      filter(du,sel==0)
      hist(filter(du,sel==1)[,"nobs"],
           col = "red",
           main = "Frequência total observações por ano",
           xlab = "Observações por ano do preço de fechamento",
           ylab = "Frequência das observações"
      )
#Limpeza das observações com baixa quantidade de observações
n <- nrow(rtn)
rtn <- merge(select(filter(du,sel==1),procindex),
             rtn,
             by.x = "procindex",
             by.y = "procindex",
             all.x = T,
             all.y = F)

      ###Relatório: Quantidade de observações excluídas:
      print(n - nrow(rtn))
      print(1-(nrow(rtn)/n))*100 #em porcentagem
rm(n,du)



## Limpar ticker que contém menos de 12 meses de observações (anteriores a 2015)
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

      ###Relatório: Ticker a serem excluídos:
      filter(s, months <= 12, stdate < "2015-01-01")[,"ticker"]
      n <- nrow(rtn)
      
for (i in unique(filter(s, months < 12, stdate < "2015-01-01")[,"ticker"])) {
rtn <- filter(rtn, ticker != i)      
} ; rm(i)
s <- filter(s, months >= 12 | stdate >= "2015-01-01")
      ###Relatório: total de observações excluídas
      print(n - nrow(rtn)) ; rm(n)
      s



      ###Relatório: Quantidade de observações por ticker:
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
#[dados](ownCloud\dissertação\tidydata\"BRAsetorfinanceiro.csv")
getwd()
write.csv2(rtn,"BRAsetorfinanceiro.csv",row.names = F,na = "")