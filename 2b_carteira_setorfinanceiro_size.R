#Carteira do setor financeiro

#Leitura arquivo do setor financeiro
##[dados](ownCloud\dissertação\data\"BRAsetorfinanceiro.csv")
fin <- read.csv2(choose.files(), 
                 header = T, 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "", 
                 stringsAsFactors = F)
library(dplyr)
fin <- select(fin, ticker, date, closeInf, returnInf)
fin$date <- as.Date(fin$date, "%Y-%m-%d")
fin <- arrange(fin, ticker, date)
str(fin)

##Divisão do returnInf por 100 para mostrar em valores absolutos
fin$returnInf <- fin$returnInf/100


#Leitura dos dados do tamanho das empresas:
mkt <- read.csv2(choose.files(), 
                 header = T, 
                 sep = ";", 
                 dec = ",", 
                 na.strings = "", 
                 stringsAsFactors = F)
mkt <- select(mkt, date, ticker, outstanding)

#Junção das tabelas
library(stringi)
mkt$proc <- stri_join(mkt$ticker,mkt$date,sep="_")
fin$proc <- stri_join(fin$ticker,fin$date,sep="_")
fin <- merge(fin,select(mkt,proc,outstanding),
             by.x = "proc",
             by.y = "proc",
             all.x = T,
             all.y = F)
rm(mkt)

#Cálculo tamanho (size)
fin$size <- fin$closeInf*fin$outstanding

#Fill missings
fin <- arrange(fin, ticker, date)
library(zoo)
missings <- select(fin, ticker, size)
missings <- as.data.frame(missings %>% group_by(ticker) %>% na.locf %>% ungroup)
fin$size_nmiss <- as.numeric(missings[,"size"])
rm(missings)


#Portfolio, peso das ações por tamanho:
mkt <- select(fin, date,ticker,size_nmiss)
nm <- c("date",stri_join(unique(mkt$ticker)))
mat <- Reduce(function(...) full_join(...,by="date"),split(select(mkt,-ticker),mkt$ticker))
colnames(mat) <- nm ; rm(nm)
mat <- arrange(mat,date)

for (i in 2:75) {
      mat[,i] <- as.numeric(mat[,i])
} ; rm(i)

tot <- as.data.frame(cbind(mat$date,rowSums(mat[,-1],na.rm = T)),stringsAsFactors = F)
tot[,2] <- as.numeric(tot[,2])
tot[,1] <- as.Date(tot[,1])
colnames(tot) <- c("date","size_total")
tot <- arrange(tot,date)
rm(mat,mkt)

fin <- merge(fin, tot,
             by.x = "date",
             by.y = "date",
             all.x = T,
             all.y = F)
rm(tot)

#porcentagens carteira do setor
fin$weight <- fin$size_nmiss/fin$size_total
      #Return fill missings
      fin <- arrange(fin, ticker, date)
      missings <- select(fin, ticker, returnInf)
      missings <- as.data.frame(missings %>% group_by(ticker) %>% na.locf %>% ungroup)
      fin$returnInf <- as.numeric(missings[,"returnInf"])
      rm(missings)

fin$portrtn <- fin$weight*fin$returnInf
portfin <- Reduce(function(...) full_join(...,by="date"),
                        split(select(fin,date,portrtn),fin$ticker))
nm <- c("date",stri_join(unique(fin$ticker)))
colnames(portfin) <- nm ; rm(nm)
portfin <- arrange(portfin, date)
portfin$FINreturnInf <- rowSums(portfin[,-1], na.rm=T)

setwd("D:\\$database\\ownCloud\\dissertação\\data")
write.csv2(portfin,"2_BRAportfolioweigth_size[tickerlevel].csv",row.names = F,na = "")
finsector <- select(portfin, date, FINreturnInf)
write.csv2(finsector,"2_BRAfin_size.csv",row.names = F,na = "")

###Relatório: Gráfico retorno e desvio padrão carteira


#Scatterplot
windows()
par(mfrow = c(1, 1), mar = c(5, 4, 2, 1))
plot(FINreturnInf ~ date, finsector,
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "retorno ajustado pela inflação",
     main = c("Retorno diário - Carteira do setor financeiro (tamanho das empresas)"))
axis(1,finsector$date,format(as.yearqtr(finsector$date),"%Y/%q"),cex.axis=.7)
abline(h=mean(finsector$FINreturnInf), col="blue", lwd = 1.5)

#Boxplot
library(lubridate)
windows()
par(mfrow = c(1, 1))
boxplot(FINreturnInf ~ year(date), data = finsector,
        col = "red",
        xlab = "ano",
        ylab = "retorno ajustado pela inflação",
        main = c("Retorno diário - Carteira do setor financeiro - tamanho das empresas)")
)

