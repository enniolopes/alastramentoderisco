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

##Missing values do returnInf preenchidos com zero, considerando que a o preço de fechamento 
#não alterou depois da última cotação disponível o retorno é zero
####library(zoo)
####return0 <- select(fin, ticker, returnInf)
####return0 <- as.data.frame(return0 %>% group_by(ticker) %>% na.locf %>% ungroup)

      ###Relatório: existe missing value?
####      unique(return0[is.na(return0$returnInf),"ticker"])

####fin$returnInf <- as.numeric(return0[,"returnInf"])
####rm(return0)



#Matriz das médias dos retornos corrigidos pela inflação
##Carteira dinâmica diária
library(stringi)
library(lubridate)
library(quadprog)
fin$index <- stri_join(fin$date, fin$ticker, sep = "_")


#Matriz covariância e vetor da média, otimização da variância com restrição das proporções
      #de cada ação e sem possibilidade de 'Short'

      #Função portfolio minima variância global (package: portfolio.r, author: Eric Zivot)
      #(indisponível para a versão 3.3.1 do R)
      globalMin.portfolio <-
            function(er, cov.mat, shorts=TRUE)
            {
                  # Compute global minimum variance portfolio
                  #
                  # inputs:
                  # er			N x 1 vector of expected returns
                  # cov.mat		N x N return covariance matrix
                  # shorts          logical, allow shorts is TRUE
                  #
                  # output is portfolio object with the following elements
                  # call			original function call
                  # er				portfolio expected return
                  # sd				portfolio standard deviation
                  # weights		N x 1 vector of portfolio weights
                  call <- match.call()
                  
                  #
                  # check for valid inputs
                  #
                  asset.names <- names(er)
                  er <- as.vector(er)				# assign names if none exist
                  cov.mat <- as.matrix(cov.mat)
                  N <- length(er)
                  if(N != nrow(cov.mat))
                        stop("invalid inputs")
                  if(any(diag(chol(cov.mat)) <= 0))
                        stop("Covariance matrix not positive definite")
                  # remark: could use generalized inverse if cov.mat is positive semi-definite
                  
                  #
                  # compute global minimum portfolio
                  #
                  if(shorts==TRUE){
                        cov.mat.inv <- solve(cov.mat)
                        one.vec <- rep(1,N)
                        w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv)
                        w.gmin <- as.vector(w.gmin)
                  } else if(shorts==FALSE){
                        Dmat <- 2*cov.mat
                        dvec <- rep.int(0, N)
                        Amat <- cbind(rep(1,N), diag(1,N))
                        bvec <- c(1, rep(0,N))
                        result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
                        w.gmin <- round(result$solution, 6)
                  } else {
                        stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
                  }
                  
                  names(w.gmin) <- asset.names
                  er.gmin <- crossprod(w.gmin,er)
                  sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
                  gmin.port <- list("call" = call,
                                    "er" = as.vector(er.gmin),
                                    "sd" = as.vector(sd.gmin),
                                    "weights" = w.gmin)
                  class(gmin.port) <- "portfolio"
                  gmin.port
            }

library(corpcor)

      
      
#Vetor da média, matriz de covariância e otimização da carteira
      #Vetor de data da última cotação
      mcot1 <- as.data.frame(as.Date(sapply(split(fin[,"date"],fin$ticker),max),"1970-01-01"))
      mcot1 <- cbind(rownames(mcot1),mcot1)
      colnames(mcot1) <- c("ticker", "date")
      mcot2 <- as.data.frame(as.Date(sapply(split(fin[,"date"],fin$ticker),min),"1970-01-01"))
      mcot2 <- cbind(rownames(mcot2),mcot2)
      colnames(mcot2) <- c("ticker", "date")
      mcot <- full_join(x = mcot1,
                        y = mcot2,
                        by = "ticker")
      colnames(mcot) <- c("ticker","date.final","date.inicial")
      mcot$ticker <- as.character(mcot$ticker)
      rm(mcot1,mcot2)
dt <- as.data.frame(unique(filter(fin, date >= "1996-01-01")[,"date"]))
colnames(dt) <- "date"
dt <- arrange(dt,date)[,1]
dt <- as.Date(dt)


for (h in dt) {
      tkrfl <- filter(mcot, date.inicial <= as.Date(h,"1970-01-01"),
                      date.final >= as.Date(h,"1970-01-01"))
      mat <- filter(fin, date <= as.Date(h,"1970-01-01"))
      mat <- merge(x = select(tkrfl,ticker),
                   y = mat,
                   by.x = "ticker",
                   by.y = "ticker",
                   all.x = T,
                   all.y = F)
      mat <- select(mat, ticker,date,returnInf)
      nm <- c("date",stri_join(unique(mat$ticker)))
      mat <- Reduce(function(...) full_join(...,by="date"),split(select(mat,-ticker),mat$ticker))
      colnames(mat) <- nm ; rm(nm)
      mat <- arrange(mat,date)

      #Vetor média:
      mu <- colMeans(mat[,-1], na.rm = T)

      #Matriz covariância
      cov <- cov(mat[,-1],use="pairwise.complete.obs")
            #Computa cov igual a zero para os missing values
      for (n in (1:ncol(cov))) {
            cov[is.na(cov[,(n)]),(n)] <- 0
      } ; rm(n)

      #Teste matriz positiva definida
      if (is.positive.definite(as.matrix(cov))) {
            print(stri_join("Matriz covariância é positiva definida, index: ", as.Date(h,"1970-01-01")))
            } else {
                  for (i in (1:(ncol(mat)-1))) {
                        mat[is.na(mat[,(i+1)]),(i+1)] <- 0
                        } ; rm(i)
                  print(stri_join("Matriz covariância não é positiva definida, shrinkage estimates applied, index: ",as.Date(h,"1970-01-01")))
                  cov <- cov.shrink(mat[,-1])
            }
      rm(mat)
      
      #Portfolio mínima variância global
      if (!all(exists("weight"),exists("matFIN"))) {
            weight <- as.data.frame(NULL)
            matFIN <- as.data.frame(NULL)
      }
      if (all(exists("weight"),exists("matFIN"))) {
            temp <- globalMin.portfolio(er = mu,cov.mat = cov,shorts = F)
            
            #vetores de pesos
            port <- as.data.frame(temp[["weights"]])
            port$ticker <- rownames(port)
            port$index <- as.Date(h,"1970-01-01")
            port$index <- stri_join(port$index, port$ticker, sep = "_")
            weight <- rbind(select(port, -ticker),weight)
            
            #Vetores de média e variância
            musd <- as.data.frame(as.Date(h,"1970-01-01"))
            colnames(musd) <- "index"
            musd$mu <- temp[["er"]]
            musd$sd <- temp[["sd"]]
            matFIN <- rbind(musd,matFIN)
      }
      rm(port,musd,cov,mu,temp)
} ; rm(h, globalMin.portfolio, dt, mcot, tkrfl)
colnames(weight) <- c("wght","index")
      
      #Vetor de pesos de cada ativo: 'weight'
      #Vetor das médias e desvios padrões dos retornos da carteira: 'matFIN'

      #Teste dos vetores de pesos
      weight$date <- as.Date(strtrim(weight$index,width=10),"%Y-%m-%d")
      ctest <- aggregate(weight$wght,
                         by=list("dia"=weight$date),
                         FUN = sum)
      ctest$x <- round(ctest$x,4)
      if (all(ctest$x==1)) {
            print("Os pesos de todos os dias somam 1")
      } else {
            print(stri_join("Os pesos dos dias ",ctest[ctest$x!=1,"dia"],"; são diferentes de 1"))
      } ; rm(ctest)
matFIN$mu <- round(matFIN$mu,6)
matFIN$sd <- round(matFIN$sd,6)
plot(matFIN$mu,
     type = "l",
     xlab = "Dias",
     ylab = "Retorno Esperado",
     main = "Retorno - Carteira do Setor")


##Retornos diários da carteira do setor
fin <- merge(fin,
             select(weight,-date),
             by.x = "index",
             by.y = "index",
             all.x = T,
             all.y = T)
fin$wrtn <- fin$returnInf*fin$wght

      #check pesos (=1)
      cm <- aggregate(fin$wght,
                      by=list("dia"=fin$date),
                      FUN = sum,
                      na.rm=T)
      cm$x <- round(cm$x,4)
      if (all(cm$x==1)) {
            print("Os pesos de todos os dias somam 1")
      } else {
            print("Os pesos dos dias listados são diferentes de 1")
            print(c(cm[cm$x!=1,"dia"]))
      } ; rm(cm)

FINsector <- aggregate(fin$wrtn,
                       by=list("date"=fin$date),
                       FUN = sum,
                       na.rm=T)
colnames(FINsector) <- c("date","returnInf")

      ###Relatório: Gráfico retorno e desvio padrão carteira

#Scatterplot
windows()
par(mfrow = c(1, 1), mar = c(5, 4, 2, 1))
plot(returnInf ~ date, FINsector,
     xaxt = "n",
     type="l",
     xlab = "trimestre",
     ylab = "retorno ajustado pela inflação",
     main = c("Retorno diário - Carteira do setor financeiro (mínima variância)"))
axis(1,FINsector$date,format(as.yearqtr(FINsector$date),"%Y/%q"),cex.axis=.7)
abline(h=mean(FINsector$returnInf), col="blue", lwd = 1.5)

#Boxplot
par(mfrow = c(1, 1))
windows()
boxplot(returnInf ~ year(date), data = filter(FINsector,date>="1996-01-01"),
        col = "red",
        xlab = "ano",
        ylab = "retorno ajustado pela inflação",
        main = c("Retorno diário - Carteira do setor financeiro - mínima variância")
        )

weight <- merge(weight,
                unique(select(fin,ticker,index)),
                by.x = "index",
                by.y = "index",
                all.x = T,
                all.y = F)
weight <- select(weight,-index)
fin <- select(fin, -index,-year,-month)
rm(matFIN)

fin <- filter(fin, date >= "1996-01-01")
FINsector <- filter(FINsector, date >= "1996-01-01")


##Gravar arquivos
##[dados](".\ownCloud\dissertação\data\")
getwd()
write.csv2(FINsector,"2_BRAfin_min-var.csv",row.names = F,na = "")
write.csv2(weight,"2_BRAportfolioweigth_min-var.csv",row.names = F,na = "")
write.csv2(fin,"2_BRAportfolioweigth_min-var[tickerlevel].csv",row.names = F,na = "")