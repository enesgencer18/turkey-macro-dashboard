Best.Lambdas.Y <- function(Y,
                          hp_lambda_1 = 10 ^ c(1:10),
                          hp_lambda_2 = 10 ^ c(1:10),
                          metric = "cor",
                          # cor or rss
                          ifPlot = T
){
  
Rolling_5Y = rollapplyr(
    data = Return.calculate(exp(Y)),
    width = 60,
    FUN = Return.annualized,
    scale = 12
  )
Rolling_1Y = rollapplyr(
    data = Return.calculate(exp(Y)),
    width = 6,
    FUN = Return.annualized,
    scale = 12
  ) 
    
min.RSS<-function(lambda_1, lambda_2){
    temp<-zoo(HP.Filter(Y,lambda_1),time(Y))
    Rolling_5Y_Trend = rollapplyr(
      data = Return.calculate(exp(temp)),
      width = 120,
      FUN = Return.annualized,
      scale = 12
    )
    temp.1<-na.omit(merge(temp,Rolling_5Y))
    rss.1 = sum((temp.1[,1]-temp.1[,2])^2)
    
    res = Y - temp
    cycle = zoo(HP.Filter(res, lambda_2), time(Y))
    Rolling_1Y_Cycle = rollapplyr(
      data = Return.calculate(exp(cycle)),
      width = 12,
      FUN = Return.annualized,
      scale = 12
    ) 
    temp.2 = na.omit(merge(cycle, Rolling_1Y))
    rss.2 = sum((temp.2[,1]-temp.2[,2])^2)
    
    return(rss.1 * rss.2)
    
  }
  
  neg.cor<-function(lambda_1, lambda_2){
    temp<-zoo(HP.Filter(Y,lambda_1),time(Y))
    Rolling_5Y_Trend = rollapplyr(
      data = Return.calculate(exp(temp)),
      width = 120,
      FUN = Return.annualized,
      scale = 12
    )
    temp.1<-na.omit(merge(temp,Rolling_5Y))
    cor.1 = -abs(cor(na.omit(merge(temp.1[,1],temp.1[,2])))[2,1])
    
    res = Y - temp
    cycle = zoo(HP.Filter(res, lambda_2), time(Y))
    Rolling_1Y_Cycle = rollapplyr(
      data = Return.calculate(exp(cycle)),
      width = 12,
      FUN = Return.annualized,
      scale = 12
    ) 
    temp.2 = na.omit(merge(cycle, Rolling_1Y))
    cor.2 = -abs(cor(na.omit(merge(temp.2[,1],temp.2[,2])))[2,1])
    return(-(cor.1 * cor.2))
  }
  
  if (metric=="cor"){
    obj<-neg.cor
  } else {
    obj<-min.RSS
  }
  
  mygrid<-expand.grid(hp_lambda_1,hp_lambda_2)
  all.rss<-sapply(1:nrow(mygrid),
                  function(i) obj(mygrid[i,1],mygrid[i,2]))
  lambda1.star <- mygrid[which.min(all.rss), 1]
  lambda2.star <- mygrid[which.min(all.rss), 2]
  
  Trend = zoo(HP.Filter(Y, as.numeric(lambda1.star)), time(Y))
  Res = Y - Trend
  Cycle = zoo(HP.Filter(Res, as.numeric(lambda2.star)), time(Y))
  Irregular = Y - Trend - Cycle
  
  
  
  final_dat <- merge(Y, Trend, Cycle, Irregular)

  
  list(best_lambda_1=lambda1.star,
       best_lambda_2=lambda2.star,
       final_dat=final_dat)}



