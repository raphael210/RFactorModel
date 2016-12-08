

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  backtesting with 'regression' method -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' build local database's regression tables
#' 
#' @examples 
#' build.lcdb.RegTables(FactorLists)
#' @export
build.lcdb.RegTables <- function(FactorLists){
  
  con <- db.local()
  endT <- dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
  endT <- intdate2r(endT)
  endT <- trday.offset(endT,by = months(-1))
  dates <- getRebDates(as.Date('2005-01-04'),endT,rebFreq = 'day')
  dates <- split(dates,cut(dates,'month'))
  for(i in 1:length(dates)){
    cat(paste(min(rdate2int(dates[[i]])),' to ',max(rdate2int(dates[[i]]))),'...\n')
    TS <- getTS(dates[[i]],indexID = 'EI000985')
    TS <- rmSuspend(TS)
    TSF <- getMultiFactor(TS,FactorLists)
    
    prd_lists <- list(d1=lubridate::days(1),
                     w1=lubridate::weeks(1),
                     w2=lubridate::weeks(2),
                     m1=months(1))
    for(j in 1:length(prd_lists)){
      TSFR <- getTSR(TSF,dure = prd_lists[[j]])
      re <- reg.TSFR(TSFR,regType='glm')
      if(j==1){
        fRtn <- re$fRtn
        res <- re$res
        RSquare <- re$RSquare
      }else{
        fRtn <- dplyr::left_join(fRtn,re$fRtn,by=c('date','fname'))
        res <- dplyr::left_join(res,re$res,by=c('date','stockID'))
        RSquare <- dplyr::left_join(RSquare,re$RSquare,by='date')
      }
    }
    colnames(fRtn) <- c('date','fname',paste(c("frtn","Tstat"),rep(names(prd_lists),each = 2),sep = '_'))
    colnames(res) <- c('date','stockID',paste('res',names(prd_lists),sep = '_'))
    colnames(RSquare) <- c('date',paste("RSquare",names(prd_lists),sep = '_'))
    
    
    if(i==1){
      dbWriteTable(con,'Reg_FactorRtn',transform(fRtn,date=rdate2int(date)),overwrite=T,append=F,row.names=F)
      dbWriteTable(con,'Reg_Residual',transform(res,date=rdate2int(date)),overwrite=T,append=F,row.names=F)
      dbWriteTable(con,'Reg_RSquare',transform(RSquare,date=rdate2int(date)),overwrite=T,append=F,row.names=F)
    }else{
      dbWriteTable(con,'Reg_FactorRtn',transform(fRtn,date=rdate2int(date)),overwrite=F,append=T,row.names=F)
      dbWriteTable(con,'Reg_Residual',transform(res,date=rdate2int(date)),overwrite=F,append=T,row.names=F)
      dbWriteTable(con,'Reg_RSquare',transform(RSquare,date=rdate2int(date)),overwrite=F,append=T,row.names=F)
    }
    
  }
  dbDisconnect(con)
  return('Done!')
}


#' update local database's regression tables
#' 
#' @examples 
#' build.lcdb.RegTables(FactorLists)
#' @export
update.lcdb.RegTables <- function(FactorLists){
  
  con <- db.local()
  begT <- dbGetQuery(con,"select max(date) from Reg_RSquare")[[1]]
  begT <- intdate2r(begT)
  endT <- dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
  endT <- intdate2r(endT)
  endT <- trday.offset(endT,by = months(-1))
  dates <- getRebDates(begT,endT,rebFreq = 'day')
  cat(paste(min(rdate2int(dates)),' to ',max(rdate2int(dates))),'...\n')
  TS <- getTS(dates,indexID = 'EI000985')
  TS <- rmSuspend(TS)
  TSF <- getMultiFactor(TS,FactorLists)
  
  prd_lists <- list(d1=lubridate::days(1),
                    w1=lubridate::weeks(1),
                    w2=lubridate::weeks(2),
                    m1=months(1))
  for(j in 1:length(prd_lists)){
    TSFR <- getTSR(TSF,dure = prd_lists[[j]])
    re <- reg.TSFR(TSFR,regType='glm')
    if(j==1){
      fRtn <- re$fRtn
      res <- re$res
      RSquare <- re$RSquare
    }else{
      fRtn <- dplyr::left_join(fRtn,re$fRtn,by=c('date','fname'))
      res <- dplyr::left_join(res,re$res,by=c('date','stockID'))
      RSquare <- dplyr::left_join(RSquare,re$RSquare,by='date')
    }
  }
  colnames(fRtn) <- c('date','fname',paste(c("frtn","Tstat"),rep(names(prd_lists),each = 2),sep = '_'))
  colnames(res) <- c('date','stockID',paste('res',names(prd_lists),sep = '_'))
  colnames(RSquare) <- c('date',paste("RSquare",names(prd_lists),sep = '_'))
  

  dbWriteTable(con,'Reg_FactorRtn',transform(fRtn,date=rdate2int(date)),overwrite=F,append=T,row.names=F)
  dbWriteTable(con,'Reg_Residual',transform(res,date=rdate2int(date)),overwrite=F,append=T,row.names=F)
  dbWriteTable(con,'Reg_RSquare',transform(RSquare,date=rdate2int(date)),overwrite=F,append=T,row.names=F)

  dbDisconnect(con)
  return('Done!')
}








#' regression_result
#'
#' Regression to the TSFR data, calculate factor return, residuals, and R squrare, etc. 
#' @name regression_result
#' @rdname regression_result
#' @aliases reg.TSFR
#' @param TSFR a \bold{TSFR} object.
#' @param regType the regress type,the default type is "glm".
#' @param glm_wgt
#' @param sectorAttr
#' @return reg.TSFR return a list, contains dataframes of frtn, residual and Rsquare
#' @export
#' @author Ruifei.yin
#' @examples
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-09-30'))
#' TS <- getTS(RebDates,indexID = 'EI000985')
#' TS <- rmSuspend(TS)
#' factorIDs <- c("F000002","F000006","F000008")
#' tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' factorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.NP_YOY",
#'                   factorPar=list(),
#'                   factorDir=1),
#'   factorStd="norm",factorNA = "median")
#' factorLists <- c(tmp,factorLists)
#' TSF <- getMultiFactor(TS,FactorLists = factorLists)
#' TSFR <- getTSR(TSF)
#' re <- reg.TSFR(TSFR)
reg.TSFR <- function(TSFR,regType=c('glm','lm'),glm_wgt=c("sqrtFV","res"),sectorAttr=defaultSectorAttr()){
  #ptm <- proc.time()
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  
  TS <- TSFR[,c('date','stockID')]
  # get sector factor
  TSS <- gf.sector(TS,sectorAttr = sectorAttr)
  TSS <- dplyr::select(TSS,-sector)
  TSFR <- merge.x(TSFR,TSS,by=c("date","stockID"))
  
  # regression
  if(glm_wgt=="sqrtFV"){
    TSw <- getTSF(TS,factorFun="gf_lcfs",factorPar=list(factorID='F000001'),factorStd = 'none',factorNA = "median")
    TSw$factorscore <- sqrt(TSw$factorscore)
    TSw <- dplyr::rename(TSw,glm_wgt=factorscore)
    TSFR <- merge(TSFR,TSw,by =c("date","stockID"))
  } else if(glm_wgt=="res"){
    
  }
  
  factorNames <- setdiff(names(TSFR),c("stockID","date","date_end","periodrtn","wgt","glm_wgt"))
  
  # loop of regression
  TSFR <- na.omit(TSFR)  # omit the NAs 
  dates <- dplyr::distinct(TSFR,date)
  fRtn <- data.frame()
  res <- data.frame()
  RSquare <- data.frame()
  for(i in 1:nrow(dates)){ 
    tmp.tsfr <- TSFR[TSFR$date==dates$date[i],]
    fml <- formula(paste("periodrtn ~ ", paste(factorNames,collapse= "+"),"-1",sep=''))
    if (regType=="glm"){
      tmp.w <- as.matrix(tmp.tsfr[,"glm_wgt"])
      lmm <- lm(fml,data = tmp.tsfr,weights = tmp.w)
    } else {
      lmm <- lm(fml,data = tmp.tsfr)
    }
    smry <- summary(lmm)
    fRtn <- rbind(fRtn,data.frame(date=dates$date[i],fname=rownames(smry$coefficients),frtn=smry$coefficients[,1],Tstat=smry$coefficients[,3]))
    res <- rbind(res,data.frame(date=dates$date[i],stockID=tmp.tsfr$stockID,res=smry$residuals))
    RSquare <- rbind(RSquare,data.frame(date=dates$date[i],RSquare=smry$r.squared))
    
    # # pure-factor-port wgt
    # tmp.x <- as.matrix(tmp.tsfr[,c(factorNames)])
    # tmp.w <- as.matrix(tmp.tsfr[,"glm_wgt"])
    # tmp.w <- diag(c(tmp.w),length(tmp.w))
    # tmp.f <- solve(crossprod(tmp.x,tmp.w) %*% tmp.x) %*% crossprod(tmp.x,tmp.w)
    # pfpwgt <- rbind(pfpwgt,data.frame(date=dates$date[i],stockID=tmp.tsfr$stockID,t(tmp.f)))
  }
  rownames(fRtn) <- NULL
  fRtn <- dplyr::filter(fRtn,!stringr::str_detect(fname,'ES'))
  re <- list(fRtn=fRtn,res=res,RSquare=RSquare)
  
  # tpassed <- proc.time()-ptm
  # tpassed <- tpassed[3]
  # cat("This function running time is ",tpassed/60,"min.\n")
  return(re)
}


#' @rdname regression_result
#' @aliases reg.TS
#' @param TS a \bold{TS} object.
#' @param factorLists a list of factor setting.
#' @param regType the regress type,the default type is "glm".
#' @param glm_wgt
#' @param sectorAttr
#' @param dure
#' @return a list, contains a \bold{TSFR} and regression result list.
#' @export
#' @examples 
#' RebDates <- getRebDates(as.Date('2015-01-31'),as.Date('2016-09-30'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' factorIDs <- c("F000002","F000006","F000008")
#' factorLists <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' re <- reg.TS(TS,factorLists)
reg.TS <- function(TS,factorLists,regType=c('glm','lm'),glm_wgt=c("sqrtFV","res"),
                   sectorAttr=defaultSectorAttr(),
                   dure=months(1)){
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  TSF <- getMultiFactor(TS,FactorLists = factorLists)
  TSFR <- getTSR(TSF,dure=dure)
  reg <- reg.TSFR(TSFR = TSFR,regType = regType,glm_wgt=glm_wgt,sectorAttr = sectorAttr)
  re <- list(TSFR=TSFR,reg=reg)
  return(re)
}



#' get factor's VIF 
#'
#' 
#' @author Andrew Dow
#' @param TS is a \bold{TS} object.
#' @param testfactorList is a factor list for test.
#' @param factorLists is a set of factors in use.
#' @param sectorAttr is sector attribution.
#' @return a VIF time series.
#' @examples 
#' RebDates <- getRebDates(as.Date('2012-12-31'),as.Date('2016-08-31'),rebFreq = 'month')
#' TS <- getTS(RebDates,'EI000985')
#' testfactorList <- buildFactorLists_lcfs('F000002',factorStd="norm",factorNA = "median")
#' factorIDs <- c("F000002","F000006","F000008")
#' factorLists <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' VIF <- factor.VIF(TS,testfactorList,factorLists)
#' @export
factor.VIF <- function(TS,testfactorList,factorLists,sectorAttr=defaultSectorAttr()){
  TSF <- getMultiFactor(TS,c(testfactorList,factorLists))
  TSS <- gf.sector(TS,sectorAttr = sectorAttr)
  TSS <- dplyr::select(TSS,-sector)
  TSF <- merge.x(TSF,TSS,by=c("date","stockID"))
  
  testfname <- sapply(testfactorList, '[[','factorName')
  fname <- c(sapply(factorLists, '[[','factorName'),setdiff(colnames(TSS),c('date','stockID','sector')))
  # loop of regression
  dates <- unique(TSF$date)
  VIF <- data.frame()
  for(i in 1:length(dates)){ 
    tmp.tsf <- TSF[TSF$date==dates[i],]
    fml <- formula(paste(testfname," ~ ", paste(fname, collapse= "+"),"-1",sep=''))
    lmm <- lm(fml,data = tmp.tsf)
    smry <- summary(lmm)
    VIF <- rbind(VIF,data.frame(date=dates[i],VIF=1/(1-smry$r.squared)))
  }
  return(VIF)
}








# ---------------------  ~~ Backtesting results --------------

tables.reg <- function(reg_results){
  
  # annrtn,annvol,sharpe,hitRatio,avg_T_sig
  # Rsquare 
  tmp <- re$frtn
  ddply(tmp,'fname',summarise,n=sum(abs(Tstat)>2)/length(Tstat))
}
charts.reg <- function(reg_results){
  # charts for each factor
}

MC.chart.reg <- function(reg_results){
  
}



#' getfRtn
#' 
#' get factor return data
#' @param TF a data frame contains date and factorNames
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param datasrc
#' @param reg_results
#' @param rollavg whether to get the rolling average factor return.
#' @param nwin rolling windows.
#' @return a factor return data frame.
#' @examples 
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-08-31'))
#' fNames <- c("mkt_cap_","PB_mrq_","pct_chg_per_60_","G_SCF_Q")
#' TF <- data.frame(date=rep(RebDates,each=length(fNames)),
#'                  fname=rep(fNames,length(RebDates)))
#' dure <- lubridate::days(1)
#' fRtn <- getfRtn(TF,dure)
#' dure <- months(1)
#' fRtn <- getfRtn(TF,dure)
#' @export
getfRtn <- function(TF,dure,datasrc=c('local','regResult'),rollavg=TRUE,nwin=250,reg_results){
  datasrc <- match.arg(datasrc)
  tmp.TF <- TF
  tmp.TF$date_from <- trday.offset(tmp.TF$date,dure*-1)
  if(dure==lubridate::days(1)){
    dbname <- 'frtn_d1'
  }else if(dure==lubridate::weeks(1)){
    dbname <- 'frtn_w1'
  }else if(dure==lubridate::weeks(2)){
    dbname <- 'frtn_w2'
  }else if(dure==months(1)){
    dbname <- 'frtn_m1'
  }
  
  
  if(datasrc=='local'){
    con <- db.local()
    if(rollavg==FALSE){
      qr <- paste("SELECT date,fname,",dbname," 'frtn'
                FROM Reg_FactorRtn")
      re <- dbGetQuery(con,qr)
      re$date <- intdate2r(re$date)
      re <- plyr::ddply(re,'fname',summarise,frtn=mean(frtn,trim = 0.025))
      suppressWarnings(re <- dplyr::left_join(TF,re,by='fname'))
      
    }else{
      tmp.TF$date_from_from <- trday.nearby(tmp.TF$date_from,nwin-1)
      tmp.TF <- tmp.TF %>% rowwise() %>% 
        do(data.frame(date_from=getRebDates(.$date_from_from, .$date_from,'day'),
                      date=rep(.$date,nwin),
                      fname=rep(.$fname,nwin)))
      tmp.TF <- transform(tmp.TF,date=rdate2int(date),date_from=rdate2int(date_from))
      dbWriteTable(con,name="yrf_tmp",value=tmp.TF,row.names = FALSE,overwrite = TRUE)
      qr <- paste("SELECT y.date,y.fname,",dbname," 'frtn'
                  FROM yrf_tmp y LEFT JOIN Reg_FactorRtn u
                  ON y.date_from=u.date and y.fname=u.fname")
      re <- dbGetQuery(con,qr)
      re_by <- dplyr::group_by(re,date,fname)
      re <- summarise(re_by, frtn = mean(frtn,na.rm = T))
      re$date <- intdate2r(re$date)
      suppressWarnings(re <- dplyr::left_join(TF,re,by=c('date','fname')))
    }
    dbDisconnect(con)
  }else if(datasrc=='regResult'){
    
  }
  
  return(re)
}



#' getResidual
#' 
#' get stocks' residual
#' @param TS a \bold{TS} object.
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param datasrc
#' @param reg_results
#' @examples 
#' RebDates <- getRebDates(as.Date('2012-01-31'),as.Date('2016-08-31'))
#' TS <- getTS(RebDates)
#' dure <- lubridate::days(1)
#' res <- getResidual(TS,dure)
#' dure <- months(1)
#' res <- getResidual(TS,dure)
#' @export
getResidual <- function(TS,dure,datasrc=c('local','regResult'),reg_results){
  datasrc <- match.arg(datasrc)
  TS$date_from <- trday.offset(TS$date,dure*-1)
  TS <- transform(TS,date=rdate2int(date),date_from=rdate2int(date_from))
  if(dure==lubridate::days(1)){
    dbname <- 'res_d1'
  }else if(dure==lubridate::weeks(1)){
    dbname <- 'res_w1'
  }else if(dure==lubridate::weeks(2)){
    dbname <- 'res_w2'
  }else if(dure==months(1)){
    dbname <- 'res_m1'
  }
  
  
  if(datasrc=='local'){
    con <- db.local()
    dbWriteTable(con,name="yrf_tmp",value=TS,row.names = FALSE,overwrite = TRUE)
    qr <- paste("SELECT y.date,y.stockID,",dbname," 'res'
                FROM yrf_tmp y LEFT JOIN Reg_Residual u
                ON y.date_from=u.date and y.stockID=u.stockID")
    re <- dbGetQuery(con,qr)
    re$date <- intdate2r(re$date)
    dbDisconnect(con)
  }else if(datasrc=='regResult'){
    
  }
  
  return(re)
}








#' calfCov
#'
#' calculate factor return covariance
#' @author Andrew Dow
#' @param TF a data frame contains date and factorNames
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param datasrc
#' @param reg_results
#' @param nwin rolling windows.
#' @return a data frame fCov.
#' @examples 
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-08-31'))
#' fNames <- c("mkt_cap_","PB_mrq_","pct_chg_per_60_","G_SCF_Q")
#' TF <- data.frame(date=rep(RebDates,each=length(fNames)),
#'                  fname=rep(fNames,length(RebDates)))
#' dure <- lubridate::days(1)
#' fCov <- calfCov(TF,dure)
#' @export
calfCov <- function(TF,dure,datasrc=c('local','regResult'),rollavg=TRUE,nwin=250,reg_results){
  datasrc <- match.arg(datasrc)
  tmp.TF <- TF
  tmp.TF$date_from <- trday.offset(tmp.TF$date,dure*-1)
  if(dure==lubridate::days(1)){
    dbname <- 'frtn_d1'
  }else if(dure==lubridate::weeks(1)){
    dbname <- 'frtn_w1'
  }else if(dure==lubridate::weeks(2)){
    dbname <- 'frtn_w2'
  }else if(dure==months(1)){
    dbname <- 'frtn_m1'
  }
  
  
  if(datasrc=='local'){
    con <- db.local()
    if(rollavg==FALSE){
      qr <- paste("SELECT date,fname,",dbname," 'frtn'
                FROM Reg_FactorRtn")
      re <- dbGetQuery(con,qr)
      re$date <- intdate2r(re$date)
      re <- re[re$fname %in% TF$fname,]
      re <- reshape2::dcast(re,date~fname,mean,value.var = 'frtn')
      fNames <- as.character(unique(TF$fname))
      re <- re[,c("date",fNames)]
      re <- data.frame(fname=fNames,cov(as.matrix(re[,fNames])))
      
      
      suppressWarnings(re <- dplyr::left_join(TF,re,by='fname'))
      re <- re[,c("date",fNames)]
      
      
    }else{
      tmp.TF$date_from_from <- trday.nearby(tmp.TF$date_from,nwin-1)
      tmp.TF <- tmp.TF %>% rowwise() %>% 
        do(data.frame(date_from=getRebDates(.$date_from_from, .$date_from,'day'),
                      date=rep(.$date,nwin),
                      fname=rep(.$fname,nwin)))
      tmp.TF <- transform(tmp.TF,date=rdate2int(date),date_from=rdate2int(date_from))
      dbWriteTable(con,name="yrf_tmp",value=tmp.TF,row.names = FALSE,overwrite = TRUE)
      qr <- paste("SELECT y.date_from,y.date,y.fname,",dbname," 'frtn'
                  FROM yrf_tmp y LEFT JOIN Reg_FactorRtn u
                  ON y.date_from=u.date and y.fname=u.fname")
      re <- dbGetQuery(con,qr)
      re <- transform(re,date_from=intdate2r(date_from),date=intdate2r(date))
      re <- reshape2::dcast(re,date_from+date~fname,value.var = 'frtn')
      re <- dplyr::arrange(re,date,date_from)
      fNames <- as.character(unique(TF$fname))
      re <- re[,c("date",fNames)]
      re <- plyr::ddply(re,'date',function(subre){
        as.data.frame(cov(as.matrix(subre[,fNames])))
      })
      
    }
    
    

    dbDisconnect(con)
  }else if(datasrc=='regResult'){
    
  }
  
  return(re)
}



#' calDelta
#'
#' calculate residual's Delta
#' @author Andrew Dow
#' @param reg_results is a factor return dataframe.
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param nwin is rolling window.
#' @param reg_results
#' @return delta.
#' @examples 
#' delta <- calDelta(TS,dure)
#' @export
calDelta <- function(TS,dure,datasrc=c('local','regResult'),reg_results,nwin=250){
  datasrc <- match.arg(datasrc)
  dates <- dplyr::distinct(TS,date)
  dates$date_tmp1 <- trday.offset(dates$date,dure*-1)
  if(dure==lubridate::days(1)){
    dbname <- 'res_d1'
  }else if(dure==lubridate::weeks(1)){
    dbname <- 'res_w1'
  }else if(dure==lubridate::weeks(2)){
    dbname <- 'res_w2'
  }else if(dure==months(1)){
    dbname <- 'res_m1'
  }
  
  
  if(datasrc=='local'){
    con <- db.local()
    dates$date_tmp2 <- trday.nearby(dates$date_tmp1,nwin-1)
    
    qr <- paste("SELECT date,stockID,",dbname," 'res'
                FROM Reg_Residual where date>=",rdate2int(min(dates$date_tmp2)),
                " and date<=",rdate2int(max(dates$date_tmp1)))
    data <- dbGetQuery(con,qr)
    dbDisconnect(con)
    data$date <- intdate2r(data$date)
    
    Delta <- data.frame()
    for(i in 1:nrow(dates)){
      tmp.data <- dplyr::filter(data,date>=dates$date_tmp2[i],date<=dates$date_tmp1[i])
      by_stock <- dplyr::group_by(tmp.data,stockID)
      tmp.delta <- dplyr::summarise(by_stock,
                                    n = n(),
                                    var = var(res))
      tmp.delta$date <- dates$date[i]
      Delta <- rbind(Delta,tmp.delta)
    }
    Delta <- dplyr::filter(Delta,n>=20)
    Delta <- Delta[,c('date','stockID','var')]
    Delta <- dplyr::left_join(TS,Delta,by = c("date", "stockID"))
    
  }else if(datasrc=='regResult'){
    
  }
  
  return(Delta)
}




biasTest <- function(reg_results,portID){
  Fcov_Delta <- calcFcov_Delta(reg_results)
  Fcov <- Fcov_Delta$Fcov
  Delta <- Fcov_Delta$Delta
  
  if(!missing(portID)){ # get port by a indexID
    
  } else { # equal-wgt-port
    
  }
  
}


# ---------------------  ~~ Get optimal port --------------






#' OptWgt
#'
#' optimize portfolio weight.
#' @author Andrew Dow
#' @param alphaf is alpha factors' name.
#' @param Fcov is the covariance matrix.
#' @param constr is optimization constraint,\bold{IndSty} means industry and style neutral,
#' \bold{IndStyTE} means besides industry and style neutral,tracking error also required.
#' @param benchmark is the benckmark for optimization.
#' @return a \bold{port} object.
#' @examples 
#' 
#' 
#' @export
OptWgt <- function(TSF,alphaf,fRtn,fCov,target=c('return','return-risk'),constr=c('IndSty','Ind','IndStyTE'),
                   benchmark='EI000905',indfexp=0.05,fexp,sectorAttr=defaultSectorAttr(),maxWgt=0.01,addEvent=TRUE){
  target <- match.arg(target)
  constr <- match.arg(constr) 
  
  fname <- setdiff(colnames(TSF),c('date','stockID'))
  dates <- unique(TSF$date)
  port <- data.frame()
  for(i in dates){
    cat(rdate2int(as.Date(i,origin = '1970-01-01')), "...\n")
    
    #get one period raw data
    tmp.TSF <- TSF[TSF$date==i,]
    tmp.TS <- tmp.TSF[,c('date','stockID')]
    tmp.TS <- rmSuspend(tmp.TS)
    if(addEvent==TRUE) tmp.TS <- rmNegativeEvents(tmp.TS)
    tmp.TSF <- tmp.TSF[tmp.TSF$stockID %in% tmp.TS$stockID,]
    
    
    tmp <- gf.sector(tmp.TSF[,c('date','stockID')],sectorAttr)
    tmp <- dplyr::select(tmp,-sector)
    tmp.TSF <- merge.x(tmp.TSF,tmp)
    if('date' %in% colnames(fRtn) ){
      tmp.fRtn <- fRtn[fRtn$date==i,-1]
    }else{
      tmp.fRtn <- fRtn
    }
    
    rownames(tmp.fRtn) <- tmp.fRtn$fname
    tmp.fCov <- fCov[fCov$date==i,-1]
    rownames(tmp.fCov) <- colnames(tmp.fCov)
    
    #get benchmark stock component weight and sector info. 
    benchmarkdata <- getIndexCompWgt(indexID = benchmark,i)
    tmp <- gf.sector(benchmarkdata[,c('date','stockID')],sectorAttr = sectorAttr)
    tmp <- tmp[,c('date','stockID','sector')]
    benchmarkdata <- merge(benchmarkdata,tmp,by=c('date','stockID'))
    totwgt <- plyr::ddply(benchmarkdata,'sector',plyr::summarise,secwgt=sum(wgt))
    totwgt$wgtlb <- totwgt$secwgt*(1-indfexp)
    totwgt$wgtub <- totwgt$secwgt*(1+indfexp)
    rownames(totwgt) <- totwgt$sector
    
    #deal with missing industry
    indfname <- colnames(tmp.TSF)[stringr::str_detect(colnames(tmp.TSF),'ES')]
    missind <- setdiff(indfname,totwgt$sector)
    if(length(missind)>0){
      for(j in 1:length(missind)){
        tmp.TSF <- tmp.TSF[tmp.TSF[,missind[j]]==0,]
        tmp.TSF <- tmp.TSF[,!colnames(tmp.TSF) %in% missind[j]]
      }
      indfname <- setdiff(indfname,missind)
    }
    totwgt <- totwgt[indfname,]
    
    # get risk matrix
    if(constr=='Ind'){
      #prepare matrix data
      riskmat <- as.matrix(tmp.TSF[,indfname])
      rownames(riskmat) <- tmp.TSF$stockID
      
    }else if(constr=='IndSty'){
      #get benchmark risk factor value
      benchmarkdata <- merge(benchmarkdata,TSF[,c('date','stockID',fname)],by=c('date','stockID'))
      benchmarkdata[is.na(benchmarkdata)] <- 0
      fwgt <- t(as.matrix(benchmarkdata$wgt)) %*% as.matrix(benchmarkdata[,fname])
      fwgt <- data.frame(sector=colnames(fwgt),secwgt=c(fwgt))
      colnames(fexp) <- c('sector','wgtlb','wgtub')
      fwgt <- merge(fwgt,fexp,by='sector')
      fwgt$wgtlb <- fwgt$secwgt+fwgt$wgtlb
      fwgt$wgtub <- fwgt$secwgt+fwgt$wgtub
      totwgt <- rbind(totwgt,fwgt)
      rownames(totwgt) <- totwgt$sector
      
      #prepare risk matrix data
      riskmat <- as.matrix(tmp.TSF[,c(indfname,fname)])
      rownames(riskmat) <- tmp.TSF$stockID
      totwgt <- totwgt[colnames(riskmat),]
      
    }
    
    
    #get alpha matrix
    alphamat <- as.matrix(tmp.TSF[,alphaf])
    rownames(alphamat) <- tmp.TSF$stockID
    dvec <- t(as.matrix(tmp.fRtn[alphaf,'frtn'])) %*% t(alphamat)
    
    
    if(target=='return-risk'){
      
      
      
      Fcovmat <- as.matrix(tmp.fCov[alphaf,alphaf])
      Dmat <- alphamat%*%Fcovmat%*%t(alphamat)
      tmp <- Matrix::nearPD(Dmat)
      Dmat <- tmp$mat
      Dmat <- matrix(Dmat,nrow = nrow(Dmat))
      
      Amat <- cbind(riskmat,-1*riskmat)
      nstock <- dim(Dmat)[1]
      Amat <- cbind(1,Amat,diag(x=1,nstock),diag(x=-1,nstock))#control weight
      bvec <- c(1,totwgt$wgtlb,-1*totwgt$wgtub,rep(0,nstock),rep(-0.01,nstock))
      system.time(res <- quadprog::solve.QP(Dmat,dvec,Amat,bvec,meq = 1))
      
      tmp <- data.frame(date=i,stockID=rownames(alphamat),wgt=res$solution)
      tmp <- tmp[tmp$wgt>0.0005,]
      colnames(tmp) <-c( "date","stockID","wgt")
      tmp <- transform(tmp,wgt=wgt/sum(wgt))
      
    }else{
      
      
      pspec <- portfolio.spec(assets=colnames(dvec))
      pspec <- add.constraint(portfolio=pspec, type="full_investment")
      pspec <- add.constraint(portfolio=pspec,type="box",min=0,max=maxWgt)
      
      pspec <- add.constraint(portfolio=pspec, type="factor_exposure",
                              B=riskmat,lower=totwgt$wgtlb, upper=totwgt$wgtub)
      pspec <- add.objective(portfolio=pspec,type='return',name='mean')
      dvec <- as.xts(dvec,order.by = as.Date(i,origin = '1970-01-01'))
      opt_maxret <- optimize.portfolio(R=dvec, portfolio=pspec,
                                       optimize_method="ROI",
                                       trace=TRUE)
      
      tmp <- data.frame(date=i,stockID=names(opt_maxret$weights),wgt=opt_maxret$weights)
      tmp <- tmp[tmp$wgt>0.0005,]
      tmp$wgt <- tmp$wgt/sum(tmp$wgt)
      
    }
    port <- rbind(port,tmp)
    
  }# for dates end
  
  port$date <- as.Date(port$date,origin = '1970-01-01')
  port$stockID <- as.character(port$stockID)
  return(port)
}


















# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  Performance & Risk Attribution -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' calculate factor exposure
#' 
#' @export
exposure.TSWF <- function(TSWF) {
  factorNames <- setdiff(names(TSWF),c("stockID","date","date_end","periodrtn","wgt","sector"))
  TSWF <- dplyr::select(TSWF,one_of(c("stockID","date","wgt",factorNames)))
  TSWF <- na.omit(TSWF)  # omit the NA value
  dates <- unique(TSWF$date)
  factorexp <- data.frame()
  for(i in 1:length(dates)){
    tmp <- TSWF[TSWF$date==dates[i],]
    fexp <- t(as.matrix(tmp[,'wgt'])) %*% as.matrix(tmp[,factorNames])
    factorexp <- rbind(factorexp,data.frame(date=dates[i],fexp))
  }
  return(factorexp)
}

exposure.port <- function(port,factorLists){
  
}



# ---------------------  ~~ Performance attribution --------------

#' getPAData
#' 
#' @export
#' @examples 
#' alphaLists <- buildFactorLists_lcfs(c("F000012","F000008"),factorStd="norm",factorNA = "median")
#' riskLists <- buildFactorLists_lcfs(c("F000002","F000006"),,factorStd="norm",factorNA = "median")
#' re <- getPAData(port,c(alphaLists,riskLists))
tables.PA <- function(port,factorLists,bmk,sectorAttr = defaultSectorAttr()){
  
  # get active wgt, if necessary
  if(!missing(bmk)){
    port <- getActivewgt(port = port,bmk = bmk,res = "active")
    port <- dplyr::rename(port,wgt=actwgt)
  }
  
  # calculate factor return 
  dates <- unique(port$date)
  TS <- getTS(dates,indexID = 'EI000985')   # get TSFR within rebDates==dates & univ==EI000985
  TSR <- getTSR(TS)
  TSFR <- getMultiFactor(TSR,factorLists)
  regdata <- reg.TSFR(TSFR = TSFR,regType = "lm",sectorAttr = sectorAttr )
  frtn <- regdata$frtn[,c("date","fname","frtn")]
  frtn <- reshape2::dcast(frtn,date~fname,value.var = 'frtn')
  frtn <- dplyr::arrange(frtn,date)

  # calculate factor exposure
  TSWF <- merge.x(port,TSFR,by=c('date','stockID'))
  TSWF <- gf.sector(TSWF,sectorAttr = sectorAttr)
  fexp <- exposure.TSWF(TSWF) 
  fexp <- dplyr::arrange(fexp,date)
  
  # calculate performance attribution
  portrtn <- plyr::ddply(TSWF,"date",plyr::summarise,rtn=sum(wgt*periodrtn, na.rm = TRUE))
  portrtn <- dplyr::arrange(portrtn,date)[-nrow(portrtn), ]
  portrtn_m <- as.matrix(portrtn[, -1])
  frtn <- dplyr::select(frtn,one_of(colnames(fexp))) # make the order of cols same with fexp
  frtn_m <- as.matrix(frtn[, -1])
  fexp_m <- as.matrix(fexp[-nrow(fexp), -1])
  fattr_m <- frtn_m*fexp_m
  res_m <- portrtn_m - as.matrix(rowSums(fattr_m))
  perfattr <- data.frame(date=portrtn$date,fattr_m,res=res_m)
  
  return(list(frtn=frtn,fexp=fexp,perfattr=perfattr,portrtn=portrtn))
}



charts.PA <- function(PA_tables,riskfnames){
  
}


# ---------------------  ~~ Risk attribution --------------
tables.RA <- function(port,factorLists,bmk,sectorAttr = defaultSectorAttr()){
  
}

charts.RA <- function(RA_tables){
  
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  utility functions ------------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



#' get factorscore of sector
#' 
#' @export
gf.sector <- function(TS, sectorAttr) {
  TSS <- getSectorID(TS,sectorAttr = sectorAttr)
  TSS <- sectorNA_fill(TSS,sectorAttr=sectorAttr)
  re <- reshape2::dcast(TSS,date+stockID~sector,length,fill=0,value.var = 'sector')
  re <- merge.x(TSS,re,by = c("date","stockID"))
  return(re)
}


#' get active wgt
#' 
#' @export
getActivewgt <- function(port,bmk,res=c("all","active")) {
  res <- match.arg(res)
  benchdata <- getIndexCompWgt(indexID = bmk,endT = unique(port$date))
  colnames(benchdata) <- c('date','stockID','benchwgt')
  # colnames(port) <- c('date','stockID','portwgt')
  port <- renameCol(port,"wgt","portwgt")
  port <- merge(benchdata,port,by=c('date','stockID'),all=TRUE)
  port[is.na(port$portwgt),'portwgt'] <- 0
  port[is.na(port$benchwgt),'benchwgt'] <- 0
  port$actwgt <- port$portwgt-port$benchwgt
  if(res=="active"){
    re <-  port[,c("date","stockID","actwgt")]
  } else {
    re <- port
  }
  return(re)
}


