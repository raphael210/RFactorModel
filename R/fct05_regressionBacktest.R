

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
#' @param secRtnOut whether output sector's return,default value is FALSE.
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
reg.TSFR <- function(TSFR,regType=c('glm','lm'),glm_wgt=c("sqrtFV","res"),sectorAttr=defaultSectorAttr(),secRtnOut=FALSE){
  #ptm <- proc.time()
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  TSFRold <- TSFR
  
  TS <- TSFR[,c('date','stockID')]
  # get sector factor
  TSS <- gf.sector(TS,sectorAttr = sectorAttr)
  nsector <- length(unique(TSS$sector))
  if(nsector>1){
    TSS <- dplyr::select(TSS,-sector)
    TSFR <- merge.x(TSFR,TSS,by=c("date","stockID"))
  }

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
    if(nsector>1){
      fml <- formula(paste("periodrtn ~ ", paste(factorNames,collapse= "+"),"-1",sep=''))
    }else{
      fml <- formula(paste("periodrtn ~ ", paste(factorNames,collapse= "+"),sep=''))
    }
  
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
  fRtn <- fRtn[fRtn$fname!='(Intercept)',]
  if(secRtnOut==FALSE){
    fRtn <- dplyr::filter(fRtn,!stringr::str_detect(fname,'ES'))
  }
  re <- list(TSFR=TSFRold,fRtn=fRtn,res=res,RSquare=RSquare)
  
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
#' @param secRtnOut whether output sector's return,default value is FALSE.
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
                   dure=months(1),secRtnOut=FALSE){
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  TSF <- getMultiFactor(TS,FactorLists = factorLists)
  TSFR <- getTSR(TSF,dure=dure)
  reg <- reg.TSFR(TSFR = TSFR,regType = regType,glm_wgt=glm_wgt,sectorAttr = sectorAttr)
  re <- list(TSFR=TSFR,fRtn=reg$fRtn,res=reg$res,RSquare=reg$RSquare)
  return(re)
}



#' get factor's VIF 
#'
#' 
#' @author Andrew Dow
#' @param TS is a \bold{TS} object.
#' @param testfactorList is a factor list for test.
#' @param factorLists is a set of factors in use.
#' @param sectorAttr is sector.
#' @param TSF is \bold{TSF} object.
#' @return a VIF time series.
#' @examples 
#' RebDates <- getRebDates(as.Date('2012-12-31'),as.Date('2016-08-31'),rebFreq = 'month')
#' TS <- getTS(RebDates,'EI000985')
#' testfactorList <- buildFactorLists_lcfs('F000002',factorStd="norm",factorNA = "median")
#' factorIDs <- c("F000002","F000006","F000008")
#' factorLists <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' VIF <- factor.VIF(TS,testfactorList,factorLists)
#' VIF <- factor.VIF(TSF=TSF)
#' @export
factor.VIF <- function(TS,testfactorList,factorLists,sectorAttr=defaultSectorAttr(),TSF){
  if(missing(TSF)){
    TSS <- gf.sector(TS,sectorAttr = sectorAttr)
    TSS <- dplyr::select(TSS,-sector)
    TSF <- getMultiFactor(TS,c(testfactorList,factorLists))
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
  }else{
    TS <- TSF[,c('date','stockID')]
    TSS <- gf.sector(TS,sectorAttr = sectorAttr)
    TSS <- dplyr::select(TSS,-sector)
    fname <- setdiff(colnames(TSF),c('date','stockID'))
    TSF <- merge.x(TSF,TSS,by=c("date","stockID"))
    indname <- setdiff(colnames(TSF),c(fname,c('date','stockID')))
    # loop of regression
    dates <- unique(TSF$date)
    VIF <- data.frame()
    for(i in 1:length(dates)){ 
      tmp.tsf <- TSF[TSF$date==dates[i],]
      for(j in fname){
        tmp <- c(indname,setdiff(fname,j))
        fml <- formula(paste(j," ~ ", paste(tmp, collapse= "+"),"-1",sep=''))
        lmm <- lm(fml,data = tmp.tsf)
        smry <- summary(lmm)
        VIF <- rbind(VIF,data.frame(date=dates[i],factorName=j,VIF=1/(1-smry$r.squared)))
      }
   
    }
  }

  return(VIF)
}








# ---------------------  ~~ Backtesting results --------------

#' regression_result_summary
#'
#' @param reg_results is regression_result
#' @name regression_result_summary
#' @seealso \link{reg.TSFR}
NULL


#' @rdname regression_result_summary
#' 
#' @export
table.reg.rsquare <- function(reg_results){
  # Rsquare 
  RSquare <- reg_results$RSquare
  re <- round(summary(RSquare$RSquare),3)
  re <- data.frame(cbind(begT=min(RSquare$date),
                   endT=max(RSquare$date),
                   NPeriod=nrow(RSquare),
                   t(re)))
  re <- transform(re,begT=as.Date(begT,origin='1970-01-01'),
                  endT=as.Date(endT,origin='1970-01-01'))
  colnames(re) <- c("begT","endT","NPeriod","Min","Qu.1st","Median","Mean","Qu.3rd","Max")
  return(re)
}


#' @rdname regression_result_summary
#' 
#' @export
table.reg.fRtn <- function(reg_results){
  # annrtn,annvol,sharpe,hitRatio,avg_T_sig
  fRtn <- reg_results$fRtn
  
  tstat <- summarise(group_by(fRtn,fname),avgT=mean(abs(Tstat)),
                     TPer=sum(abs(Tstat)>2)/length(Tstat))
  colnames(tstat) <- c("fname","mean(abs(T))","percent abs(T)>2")
  tstat$fname <- as.character(tstat$fname)
  
  fRtn <- reshape2::dcast(fRtn,date~fname,value.var = 'frtn')
  fRtn <- xts::xts(fRtn[,-1],fRtn[,1])
  rtnsum <- t(rtn.summary(fRtn))
  rtnsum <- data.frame(fname=rownames(rtnsum),rtnsum)
  rownames(rtnsum) <- NULL
  colnames(rtnsum) <- c("fname","Annual Return","Annual StdDev","Sharpe","HitRatio","Worst Drawdown" )
  rtnsum$fname <- as.character(rtnsum$fname)
  
  TSF <- reg_results$TSFR
  TSF <- dplyr::select(TSF,-date_end,-periodrtn)
  VIF <- factor.VIF(TSF=TSF)
  VIF <- summarise(group_by(VIF,factorName),VIF=mean(VIF))
  VIF <- dplyr::rename(VIF,fname=factorName)
  VIF$fname <- as.character(VIF$fname)
  
  re <- left_join(rtnsum,tstat,by='fname')
  re <- left_join(re,VIF,by='fname')
  return(re)
}


#' @rdname regression_result_summary
#' 
#' @export
chart.reg.fRtnWealthIndex <- function(reg_results,facet=FALSE){
  # charts for each factor
  fRtn <- reg_results$fRtn
  fRtn <- reshape2::dcast(fRtn,date~fname,value.var = 'frtn')
  fRtn <- xts::xts(fRtn[,-1],fRtn[,1])
  if(facet==FALSE){
    ggplot.WealthIndex(fRtn)
  }else{
    N <- floor(sqrt(ncol(fRtn)))
    fRtn <- WealthIndex(fRtn)
    fRtn <- melt.ts(fRtn)
    ggplot(fRtn, aes(x=time, y=value)) +ggtitle('wealth index')+
      geom_line(size=1,colour = "red")+facet_wrap( ~ variable,ncol = N)
  }
}


#' @rdname regression_result_summary
#' 
#' @export
chart.reg.fRtnBar <- function(reg_results){
  # charts for each factor
  fRtn <- reg_results$fRtn
  N <- floor(sqrt(length(unique(fRtn$fname))))
  ggplot(fRtn, aes(x=date, y=frtn)) +ggtitle('factor return')+
    geom_bar(position="dodge",stat="identity")+facet_wrap( ~ fname,ncol = N)
}


#' @rdname regression_result_summary
#' 
#' @export
chart.reg.rsquare <- function(reg_results){
  RSquare <- reg_results$RSquare
  RSquare <- xts::xts(RSquare[,-1],RSquare[,1])
  colnames(RSquare) <- c('RSquare')
  tmp <- zoo::rollmean(RSquare,12,align='right')
  tmp <- data.frame(date=zoo::index(tmp),RSquareMA=zoo::coredata(tmp))
  RSquare <- data.frame(time=time(RSquare),zoo::coredata(RSquare))
  ggplot(RSquare, aes(x=time, y=RSquare))+geom_line(color="#D55E00") +
      ggtitle('RSquare(with MA series)') +geom_line(data=tmp,aes(x=date,y=RSquare),size=1,color="#56B4E9")
  
}


#' @rdname regression_result_summary
#' 
#' @export
MC.chart.regCorr <- function(reg_results){
  fRtn <- reg_results$fRtn

  fRtn <- reshape2::dcast(fRtn,date~fname,value.var = 'frtn')
  fRtn <- as.matrix(fRtn[,-1])
  fRtn.cor <- cor(fRtn)
  corrplot::corrplot(fRtn.cor,method = 'number')

}


#' raw_factor_correlation
#' 
#' @name raw_factor_correlation
#' @param TSF is a \bold{TSF} object.
#' @param Nbin the number of the groups the timespan is cut to when plotting the scatter by time series.It could also be a character of interval specification,See \code{\link{cut.Date}} for detail. The default value is "day",which means no cutting, the scatters of every date are ploted.
#' @examples
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-09-30'))
#' TS <- getTS(RebDates,indexID = 'EI000985')
#' factorIDs <- c("F000006","F000008","F000012")
#' FactorLists <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "mean")
#' TSF <- getMultiFactor(TS,FactorLists = FactorLists)
#' MC.chart.fCorr(TSF)
#' MC.chart.fCorr(TSF,Nbin='year')
#' @export
MC.chart.fCorr <- function(TSF,Nbin){
  
  fnames <- setdiff(colnames(TSF),c('date','stockID','date_end','periodrtn'))
  TSF_by <- dplyr::group_by(TSF[,c('date',fnames)],date)
  
  cordata <- TSF_by %>% do(cormat = cor(.[,fnames],method='spearman'))
  cordata <- cordata %>% do(data.frame(date=.$date,fname=fnames,.$cormat))
  cordata <- reshape2::melt(cordata,id=c('date','fname'),
                        variable.name='fnamecor',factorsAsStrings=F)
  cordata <- transform(cordata,fname=as.character(fname),
                       fnamecor=as.character(fnamecor))
  
  subfun <- function(df){
    df <- dplyr::arrange(df,fname,fnamecor)
    df <- reshape2::dcast(df,fname~fnamecor)
    rownames(df) <- df$fname
    df <- as.matrix(df[,-1])
    df[upper.tri(df)] <- NA
    df <- reshape2::melt(df, na.rm = TRUE)
    colnames(df) <- c("fname","fnamecor",'value')
    return(df)
  }
  
  if(missing(Nbin)){
    cordata_by <- dplyr::group_by(cordata,fname,fnamecor)
    cordata_by <- dplyr::summarise(cordata_by,value=round(mean(value,trim=0.05),2))
    cordata_by <- subfun(cordata_by)

    ggplot(data=cordata_by,aes(fname,fnamecor,fill=value))+geom_tile()+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white")+
      geom_text(aes(fname,fnamecor, label = value), color = "black")+
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))
  }else{
    cordata$date <- cut.Date2(cordata$date,Nbin)
    N <- length(unique(cordata$date))
    N <- floor(sqrt(N))
    cordata_by <- dplyr::group_by(cordata,date,fname,fnamecor)
    cordata_by <- dplyr::summarise(cordata_by,value=round(mean(value,trim=0.05),2))
    cordata_by <- split(cordata_by[,-1],cordata_by$date)
    cordata_by <- plyr::ldply(cordata_by,subfun,.id = 'date')
    
    cordata_by$value <- round(cordata_by$value,2)
    ggplot(data=cordata_by,aes(fname,fnamecor,fill=value))+geom_tile()+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white")+
      geom_text(aes(fname,fnamecor, label = value), color = "black")+facet_wrap(~ date,ncol=N)+
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))
  }
  
}


#' MC.table.fCorr
#' 
#' @rdname raw_factor_correlation
#' @examples 
#' #-------------------------------------------------------#
#' MC.table.fCorr(TSF)
#' MC.table.fCorr(TSF,Nbin='year')
#' @export
MC.table.fCorr <- function(TSF,Nbin){
  
  fnames <- setdiff(colnames(TSF),c('date','stockID','date_end','periodrtn'))
  TSF_by <- dplyr::group_by(TSF[,c('date',fnames)],date)
  
  cordata <- TSF_by %>% do(cormat = cor(.[,fnames],method='spearman'))
  cordata <- cordata %>% do(data.frame(date=.$date,fname=fnames,.$cormat))
  cordata <- reshape2::melt(cordata,id=c('date','fname'),
                            variable.name='fnamecor',factorsAsStrings=F)
  cordata <- transform(cordata,fname=as.character(fname),
                       fnamecor=as.character(fnamecor))
  
  if(missing(Nbin)){
    cordata_by <- dplyr::group_by(cordata,fname,fnamecor)
    cordata_by <- dplyr::summarise(cordata_by,value=round(mean(value,trim=0.05),2))
    cordata_by <- dplyr::arrange(cordata_by,fname,fnamecor)
    cordata_by <- reshape2::dcast(cordata_by,fname~fnamecor)
    rownames(cordata_by) <- cordata_by$fname
    cordata_by <- as.matrix(cordata_by[,-1])
  }else{
    cordata$tmp <- cut.Date2(cordata$date,Nbin)
    cordata_by <- dplyr::group_by(cordata,tmp,fname,fnamecor)
    cordata_by <- dplyr::summarise(cordata_by,value=round(mean(value,trim=0.05),2))
    cordata_by <- dplyr::arrange(cordata_by,tmp,fname,fnamecor)
    cordata_by <- reshape2::dcast(cordata_by,tmp+fname~fnamecor)
    cordata_by <- split(cordata_by[,-1],unique(cordata_by$tmp))
    cordata_by <- plyr::llply(cordata_by,function(df){
      rownames(df) <- df$fname
      df <- as.matrix(df[,-1])
      return(df)
    })
  }
  return(cordata_by)
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



getfRtnNEW <- function(dates,factorNames,dure,rollavg=TRUE,reg_results){
  
  TF <- data.frame(date=rep(dates,each=length(factorNames)),
                   fname=rep(factorNames,length(dates)))
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
  
  
  if(missing(reg_results)){
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
  }else{
    
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

#' calculate port exposure
#' 
#' @export
exposure.port <- function(port,factorLists,sectorAttr = defaultSectorAttr()){
  dates <- unique(port$date)
  TS <- getTS(dates,indexID = 'EI801003')   # get TSFR within rebDates==dates & univ==EI000985
  TSF <- getMultiFactor(TS,factorLists)
  TSWF <- merge.x(port,TSF,by=c('date','stockID'))
  TSWF <- na.omit(TSWF)
  if(sectorAttr!=NULL){
    TSWF <- gf.sector(TSWF,sectorAttr = sectorAttr)
  }
  
  fexp <- exposure.TSWF(TSWF) 
  fexp <- dplyr::arrange(fexp,date)
  return(fexp)
}



# ---------------------  ~~ Performance attribution --------------

#' getPAData
#' 
#' @export
#' @examples 
#' tmp <- buildFactorLists(buildFactorList(factorFun="gf.NP_YOY",
#'                 factorPar=list(),factorDir=1),factorStd="norm",factorNA = "median")
#' alphaLists <- buildFactorLists_lcfs(c("F000012","F000008"),factorStd="norm",factorNA = "median")
#' alphaLists <- c(tmp,alphaLists)
#' riskLists <- buildFactorLists_lcfs(c("F000002","F000006"),factorStd="norm",factorNA = "median")
#' PA_tables <- getPAData(port,c(alphaLists,riskLists))
#' PA_tables <- getPAData(port,c(alphaLists,riskLists),bmk='EI000905')
getPAData <- function(port,factorLists,bmk,sectorAttr = defaultSectorAttr()){
  
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
  regdata <- reg.TSFR(TSFR = TSFR,regType = "glm",sectorAttr = sectorAttr,secRtnOut = T)
  frtn <- regdata$fRtn[,c("date","fname","frtn")]
  frtn <- reshape2::dcast(frtn,date~fname,value.var = 'frtn')
  frtn <- dplyr::arrange(frtn,date)

  # calculate factor exposure
  TSWF <- merge.x(port,TSFR,by=c('date','stockID'))
  TSWF <- na.omit(TSWF)
  TSWF <- gf.sector(TSWF,sectorAttr = sectorAttr)
  fexp <- exposure.TSWF(TSWF) 
  fexp <- dplyr::arrange(fexp,date)
  
  # calculate performance attribution
  if(!missing(bmk)){
    rtn.short <- unique(TSWF[,c('date','date_end')])
    rtn.short <- getPeriodrtn_EI(stockID=bmk,begT=rtn.short$date, endT=rtn.short$date_end)
    rtn.short <- dplyr::rename(rtn.short,date=begT,date_end=endT,bmkrtn=periodrtn)
    rtn.short <- rtn.short[,c( "date","date_end","bmkrtn")]
    TSWF <- merge.x(TSWF,rtn.short)
    TSWF$periodrtn <- TSWF$periodrtn-TSWF$bmkrtn
    portrtn <- plyr::ddply(TSWF,"date",plyr::summarise,rtn=sum(wgt*periodrtn, na.rm = TRUE))
  }else(
    portrtn <- plyr::ddply(TSWF,"date",plyr::summarise,rtn=sum(wgt*periodrtn, na.rm = TRUE))
    
  )
  #portrtn <- dplyr::arrange(portrtn,date)[-nrow(portrtn), ]
  portrtn <- dplyr::arrange(portrtn,date)
  portrtn_m <- as.matrix(portrtn[, -1])
  frtn <- dplyr::select(frtn,one_of(colnames(fexp))) # make the order of cols same with fexp
  frtn_m <- as.matrix(frtn[, -1])
  #fexp_m <- as.matrix(fexp[-nrow(fexp), -1])
  fexp_m <- as.matrix(fexp[, -1])
  fattr_m <- frtn_m*fexp_m
  res_m <- portrtn_m - as.matrix(rowSums(fattr_m))
  perfattr <- data.frame(date=portrtn$date,fattr_m,res=res_m)
  
  return(list(frtn=frtn,fexp=fexp,perfattr=perfattr,portrtn=portrtn))
}



#' chart.PA.exposure
#' 
#' @export
#' @examples 
#' riskfnames <- sapply(riskLists,'[[','factorName')
#' chart.PA.exposure(PA_tables,riskfnames,plotInd=FALSE)
#' chart.PA.exposure(PA_tables,riskfnames,plotInd=TRUE)
chart.PA.exposure <- function(PA_tables,riskfnames,plotInd=FALSE){
  factorexp <- PA_tables$fexp
 
  #plot factor exposure
  indnames <- colnames(factorexp)[substr(colnames(factorexp),1,2)=='ES']
  alphafnames <- setdiff(colnames(factorexp),c('date',indnames,riskfnames))
  factormean <- colMeans(factorexp[,c(alphafnames,riskfnames)])
  
  if(plotInd==TRUE){
    indmean <- colMeans(factorexp[,indnames])
    indmean <- data.frame(factorName=sectorID2name(names(indmean)),
                      factorExposure=unname(indmean))
    factormean <- data.frame(factorName=names(factormean),factorExposure=unname(factormean))
    factormean <- rbind(factormean,indmean)
    
    factormean$tag <- "industry"
    factormean[factormean$factorName %in% alphafnames,'tag'] <- 'alpha'
    factormean[factormean$factorName %in% riskfnames,'tag'] <- 'risk'
  }else{
    factormean <- data.frame(factorName=names(factormean),factorExposure=unname(factormean))
    factormean$tag <- ""
    factormean[factormean$factorName %in% alphafnames,'tag'] <- 'alpha'
    factormean[factormean$factorName %in% riskfnames,'tag'] <- 'risk'
  }
  ggplot(factormean,aes(x=reorder(factorName,-factorExposure),y=factorExposure,fill=tag))+
    geom_bar(stat = "identity")+labs(title='Factor Exposure',x='',y='')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


#' chart.PA.attr
#' 
#' @export
#' @examples 
#' riskfnames <- sapply(riskLists,'[[','factorName')
#' chart.PA.attr(PA_tables,riskfnames)
#' chart.PA.attr(PA_tables,riskfnames,plotInd=TRUE)
chart.PA.attr <- function(PA_tables,riskfnames,plotInd=FALSE,attributeAnn=TRUE){
  perfattr <- PA_tables$perfattr
  
  indnames <- colnames(perfattr)[substr(colnames(perfattr),1,2)=='ES']
  alphafnames <- setdiff(colnames(perfattr),c('date',indnames,riskfnames,'res'))
  
  #plot summary factor performance attribution
  if(plotInd==FALSE){
    perfattr <- perfattr[,c('date',alphafnames,riskfnames,'res')]
  }
  perfts <- xts::xts(perfattr[,-1],order.by = perfattr[,1])
  
  if(attributeAnn==TRUE){
    rtnsum <- rtn.summary(perfts)
    rtnsum <- rtnsum['Annualized Return',]
  }else{
    rtnsum <- rtn.periods(perfts)
    rtnsum <- rtnsum["Cumulative Return",]
  }
  
  rtnsum <- data.frame(factorName=names(rtnsum),factorAttribution=unname(rtnsum))
  if(plotInd==TRUE){
    rtnsum$tag <- "industry"
    rtnsum.p1 <- rtnsum[rtnsum$factorName %in% indnames,]
    rtnsum.p1$factorName <- sectorID2name(rtnsum.p1$factorName)
    rtnsum.p2 <- rtnsum[!(rtnsum$factorName %in% indnames),]
    rtnsum <- rbind(rtnsum.p1,rtnsum.p2)
  }else{
    rtnsum$tag <- ""
  }
  rtnsum[rtnsum$factorName %in% alphafnames,'tag'] <- 'alpha'
  rtnsum[rtnsum$factorName %in% riskfnames,'tag'] <- 'risk'
  rtnsum[rtnsum$factorName=='res','tag'] <- 'residual'
  
  if(attributeAnn==TRUE){
    ggplot(rtnsum,aes(x=reorder(factorName,-factorAttribution),y=factorAttribution,fill=tag))+
      geom_bar(stat = "identity")+labs(title='Factor Attribution(Annulized)',x='',y='')+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }else{
    ggplot(rtnsum,aes(x=reorder(factorName,-factorAttribution),y=factorAttribution,fill=tag))+
      geom_bar(stat = "identity")+labs(title='Factor Attribution',x='',y='')+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
   }
  
  
}



# ---------------------  ~~ Risk attribution --------------

#' getRAData
#' 
#' @export
#' @examples 
#' tmp <- buildFactorLists(buildFactorList(factorFun="gf.NP_YOY",
#'                 factorPar=list(),factorDir=1),factorStd="norm",factorNA = "median")
#' alphaLists <- buildFactorLists_lcfs(c("F000012","F000008"),factorStd="norm",factorNA = "median")
#' alphaLists <- c(tmp,alphaLists)
#' riskLists <- buildFactorLists_lcfs(c("F000002","F000006"),factorStd="norm",factorNA = "median")
#' RA_tables <- getPAData(port,c(alphaLists,riskLists))
#' RA_tables <- getPAData(port,c(alphaLists,riskLists),bmk='EI000905')
getRAData <- function(port,factorLists,bmk,sectorAttr = defaultSectorAttr()){
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
  regdata <- reg.TSFR(TSFR = TSFR,regType = "glm",sectorAttr = sectorAttr,secRtnOut = F)
  frtn <- regdata$fRtn[,c("date","fname","frtn")]
  frtn <- reshape2::dcast(frtn,date~fname,value.var = 'frtn')
  frtn <- dplyr::arrange(frtn,date)
  
  # calculate factor exposure
  TSWF <- merge.x(port,TSFR,by=c('date','stockID'))
  TSWF <- na.omit(TSWF)
  TSWF <- gf.sector(TSWF,sectorAttr = sectorAttr)
  fexp <- exposure.TSWF(TSWF) 
  fexp <- dplyr::arrange(fexp,date)
  
  # calculate performance attribution
  if(!missing(bmk)){
    rtn.short <- unique(TSWF[,c('date','date_end')])
    rtn.short <- getPeriodrtn_EI(stockID=bmk,begT=rtn.short$date, endT=rtn.short$date_end)
    rtn.short <- dplyr::rename(rtn.short,date=begT,date_end=endT,bmkrtn=periodrtn)
    rtn.short <- rtn.short[,c( "date","date_end","bmkrtn")]
    TSWF <- merge.x(TSWF,rtn.short)
    TSWF$periodrtn <- TSWF$periodrtn-TSWF$bmkrtn
    portrtn <- plyr::ddply(TSWF,"date",plyr::summarise,rtn=sum(wgt*periodrtn, na.rm = TRUE))
  }else(
    portrtn <- plyr::ddply(TSWF,"date",plyr::summarise,rtn=sum(wgt*periodrtn, na.rm = TRUE))
    
  )
  #portrtn <- dplyr::arrange(portrtn,date)[-nrow(portrtn), ]
  portrtn <- dplyr::arrange(portrtn,date)
  portrtn_m <- as.matrix(portrtn[, -1])
  frtn <- dplyr::select(frtn,one_of(colnames(fexp))) # make the order of cols same with fexp
  frtn_m <- as.matrix(frtn[, -1])
  #fexp_m <- as.matrix(fexp[-nrow(fexp), -1])
  fexp_m <- as.matrix(fexp[, -1])
  fattr_m <- frtn_m*fexp_m
  res_m <- portrtn_m - as.matrix(rowSums(fattr_m))
  perfattr <- data.frame(date=portrtn$date,fattr_m,res=res_m)
  
  return(list(frtn=frtn,fexp=fexp,perfattr=perfattr,portrtn=portrtn))
}



#' chart.RA.attr
#' 
#' @export
#' @examples 
chart.RA.attr <- function(RA_tables){
  
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


