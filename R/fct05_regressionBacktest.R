

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  backtesting with 'regression' method -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' lcdb_regtables
#' 
#' build and update local database's regression result tables
#' @name lcdb_regtables
#' @rdname lcdb_regtables
#' @param begT is begin date
#' @param endT is end date
#' @param FactorLists
#' @examples
#' begT <- as.Date('2010-01-01')
#' endT <- as.Date('2012-12-31')
#' lcdb.build.RegTables(begT,endT,FactorLists)
#' begT <- as.Date('2012-06-01')
#' endT <- as.Date('2014-12-31')
#' lcdb.update.RegTables(begT,endT,FactorLists)
#' @export
lcdb.build.RegTables <- function(begT,endT,FactorLists){
  con <- db.local()
  if(dbExistsTable(con, "Reg_FactorRtn")) dbRemoveTable(con,'Reg_FactorRtn')
  if(dbExistsTable(con, "Reg_Residual")) dbRemoveTable(con,'Reg_Residual')
  if(dbExistsTable(con, "Reg_RSquare")) dbRemoveTable(con,'Reg_RSquare')
  
  dbGetQuery(con,"CREATE TABLE Reg_FactorRtn (
              date int  NOT NULL,
              fname TEXT NOT NULL,
              frtn_d1 decimal(10,6) NULL,
              tstat_d1 decimal(10,4) NULL,
              frtn_w1 decimal(10,6) NULL,
              tstat_w1 decimal(10,4) NULL,
              frtn_w2 decimal(10,6) NULL,
              tstat_w2 decimal(10,4) NULL,
              frtn_m1 decimal(10,6) NULL,
              tstat_m1 decimal(10,4) NULL)")
  dbGetQuery(con,"CREATE UNIQUE INDEX IX_Reg_FactorRtn ON Reg_FactorRtn(date,fname)")
  
  dbGetQuery(con,"CREATE TABLE Reg_Residual (
              date int  NOT NULL,
             stockID TEXT NOT NULL,
             res_d1 decimal(10,8) NULL,
             res_w1 decimal(10,8) NULL,
             res_w2 decimal(10,8) NULL,
             res_m1 decimal(10,8) NULL)")
  dbGetQuery(con,"CREATE UNIQUE INDEX IX_Reg_Residual ON Reg_Residual(date,stockID)")
  
  dbGetQuery(con,"CREATE TABLE Reg_RSquare (
             date int  NOT NULL,
             rsquare_d1 decimal(10,4) NULL,
             rsquare_w1 decimal(10,4) NULL,
             rsquare_w2 decimal(10,4) NULL,
             rsquare_m1 decimal(10,4) NULL)")
  dbGetQuery(con,"CREATE UNIQUE INDEX IX_Reg_RSquare ON Reg_RSquare(date)")
  
  
  if(missing(begT)) begT <- as.Date('2005-01-04')
  if(missing(endT)){
    endT <- dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
    endT <- trday.offset(intdate2r(endT),by = months(-1))
  }
  dbDisconnect(con)
  dates <- getRebDates(begT,endT,rebFreq = 'day')
  dates <- split(dates,cut(dates,'month'))
  plyr::l_ply(dates,lcdb.subfun.regtables,FactorLists,.progress = plyr::progress_text(style=3))

  return('Done!')
}


lcdb.subfun.regtables <- function(dates,FactorLists){

  cat(paste(min(rdate2int(dates)),' to ',max(rdate2int(dates))),'...\n')
  TS <- getTS(dates,indexID = 'EI000985')
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
  colnames(fRtn) <- c('date','fname',paste(c("frtn","tstat"),rep(names(prd_lists),each = 2),sep = '_'))
  colnames(res) <- c('date','stockID',paste('res',names(prd_lists),sep = '_'))
  colnames(RSquare) <- c('date',paste("rsquare",names(prd_lists),sep = '_'))
  
  con <- db.local()
  dbWriteTable(con,'Reg_FactorRtn',transform(fRtn,date=rdate2int(date)),overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'Reg_Residual',transform(res,date=rdate2int(date)),overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'Reg_RSquare',transform(RSquare,date=rdate2int(date)),overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}



#' @rdname lcdb_regtables
#' 
#' @export
lcdb.update.RegTables <- function(begT,endT,FactorLists){
  con <- db.local()
  if(missing(begT)){
    tmp.begT <- dbGetQuery(con,"select max(date) from Reg_RSquare")[[1]]
    tmp.begT <- trday.offset(intdate2r(tmp.begT),lubridate::days(1))
  }
  if(missing(endT)){
    endT <- dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
    endT <- trday.offset(intdate2r(endT),by = months(-1))
  }
  if(begT>endT) return('Done!')
  
  tmp.dates <- dbGetQuery(con,"select min(date) 'mindate',max(date) 'maxdate' from Reg_RSquare")
  tmp.dates <- transform(tmp.dates,mindate=intdate2r(mindate),maxdate=intdate2r(maxdate))
  if(begT<= tmp.dates$maxdate & endT>= tmp.dates$mindate){
    dbGetQuery(con, paste("delete from Reg_FactorRtn WHERE date>=",rdate2int(begT),
                          " and date<=",rdate2int(endT)))
    dbGetQuery(con, paste("delete from Reg_RSquare WHERE date>=",rdate2int(begT),
                          " and date<=",rdate2int(endT)))
    dbGetQuery(con, paste("delete from Reg_Residual WHERE date>=",rdate2int(begT),
                          " and date<=",rdate2int(endT)))
  }
  dbDisconnect(con)
  
  dates <- getRebDates(begT,endT,rebFreq = 'day')
  dates <- split(dates,cut(dates,'month'))
  plyr::l_ply(dates,lcdb.subfun.regtables,FactorLists,.progress = plyr::progress_text(style=3))
  return('Done!')
}








#' regression_result
#'
#' Regression to the TSFR data, calculate factor return, residuals, and R squrare, etc. 
#' @name regression_result
#' @rdname regression_result
#' @aliases reg.TSFR
#' @param TS a \bold{TS} object.
#' @param dure see example in \code{\link{getTSR}}.
#' @param TSFR a \bold{TSFR} object.
#' @param regType the regress type,the default type is "glm".
#' @param glm_wgt glm's weight data, default value is sqrt of floating market value.
#' @param sectorAttr sector attribute.
#' @param secRtnOut whether output sector's return,default value is \code{FALSE}.
#' @return return a list, contains dataframes of frtn, residual and Rsquare.
#' @export
#' @author Ruifei.yin
#' @examples
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-09-30'))
#' TS <- getTS(RebDates,indexID = 'EI000985')
#' factorIDs <- c("F000002","F000006","F000008")
#' tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' FactorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.NP_YOY",
#'                   factorPar=list(),
#'                   factorDir=1),
#'   factorStd="norm",factorNA = "median")
#' FactorLists <- c(tmp,FactorLists)
#' re <- reg.TS(TS)
#' re <- reg.TS(TS,FactorLists)
#' ----------------------------------------------------------
#' TSF <- getMultiFactor(TS,FactorLists)
#' TSFR <- getTSR(TSF)
#' re <- reg.TSFR(TSFR)
reg.TSFR <- function(TSFR,regType=c('glm','lm'),glm_wgt=c("sqrtFV","res"),
                     sectorAttr=defaultSectorAttr(),secRtnOut=FALSE){
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  
  TSFR_save <- TSFR
  if(sum(is.na(TSFR))>0){
    warning('NAs in TSFR!')
    TSFR <- na.omit(TSFR)  # omit the NAs 
  }
  factorNames <- guess_factorNames(TSFR)

  # regression
  if(regType=='glm'){ #get glm_wgt data
    if(glm_wgt=="sqrtFV"){
      TSw <- getTSF(TSFR[,c('date','stockID')],factorFun="gf_lcfs",factorPar=list(factorID='F000001'),factorStd = 'none',factorNA = "median")
      TSw <- transform(TSw,factorscore=sqrt(factorscore))
      TSw <- dplyr::rename(TSw,glm_wgt=factorscore)
      TSFR <- merge.x(TSFR,TSw,by =c("date","stockID"))
    }else if(glm_wgt=="res"){
      
    }
  }  
  
  # loop of regression
  dates <- dplyr::distinct(TSFR,date)
  fRtn <- data.frame()
  res <- data.frame()
  RSquare <- data.frame()
  for(i in 1:nrow(dates)){ 
    tmp.tsfr <- TSFR[TSFR$date==dates$date[i],]
    tmp.ts <- tmp.tsfr[,c("date","stockID")]
    
    # get sector factor
    if(!is.null(sectorAttr)){
      tss <- gf.sector(tmp.ts,sectorAttr)
      secNames <- as.character(unique(tss$sector))
      if(length(secNames)>1){
        tss <- dplyr::select(tss,-sector)
        tmp.tsfr <- merge.x(tmp.tsfr,tss,by=c("date","stockID"))
        if(length(factorNames)==0){
          fml <- formula(paste("periodrtn ~ ", paste(secNames,collapse= "+"),"-1",sep=''))
        }else{
          fml <- formula(paste("periodrtn ~ ", paste(c(factorNames,secNames),collapse= "+"),"-1",sep=''))
        }
      }
    }else if(length(factorNames)==0){
      stop('missing x variables!')
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
    fRtn <- rbind(fRtn,data.frame(date=dates$date[i],fname=rownames(smry$coefficients),
                                  frtn=smry$coefficients[,1],Tstat=smry$coefficients[,3],
                                  stringsAsFactors=FALSE))
    res <- rbind(res,data.frame(date=dates$date[i],stockID=tmp.tsfr$stockID,res=lmm$residuals,
                                stringsAsFactors=FALSE))
    RSquare <- rbind(RSquare,data.frame(date=dates$date[i],RSquare=smry$r.squared,
                                        stringsAsFactors=FALSE))
    
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
  re <- list(TSFR=TSFR_save,fRtn=fRtn,res=res,RSquare=RSquare)
  return(re)
}




#' @rdname regression_result
#' @aliases reg.TS
#' @export
reg.TS <- function(TS,FactorLists,dure=months(1),regType=c('glm','lm'),glm_wgt=c("sqrtFV","res"),
                   sectorAttr=defaultSectorAttr(),secRtnOut=FALSE){
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  if(missing(FactorLists)){
    TSR <- getTSR(TS,dure)
    reg <- reg.TSFR(TSR, regType, glm_wgt, sectorAttr, secRtnOut)
    re <- list(TSFR=TSR,fRtn=reg$fRtn,res=reg$res,RSquare=reg$RSquare)
  }else{
    TSF <- getMultiFactor(TS,FactorLists)
    TSFR <- getTSR(TSF,dure)
    reg <- reg.TSFR(TSFR, regType, glm_wgt, sectorAttr, secRtnOut)
    re <- list(TSFR=TSFR,fRtn=reg$fRtn,res=reg$res,RSquare=reg$RSquare)
  }
  return(re)
}



#' factor.VIF
#'
#' get factor's VIF or stock's residual
#' @author Andrew Dow
#' @param TSF is a \bold{TSF} object.
#' @param testf is test factor name, can be missing.
#' @param sectorAttr is sector attribute.
#' @return data frame of VIF or residual.
#' @examples
#' VIF <- factor.VIF(TSF)
#' VIF <- factor.VIF(TSF,testf)[[1]]
#' res <- factor.VIF(TSF,testf)[[2]]
#' @export
factor.VIF <- function(TSF,testf,sectorAttr=defaultSectorAttr()){
  fname <- setdiff(colnames(TSF),c('date','stockID'))
  
  if(missing(testf)){
    if(length(fname)==1 & is.null(sectorAttr)) stop('NO x variable!')
  }else{
    if(!(testf %in% fname)){
      stop('testf not in TSF!')
    }else if(length(fname)==1){
      stop('NO x variable!')
    }
  }

  
  dates <- unique(TSF$date)
  secNames <- NULL
  VIF <- data.frame()
  res <- data.frame()
  # loop of regression
  for(i in 1:length(dates)){ 
    tmp.tsf <- subset(TSF,date==dates[i])
    # get sector factor
    if(!is.null(sectorAttr)){
      tss <- gf.sector(tmp.tsf[,c('date','stockID')],sectorAttr)
      if(ncol(tss)>4){
        secNames <- as.character(unique(tss$sector))
        tss <- dplyr::select(tss,-sector)
        tmp.tsf <- merge.x(tmp.tsf,tss,by=c("date","stockID"))
      }
    }
    
    if(missing(testf)){
      testf <- fname
    }
    
    for(j in testf){
      if(is.null(secNames)){
        fml <- formula(paste(j," ~ ", paste(c(setdiff(fname,j)), collapse= "+"),sep=''))
      }else{
        fml <- formula(paste(j," ~ ", paste(c(setdiff(fname,j),secNames), collapse= "+"),"-1",sep=''))
      }
      smry <- summary(lm(fml,data = tmp.tsf))
      VIF <- rbind(VIF,data.frame(date=dates[i],fname=j,vif=1/(1-smry$r.squared)))
      res <- rbind(res,data.frame(tmp.tsf[,c('date','stockID')],fname=j,res=smry$residuals))
    }

  }
  return(list(VIF,res))
}





#' factor select
#' 
#' @name factor_select
#' @rdname factor_select
#' @param TSFR a \bold{TSFR} object.
#' @export
#' @examples 
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-09-30'))
#' TS <- getTS(RebDates,indexID = 'EI000905')
#' factorIDs <- c("F000006","F000007","F000008","F000012","F000013","F000014",
#' "F000015","F000016","F000017","F000018")
#' tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' factorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.NP_YOY",
#'                   factorPar=list(),
#'                   factorDir=1),
#'   buildFactorList(factorFun="gf.ln_mkt_cap",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.G_MLL_Q",
#'                   factorPar=list(),
#'                   factorDir=1),
#'    buildFactorList(factorFun="gf.GG_NP_Q",
#'                    factorPar=list(filt = 0),
#'                    factorDir=1),
#'    buildFactorList(factorFun="gf.GG_OR_Q",
#'                    factorPar=list(filt = 0),
#'                    factorDir=1),                
#'   factorStd="norm",factorNA = "median")
#' factorLists <- c(tmp,factorLists)
#' TSF <- getMultiFactor(TS,FactorLists = factorLists)
#' TSFR <- getTSR(TSF)
#' re <- reg.factor.select(TSFR)
#' re <- reg.factor.select(TSFR,sectorAttr=NULL)
#' nstock <- length(factorLists)
#' re <- reg.factor.select(TSFR,forder=sample(1:nstock,nstock)
reg.factor.select <- function(TSFR,sectorAttr=defaultSectorAttr(),forder){
  #sector only
  if(!is.null(sectorAttr)){
    secrs <- reg.TSFR(TSFR[,c("date","date_end","stockID","periodrtn")],sectorAttr = sectorAttr)[[4]]
    result <- data.frame(fname='sector',rsquare=mean(secrs$RSquare,na.rm = TRUE), 
                         frtn=NA,fttest=NA,pttest=NA,tag='risk')
  }else{
    result <- data.frame()
  }
  
  fname <- guess_factorNames(TSFR)
  if(!missing(forder)){
    fname <- fname[forder]
  }
  
  selectf <- NULL
  while(length(setdiff(fname,selectf))>0){
    rsquare <- data.frame()
    frtn <- data.frame()
    res <- data.frame()
    if(missing(forder)){
      fnameset <- setdiff(fname,selectf)
    }else{
      fnameset <- setdiff(fname,selectf)[1]
    }
    
    for(i in fnameset){
      tmp.TSF <- TSFR[,c("date","stockID",union(selectf,i))]
      if(is.null(sectorAttr) & ncol(tmp.TSF)==3){
        tmp.res <- data.frame(tmp.TSF[,c('date','stockID')],fname=i,res=tmp.TSF[,i])
      }else{
        if(ncol(tmp.TSF)==3){
          tmp.res <- factor.VIF(tmp.TSF,sectorAttr = sectorAttr)[[2]]
        }else{
          tmp.res <- factor.VIF(tmp.TSF,i,sectorAttr)[[2]]
        }
        tmp.TSF[,i] <- dplyr::left_join(tmp.TSF[,c("date","stockID")],
                                        tmp.res[,c("date","stockID","res")],by=c("date","stockID"))[,3]
      }
      
      tmp.TSFR <- dplyr::left_join(tmp.TSF,TSFR[,c("date","date_end","stockID","periodrtn")],
                                   by=c("date","stockID"))
      frs <- reg.TSFR(tmp.TSFR,sectorAttr = sectorAttr)
      tmp <- data.frame(frs$RSquare,fname=i)
      rsquare <- rbind(rsquare,tmp)
      res <- rbind(res,tmp.res)
      frtn <- rbind(frtn,frs$fRtn)
    }
    rsquare <- rsquare %>% group_by(fname) %>%
      dplyr::summarise(rsquare = mean(RSquare,trim = 0.025,na.rm = TRUE)) %>% 
      arrange(desc(rsquare)) %>% slice(1)
    tmp.selectf <- as.character(rsquare$fname)
    tmp.frtn <- frtn[frtn$fname==tmp.selectf,'frtn']
    testres <- t.test(tmp.frtn)
    rsquare <- transform(rsquare,frtn=mean(tmp.frtn,trim = 0.025,na.rm = TRUE),
                         fttest=testres$statistic,
                         pttest=testres$p.value,
                         tag=ifelse(testres$statistic>2,'alpha','risk'))
    result <-  rbind(result,rsquare)
    selectf <- c(selectf,tmp.selectf)
    
    res <- res[res$fname==tmp.selectf,c('date','stockID','res')]
    TSFR[,tmp.selectf] <- dplyr::left_join(TSFR[,c("date","stockID")],
                                           res,by=c("date","stockID"))[,3]
  }
  
  
  rownames(result) <- NULL
  tmp <- as.character(result[result$fname!='sector','fname'])
  TSFR <- TSFR[,c("date","date_end","stockID",tmp,"periodrtn" )]
  return(list(result=result,TSFR=TSFR))
}





#' factorlists recommend
#' 
#' @param indexID.
#' @export
#' @examples 
#' FactorLists <- reg.factorlists.recommend(indexID='EI000300')
#' FactorLists <- reg.factorlists.recommend(indexID='EI000905')
#' FactorLists <- reg.factorlists.recommend(indexID='EI000985')
#' FactorLists <- reg.factorlists.recommend(indexID='ES33370000')
reg.factorlists.recommend <- function(indexID){
  
  if(indexID=='EI000300'){
    factorIDs <- c("F000006","F000014","F000015","F000016","F000017")
    tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
    FactorLists <- buildFactorLists(
      buildFactorList(factorFun="gf.ln_mkt_cap",
                      factorPar=list(),
                      factorDir=-1),
      factorStd="norm",factorNA = "median")
    FactorLists <- c(tmp,FactorLists)
  }else if(indexID=='EI000905'){
    factorIDs <- c("F000006","F000008","F000013","F000014","F000016","F000017")
    tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
    FactorLists <- buildFactorLists(
      buildFactorList(factorFun="gf.ln_mkt_cap",
                      factorPar=list(),
                      factorDir=-1),
      factorStd="norm",factorNA = "median")
    FactorLists <- c(tmp,FactorLists)
  }else{
    factorIDs <- c("F000006","F000014","F000015","F000016")
    tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median",factorOutlier = 0.01)
    FactorLists <- buildFactorLists(
      buildFactorList(factorFun="gf.ln_mkt_cap",
                      factorPar=list(),
                      factorDir=-1),
      buildFactorList(factorFun="gf.NP_YOY",
                      factorPar=list(),
                      factorDir=1),
      factorStd="norm",factorNA = "median",factorOutlier = 0.01)
    FactorLists <- c(tmp,FactorLists)
  }
  return(FactorLists)
}





# ---------------------  ~~ Backtesting results --------------

#' regression_result_summary
#' 
#' summary of regression result, such as chart of rsquare and factor return,etc.
#' @param reg_results is regression_result
#' @param factet whether to plot wealth index of factor's return in one graph, the default value is FALSE.
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
  
  tstat <- dplyr::summarise(group_by(fRtn,fname),avgT=mean(abs(Tstat)),
                     TPer=sum(Tstat>2)/length(Tstat))
  colnames(tstat) <- c("fname","mean(abs(T))","percent T>2")
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
  VIF <- factor.VIF(TSF,sectorAttr = NULL)[[1]]
  VIF <- dplyr::summarise(group_by(VIF,fname),vif=mean(vif))
  VIF$fname <- as.character(VIF$fname)
  
  re <- dplyr::left_join(rtnsum,tstat,by='fname')
  re <- dplyr::left_join(re,VIF,by='fname')
  re <- dplyr::arrange(re,dplyr::desc(Sharpe))
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
  
  # fnames <- setdiff(colnames(TSF),c('date','stockID','date_end','periodrtn'))
  fnames <- guess_factorNames(TSF)
  TSF_by <- dplyr::group_by(TSF[,c('date',fnames)],date)
  
  cordata <- TSF_by %>% dplyr::do(cormat = cor(.[,fnames],method='spearman'))
  cordata <- cordata %>% dplyr::do(data.frame(date=.$date,fname=fnames,.$cormat))
  cordata <- reshape2::melt(cordata,id=c('date','fname'),
                        variable.name='fnamecor',factorsAsStrings=FALSE)
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
  
  # fnames <- setdiff(colnames(TSF),c('date','stockID','date_end','periodrtn'))
  fnames <- guess_factorNames(TSF)
  TSF_by <- dplyr::group_by(TSF[,c('date',fnames)],date)
  
  cordata <- TSF_by %>% dplyr::do(cormat = cor(.[,fnames],method='spearman'))
  cordata <- cordata %>% dplyr::do(data.frame(date=.$date,fname=fnames,.$cormat))
  cordata <- reshape2::melt(cordata,id=c('date','fname'),
                            variable.name='fnamecor',factorsAsStrings=FALSE)
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
    cordata_by <- split(cordata_by[,-1],cordata_by$tmp)
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
#' @param RebDates
#' @param fNames
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param reg_results
#' @param rollavg whether to get the rolling average factor return.
#' @param nwin rolling windows.
#' @return a factor return data frame.
#' @examples 
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-08-31'))
#' fNames <- c("ln_mkt_cap_","PB_mrq_","pct_chg_per_60_","IVR_")
#' dure <- lubridate::days(1)
#' @export
getfRtn <- function(RebDates,fNames,dure=months(1),rollavg=TRUE,
                    nwin=lubridate::years(-2),reg_results){
  TF <- expand.grid(date=RebDates,fname=fNames,stringsAsFactors = FALSE)
  TF <- dplyr::arrange(TF,date,fname)
  
  if(missing(reg_results)){
    re <- getRawfRtn(dure=dure)
  }else{
    re <- getRawfRtn(reg_results=reg_results)
  }
  
  if(rollavg==FALSE){
    re <- re %>% group_by(fname) %>% 
      summarise(frtn = mean(frtn,na.rm = TRUE))
    re <- dplyr::left_join(TF,re,by='fname')
    
  }else{
    tmp <- dplyr::mutate(TF,endT=trday.offset(date,dure*-1),
                         begT=trday.offset(endT,nwin))
    tmp$fname <- factor(tmp$fname)
    tmp <- tmp %>% dplyr::rowwise() %>% 
      do(data.frame(tmpdate=getRebDates(.$begT, .$endT,'day'),
                    date=.$date,fname=.$fname))
    class(tmp) <- c( "tbl_df", "data.frame")
    tmp$fname <- as.character(tmp$fname)
    re <- dplyr::rename(re,tmpdate=date)
    re <- dplyr::left_join(tmp,re,by=c('tmpdate','fname'))
    re <- re %>% group_by(date,fname) %>% 
      summarise(frtn = mean(frtn,na.rm = TRUE))
    re <- dplyr::left_join(TF,re,by=c('date','fname'))
  }
  re <- na.omit(re)
  return(re)
}





getRawfRtn <- function(begT,endT,dure,reg_results){
  if(missing(begT)) begT <- as.Date('1990-01-01')
  if(missing(endT)) endT <- as.Date('2100-01-01')
  
  if(missing(reg_results)){
    if(dure==lubridate::days(1)){
      dbname <- 'frtn_d1'
    }else if(dure==lubridate::weeks(1)){
      dbname <- 'frtn_w1'
    }else if(dure==lubridate::weeks(2)){
      dbname <- 'frtn_w2'
    }else if(dure==months(1)){
      dbname <- 'frtn_m1'
    }
    
    con <- db.local()
    qr <- paste("SELECT date,fname,",dbname," 'frtn'
                FROM Reg_FactorRtn where date>=",rdate2int(begT),
                " and date<=",rdate2int(endT))
    re <- dbGetQuery(con,qr)
    re <- transform(re,date=intdate2r(date))
    dbDisconnect(con)
  }else{
    re <- reg_results$fRtn
    re <- dplyr::select(re,-Tstat)
    re <- dplyr::filter(re,date>=begT,date<=endT)
  }
  
  return(re)
}



#' calfCov
#'
#' calculate factor return covariance
#' @author Andrew Dow
#' @param TF a data frame contains date and factorNames
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
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
calfCov <- function(RebDates,fNames,dure=months(1),rollavg=TRUE,
                    nwin=lubridate::years(-2),reg_results){
  TF <- expand.grid(date=RebDates,fname=fNames,stringsAsFactors = FALSE)
  TF <- dplyr::arrange(TF,date,fname)
  
  if(missing(reg_results)){
    re <- getRawfRtn(dure=dure)
  }else{
    re <- getRawfRtn(reg_results=reg_results)
  }
  

  if(rollavg==FALSE){
    re <- re[re$fname %in% TF$fname,]
    re <- reshape2::dcast(re,date~fname,mean,value.var = 'frtn')
    tmp <- setdiff(colnames(re),'date')
    re <- data.frame(fname=tmp,cov(as.matrix(re[,tmp])),stringsAsFactors = FALSE)
    TF <- subset(TF,fname %in% tmp)
    re <- dplyr::left_join(TF,re,by='fname')
    re <- re[,c("date",tmp)]
    
  }else{
    tmp <- dplyr::mutate(TF,endT=trday.offset(date,dure*-1),
                         begT=trday.offset(endT,nwin))
    tmp$fname <- factor(tmp$fname)
    tmp <- tmp %>% dplyr::rowwise() %>% 
      do(data.frame(tmpdate=getRebDates(.$begT, .$endT,'day'),
                    date=.$date,fname=.$fname))
    class(tmp) <- c( "tbl_df", "data.frame")
    tmp$fname <- as.character(tmp$fname)
    re <- dplyr::rename(re,tmpdate=date)
    re <- dplyr::left_join(tmp,re,by=c('tmpdate','fname'))
    re <- na.omit(re)
    re <- reshape2::dcast(re,date+tmpdate~fname,value.var = 'frtn')
    re <- dplyr::arrange(re,date,tmpdate)
    tmp <- setdiff(colnames(re),c('date','tmpdate'))
    re <- plyr::ddply(re,'date',function(subre){
      as.data.frame(cov(as.matrix(subre[,tmp])))
    })
    re <- subset(re,date %in% TF$date) 
    re <- na.omit(re)
    # remove too short period to do
    
  }
  return(re)
}


#' getResidual
#' 
#' get stocks' residual
#' @param TS a \bold{TS} object.
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param reg_results
#' @examples 
#' RebDates <- getRebDates(as.Date('2012-01-31'),as.Date('2016-08-31'))
#' TS <- getTS(RebDates)
#' dure <- lubridate::days(1)
#' res <- getResidual(TS,dure)
#' dure <- months(1)
#' res <- getResidual(TS,dure)
#' @export
getResidual <- function(TS,dure,reg_results){
  if(missing(reg_results)){
    con <- db.local()
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
    dates$date_tmp2 <- trday.nearby(dates$date_tmp1,-(nwin-1))
    
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
#' @param TSF 
#' @param alphaf is alpha factors' name, can be missing.
#' @param fRtn
#' @param Fcov is the covariance matrix.
#' @param bmk is the benckmark for optimization,can be missing.
#' @return a \bold{port} object.
#' @examples 
#' 
#' 
#' @export
OptWgt <- function(TSF,alphaf,fRtn,fCov,
                      target=c('return','balance'),
                      bmk,sectorAttr=defaultSectorAttr(),
                      factorExp=buildFactorExp(),wgtSet=buildWgtSet(),boxConstr,
                      addEvent=FALSE,optWay=c('ipop','solve.QP','Matlab')){
  target <- match.arg(target)
  optWay <- match.arg(optWay)
  
  if(optWay == "Matlab"){
    R.matlab::Matlab$startServer()
    matlab <- R.matlab::Matlab()
    open(matlab)
  }
  
  fnames <- guess_factorNames(TSF)
  if(missing(alphaf)){
    alphaf <- fnames
  }
  dates <- unique(TSF$date)
  port <- data.frame()
  for(i in dates){
    cat(rdate2int(as.Date(i,origin = '1970-01-01')), "...\n")
    
    #get one period raw data
    tmp.TSF <- TSF[TSF$date==i,]
    #add sector factor
    if(!is.null(sectorAttr)){
      tmp <- gf.sector(tmp.TSF[,c('date','stockID')],sectorAttr)
      tmp <- dplyr::select(tmp,-sector)
      tmp.TSF <- dplyr::left_join(tmp.TSF,tmp,by=c('date','stockID'))
    }
    
    #get factor return and factor covariance
    if('date' %in% colnames(fRtn)){
      tmp.fRtn <- fRtn[fRtn$date==i,-1]
    }else{
      tmp.fRtn <- fRtn
    }
    rownames(tmp.fRtn) <- tmp.fRtn$fname
    
    if('date' %in% colnames(fCov)){
      tmp.fCov <- fCov[fCov$date==i,-1]
    }else{
      tmp.fCov <- fCov
    }
    rownames(tmp.fCov) <- colnames(tmp.fCov)
    
    #remove unqualified TS
    #tmp.TS <- quantbox::rmSuspend(tmp.TSF[,c('date','stockID')],datasrc = 'ts')
    tmp.TS <- rm_suspend(tmp.TSF[,c('date','stockID')])
    if(addEvent==TRUE){
      tmp.TS <- quantbox::rmNegativeEvents(tmp.TS)
    }
    tmp.TSF <- tmp.TSF[tmp.TSF$stockID %in% tmp.TS$stockID,]
    
    
    #get factor exposure up down limit
    if(missing(bmk)){
      totwgt <- getfExpLimit(factorExp,TSF=tmp.TSF)
    }else{
      bmkdata <- getbmkfExp(tmp.TSF,bmk)
      totwgt <- getfExpLimit(factorExp,bmk=bmkdata)
    }
    totwgt <- totwgt[,-1]
    rownames(totwgt) <- totwgt$fname
    
    
    # get risk matrix
    if(!missing(boxConstr)){
      tmpdata <- getnewTSFtotwgt(tmp.TSF,totwgt,boxConstr)
      tmp.TSF <- tmpdata$TSF
      totwgt <- tmpdata$totwgt
    }
    
    riskmat <- as.matrix(tmp.TSF[,totwgt$fname])
    rownames(riskmat) <- tmp.TSF$stockID
    
    #get alpha matrix
    alphamat <- as.matrix(tmp.TSF[,alphaf])
    rownames(alphamat) <- tmp.TSF$stockID
    dvec <- t(as.matrix(tmp.fRtn[alphaf,'frtn'])) %*% t(alphamat)
    if(addEvent){
      # add event return
    }
    wgtLimit <- getStockWgtLimit(tmp.TS,wgtSet,sectorAttr)
    
    if(target=='balance'){
      
      Fcovmat <- as.matrix(tmp.fCov[alphaf,alphaf])
      Dmat <- alphamat %*% Fcovmat %*% t(alphamat)
      tmp <- Matrix::nearPD(Dmat)
      Dmat <- tmp$mat
      Dmat <- matrix(Dmat,nrow = nrow(Dmat))
      nstock <- dim(Dmat)[1]
      
      if(optWay == "solve.QP"){
        Amat <- cbind(riskmat,-1*riskmat)
        Amat <- cbind(1,Amat,diag(x=1,nstock),diag(x=-1,nstock))#control weight
        bvec <- c(1,totwgt$min,-1*totwgt$max,wgtLimit$min,-1*wgtLimit$max)
        system.time(res <- quadprog::solve.QP(Dmat,dvec,Amat,bvec,meq = 1))
        tmp <- data.frame(date=i,stockID=rownames(alphamat),wgt=res$solution)
        
      }else if(optWay == "ipop"){
        f.ipop <- as.matrix(-dvec, ncol = 1)
        A.ipop <- t(cbind(1,riskmat))
        b.ipop <- c(1,totwgt$min)
        dif.ipop <- totwgt$max - totwgt$min
        r.ipop <- c(0, dif.ipop)
        lb.ipop <- matrix(data = wgtLimit$min, nrow = nstock, ncol = 1)
        ub.ipop <- matrix(data = wgtLimit$max, nrow = nstock, ncol = 1)
        system.time(res.ipop <- kernlab::ipop(c = f.ipop, H = Dmat,
                                              A = A.ipop, b = b.ipop, r = r.ipop,
                                              l = lb.ipop, u = ub.ipop,
                                              maxiter = 3000))
        tmp <- data.frame(date=i,stockID=rownames(alphamat),wgt=res.ipop@primal)
        
      }else if(optWay == "Matlab"){
        H.matlab <- Dmat
        f.matlab <- as.matrix(-dvec, ncol = 1)
        A.matlab <- t(cbind(-1*riskmat, riskmat))
        b.matlab <- as.matrix(c(-1*totwgt$min, totwgt$max), ncol=1)
        Aeq.matlab <- matrix(data = 1, nrow = 1, ncol = nstock)
        beq.matlab <- 1
        lb.matlab <- matrix(data = totwgt$min, nrow = nstock, ncol = 1)
        ub.matlab <- matrix(data = totwgt$max, nrow = nstock, ncol = 1)
        
        R.matlab::setVariable(matlab, H = H.matlab, f = f.matlab, A = A.matlab, b = b.matlab,
                              Aeq = Aeq.matlab, beq = beq.matlab, lb = lb.matlab, ub = ub.matlab)
        R.matlab::evaluate(matlab, "optionn = optimoptions(@quadprog,'Algorithm','interior-point-convex','MaxIter',5000);")
        R.matlab::evaluate(matlab, "res = quadprog(H,f,A,b,Aeq,beq,lb,ub,[],optionn);")
        res.tmp <- R.matlab::getVariable(matlab, "res")
        res.matlab <- res.tmp$res
        
        tmp <- data.frame(date=i,stockID=rownames(alphamat),wgt=res.matlab)
      }
      
      tmp <- tmp[tmp$wgt>0.0005,]
      colnames(tmp) <-c( "date","stockID","wgt")
      tmp <- transform(tmp,wgt=wgt/sum(wgt))
      
    }else{
      require(PortfolioAnalytics)
      pspec <- portfolio.spec(assets=colnames(dvec))
      pspec <- add.constraint(portfolio=pspec, type="full_investment")
      pspec <- add.constraint(portfolio=pspec,type="box",min=wgtLimit$min,max=wgtLimit$max)
      
      pspec <- add.constraint(portfolio=pspec, type="factor_exposure",
                              B=riskmat,lower=totwgt$min, upper=totwgt$max)
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
  
  if(optWay == "Matlab"){
    close(matlab)
  }
  port$date <- as.Date(port$date,origin = '1970-01-01')
  port$stockID <- as.character(port$stockID)
  return(port)
}






#' buildWgtSet
#' 
#' build weight setting in optimization function
#' @examples 
#' wgtSet <- buildWgtSet()
#' wgtSet <- buildWgtSet(wgtall=c(0,0.02),EQ601318=c(0.05,0.15))
#' wgtSet <- buildWgtSet(EQ601318=c(0.05,0.15),ES33370000=c(0,0.5))
#' @export
buildWgtSet <- function(wgtall=c(0,0.01),...){
  if(is.null(wgtall)){
    result <- data.frame()
  }else{
    result <- data.frame(ID='wgtall',
                         min=wgtall[1],
                         max=wgtall[2])
  }

  tmp <- list(...)
  if(length(tmp)>0){
    tmp <- plyr::ldply(tmp)
    colnames(tmp) <- colnames(result)
    result <- rbind(result,tmp)
  }
  if(any(result$max<=result$min)){
    warning('wgtmax less than wgtmin',call. = FALSE)
  }
  result$ID <- as.character(result$ID)
  return(result)
}



#' buildFactorExp
#' 
#' build factor exposure setting in optimization function
#' @examples 
#' fexp <- buildFactorExp()
#' fexp <- buildFactorExp(disposition_=c(-0.01,1),beta_ = c(-0.01, 1))
#' fexp <- buildFactorExp(sectorall=c(-0.05,0.05),disposition_=c(-0.01,1),ES33480000=c(0.15,0.25))
#' @export
buildFactorExp <- function(sectorall=c(-0.05,0.05),...){
  if(is.null(sectorall)){
    result <- data.frame()
  }else{
    result <- data.frame(fname="sectorall",
                         min=sectorall[1],
                         max=sectorall[2])
  }

  tmp <- list(...)
  if(length(tmp)>0){
    tmp <- plyr::ldply(tmp)
    colnames(tmp) <- c('fname','min','max')
    result <- rbind(result,tmp)
  }
  if(any(result$max<=result$min)){
    warning('max less than min',call. = FALSE)
  }
  result$fname <- as.character(result$fname)
  return(result)
}


#' buildBoxConstr
#' 
#' build box constraint in optimization function
#' @examples 
#' boxConstr <- buildBoxConstr(EI000905=c(0.75,0.85))
#' boxConstr <- buildBoxConstr(EI000905=c(0.75,0.85),EI000300=c(0.15,0.35))
#' @export
buildBoxConstr <- function(...){
  tmp <- list(...)
  if(length(tmp)>0){
    tmp <- plyr::ldply(tmp)
    colnames(tmp) <- c('indexID','min','max')
  }
  if(any(tmp$max<=tmp$min)){
    warning('max less than min',call. = FALSE)
  }
  return(tmp)
}




# getbmkfExp
# 
# inner function, get benchmark's factor exposure
# bmkdata <- getbmkfExp(TSF,bmk)
getbmkfExp <- function(TSF,bmk){
  bmkdata <- getIndexCompWgt(bmk,unique(TSF$date))
  bmkdata <- dplyr::left_join(bmkdata,TSF,by=c('date','stockID'))
  bmkdata[is.na(bmkdata)] <- 0
  fnames <- setdiff(colnames(bmkdata),c("date","stockID","wgt"))
  
  result <- data.frame()
  for(i in unique(bmkdata$date)){
    tmp <- subset(bmkdata,date==i)
    tmp <- t(as.matrix(tmp$wgt)) %*% as.matrix(tmp[,fnames])
    tmp <- data.frame(date=i,fname=colnames(tmp),fexp=t(unname(tmp)))
    result <- rbind(result,tmp)
  }
  result$date <- as.Date(result$date,origin='1970-01-01')
  tmp <- dplyr::filter(result,substr(fname,1,2)=='ES',fexp==0)
  result <- dplyr::setdiff(result,tmp)
  result$fname <- as.character(result$fname)
  return(result)
}


# getfExpLimit
# 
# inner function, get benchmark's factor exposure
# totwgt <- getfExpLimit(factorExp,bmkdata)
getfExpLimit <- function(factorExp,bmk,TSF){
  if(missing(bmk)){
    fnames <- guess_factorNames(TSF)
    
    fnames.f <- fnames[substr(fnames,1,2)!='ES']
    tmp.result1 <- data.frame()
    if(length(fnames.f)>0){
      tmp.result1 <- expand.grid(date=unique(TSF$date),fname=fnames.f,
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      tmp.result1 <- dplyr::left_join(tmp.result1,factorExp,by='fname')
      if(sum(is.na(tmp.result1))>0){
        tmp <- unique(tmp.result1[is.na(tmp.result1$min),'fname'])
        warning(paste('miss ',tmp,' exposure setting!'))
        tmp.result1[is.na(tmp.result1$min),'min'] <- -0.01
        tmp.result1[is.na(tmp.result1$max),'max'] <- 100
      }
    }
    
    
    fnames.sec <- fnames[substr(fnames,1,2)=='ES']
    tmp.result2 <- data.frame()
    if(length(fnames.sec)>0){
      tmp.result2 <- expand.grid(date=unique(TSF$date),fname=fnames.sec,
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      tmp.result2 <- dplyr::left_join(tmp.result2,factorExp,by='fname')
      if(any(factorExp$fname=='sectorall')){
        tmp.min <- factorExp[factorExp$fname=='sectorall','min']
        tmp.max <- factorExp[factorExp$fname=='sectorall','max']
        tmp.result2 <- transform(tmp.result2,
                                     min=ifelse(is.na(min),tmp.min,min),
                                     max=ifelse(is.na(max),tmp.max,max))
      }else{
        warning('miss sectorall argument!')
        tmp.result2 <- transform(tmp.result2,
                                     min=0,
                                     max=0.2)
        
      }
      
    }
    result <- rbind(tmp.result1,tmp.result2)

    
    
  }else{
    tmp.result1 <- subset(bmk,fname %in% factorExp$fname)
    if(nrow(tmp.result1)>0){
      tmp.result1 <- dplyr::left_join(tmp.result1,factorExp,by='fname')
      tmp.result1 <- transform(tmp.result1,min=fexp+min,
                               max=fexp+max)
    }
    
    tmp.result2 <- subset(bmk,!(fname %in% factorExp$fname))
    if(nrow(tmp.result2)>0){
      tmp.result2.sec <- subset(tmp.result2,substr(fname,1,2)=='ES')
      if(nrow(tmp.result2.sec)>0){
        if(any(factorExp$fname=='sectorall')){
          tmp.result2.sec <- transform(tmp.result2.sec,
                                       min=fexp*(1+factorExp[factorExp$fname=='sectorall','min']),
                                       max=fexp*(1+factorExp[factorExp$fname=='sectorall','max']))
        }else{
          warning('miss sectorall argument!')
          tmp.result2.sec <- transform(tmp.result2.sec,
                                       min=fexp*(1-0.05),
                                       max=fexp*(1+0.05))
        }
      }
      
      tmp.result2.f <- subset(tmp.result2,substr(fname,1,2)!='ES')
      if(nrow(tmp.result2.f)>0){
        warning(paste('miss ',unique(tmp.result2.f$fname),' exposure setting!'))
        tmp.result2.f <- transform(tmp.result2.f,
                                   min=-0.01,
                                   max=100)
      }
      tmp.result2 <- rbind(tmp.result2.sec,tmp.result2.f)
    }
    
    result <- rbind(tmp.result1,tmp.result2)
    result <- result[,c("date","fname","min","max")]
    result <- dplyr::arrange(result,date)
  }

  
  return(result)
}



# getStockWgtLimit
# 
# inner function, get stock's weight limit in optimization function.
# wgtLimit <- getStockWgtLimit(TS,wgtSet,sectorAttr)
getStockWgtLimit <- function(TS,wgtSet,sectorAttr){
  result <- transform(TS,min=wgtSet[wgtSet$ID=='wgtall','min'],
                  max=wgtSet[wgtSet$ID=='wgtall','max'])
  
  if(any(substr(wgtSet$ID,1,2)=='EQ')){
    tmp <- result[result$stockID %in% wgtSet$ID,c('date','stockID')]
    tmp <- dplyr::left_join(tmp,wgtSet,by=c('stockID'='ID'))
    result <- result[!(result$stockID %in% wgtSet$ID),]
    result <- rbind(result,tmp)
  }
  if(any(substr(wgtSet$ID,1,2)=='ES')){
    TSS <- gf.sector(TS,sectorAttr)
    TSS <- TSS[,c('date','stockID','sector')]
    result <- dplyr::left_join(result,TSS,by=c('date','stockID'))
    
    tmp <- result[result$sector %in% wgtSet$ID,c('date','stockID','sector')]
    tmp <- dplyr::left_join(tmp,wgtSet,by=c('sector'='ID'))
    tmp <- tmp[,c("date","stockID","min","max")]
    result <- result[!(result$sector %in% wgtSet$ID),c('date','stockID','min','max')]
    result <- rbind(result,tmp)
  }
  result <- dplyr::left_join(TS,result,by=c('date','stockID'))
  return(result)
}


# getnewTSFtotwgt
#
# inner function
getnewTSFtotwgt <- function(TSF,totwgt,boxConstr){
  for(i in 1:nrow(boxConstr)){
    indexComp <- data.frame(stockID=getIndexComp(boxConstr$indexID[i],unique(TSF$date)),value=c(1))
    colnames(indexComp) <- c('stockID',boxConstr$indexID[i])
    indexComp$stockID <- as.character(indexComp$stockID)
    TSF <- dplyr::left_join(TSF,indexComp,by='stockID')
  }
  TSF[is.na(TSF)] <- 0
  colnames(boxConstr) <- c("fname","min","max")
  totwgt <- rbind(totwgt,boxConstr)
  rownames(totwgt) <- totwgt$fname
  return(list(TSF=TSF,totwgt=totwgt))
}











# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  Performance & Risk Attribution -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' calculate factor exposure
#' 
#' @export
exposure.TSWF <- function(TSWF) {
  # factorNames <- setdiff(names(TSWF),c("stockID","date","date_end","periodrtn","wgt","sector"))
  factorNames <- guess_factorNames(TSWF)
  
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
  if(!is.null(sectorAttr)){
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
  regdata <- reg.TSFR(TSFR = TSFR,regType = "glm",sectorAttr = sectorAttr,secRtnOut = TRUE)
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
  regdata <- reg.TSFR(TSFR = TSFR,regType = "glm",sectorAttr = sectorAttr,secRtnOut = FALSE)
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


