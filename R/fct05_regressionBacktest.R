

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  backtesting with 'regression' method -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

# ---------------------  ~~ database update  --------------
#' lcdb_regtables
#' 
#' build and update local database's regression result tables
#' @name lcdb_regtables
#' @rdname lcdb_regtables
#' @param begT is begin date
#' @param endT is end date
#' @param FactorLists see example in \code{\link{buildFactorLists}}.
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
  if(RSQLite::dbExistsTable(con, "Reg_FactorRtn")) RSQLite::dbRemoveTable(con,'Reg_FactorRtn')
  if(RSQLite::dbExistsTable(con, "Reg_Residual")) RSQLite::dbRemoveTable(con,'Reg_Residual')
  if(RSQLite::dbExistsTable(con, "Reg_RSquare")) RSQLite::dbRemoveTable(con,'Reg_RSquare')
  
  RSQLite::dbGetQuery(con,"CREATE TABLE Reg_FactorRtn (
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
  RSQLite::dbGetQuery(con,"CREATE UNIQUE INDEX IX_Reg_FactorRtn ON Reg_FactorRtn(date,fname)")
  
  RSQLite::dbGetQuery(con,"CREATE TABLE Reg_Residual (
              date int  NOT NULL,
             stockID TEXT NOT NULL,
             res_d1 decimal(10,8) NULL,
             res_w1 decimal(10,8) NULL,
             res_w2 decimal(10,8) NULL,
             res_m1 decimal(10,8) NULL)")
  RSQLite::dbGetQuery(con,"CREATE UNIQUE INDEX IX_Reg_Residual ON Reg_Residual(date,stockID)")
  
  RSQLite::dbGetQuery(con,"CREATE TABLE Reg_RSquare (
             date int  NOT NULL,
             rsquare_d1 decimal(10,4) NULL,
             rsquare_w1 decimal(10,4) NULL,
             rsquare_w2 decimal(10,4) NULL,
             rsquare_m1 decimal(10,4) NULL)")
  RSQLite::dbGetQuery(con,"CREATE UNIQUE INDEX IX_Reg_RSquare ON Reg_RSquare(date)")
  
  
  if(missing(begT)) begT <- as.Date('2005-01-04')
  if(missing(endT)){
    endT <- RSQLite::dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
    endT <- trday.offset(intdate2r(endT),by = months(-1))
  }
  RSQLite::dbDisconnect(con)
  dates <- getRebDates(begT,endT,rebFreq = 'day')
  dates <- split(dates,cut(dates,'month'))
  plyr::l_ply(dates,lcdb.subfun.regtables,FactorLists,.progress = plyr::progress_text(style=3))

  return('Done!')
}


#inner function
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
  RSQLite::dbWriteTable(con,'Reg_FactorRtn',transform(fRtn,date=rdate2int(date)),overwrite=FALSE,append=TRUE,row.names=FALSE)
  RSQLite::dbWriteTable(con,'Reg_Residual',transform(res,date=rdate2int(date)),overwrite=FALSE,append=TRUE,row.names=FALSE)
  RSQLite::dbWriteTable(con,'Reg_RSquare',transform(RSquare,date=rdate2int(date)),overwrite=FALSE,append=TRUE,row.names=FALSE)
  RSQLite::dbDisconnect(con)
}



#' @rdname lcdb_regtables
#' 
#' @export
lcdb.update.RegTables <- function(begT,endT,FactorLists){
  con <- db.local()
  if(missing(begT)){
    tmp.begT <- RSQLite::dbGetQuery(con,"select max(date) from Reg_RSquare")[[1]]
    tmp.begT <- trday.offset(intdate2r(tmp.begT),lubridate::days(1))
  }
  if(missing(endT)){
    endT <- RSQLite::dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
    endT <- trday.offset(intdate2r(endT),by = months(-1))
  }
  if(begT>endT) return('Done!')
  
  tmp.dates <- RSQLite::dbGetQuery(con,"select min(date) 'mindate',max(date) 'maxdate' from Reg_RSquare")
  tmp.dates <- transform(tmp.dates,mindate=intdate2r(mindate),maxdate=intdate2r(maxdate))
  if(begT<= tmp.dates$maxdate & endT>= tmp.dates$mindate){
    RSQLite::dbGetQuery(con, paste("delete from Reg_FactorRtn WHERE date>=",rdate2int(begT),
                          " and date<=",rdate2int(endT)))
    RSQLite::dbGetQuery(con, paste("delete from Reg_RSquare WHERE date>=",rdate2int(begT),
                          " and date<=",rdate2int(endT)))
    RSQLite::dbGetQuery(con, paste("delete from Reg_Residual WHERE date>=",rdate2int(begT),
                          " and date<=",rdate2int(endT)))
  }
  RSQLite::dbDisconnect(con)
  
  dates <- getRebDates(begT,endT,rebFreq = 'day')
  dates <- split(dates,cut(dates,'month'))
  plyr::l_ply(dates,lcdb.subfun.regtables,FactorLists,.progress = plyr::progress_text(style=3))
  return('Done!')
}





# ---------------------  ~~ Backtesting  --------------


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
#' @return return a list, contains dataframes of TSFR,frtn, residual and Rsquare.
#' @export
#' @author Ruifei.yin
#' @examples
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-09-30'))
#' TS <- getTS(RebDates,indexID = 'EI000985')
#' factorIDs <- c("F000002","F000006","F000008")
#' FactorLists <- buildFactorLists_lcfs(factorIDs,factorRefine=refinePar_default("robust"))
#' FactorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.NP_YOY",
#'                   factorPar=list(),
#'                   factorDir=1),
#'   factorRefine=refinePar_default("robust"))
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
  factorNames <- guess_factorNames(TSFR,no_factorname = c('glm_wgt','sector'),is_factorname = 'factorscore')
  
  if(!is.null(sectorAttr)){
    if(identical(sectorAttr,"existing")){
      secNames <- unique(TSFR$sector)
      factorNames <- setdiff(factorNames,secNames)
    }else{
      TSFR <- gf_sector(TSFR,sectorAttr)
    }
  }
  
  if(regType=='glm'){ #get glm_wgt data
    if(!('glm_wgt' %in% colnames(TSFR))){
      if(glm_wgt=="sqrtFV"){
        TSw <- getTSF(TSFR[,c('date','stockID')],factorFun="gf_lcfs",factorPar=list(factorID='F000001'),
                      factorRefine=setrefinePar(refinePar_default(type="none",sectorAttr = NULL),na_method="median"))
        TSw <- transform(TSw,factorscore=sqrt(factorscore))
        TSw <- dplyr::rename(TSw,glm_wgt=factorscore)
        TSFR <- merge.x(TSFR,TSw,by =c("date","stockID"))
      }else if(glm_wgt=="res"){
        
      }
    }
  }  
  
  if(is.null(sectorAttr)){
    re <- lm_NPeriod(TSFR,y='periodrtn',x=factorNames,lmtype = regType)
  }else{
    re <- lm_NPeriod(TSFR,y='periodrtn',x=factorNames,lmtype = regType,secIN =TRUE)
  }
  
  
  fRtn <- re$coef[,c('date','term','estimate','statistic')]
  colnames(fRtn) <- c('date','fname','frtn','Tstat')
  fRtn <- fRtn[fRtn$fname!='(Intercept)',]
  if(secRtnOut==FALSE){
    fRtn <- dplyr::filter(fRtn,!substr(fname,1,2)=='ES')
  }
  
  res <- re$resd[,c('date','stockID','res')]
  
  RSquare <- re$rsq
  colnames(RSquare) <- c('date','RSquare')
    
  # # pure-factor-port wgt
  # tmp.x <- as.matrix(tmp.tsfr[,c(factorNames)])
  # tmp.w <- as.matrix(tmp.tsfr[,"glm_wgt"])
  # tmp.w <- diag(c(tmp.w),length(tmp.w))
  # tmp.f <- solve(crossprod(tmp.x,tmp.w) %*% tmp.x) %*% crossprod(tmp.x,tmp.w)
  # pfpwgt <- rbind(pfpwgt,data.frame(date=dates$date[i],stockID=tmp.tsfr$stockID,t(tmp.f)))
  
  result <- list(TSFR=TSFR_save,fRtn=fRtn,res=res,RSquare=RSquare)
  return(result)
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



#' factorlists recommend
#' 
#' @param indexID is index ID.
#' @export
#' @examples 
#' ##################get the recommended factorLists of last 12 months########## 
#' begT <- Sys.Date()-lubridate::years(1)
#' endT <- Sys.Date()-1
#' indexID <- 'EI000905'
#' FactorLists <- reg.factorlists_recommend(indexID,begT,endT)
#' ##################get the recommended factorLists of last 4 weeks########## 
#' begT <- Sys.Date()-months(1)
#' endT <- Sys.Date()-1
#' indexID <- 'EI000985'
#' FactorLists <- reg.factorlists_recommend(indexID,begT,endT,rebFreq = "week")
reg.factorlists_recommend <- function(indexID,begT,endT,rebFreq = "month",rsqBar=1,forder){
  RebDates <- getRebDates(begT,endT,rebFreq)
  
  TS <- getTS(RebDates,indexID)
  factorIDs <- CT_FactorLists()$factorID
  tmp <- buildFactorLists_lcfs(factorIDs,factorRefine=refinePar_default("robust"))
  FactorLists <- buildFactorLists(
    buildFactorList(factorFun="gf.ln_mkt_cap",
                    factorPar=list(),
                    factorDir=-1),
    buildFactorList(factorFun="gf.NP_YOY",
                    factorPar=list(),
                    factorDir=1),
    buildFactorList(factorFun="gf.G_MLL_Q",
                    factorPar=list(),
                    factorDir=1),
    buildFactorList(factorFun="gf.G_OCF_Q",
                    factorPar=list(),
                    factorDir=1),
    buildFactorList(factorFun="gf.G_scissor_Q",
                    factorPar=list(),
                    factorDir=1),
    buildFactorList(factorFun="gf.ROE_ttm",
                    factorPar=list(),
                    factorDir=1),
    factorRefine=refinePar_default("robust"))
  FactorLists <- c(tmp,FactorLists)
  TSF <- getMultiFactor(TS,FactorLists)
  TSFR <- na.omit(getTSR(TSF))
  
  #factor select 
  re <- reg.factor_select(TSFR,sectorAttr = NULL,forder)
  result <- re$result
  result <- result[c(TRUE,result$rsqPct[-1]>rsqBar),]
  TSFR <- TSFR[,c("date","date_end","stockID",result$fname,"periodrtn")]
  FactorLists <- FactorLists[sapply(FactorLists,function(x) x$factorName %in% result$fname)]
  re <- list(FactorLists=FactorLists,result=result,TSFR=TSFR)
  return(re)
}







#' factor_select
#' 
#' \bold{reg.factor_select} select alpha or risk factors using regression method.
#' \bold{factor_VIF} caculate factor's VIF.
#' @name factor_select
#' @rdname factor_select
#' @param TSFR a \bold{TSFR} object.
#' @param forder self defined factor importance order,can be missing,can be set of character or number,length of \code{forder} can be shorter than factors.
#' @export
#' @examples 
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-09-30'))
#' TS <- getTS(RebDates,indexID = 'EI000905')
#' factorIDs <- c("F000006","F000008","F000012","F000015",
#' "F000016")
#' tmp <- buildFactorLists_lcfs(factorIDs,factorRefine=refinePar_default("robust"))
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
#'   buildFactorLists_lcfs(factorIDs,factorRefine=refinePar_default("robust")))
#' factorLists <- c(tmp,factorLists)
#' TSF <- getMultiFactor(TS,FactorLists = factorLists)
#' ----------------------VIF----------------------
#' VIF <- factor_VIF(TSF)
#' 
#' TSFR <- getTSR(TSF)
#' re <- reg.factor_select(TSFR)
#' re <- reg.factor_select(TSFR,sectorAttr=NULL)
#' nstock <- length(factorLists)
#' re <- reg.factor_select(TSFR,forder=sample(1:nstock,nstock))
reg.factor_select <- function(TSFR,sectorAttr=defaultSectorAttr(),forder){
  cols <- colnames(TSFR)
  fname <- guess_factorNames(TSFR)
  
  #sector only
  result <- data.frame()
  if(!is.null(sectorAttr)){
    TSFR <- gf_sector(TSFR,sectorAttr = sectorAttr)
    secNames <- unique(TSFR$sector)
    secrs <- reg.TSFR(TSFR[,c("date","date_end","stockID",secNames,'sector',"periodrtn")],sectorAttr = 'existing')[[4]]
    result <- data.frame(fname='sector',rsquare=mean(secrs$RSquare,na.rm = TRUE), 
                         frtn=NA,fttest=NA,pttest=NA,tag='risk')
    TSF <- TSFR[,c('date','stockID',fname,secNames,'sector')]
  } else {
    TSF <- TSFR[,c('date','stockID',fname)]
  }
  
  
  if(!missing(forder)){
    if(typeof(forder)=='character'){
      if(length(forder)==length(fname)){
        fname <- forder
      }else{
        fname <- c(forder,setdiff(fname,forder))
      }
    }else{
      if(length(forder)==length(fname)){
        fname <- fname[forder]
      }else{
        fname <- c(fname[forder],fname[setdiff(seq(1:length(fname)),forder)])
      }
    }
    
  }
  
  selectf <- NULL
  while(length(setdiff(fname,selectf))>0){
    rsquare <- data.frame()
    frtn <- data.frame()
    res <- data.frame()
    if(missing(forder)){
      fnameset <- setdiff(fname,selectf)
    }else{
      if(length(forder)==length(fname)){
        fnameset <- setdiff(fname,selectf)[1]
      }else{
        if(length(selectf)<length(forder)){
          fnameset <- setdiff(fname,selectf)[1]
        }else{
          fnameset <- setdiff(fname,selectf)
        }
      }
    }
    
    for(i in fnameset){
      if(is.null(sectorAttr)){
        tmp.TSF <- TSF[,c("date","stockID",union(selectf,i))]
        if(ncol(tmp.TSF)>3){
          tmp.TSF <- factor_orthogon_single(tmp.TSF,y=i,sectorAttr = NULL)
        }
        tmp.TSFR <- dplyr::left_join(tmp.TSF,TSFR[,c("date","date_end","stockID","periodrtn")],
                                     by=c("date","stockID"))
        frs <- reg.TSFR(tmp.TSFR,sectorAttr = NULL)
      }else{
        tmp.TSF <- TSF[,c("date","stockID",union(selectf,i),secNames,'sector')]
        tmp.TSF <- factor_orthogon_single(tmp.TSF,y=i,sectorAttr = 'existing')
        tmp.TSFR <- dplyr::left_join(tmp.TSF,TSFR[,c("date","date_end","stockID","periodrtn")],
                                     by=c("date","stockID"))
        frs <- reg.TSFR(tmp.TSFR,sectorAttr = 'existing')
      }
      tmp.res <- data.frame(tmp.TSF[,c('date','stockID')],fname=i,res=tmp.TSF[,i])
      
      tmp <- data.frame(frs$RSquare,fname=i)
      rsquare <- rbind(rsquare,tmp)
      res <- rbind(res,tmp.res)
      frtn <- rbind(frtn,data.frame(frs$fRtn))
    }
    rsquare <- rsquare %>% dplyr::group_by(fname) %>%
      dplyr::summarise(rsquare = mean(RSquare,trim = 0.025,na.rm = TRUE)) %>% 
      dplyr::arrange(desc(rsquare)) %>% dplyr::slice(1)
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
  result <- transform(result,fname=as.character(fname),
                      rsquare=round(rsquare,digits = 3),
                      frtn=round(frtn,digits = 4),
                      fttest=round(fttest,digits = 2),
                      pttest=round(pttest,digits = 3),
                      tag=as.character(tag),
                      rsqPct=round((rsquare/dplyr::lag(rsquare)-1)*100,digits = 1))
  TSFR <- TSFR[,cols]
  return(list(result=result,TSFR=TSFR))
}



#' @rdname factor_select
#' @param TSF is a \bold{TSF} object.
#' @param testf is test factor name, can be missing.
#' @param sectorAttr a sector-attribute list or NULL or 'existing'. If a list, regress with the sectors specified by sectorAttr;if "existing", use the existing sector data in TSF(Make sure they are already exist!); if null, not regress with sectors.
#' @return data frame of VIF and residual.
#' @export
factor_VIF <- function(TSF,sectorAttr=defaultSectorAttr()){
  fname <- guess_factorNames(TSF,is_factorname = "factorscore")
  if(!is.null(sectorAttr) & !identical(sectorAttr,"existing")){
    TSF <- gf_sector(TSF,sectorAttr = sectorAttr)
  }
  
  result <- data.frame()
  for(j in 1:length(fname)){
    fname_j <- fname[j]
    if(is.null(sectorAttr)){
      re <- lm_NPeriod(TSF,fname_j,x=setdiff(fname,fname_j))
    }else{
      re <- lm_NPeriod(TSF,fname_j,x=setdiff(fname,fname_j),secIN = TRUE)
    } 
    
    VIF <- re$rsq
    VIF <- transform(VIF,vif=1/(1-rsq),
                     fname=fname_j)
    result <- rbind(result,VIF)
  }
  result <- result[,c("fname","date","vif")]
  return(result)
}




#' @rdname factor_select
#' 
#' @export
factor_orthogon_single <- function(TSF,y,x,sectorAttr=defaultSectorAttr()){
  cols <- colnames(TSF)
  fname <- guess_factorNames(TSF,is_factorname = "factorscore",silence=TRUE)
  
  if(length(fname)==1 & is.null(sectorAttr)){
    stop('NO x variable!')
  }
  if(!(y %in% fname)){
    stop('y not in TSF!')
  }
  if(missing(x)){
    x <- setdiff(fname,y)
  }
  if(!is.null(sectorAttr) & !identical(sectorAttr,"existing")){
    TSF <- gf_sector(TSF,sectorAttr = sectorAttr)
  }

  if(is.null(sectorAttr)){
    re <- lm_NPeriod(TSF,y,x)
  }else{
    re <- lm_NPeriod(TSF,y,x,secIN = TRUE)
  }
  
  res <- re$resd
  res$fitted <- NULL
  colnames(res) <- c('date','stockID',y)
  TSF <- dplyr::left_join(TSF[,colnames(TSF)!=y],res,by =c("date","stockID"))
  return(TSF[,cols])
}


#' @rdname factor_select
#' 
#' @export
factor_orthogon <- function(TSF,forder,sectorAttr=defaultSectorAttr()){
  cols <- colnames(TSF)
  fname <- guess_factorNames(TSF,is_factorname = "factorscore")
  if(missing(forder)){
    forder <- fname
  }
  if(is.numeric(forder)){
    forder <- fname[forder]
  }
  if(!is.null(sectorAttr) & !identical(sectorAttr,"existing")){
    TSF <- gf_sector(TSF,sectorAttr = sectorAttr)
  }
  sectorAttr_ <- if(is.null(sectorAttr)) NULL else "existing"
  if(!is.null(sectorAttr)){ # forder[1]
    TSF <- factor_orthogon_single(TSF, y = forder[1], x=NULL,sectorAttr = "existing")
  }
  for(j in 2:length(forder)){ # forder[2:length]
    TSF <- factor_orthogon_single(TSF, y = forder[j], x=forder[1:(j-1)],sectorAttr = sectorAttr_)
  }
  return(TSF[,cols])
}





#inner function 
lm_NPeriod <- function(data,y,x,lmtype=c('lm','glm'),secIN=FALSE){
  check.colnames(data,c('date','stockID'))
  lmtype <- match.arg(lmtype)
  
  rsq <- data.frame()
  coef <- data.frame()
  resd <- data.frame()
  if(secIN){
    check.colnames_sectorfs(data)
    secdf <- data %>% dplyr::group_by(date,sector) %>% 
      dplyr::summarise(n=1) %>% dplyr::ungroup()
    secdf <- reshape2::dcast(secdf,date~sector,fill = 0,value.var = 'n')
    secNum <- ncol(secdf)-1
    for(i in 1:nrow(secdf)){
      secdf$rowtag[i] <- strtoi(paste(secdf[i,2:(1+secNum)],collapse = ''),base=2)
    }
    
    while(nrow(secdf)>0){
      tmp.secdf <- secdf[secdf$rowtag==max(secdf$rowtag),]
      tmp.secdf$rowtag <- NULL
      tmp.secdf <- tmp.secdf[,c(TRUE,colSums(tmp.secdf[,-1])>0)]
      secNames <- colnames(tmp.secdf)[-1]
      tmp.data <- data[data$date %in% tmp.secdf$date,]
      
      if(length(secNames)>1){
        fml <- formula(paste(y," ~ ", paste(c(x,secNames), collapse= "+"),"-1",sep=''))
      }else{
        fml <- formula(paste(y," ~ ", paste(x, collapse= "+"),sep=''))
      }
      
      if(lmtype=='lm'){
        models <- tmp.data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = .))
      }else{
        models <- tmp.data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = .,weights=glm_wgt))
      }
      rsq <- rbind(rsq,dplyr::summarise(models,date=date,rsq = summary(mod)$r.squared))
      coef <- rbind(coef,data.frame(models %>% broom::tidy(mod)))
      tmp.resd <- models %>% broom::augment(mod)
      resd <- rbind(resd,cbind(tmp.data[,c('date','stockID')],tmp.resd[,c('.fitted','.resid')]))
      
      secdf <- secdf[secdf$rowtag<max(secdf$rowtag),]
    }
    rsq <- dplyr::arrange(rsq,date)
    coef <- dplyr::arrange(coef,date,term)
    resd <- merge.x(data[,c("date","stockID")],resd,by=c("date","stockID"))
    
  }else{
    fml <- formula(paste(y," ~ ", paste(x, collapse= "+"),sep=''))
    if(lmtype=='lm'){
      models <- data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = .))
    }else{
      models <- data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = .,weights=glm_wgt))
    }
    rsq <- dplyr::summarise(models,date=date,rsq = summary(mod)$r.squared)
    coef <- models %>% broom::tidy(mod)
    suppressWarnings(resd <- models %>% broom::augment(mod))
    resd <- cbind(data[,c('date','stockID')],resd[,c('.fitted','.resid')])
  }
  rsq <- as.data.frame(rsq)
  colnames(resd) <- c('date','stockID','fitted','res')
  return(list(rsq=rsq,coef=coef,resd=resd))
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
table.reg.fRtn <- function(reg_results,includeVIF=FALSE){
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

  re <- dplyr::left_join(rtnsum,tstat,by='fname')
  if(includeVIF){
    VIF <- factor_VIF(TSF,sectorAttr = NULL)
    VIF <- VIF %>% dplyr::group_by(fname) %>% dplyr::summarise(vif=mean(vif)) %>% ungroup()
    re <- dplyr::left_join(re,VIF,by='fname')
  }
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
  Nperiod <- nrow(RSquare)

  if(Nperiod>12){
    RSquare <- xts::xts(RSquare[,-1],RSquare[,1])
    colnames(RSquare) <- c('RSquare')
    tmp <- zoo::rollmean(RSquare,12,align='right')
    tmp <- data.frame(date=zoo::index(tmp),RSquareMA=zoo::coredata(tmp))
    RSquare <- data.frame(time=time(RSquare),zoo::coredata(RSquare))
    ggplot(RSquare, aes(x=time, y=RSquare))+geom_line(color="#D55E00") +
      ggtitle('RSquare(with MA series)') +geom_line(data=tmp,aes(x=date,y=RSquare),size=1,color="#56B4E9")
  }else{
    ggplot(RSquare, aes(x=date, y=RSquare))+geom_line(color="#D55E00") + ggtitle('RSquare')
  }

}


#' @rdname regression_result_summary
#' 
#' @export
MC.chart.reg.corr <- function(reg_results){
  fRtn <- reg_results$fRtn
  
  fRtn <- reshape2::dcast(fRtn,date~fname,value.var = 'frtn')
  fRtn.cor <- cor(as.matrix(fRtn[,-1]))
  ggplot.corr(fRtn.cor)
  
}







#' factor return,covariance and delta
#' 
#' calculate factor return, factor covariance and residual variance.
#' @name f_rtn_cov_delta
#' @rdname f_rtn_cov_delta
#' @param RebDates is date set, can be missing.
#' @param fname is factor names, can be missing.
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param rolling default value is \code{FALSE}, if value is \code{TRUE} means the data period is \code{nwin} forward.
#' @param rtntype is method to caculate factor return,\bold{mean} means average of historical data,\bold{forcast} means forcast factor return based on historical data,it may take a while,the forcast method come from package \code{\link[forecast]{ets}}.
#' @param covtype means type of caculating covariance,\bold{shrink} can see example in \code{\link[nlshrink]{nlshrink_cov}},simple see \code{\link{cov}}.
#' @param nwin is rolling windows forward.
#' @param reg_results see examples in \code{\link{reg.TSFR}}
#' @return a data frame of factors' return .
#' @examples 
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-08-31'))
#' fname <- c("NP_YOY","PB_mrq_","disposition_","ln_mkt_cap_")
#' fRtn <- getfRtn(RebDates,fname,reg_results=reg_results)
#' fCov <- getfCov(RebDates,fname,reg_results=reg_results)
#' Delta <- getDelta(RebDates,dure=months(1),rolling=FALSE,nwin=24,reg_results)
#' rtn_cov_delta <- f_rtn_cov_delta(reg_results=reg_results)
#' @export
f_rtn_cov_delta <- function(RebDates,fname,dure=months(1),rolling=FALSE,rtntype=c('mean','forcast'),
                            covtype=c('shrink','simple'),nwin=24,reg_results) {
  rtntype <- match.arg(rtntype)
  covtype <- match.arg(covtype)
  
  fRtn <- getfRtn(RebDates,fname,dure=dure,rolling=rolling,rtntype=rtntype,
                      nwin=nwin,reg_results)
  fCov <- getfCov(RebDates,fname,dure=dure,rolling=rolling,covtype=covtype,
                              nwin=nwin,reg_results)
  Delta <- getDelta(RebDates,dure=dure,rolling=rolling,nwin=nwin,reg_results)
  
  re <- list(fRtn=fRtn,fCov=fCov,Delta=Delta)
  return(re)
}




# inner function
get_frtn_res <- function(begT,endT,dure,reg_results,outtype=c('frtn','res')){
  if(missing(begT)) begT <- as.Date('1990-01-01')
  if(missing(endT)) endT <- as.Date('2100-01-01')
  outtype <- match.arg(outtype)
  
  if(missing(reg_results)){
    if(dure==lubridate::days(1)){
      dbname <- 'd1'
    }else if(dure==lubridate::weeks(1)){
      dbname <- 'w1'
    }else if(dure==lubridate::weeks(2)){
      dbname <- 'w2'
    }else if(dure==months(1)){
      dbname <- 'm1'
    }
    dbname <- paste(outtype,dbname,sep = '_')
    
    con <- db.local()
    if(outtype=='frtn'){
      qr <- paste("SELECT date,fname,",dbname," 'frtn'
                FROM Reg_FactorRtn where date>=",rdate2int(begT),
                  " and date<=",rdate2int(endT))
    }else if(outtype=='res'){
      qr <- paste("SELECT date,stockID,",dbname," 'res'
                FROM Reg_Residual where date>=",rdate2int(begT),
                  " and date<=",rdate2int(endT))
    }

    re <- dbGetQuery(con,qr)
    re <- transform(re,date=intdate2r(date))
    dbDisconnect(con)
  }else{
    if(outtype=='frtn'){
      re <- reg_results$fRtn
      re <- dplyr::select(re,-Tstat)
      re <- dplyr::filter(re,date>=begT,date<=endT)
    }else if(outtype=='res'){
      re <- reg_results$res
      re <- dplyr::filter(re,date>=begT,date<=endT)
    }

  }
  
  return(re)
}


#' @rdname f_rtn_cov_delta
#' 
#' @export
getfRtn <- function(RebDates,fname,dure=months(1),rolling=FALSE,rtntype=c('mean','forcast'),
                    nwin=24,reg_results){
  rtntype <- match.arg(rtntype)
  
  if(missing(reg_results)){
    rtndata <- get_frtn_res(dure=dure)
  }else{
    rtndata <- get_frtn_res(reg_results=reg_results)
  }
  
  if(missing(fname)){
    fname <- unique(rtndata$fname)
  }else{
    missf <- setdiff(fname,unique(rtndata$fname))
    if(length(missf)>0){
      warning(paste('missing factor:',paste(missf,collapse=',')),call. = FALSE)
    }
    rtndata <- rtndata[rtndata$fname %in% fname,]
  }
  rtndata$tmpdate <- trday.offset(rtndata$date,dure)
  rtndata <- reshape2::dcast(rtndata,date+tmpdate~fname,value.var = 'frtn')
  
  missingtag <- 0
  if(missing(RebDates)){
    RebDates <- trday.offset(max(rtndata$date),dure)
    missingtag <- 1
  }
  
  result <- data.frame()

  for(i in RebDates){
    tmp.rtndata <- rtndata %>% dplyr::filter(tmpdate<=i) %>% dplyr::select(-date,-tmpdate)
    if(rolling){
      tmp.rtndata <- tail(tmp.rtndata,nwin)
    }
    
    if(rtntype=='mean'){
      tmp <- colMeans(tmp.rtndata)
      tmp <- data.frame(date=as.Date(i,origin='1970-01-01'),
                         fname=names(tmp),
                         frtn=unname(tmp))
      result <- rbind(result,tmp)
    }else if(rtntype=='forcast'){
      for(j in 1:ncol(tmp.rtndata)){
        myts <- ts(data= tmp.rtndata[,j])
        fit <- forecast::ets(myts)
        fit.forcast <- forecast::forecast(fit, 1)
        tmp <- data.frame(date=as.Date(i,origin='1970-01-01'),
                          fname=colnames(tmp.rtndata)[j],
                          frtn=as.numeric(fit.forcast$mean))
        result <- rbind(result,tmp)
      }
    }
    
  }
  result <- transform(result,fname=as.character(fname))
  if(missingtag){
    result$date <- NULL
  }
  return(result)
}


#' @rdname f_rtn_cov_delta
#' 
#' @export
getfCov <- function(RebDates,fname,dure=months(1),rolling=FALSE,covtype=c('shrink','simple'),
                    nwin=24,reg_results){
  covtype <- match.arg(covtype)
  
  if(missing(reg_results)){
    rtndata <- get_frtn_res(dure=dure)
  }else{
    rtndata <- get_frtn_res(reg_results=reg_results)
  }
  
  if(missing(fname)){
    fname <- unique(rtndata$fname)
  }else{
    missf <- setdiff(fname,unique(rtndata$fname))
    if(length(missf)>0){
      warning(paste('missing factor:',paste(missf,collapse=',')),call. = FALSE)
    }
    rtndata <- rtndata[rtndata$fname %in% fname,]
  }
  rtndata$tmpdate <- trday.offset(rtndata$date,dure)
  rtndata <- reshape2::dcast(rtndata,date+tmpdate~fname,value.var = 'frtn')
  
  missingtag <- 0
  if(missing(RebDates)){
    RebDates <- trday.offset(max(rtndata$date),dure)
    missingtag <- 1
  }
  
  result <- data.frame()
  
  for(i in RebDates){
    tmp.rtndata <- rtndata %>% dplyr::filter(tmpdate<=i) %>% dplyr::select(-date,-tmpdate)
    if(rolling){
      tmp.rtndata <- tail(tmp.rtndata,nwin)
    }
    
    if(covtype=='simple'){
      tmp <- cov(tmp.rtndata)
      tmp <- data.frame(date=as.Date(i,origin='1970-01-01'),tmp)
      result <- rbind(result,tmp)
    }else if(covtype=='shrink'){
      tmp <- data.frame(nlshrink::nlshrink_cov(as.matrix(tmp.rtndata)))
      colnames(tmp) <- colnames(tmp.rtndata)
      tmp <- data.frame(date=as.Date(i,origin='1970-01-01'),tmp)
      result <- rbind(result,tmp)

    }
    
  }
  if(missingtag){
    result$date <- NULL
  }
  return(result)
}




#' @rdname f_rtn_cov_delta
#'
#' @export
getDelta <- function(RebDates,dure=months(1),rolling=FALSE,nwin=24,reg_results){
  
  if(missing(reg_results)){
    resdata <- get_frtn_res(dure=dure,outtype = 'res')
  }else{
    resdata <- get_frtn_res(reg_results=reg_results,outtype = 'res')
  }
  resdata$tmpdate <- trday.offset(resdata$date,dure)
  if(rolling){
    resdata <- reshape2::dcast(resdata,date+tmpdate~stockID,value.var = 'res')
  }
  
  missingtag <- 0
  if(missing(RebDates)){
    RebDates <- trday.offset(max(resdata$date),dure)
    missingtag <- 1
  }
  
  result <- data.frame()
  for(i in RebDates){
    tmp.resdata <- resdata %>% dplyr::filter(tmpdate<=i) %>% dplyr::select(-date,-tmpdate)
    if(rolling){
      tmp.resdata <- tail(tmp.resdata,nwin)
      tmp.resdata <- reshape2::melt(tmp.resdata,variable.name = "stockID", na.rm = TRUE,value.name = "res")
    }
    tmp <- tmp.resdata %>% dplyr::group_by(stockID) %>% dplyr::summarise(n =n(),var = var(res))
    
    if(rolling){
      tmp <- tmp %>% dplyr::filter(n>=nwin/3) %>% dplyr::select(-n)
    }else{
      tmp <- tmp %>% dplyr::filter(n>=3) %>% dplyr::select(-n)
    }

    tmp <- data.frame(date=as.Date(i,origin='1970-01-01'),tmp)
    result <- rbind(result,tmp)
    
  }
  result <- transform(result,stockID=as.character(stockID))
  if(missingtag){
    result$date <- NULL
  }
  return(result)
}




biasTest <- function(reg_results,portID){
  Fcov_Delta <- calcFcov_Delta(reg_results)
  Fcov <- Fcov_Delta$Fcov
  Delta <- Fcov_Delta$Delta
  
  if(!missing(portID)){ # get port by a indexID
    
  } else { # equal-wgt-port
    
  }
  
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
    TSWF <- gf_sector(TSWF,sectorAttr = sectorAttr)
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
#' alphaLists <- buildFactorLists_lcfs(c("F000012","F000008"),factorRefine=refinePar_default("robust"))
#' riskLists <- buildFactorLists_lcfs(c("F000002","F000006"),factorRefine=refinePar_default("robust"))
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
  TSWF <- gf_sector(TSWF,sectorAttr = sectorAttr)
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
#' alphaLists <- buildFactorLists_lcfs(c("F000012","F000008"),factorRefine=refinePar_default("robust"))
#' alphaLists <- c(tmp,alphaLists)
#' riskLists <- buildFactorLists_lcfs(c("F000002","F000006"),factorRefine=refinePar_default("robust"))
#' RA_tables <- getRAData(port,c(alphaLists,riskLists))
#' RA_tables <- getRAData(port,c(alphaLists,riskLists),bmk='EI000905')
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
  TSWF <- gf_sector(TSWF,sectorAttr = sectorAttr)
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
chart.RA.attr <- function(RA_tables){
  
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  utility functions ------------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



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




