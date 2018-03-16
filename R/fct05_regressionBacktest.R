

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
#' begT <- as.Date('2005-01-04')
#' endT <- as.Date('2009-12-31')
#' FactorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.SIZE"),
#'   buildFactorList(factorFun="gf.GROWTH"),
#'   buildFactorList(factorFun="gf.TRADING"),
#'   buildFactorList(factorFun="gf.EARNINGYIELD"),
#'   buildFactorList(factorFun="gf.VALUE"),
#'   buildFactorList(factorFun="gf.OTHER"))
#' lcdb.build.RegTables(begT,endT,FactorLists)
#' begT <- as.Date('2010-01-04')
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
    begT <- RSQLite::dbGetQuery(con,"select max(date) from Reg_RSquare")[[1]]
    begT <- trday.offset(intdate2r(begT),lubridate::days(1))
  }
  if(missing(endT)){
    endT <- RSQLite::dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
    endT <- trday.offset(intdate2r(endT),by = months(-1))
  }
  if(begT>endT) return('Done!')
  
  tmp.dates <- RSQLite::dbGetQuery(con,"select min(date) 'mindate',max(date) 'maxdate' from Reg_RSquare")
  tmp.dates <- transform(tmp.dates,mindate=intdate2r(mindate),maxdate=intdate2r(maxdate))
  if(begT<= tmp.dates$maxdate && endT>= tmp.dates$mindate){
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
#' FactorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.SIZE"),
#'   buildFactorList(factorFun="gf.GROWTH"),
#'   buildFactorList(factorFun="gf.TRADING"),
#'   buildFactorList(factorFun="gf.EARNINGYIELD"),
#'   buildFactorList(factorFun="gf.VALUE"),
#'   buildFactorList(factorFun="gf.OTHER"))
#' reg_results <- reg.TS(TS)
#' reg_results <- reg.TS(TS,FactorLists)
#' ----------------------------------------------------------
#' TSF <- getMultiFactor(TS,FactorLists)
#' TSFR <- getTSR(TSF)
#' reg_results <- reg.TSFR(TSFR)
reg.TSFR <- function(TSFR,regType=c('glm','lm'),glm_wgt=c("sqrtFV","res"),
                     sectorAttr=defaultSectorAttr(),secRtnOut=FALSE){
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  
  TSFRraw <- TSFR
  factorNames <- guess_factorNames(TSFR,no_factorname = c('glm_wgt','sector'),is_factorname = 'factorscore',silence=TRUE)
  
  if(!is.null(sectorAttr)){
    TSFR <- getSectorID(TS = TSFR,sectorAttr = sectorAttr,fillNA = TRUE)
  }
  
  if(regType=='glm'){ #get glm_wgt data
    if(!('glm_wgt' %in% colnames(TSFR))){
      if(glm_wgt=="sqrtFV"){
        TSw <- gf_cap(TSFR[,c('date','stockID')],var="float_cap",na_fill=TRUE)
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
  
  fRtn <- re$coef %>% dplyr::select(date,term,estimate,statistic) %>% 
    dplyr::rename(fname=term,frtn=estimate,Tstat=statistic) %>% 
    dplyr::filter(fname!='(Intercept)') %>% 
    dplyr::mutate(fname=ifelse(substr(fname,1,8)=='sectorES',stringr::str_replace(fname,'sectorES','ES'),fname))
  if(secRtnOut==FALSE){
    fRtn <- dplyr::filter(fRtn,substr(fname,1,2)!='ES')
  }
  
  res <- re$resd %>% dplyr::select(date,stockID,res) %>% dplyr::filter(!is.na(res))
  
  RSquare <- re$rsq %>% dplyr::rename(rsquare=rsq)
  
  # # pure-factor-port wgt
  # tmp.x <- as.matrix(tmp.tsfr[,c(factorNames)])
  # tmp.w <- as.matrix(tmp.tsfr[,"glm_wgt"])
  # tmp.w <- diag(c(tmp.w),length(tmp.w))
  # tmp.f <- solve(crossprod(tmp.x,tmp.w) %*% tmp.x) %*% crossprod(tmp.x,tmp.w)
  # pfpwgt <- rbind(pfpwgt,data.frame(date=dates$date[i],stockID=tmp.tsfr$stockID,t(tmp.f)))
  
  result <- list(TSFR=TSFRraw,fRtn=fRtn,res=res,RSquare=RSquare)
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
#' tmp <- buildFactorLists_lcfs(factorIDs,factorRefine=refinePar_default("scale"))
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
#'   factorRefine=refinePar_default("scale"))
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
    TSFR <- getSectorID(TSFR,sectorAttr = sectorAttr,fillNA = TRUE)
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
      dplyr::summarise(rsquare = mean(rsquare,trim = 0.025,na.rm = TRUE)) %>% 
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
  fname <- guess_factorNames(TSF,is_factorname = "factorscore",silence=TRUE)
  if(!is.null(sectorAttr)){
    TSF <- getSectorID(TSF,sectorAttr = sectorAttr,fillNA = TRUE)
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
factor_orthogon_single <- function(TSF,y,x,sectorAttr=defaultSectorAttr(),regType=c('lm','glm')){
  regType <- match.arg(regType)
  cols <- colnames(TSF)
  fname <- guess_factorNames(TSF,no_factorname="glm_wgt",is_factorname = "factorscore",silence=TRUE)
  
  if(length(fname)==1 && is.null(sectorAttr)){
    stop('NO x variable!')
  }
  if(!(y %in% fname)){
    stop('y not in TSF!')
  }
  if(missing(x)){
    x <- setdiff(fname,y)
  }
  if(!is.null(sectorAttr)){
    TSF <- getSectorID(TSF,sectorAttr = sectorAttr,fillNA = TRUE)
  }
  
  if(is.null(sectorAttr)){
    resd <- lm_NPeriod(TSF,y,x,lmtype=regType)
  }else{
    resd <- lm_NPeriod(TSF,y,x,secIN = TRUE,lmtype=regType)
  }
  
  resd <- resd$resd
  re <- TSF[,cols]
  re[,y] <- resd$res
  return(re)
}


#' @rdname factor_select
#' 
#' @export
factor_orthogon <- function(TSF,forder,sectorAttr=defaultSectorAttr(),regType=c('lm','glm')){
  regType <- match.arg(regType)
  cols <- colnames(TSF)
  fname <- guess_factorNames(TSF,is_factorname = "factorscore",silence=TRUE)
  if(missing(forder)){
    forder <- fname
  }
  if(is.numeric(forder)){
    forder <- fname[forder]
  }
  if(!is.null(sectorAttr)){
    TSF <- getSectorID(TSF,sectorAttr = sectorAttr,fillNA = TRUE)
  }
  sectorAttr_ <- if(is.null(sectorAttr)) NULL else "existing"
  if(!is.null(sectorAttr)){ # forder[1]
    TSF <- factor_orthogon_single(TSF, y = forder[1], x=NULL,sectorAttr = "existing",regType=regType)
  }
  for(j in 2:length(forder)){ # forder[2:length]
    TSF <- factor_orthogon_single(TSF, y = forder[j], x=forder[1:(j-1)],sectorAttr = sectorAttr_,regType=regType)
  }
  return(TSF[,cols])
}





# inner function
# if lmtpye=='glm', data must include 'gml_wgt' column.
lm_NPeriod <- function(data,y,x,lmtype=c('lm','glm'),secIN=FALSE,silence=FALSE){
  check.colnames(data,c('date','stockID'))
  lmtype <- match.arg(lmtype)
  
  TS <- data[,c('date','stockID')]
  data <- data[rowSums(is.na(data[,c(x,y),drop=FALSE]))==0,] # remove NA
  if(!silence && nrow(data)<nrow(TS)){
    warning("NAs found in x or y part!")
  }
  
  if(secIN){
    data$sector <- as.factor(data$sector)
    fml <- formula(paste(y," ~ ", paste(c(x,"sector"), collapse= "+"),"-1",sep=''))
  }else{
    fml <- formula(paste(y," ~ ", paste(x, collapse= "+"),sep=''))
  }
  
  if(lmtype=='lm'){
    models <- data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = . ,na.action = "na.exclude"))
  }else{
    models <- data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = .,weights=glm_wgt ,na.action = "na.exclude"))
  }
  
  rsq <- dplyr::summarise(models,date=date,rsq = summary(mod)$r.squared)
  coef <- models %>% broom::tidy(mod)
  resd <- models %>% broom::augment(mod)
  if(lmtype == "glm"){
    resd$.resid <- sqrt(resd$X.weights.) * resd$.resid
  }
  resd <- cbind(data[,c('date','stockID')],resd[,c('.fitted','.resid')])
  colnames(resd) <- c('date','stockID','fitted','res')
  resd <- merge.x(TS,resd,by=c('date','stockID'))
  
  rsq <- as.data.frame(rsq)
  coef <- as.data.frame(coef)
  resd <- as.data.frame(resd)
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
  re <- round(summary(RSquare$rsquare),3)
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
  
  tstat <- fRtn %>% dplyr::group_by(fname) %>% dplyr::summarise(avgT=mean(abs(Tstat)),
                                                                TPer=sum(Tstat>2)/length(Tstat))
  colnames(tstat) <- c("fname","mean(abs(T))","percent T>2")
  tstat$fname <- as.character(tstat$fname)
  
  fRtn <- reshape2::dcast(fRtn,date~fname,value.var = 'frtn')
  fRtn <- xts::xts(fRtn[,-1],fRtn[,1])
  rtnsum <- t(rtn.summary(fRtn))
  rtnsum <- data.frame(fname=rownames(rtnsum),rtnsum,stringsAsFactors = FALSE)
  rownames(rtnsum) <- NULL
  colnames(rtnsum) <- c("fname","ann_rtn","ann_sd","ann_Sharpe","hit_ratio","max_drawdown")
  re <- dplyr::left_join(rtnsum,tstat,by='fname')
  
  if(includeVIF){
    TSF <- reg_results$TSFR %>% dplyr::select(-date_end,-periodrtn)
    VIF <- factor_VIF(TSF,sectorAttr = NULL)
    VIF <- VIF %>% dplyr::group_by(fname) %>% dplyr::summarise(vif=mean(vif)) %>% dplyr::ungroup()
    re <- dplyr::left_join(re,VIF,by='fname')
  }
  re <- dplyr::arrange(re,dplyr::desc(ann_Sharpe))
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
    ggplot.WealthIndex(fRtn,size=1)
  }else{
    N <- floor(sqrt(ncol(fRtn)))
    fRtn <- WealthIndex(fRtn)
    fRtn <- melt.ts(fRtn)
    ggplot(fRtn, aes(x=time, y=value)) +ggtitle('wealth index')+
      geom_line(size=1,colour = "red")+facet_wrap( ~ variable,scales = 'free',ncol = N)
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
    geom_bar(position="dodge",stat="identity")+facet_wrap( ~ fname,scales = 'free',ncol = N)
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
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param rolling default value is \code{FALSE}, if value is \code{TRUE} means the data period is \code{nwin} forward.
#' @param rtntype is method to caculate factor return,\bold{mean} means average of historical data,\bold{forcast} means forcast factor return based on historical data,it may take a while,the forcast method come from package \code{\link[forecast]{ets}}.
#' @param covtype means type of caculating covariance,\bold{shrink} can see example in \code{\link[nlshrink]{nlshrink_cov}},simple see \code{\link{cov}}.
#' @param nwin is rolling windows forward.
#' @param reg_results see examples in \code{\link{reg.TSFR}}
#' @return a data frame of factors' return .
#' @examples 
#' fRtn <- getfRtn(reg_results=reg_results)
#' fCov <- getfCov(reg_results=reg_results)
#' Delta <- getDelta(dure=months(1),rolling=FALSE,nwin=24,reg_results)
#' rtn_cov_delta <- f_rtn_cov_delta(reg_results=reg_results)
#' @export
f_rtn_cov_delta <- function(dure=months(1),rolling=FALSE,rtntype=c('mean','forcast'),
                            covtype=c('shrink','simple'),nwin=24,reg_results) {
  rtntype <- match.arg(rtntype)
  covtype <- match.arg(covtype)
  
  fRtn <- getfRtn(dure=dure,rolling=rolling,rtntype=rtntype,
                  nwin=nwin,reg_results)
  fCov <- getfCov(dure=dure,rolling=rolling,covtype=covtype,
                  nwin=nwin,reg_results)
  Delta <- getDelta(dure=dure,rolling=rolling,nwin=nwin,reg_results)
  
  re <- list(fRtn=fRtn,fCov=fCov,Delta=Delta)
  return(re)
}




# inner function
get_frtn_res <- function(begT,endT,dure,reg_results,outtype=c('frtn','res')){
  outtype <- match.arg(outtype)
  if(missing(begT)) begT <- as.Date('1990-01-01')
  if(missing(endT)) endT <- as.Date('2100-01-01')
  
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
    
    if(outtype=='frtn'){
      qr <- paste("SELECT date,fname,",dbname," 'frtn'
                  FROM Reg_FactorRtn where date>=",rdate2int(begT),
                  " and date<=",rdate2int(endT))
    }else if(outtype=='res'){
      qr <- paste("SELECT date,stockID,",dbname," 'res'
                  FROM Reg_Residual where date>=",rdate2int(begT),
                  " and date<=",rdate2int(endT))
    }
    con <- db.local()
    re <- dbGetQuery(con,qr)
    dbDisconnect(con)
    re <- re %>% dplyr::mutate(date=intdate2r(date),date_end=trday.offset(date,dure))
    
  }else{
    if(outtype=='frtn'){
      re <- reg_results$fRtn %>% dplyr::select(-Tstat) %>% dplyr::filter(date>=begT,date<=endT)
    }else{
      re <- reg_results$res %>% dplyr::filter(date>=begT,date<=endT)
    }
    dates <- reg_results$TSFR %>% dplyr::select(date,date_end) %>% dplyr::distinct()
    re <- re %>% dplyr::left_join(dates,by='date')
  }
  return(re)
}


#' @rdname f_rtn_cov_delta
#' 
#' @export
getfRtn <- function(dure=months(1),rolling=FALSE,rtntype=c('mean','forcast'),nwin=24,reg_results){
  rtntype <- match.arg(rtntype)
  
  if(missing(reg_results)){
    rtndata <- get_frtn_res(dure=dure)
  }else{
    rtndata <- get_frtn_res(reg_results=reg_results)
  }

  if(rtntype=='mean'){
    if(rolling){
      result <- rtndata %>% dplyr::arrange(fname,date_end) %>% dplyr::group_by(fname) %>%
        dplyr::mutate(frtnroll=zoo::rollmean(frtn,nwin,na.pad = TRUE,align='right')) %>% dplyr::ungroup() %>% 
        dplyr::select(date_end,fname,frtnroll) %>% dplyr::rename(date=date_end,frtn=frtnroll) %>% 
        dplyr::arrange(date,fname) %>% filter(!is.na(frtn))

    }else{
      result <- rtndata %>% dplyr::group_by(fname) %>% dplyr::summarise(frtn=mean(frtn,na.rm = TRUE)) %>% dplyr::ungroup()
    }  
    result <- as.data.frame(result)
    
  }else if(rtntype=='forcast'){
    rtndata <- reshape2::dcast(rtndata,date_end~fname,value.var = 'frtn')
    
    if(rolling){
      RebDates <- rtndata$date_end
    }else{
      RebDates <- max(rtndata$date_end)
    }
    
    result <- data.frame()
    for(i in 1:length(RebDates)){
      rtndata_ <- rtndata %>% dplyr::filter(date_end<=RebDates[i]) %>% dplyr::select(-date_end)
      if(rolling && nrow(rtndata_)<nwin){
        next
      } 
      for(j in 1:ncol(rtndata_)){
        myts <- ts(data= rtndata_[,j])
        fit <- forecast::ets(rtndata_[,j])
        fit.forcast <- forecast::forecast(fit, 1)
        result_ <- data.frame(date=RebDates[i],fname=colnames(rtndata_)[j],
                              frtn=as.numeric(fit.forcast$mean),stringsAsFactors = FALSE)
        result <- rbind(result,result_)
      }
    }
    
    result <- dplyr::arrange(result,date,fname)
    if(!rolling){
      result <- transform(result,date=NULL)
    }
  }

  return(result)
}




#' @rdname f_rtn_cov_delta
#' 
#' @export
getfCov <- function(dure=months(1),rolling=FALSE,covtype=c('shrink','simple'),
                    nwin=24,reg_results){
  covtype <- match.arg(covtype)
  
  if(missing(reg_results)){
    rtndata <- get_frtn_res(dure=dure)
  }else{
    rtndata <- get_frtn_res(reg_results=reg_results)
  }
  rtndata <- reshape2::dcast(rtndata,date_end~fname,value.var = 'frtn')
  
  if(rolling){
    RebDates <- rtndata$date_end
    result <- data.frame()
    for(i in 1:length(RebDates)){
      rtnmat <- rtndata %>% dplyr::filter(date_end<=RebDates[i]) %>% dplyr::select(-date_end)
      rtnmat <- tail(rtnmat,nwin)
      rtnmat <- as.matrix(rtnmat)
      if(nrow(rtnmat)<nwin){
        next
      } 
      if(covtype=='simple'){
        result_ <- as.data.frame(cov(rtnmat))
      }else{
        result_ <- as.data.frame(nlshrink::nlshrink_cov(rtnmat))
        colnames(result_) <- colnames(rtnmat)
      }
      result_ <- data.frame(date=RebDates[i],result_)
      result <- rbind(result,result_)
    }
    
  }else{
    
    rtnmat <- as.matrix(rtndata[,-1])
    if(covtype=='simple'){
      result <- as.data.frame(cov(rtnmat))
    }else{
      result <- as.data.frame(nlshrink::nlshrink_cov(rtnmat))
      colnames(result) <- colnames(rtnmat)
      rownames(result) <- colnames(rtnmat)
    }
  }
  return(result)
}




#' @rdname f_rtn_cov_delta
#'
#' @export
getDelta <- function(dure=months(1),rolling=FALSE,nwin=24,reg_results){
  
  if(missing(reg_results)){
    resdata <- get_frtn_res(dure=dure,outtype = 'res')
  }else{
    resdata <- get_frtn_res(reg_results=reg_results,outtype = 'res')
  }
  
  if(rolling){
    resdata <- reshape2::dcast(resdata,date_end~stockID,value.var = 'res')
    RebDates <- resdata$date_end
    result <- data.frame()
    for(i in 1:length(RebDates)){
      resdata_ <- resdata %>% dplyr::filter(date_end<=RebDates[i])
      resdata_ <- tail(resdata_,nwin)
      if(nrow(resdata_)<nwin){
        next
      }
      resdata_ <- reshape2::melt(resdata_,id.vars='date_end',variable.name = "stockID", na.rm = TRUE,value.name = "res")
      
      result_ <- resdata_ %>% dplyr::group_by(stockID) %>% dplyr::summarise(n =n(),var = var(res)) %>% 
        dplyr::ungroup() %>% dplyr::filter(n>=nwin/2) %>% dplyr::select(-n)
      result_ <- data.frame(date=RebDates[i],result_)
      result <- rbind(result,result_)
    }

  }else{
    result <- resdata %>% dplyr::group_by(stockID) %>% dplyr::summarise(n =n(),var = var(res)) %>% 
      dplyr::ungroup() %>% dplyr::filter(n>=3) %>% dplyr::select(-n)
  }
  
  result <- as.data.frame(result)
  return(result)
}


#' biasTest
#' 
#' @export
#' @examples 
#' biasTest(reg_results)
biasTest <- function(reg_results,portID='EI000300',nwin=12){
  rtn_cov_delta <- f_rtn_cov_delta(rolling = TRUE,nwin = nwin,reg_results=reg_results)
  fcov <- rtn_cov_delta$fCov
  fnames <- setdiff(colnames(fcov),'date')
  delta <- rtn_cov_delta$Delta
  # calculate factor return
  TSFR_total <- reg_results$TSFR
  dates <- unique(TSFR_total$date_end)
  port <- getIndexCompWgt(portID,dates)
  
  TSWF <- dplyr::left_join(port,TSFR_total,by=c('date','stockID'))
  biasdf <- data.frame()
  for(i in 1:length(dates)){
    TSWF_ <- TSWF %>% dplyr::filter(date==dates[i])
    TSWF_ <- na.omit(TSWF_)
    portrtn_ <- sum(TSWF_$wgt*TSWF_$periodrtn,na.rm = TRUE)
    
    wgt_ <- as.matrix(TSWF_$wgt,ncol=1)
    Xmat_ <- as.matrix(TSWF_[,fnames])
    if('date' %in% colnames(fcov)){
      Fmat_ <- as.matrix(fcov[fcov$date==dates[i],-1])
    }else{
      Fmat_ <- as.matrix(fcov)
    }
    if(nrow(Fmat_)==0) next
    
    
    if('date' %in% colnames(delta)){
      delta_ <- delta[delta$date==dates[i],-1]
    }else{
      delta_ <- delta
    }
    if(nrow(delta_)==0) next
    
    deltamat_ <- dplyr::left_join(TSWF_[,'stockID',drop=FALSE],delta_,by='stockID')
    deltamat_[is.na(deltamat_$var),'var'] <- median(deltamat_$var,na.rm = TRUE)
    deltamat_ <- diag(deltamat_$var)
    portvar_ <- sqrt(as.numeric(t(wgt_) %*% (Xmat_ %*% Fmat_ %*% t(Xmat_)+deltamat_) %*% wgt_))
    biasdf <- rbind(biasdf,data.frame(date=dates[i],rtn=portrtn_,var=portvar_))
  }
  biasdf <- transform(biasdf,b=rtn/var)
  biasdf <- xts::xts(biasdf[,'b'],order.by = biasdf[,'date'])
  names(biasdf) <- 'b'
  biasdf <- zoo::rollapply(biasdf,nwin,sd,align='right')
  biasdf <- na.omit(biasdf)
  ggplot.ts.line(biasdf)
}







# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  Performance & Risk Attribution -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' calculate factor exposure
#' 
#' @export
exposure.TSWF <- function(TSWF) {
  factorNames <- guess_factorNames(TSWF,silence = TRUE)
  
  TSWF <- TSWF %>% dplyr::select(one_of(c("date","wgt",factorNames)))
  factorexp <- TSWF %>% tidyr::gather(key='fname',value='fexp',-date,-wgt) %>% group_by(date,fname) %>% 
    dplyr::summarise(fexptot=sum(wgt*fexp,na.rm = TRUE)) %>% dplyr::ungroup() 
  factorexp <- factorexp %>% tidyr::spread(fname,fexptot) %>% dplyr::select(one_of(c("date",factorNames)))
  factorexp <- as.data.frame(factorexp)
  return(factorexp)
}


#' calculate port exposure
#' 
#' @export exposure.port
exposure.port <- function(port,factorLists,bmk=NULL,univ=NULL,
                          sectorAttr = defaultSectorAttr()){
  # get active wgt if bmk is provided.
  if(!is.null(bmk)){
    port <- getActivewgt(port = port,bmk = bmk,res = "all")
  }
  
  # univ is nessecary when any of factorStd is not 'none'.
  if(!is.null(univ)){ # get factorscore in univ
    dates <- unique(port$date)
    TS <- getTS(dates,indexID = univ)
    TSF <- getMultiFactor(TS,factorLists)
    TSWF <- merge.x(port,TSF,by=c('date','stockID'))
  } else { # get factorscore only in port
    factorSTD <- sapply(factorLists, function(x){x$factorRefine$std$method})
    if(any(factorSTD != "none")){
      warning("univ is nessecary when any of factorStd is not 'none'!")
    }
    TSWF <- getMultiFactor(port,factorLists)
  }
  
  if(!is.null(sectorAttr)){
    TSWF <- gf_sector(TSWF,sectorAttr = sectorAttr)
  }
  
  # arrange exposure
  if(!is.null(bmk)){
    # bmk
    TSWF_bmk <- subset(TSWF, select = -c(portwgt,actwgt))
    TSWF_bmk <- dplyr::rename(TSWF_bmk, wgt = benchwgt)
    fexp_bmk <- exposure.TSWF(TSWF_bmk)
    fexp_bmk <- reshape2::melt(fexp_bmk, id = "date")
    fexp_bmk <- dplyr::rename(fexp_bmk, bmk_exposure = value)
    # port
    TSWF_port <- subset(TSWF, select = -c(benchwgt,actwgt))
    TSWF_port <- dplyr::rename(TSWF_port, wgt = portwgt)
    fexp_port <- exposure.TSWF(TSWF_port) 
    fexp_port <- reshape2::melt(fexp_port, id = "date")
    fexp_port <- dplyr::rename(fexp_port, port_exposure = value)
    # merge and compute act
    fexp <- merge(fexp_bmk, fexp_port, by = c("date", "variable"))
    fexp <- dplyr::rename(fexp, fName = variable)
    fexp$act_exposure <- fexp$port_exposure - fexp$bmk_exposure
    fexp <- dplyr::arrange(fexp, fName, date)
  }else{
    fexp <- exposure.TSWF(TSWF)
    fexp <- dplyr::arrange(fexp,date)
    fexp <- reshape2::melt(fexp, id.vars="date", variable.name="fName", value.name="exposure")
  }
  return(fexp)
}



# ---------------------  ~~ Performance attribution --------------

#' PA_RA_Analysis
#' 
#' performance attribution and risk attribution analysis.
#' @name PA_RA_Analysis
NULL


#' getPAData
#' 
#' @rdname PA_RA_Analysis
#' @export
#' @examples 
#' FactorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.SIZE"),
#'   buildFactorList(factorFun="gf.GROWTH"),
#'   buildFactorList(factorFun="gf.TRADING"),
#'   buildFactorList(factorFun="gf.FORECAST"),
#'   buildFactorList(factorFun="gf.EARNINGYIELD"),
#'   buildFactorList(factorFun="gf.VALUE"),
#'   buildFactorList(factorFun="gf.QUALITY"))
#' PA_tables <- getPAData(port,FactorLists)
#' PA_tables <- getPAData(port,FactorLists,bmk='EI000905')
getPAData <- function(port,FactorLists,bmk=NULL,univ="EI000985",sectorAttr = defaultSectorAttr()){
  
  # get active wgt, if necessary
  if(!is.null(bmk)){
    port <- getActivewgt(port = port,bmk = bmk,res = "active")
    port <- dplyr::rename(port,wgt=actwgt)
  }
  
  # calculate factor return 
  TS <- getTS(unique(port$date),indexID = univ)   # get TSFR within rebDates==dates & univ==univ
  TSF <- getMultiFactor(TS,FactorLists)
  fnames <- guess_factorNames(TSF,silence = TRUE)
  TSFR <- getTSR(TSF)
  regdata <- (reg.TSFR(TSFR,sectorAttr = sectorAttr,secRtnOut = TRUE))[['fRtn']]
  frtn <- reshape2::dcast(regdata,date~fname,value.var = 'frtn')
  
  #calculate factor covariance
  fcov <- nlshrink::nlshrink_cov(as.matrix(frtn[,fnames]))
  colnames(fcov) <- fnames
  rownames(fcov) <- fnames
  
  # calculate factor exposure
  TSWF <- merge.x(port,TSFR,by=c('date','stockID'))
  TSWF <- na.omit(TSWF)
  if(!is.null(sectorAttr)){
    TSWF <- gf_sector(TSWF,sectorAttr = sectorAttr)
  }
  fexp <- exposure.TSWF(TSWF) 
  fexp <- dplyr::arrange(fexp,date)
  
  # calculate performance attribution
  if(!missing(bmk)){
    rtn.short <- unique(TSWF[,c('date','date_end')])
    rtn.short <- getPeriodrtn_EI(stockID=bmk,begT=rtn.short$date, endT=rtn.short$date_end)
    rtn.short <- dplyr::rename(rtn.short,date=begT,date_end=endT,bmkrtn=periodrtn)
    TSWF <- merge.x(TSWF,rtn.short[,c( "date","date_end","bmkrtn")])
    TSWF <- transform(TSWF,periodrtn=periodrtn-bmkrtn)
  }
  portrtn <- TSWF %>% dplyr::group_by(date) %>% dplyr::summarise(rtn=sum(wgt*periodrtn, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% dplyr::arrange(date)
  portrtn <- as.data.frame(portrtn)
  
  frtn <- dplyr::select(frtn,one_of(colnames(fexp))) # make the order of cols same with fexp
  fattr_m <- as.matrix(frtn[, -1])*as.matrix(fexp[, -1])
  res_m <- data.frame(res=portrtn[,-1]-rowSums(fattr_m))
  perfattr <- data.frame(date=portrtn$date,fattr_m,res_m)
  
  # calculate risk attribution
  riskattr <- data.frame()
  dates <- unique(TSWF$date)
  for(i in 1:length(dates)){
    TSWF_ <- TSWF %>% dplyr::filter(date==dates[i])
    wgtmat <- matrix(TSWF_$wgt,ncol = 1)
    Xmat <- as.matrix(TSWF_[,fnames])
    for(j in fnames){
      Xmatk <- Xmat
      Xmatk[,setdiff(fnames,j)] <- 0
      # Xmatk[,j] <- 1 
      riskattr_ <- data.frame(date=dates[i],
                              fname=j,
                              frisk=t(wgtmat) %*% Xmat %*% fcov %*% t(t(wgtmat) %*% Xmatk),stringsAsFactors = FALSE)
      riskattr <- rbind(riskattr,riskattr_)
    }
  }
  riskattr <- reshape2::dcast(riskattr,date~fname,value.var = 'frisk')
  riskattr <- riskattr[,c('date',fnames)]
  
  return(list(frtn=frtn,fexp=fexp,perfattr=perfattr,portrtn=portrtn,riskattr=riskattr))
}



#' chart.PA.exposure
#' 
#' @rdname PA_RA_Analysis
#' @export
#' @examples 
#' chart.PA.exposure(PA_tables)
#' chart.PA.exposure(PA_tables,plotInd=TRUE)
chart.PA.exposure <- function(PA_tables,plotInd=FALSE){
  factorexp <- PA_tables$fexp
  
  #plot factor exposure
  fnames <- guess_factorNames(factorexp,silence = TRUE)
  indnames <- fnames[stringr::str_detect(fnames,'^ES\\d')]
  fnames <- setdiff(fnames,indnames)
  factormean <- colMeans(factorexp[,c(fnames,indnames)])
  factormean <- data.frame(factorName=names(factormean),
                           factorExposure=unname(factormean),stringsAsFactors = FALSE)
  factormean <- transform(factormean,
                          factorName=ifelse(factorName %in% indnames,sectorID2name(factorName),factorName),
                          tag=ifelse(factorName %in% fnames,'style','industry'))
  if(!plotInd){
    factormean <- dplyr::filter(factormean,tag=='style')
  }
  ggplot(factormean,aes(x=reorder(factorName,-factorExposure),y=factorExposure,fill=tag))+
    geom_bar(stat = "identity")+labs(title='Factor Exposure',x='',y='')+
    facet_wrap(~tag,scales = "free",ncol = 1)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


#' chart.PA.attr
#' 
#' @rdname PA_RA_Analysis
#' @export
#' @examples 
#' chart.PA.attr(PA_tables)
#' chart.PA.attr(PA_tables,plotInd=TRUE)
chart.PA.attr <- function(PA_tables,plotInd=FALSE,attributeAnn=TRUE){
  perfattr <- PA_tables$perfattr
  
  fnames <- guess_factorNames(perfattr,no_factorname = 'res',silence = TRUE)
  indnames <- fnames[stringr::str_detect(fnames,'^ES\\d')]
  fnames <- setdiff(fnames,indnames)
  
  #plot summary factor performance attribution
  if(!plotInd){
    perfattr <- perfattr[,c('date',fnames,'res')]
  }
  perfts <- xts::xts(perfattr[,-1],order.by = perfattr[,1])
  
  if(attributeAnn){
    rtnsum <- rtn.summary(perfts)
    rtnsum <- rtnsum['Annualized Return',]
  }else{
    rtnsum <- rtn.periods(perfts)
    rtnsum <- rtnsum["Cumulative Return",]
  }
  
  rtnsum <- data.frame(factorName=names(rtnsum),factorAttribution=unname(rtnsum),stringsAsFactors = FALSE)
  rtnsum <- transform(rtnsum,
                      factorName=ifelse(factorName %in% indnames,sectorID2name(factorName),factorName),
                      tag=ifelse(factorName %in% c(fnames,'res'),'style','industry'))
  
  p1 <- ggplot(rtnsum,aes(x=reorder(factorName,-factorAttribution),y=factorAttribution,fill=tag))+
    geom_bar(stat = "identity")+
    facet_wrap(~tag,scales = "free",ncol = 1)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  if(attributeAnn==TRUE){
    p1+labs(title='Factor Attribution(Annulized)',x='',y='')
  }else{
    p1+labs(title='Factor Attribution',x='',y='')
  }
  
}



# ---------------------  ~~ Risk attribution --------------



#' chart.RA.attr
#' 
#' @rdname PA_RA_Analysis
#' @export
#' @examples 
#' chart.RA.attr(PA_tables)
chart.RA.attr <- function(PA_tables){
  riskattr <- PA_tables$riskattr
  
  fnames <- guess_factorNames(riskattr,silence = TRUE)
  riskattr <- tidyr::gather(riskattr,'fname','frisk',fnames)
  riskattr <- riskattr %>% group_by(fname) %>% summarise(risk=sum(frisk))
  ggplot(riskattr,aes(x=reorder(fname,-risk),y=risk))+
    geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
  benchdata <- dplyr::rename(benchdata,benchwgt=wgt)
  port <- dplyr::rename(port,portwgt=wgt)
  port <- port %>% dplyr::full_join(benchdata,by=c('date','stockID')) %>% 
    dplyr::mutate(portwgt=ifelse(is.na(portwgt),0,portwgt),benchwgt=ifelse(is.na(benchwgt),0,benchwgt)) %>% 
    dplyr::mutate(actwgt=portwgt-benchwgt) %>% dplyr::arrange(date,stockID)
  if(res=="active"){
    port <-  port[,c("date","stockID","actwgt")]
  }
  return(port)
}




