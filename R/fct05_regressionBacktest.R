

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  backtesting with 'regression' method -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



#' regression_result
#'
#' Regression to the TSFR data, calculate factor return, residuals, and R squrare, etc. 
#' @name regression_result
#' @rdname regression_result
#' @aliases reg.TSFR
#' @param TSFR a TSFR object.
#' @param regType the regress type,the default type is "lm".
#' @param glm_wgt
#' @param sectorAttr
#' @return reg.TSFR return a list, contains dataframes of frtn, residual, Rsquare, pfp
#' @export
#' @author Ruifei.yin
#' @examples 
#' reg.TSFR(TSFR)
reg.TSFR <- function(TSFR,regType=c('lm','glm'),glm_wgt=c("sqrtFV","res"),sectorAttr=defaultSectorAttr()){
  ptm <- proc.time()
  regType <- match.arg(regType)
  glm_wgt <- match.arg(glm_wgt)
  
  
  
  # standardize the factorscores and the NA dealing !
  # TSF <- stdxxxx(TSF)
  
  
  TS <- TSFR[,c('date','stockID')]
  # get sector factor
  TSS <- gf.sector(TS,sectorAttr = sectorAttr)
  TSFR <- merge.x(TSFR,TSS,by=c("date","stockID"))
  
  # regression
  if(regType=='glm'){ #get glm_wgt data
    if(glm_wgt=="sqrtFV"){
      TSw <- getTSF(TS,'gf.float_cap',factorStd = 'none',factorNA = "median")
      TSw$factorscore <- sqrt(TSw$factorscore)
      TSw <- dplyr::rename(TSw,glm_wgt=factorscore)
      TSFR <- merge(TSFR,TSw,by =c("date","stockID"))
    } else if(glm_wgt=="res"){
      
    }
  }
  
  factorNames <- setdiff(names(TSFR),c("stockID","date","date_end","periodrtn","wgt","glm_wgt","sector"))
  indname <- colnames(TSS)[-1:-2]
  
  # loop of regression
  TSFR <- na.omit(TSFR)  # omit the NAs 
  dates <- unique(TSFR$date)
  frtn <- data.frame()
  residual <- data.frame()
  Rsquare <- data.frame()
  for(i in 1:length(dates)){ 
    tmp.tsfr <- TSFR[TSFR$date==dates[i],]
    fml <- formula(paste("periodrtn ~ ", paste(c(factorNames,indname), collapse= "+"),"-1",sep=''))
    if (regType=="glm"){
      tmp.w <- as.matrix(tmp.tsfr[,"glm_wgt"])
      lmm <- lm(fml,data = tmp.tsfr,weights = tmp.w)
    } else {
      lmm <- lm(fml,data = tmp.tsfr)
    }
    smry <- summary(lmm)
    frtn <- rbind(frtn,data.frame(date=dates[i],fname=rownames(smry$coefficients),frtn=smry$coefficients[,1],Tstat=smry$coefficients[,3]))
    residual <- rbind(residual,data.frame(date=dates[i],stockID=tmp.tsfr$stockID,residual=smry$residuals))
    Rsquare <- rbind(Rsquare,data.frame(date=dates[i],Rsquare=smry$r.squared))
    
    # pure-factor-port wgt 
    pfp <- list()  # to-do......
    
    
  }
  
  re <- list(frtn=frtn,residual=residual,Rsquare=Rsquare,pfp=pfp)
  
  tpassed <- proc.time()-ptm
  tpassed <- tpassed[3]
  cat("This function running time is ",tpassed/60,"min.")
  return(re)
}


#' @rdname regression_result
#' @aliases reg.TS
#' @param TS
#' @param factorLists
#' @param dure
#' @return reg.TS return a list, contains dataframes of TSFR,frtn, residual, Rsquare
#' @export
reg.TS <- function(TS,factorLists,regType=c('glm','lm'),glm_wgt=c("sqrtFV","res"),
                   sectorAttr=defaultSectorAttr(),
                   dure=months(1)){
  TSF <- getMultiFactor(TS,FactorLists = factorLists)
  TSFR <- getTSR(TSF,dure=dure)
  reg <- reg.TSFR(TSFR = TSFR,regType = regType,glm_wgt=glm_wgt,sectorAttr = sectorAttr)
  re <- c(list(TSFR),reg)
}





# ---------------------  ~~ Backtesting results --------------

tables.reg <- function(reg_results){
  
  # annrtn,annvol,sharpe,hitRatio,avg_T_sig
  # Rsquare 
  
}
charts.reg <- function(reg_results){
  # charts for each factor
}

MC.chart.reg <- function(reg_results){
  
}




calcFcov_Delta <- function(reg_results, lumda){
  # calc Fcov and Delta
}

calcFrtn <- function(reg_results){
  # calc Alphaf
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





OptWgt <- function(TSF,Frtn,Fcov,Delta,constr=c('IndSty','Ind','IndStyTE'),benchmark='EI000905',riskavr=1,indfexp=0.05,...){
  
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

getfrtn_lcdb <- function(){
  
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
  portrtn <- plyr::ddply(TSWF,"date",summarise,rtn=sum(wgt*periodrtn, na.rm = TRUE))
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
  colnames(port) <- c('date','stockID','portwgt')
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








