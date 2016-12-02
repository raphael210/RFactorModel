


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  get RebDates,TS,TSR,TSF,TSFR object ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' getRebDates
#'
#' get the rebalance dates(a \bold{RebDates} object) between begT and endT, and then shift it afterward if necessary.
#' @param begT the begin date of class Date
#' @param endT the end date of class Date
#' @param rebFreq the rebalancing freq. an interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s". (See \code{\link{cut.Date}} for detail.) Or a character string of "allspan". "allspan" means no rebalance during the whole period between begT and endT, which could be realized also by a very large interval specification, eg. "100 year".
#' @param shiftby a integer,how many days the rebalancing date be shifted afterward
#' @param dates a vector of class Date. 
#' @return a \bold{RebDates} object giving the rebalancing dates list,a vector with class of \code{Date}. If dates is not null, then return the dates themselves.
#' @author Ruifei.Yin
#' @export
#' @family frame building functions
#' @examples 
#' RebDates <- getRebDates(as.Date('2010-01-01'),as.Date('2012-06-30'),'3 month', 10)
#' getRebDates(as.Date('2010-01-01'),as.Date('2012-06-30'),'allspan')
#' getRebDates(as.Date('2010-01-01'),as.Date('2012-06-30'),'100 year')
#' getRebDates(dates=as.Date(c("2003-04-09","2003-07-06","2003-09-12")))
getRebDates <- function(begT,endT,rebFreq="month",shiftby=0, dates=NULL){
  if(!is.null(dates)){
    re <- trday.nearest(dates)
  } else {
    begT <- trday.nearest(begT)
    endT <- trday.nearest(endT)
    trdays <- trday.get(begT,endT)
    # cut by rebalancing frequency
    if(rebFreq != "allspan"){
      rebdays <- as.Date(unique(cut.Date2(trdays,rebFreq))) 
    } else {
      rebdays <- c(begT,endT)
    }   
    # add begT as begin point
    if(rebdays[1] != begT){
      rebdays <- c(begT,rebdays) 
    } 
    # shift the rebalancing dates afterward
    re <- trday.nearby(rebdays,-shiftby)
  }
  
  return(re)
}




#' getTS
#'
#' get the 'time*stock frame'(a \bold{TS} object) given the rebalancing dates and the universe. The universe could be a index, a sector or a plate.
#' @param RebDates a \bold{RebDates} object. a vector,with class of Date,usually the rebalancing dates list
#' @param indexID a character string. The ID of the index, sector or plate. Could be a single-ID-code(eg. "EI000300","ES09440000",...) or a more complicated express containing some set operations and ID-codes(eg. "setdiff(union(EI000300,EI399006),ES09440000)"). See detail in \code{\link{getComps}}.
#' @param stocks a vector of stockIDs
#' @return a \bold{TS} object. a dataframe,with cols:
#'   \itemize{
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item stockID: the stockID 
#'   }.IF stocks is not null then return \code{expand.grid} of stocks and RebDates, else return components of index on RebDates. 
#' @author Ruifei.Yin
#' @seealso \code{\link{getComps}}
#' @export 
#' @family frame building functions
#' @examples
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' TS2 <- getTS(RebDates,'ES09440000')  # financial servive sector
#' TS3 <- getTS(RebDates,'setdiff(EI000300,ES09440000)')  # CSI300 ex. financial servive sector
#' TS4 <- getTS(RebDates,stocks=c("EQ000002","EQ000023","EQ600000"))
getTS <- function(RebDates,indexID="EI000300",stocks=NULL){  
  if(!is.null(stocks)){
    TS <- expand.grid(stockID=stocks,date=RebDates)
  } else {
    TS <- getComps(sectorIDs=indexID,endT=RebDates,drop=FALSE) 
  }
  return (TS)
}




#' getTSR
#'
#' Get the \bold{TSR}('time-stock-rtn') or \bold{TSFR}('time-stock-factor-rtn') object by adding the period rtn between the rebalance date and the date_end(\strong{(close of "date_end")/(prevclose of the day after "date")-1)}) to the \bold{TS} or \bold{TSF} object.
#' @param TS a \bold{TS} or \bold{TSF} object (See detail in \code{\link{getTS}} and \code{\link{getTSF}})
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param date_end_pad a Date value, padding the NAs of the last date_end. if missing, calculated automatically.
#' @return given a \bold{TS} object,return a \bold{TSR} object,a dataframe containing cols:
#'   \itemize{ 
#'   \item stockID: the stockID contained in the index or BK
#'   \item date: the rebalance dates, with \code{Date} class 
#'   \item date_end: the next rebalance dates, with \code{Date} class
#'   \item periodrtn:the period rtn between the rebalance date and the date_end
#'   }
#'   given a \bold{TSF} object,return a \bold{TSFR} object,a dataframe containing cols:
#'   \itemize{ 
#'   \item stockID: the stockID contained in the index or BK
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item factorscore:the factor score of the stocks on the  rebalancing date  
#'   \item date_end: the next rebalance dates, with \code{Date} class
#'   \item periodrtn:the period rtn between the rebalance date and the date_end
#'   }  
#' @author Ruifei.Yin
#' @export
#' @family frame building functions
#' @examples
#' ## -- get a TSR object
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300') 
#' TSR <- getTSR(TS) # rebalance properly joined
#' require(lubridate)
#' TSR2 <- getTSR(TS,weeks(2)); TSR3 <- getTSR(TS,months(2))  # rebalance not properly joined
#' ## -- get a TSFR object
#' factorFun <- "gf.demo"
#' factorPar <- list(0,1)
#' TSF <- getTSF(TS,factorFun,factorPar)
#' TSFR <-  getTSR(TSF)
getTSR <- function(TS, dure=NULL, date_end_pad){ 
  cat(paste("Function getTSR: getting the periodrtn ....\n"))
  check.TS(TS)
  # ---- Add the end date (and the decayed dates)
  date <- unique(TS$date)
  if (is.null(dure)){
    if(missing(date_end_pad)){ # calculate the padding value automatically
      date_end_pad <- trday.nearest(max(date)+periodicity_Ndays(date), dir = -1L)
    }
    date_end <- c(date[-1],date_end_pad)
  } else {
    date_end <- trday.offset(date,dure,dir=-1L)
  }  
  merging <- data.frame(date,date_end)
  TS <- merge.x(TS,merging,by="date")  
  periodrtn <- data.frame(periodrtn=getPeriodrtn(renameCol(TS,c("date","date_end"),c("begT","endT")),exclude.SP=TRUE))
  TSR <- cbind(TS,periodrtn)
  return(TSR)
}


#' getTSR_decay
#' 
#' @param prdLists
#' @return a \bold{TSR} including the decayed period rtns
#' @export
getTSR_decay <- function(TS, prd_lists = list(w1=lubridate::weeks(1),
                                              w2=lubridate::weeks(2),
                                              m1=months(1),
                                              m2=months(2),
                                              m3=months(3),
                                              m6=months(6))){
  check.TS(TS)
  date <- unique(TS$date)
  if(TRUE){ # --- add endTs
    prd_names <- names(prd_lists)
    if(all(is.na(prd_names))){
      warning("The names of prd_lists is missing, getting the names automatically.")
      prd_names <- sapply(prd_lists,substr,1,5)
    }
    date.decay <- data.frame()
    for(ii in 1:length(prd_lists)){
      endT_i <- trday.offset(date,prd_lists[[ii]],dir=-1L)
      if(ii==1L){
        date.decay <- data.frame(endT_i)
      } else {
        date.decay <- cbind(date.decay,endT_i)
      }
    }
    names(date.decay) <- paste("endT_",prd_names,sep = "")
    merging <- data.frame(date,date.decay)
  } 
  TS <- merge.x(TS,merging,by="date")   
  if(TRUE){ # --- add prdrtns
    cat("Getting the decayed rtn ....\n")
    pb_ <- txtProgressBar(min=0,max=length(prd_lists),initial=0,style=3)
    for(ii in 1:length(prd_lists)){      
      rtnI <- data.frame(rtn=getPeriodrtn(renameCol(TS,src=c("date",paste("endT_",prd_names[ii],sep="")),tgt=c("begT","endT")),exclude.SP=TRUE))      
      rtnI <- renameCol(rtnI,"rtn",paste("prdrtn_",prd_names[ii],sep=""))
      TS <- cbind(TS,rtnI)
      setTxtProgressBar(pb_,ii)
    }
    close(pb_)
    TSR <- TS[,!names(TS) %in% paste("endT_",prd_names,sep="")]
  }
}





#' getTSF
#'
#' get the \bold{TSF} ('time-stock-factor') or \bold{TSFR}('time-stock-factor-rtn') object,by adding the factor score to a \bold{TS} or \bold{TSR} object.If necessary,cleaning and stardizing the factor score.
#' @param TS a \bold{TS} or \bold{TSR} objectobject(See detail in \code{\link{getTS}} and  \code{\link{getTSR}})
#' @param factorFun a character string naming the function to get the factor scores.
#' @param factorPar either a list or a character string, containing the parameters of the \code{factorFun}. If a character string, parameters are seprated by commas and the character parameters must quoted by  "\""s, see examples for details.
#' @param factorDir a integer,should be 1 or -1 (1 for the positive factor,-1 for the negative one).
#' @param factorOutlier a numeric value of standard deviations, e.g. 10 means that factor score larger or smaller than ten times the standard deviation will be removed
#' @param factorNA a character string, indicating the method to deal with the missing value of the factorscore, could be one of "na"(default,keeping the missing value as it is),"mean","median","max","min".
#' @param factorStd a character string, indicating the standardizing pattern of the factor score,could be one of "none"(no scaling),"norm"(scaling to 0 mean and 1 stdev),"sectorNe"(scaling to 0 mean and 1 stdev in each sector).
#' @param sectorAttr
#' @param factorList
#' @param splitbin a integer or a charactor string of 'year','month',...
#' @return given a \bold{TS} object,return a \bold{TSF} object,a dataframe containing cols:
#'   \itemize{
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item stockID: the stockID contained in the index or BK
#'   \item factorscore:the factor score of the stocks on the  rebalancing date
#'   \item sector(optional):the sector of a stock
#'   }
#'   given a \bold{TSR} object,return a \bold{TSFR} object,a dataframe containing cols:
#'   \itemize{ 
#'   \item stockID: the stockID contained in the index or BK
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item factorscore:the factor score of the stocks on the  rebalancing date  
#'   \item date_end(optional): the next rebalance dates, with \code{Date} class
#'   \item periodrtn:the period rtn between the rebalance date and the date_end
#'   \item sector(optional):the sector of a stock
#'   }
#' @author Ruifei.Yin
#' @export
#' @family frame building functions
#' @examples
#' # -- get the TSF step by step --
#' 
#' # - get a TSF object
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' factorFun <- "gf.demo"
#' factorPar <- list(mean=0,sd=1)
#' TSF <- getTSF(TS,factorFun,factorPar)
#' # - get a TSFR object
#' TSR <- getTSR(TS)
#' TSFR <- getTSF(TSR)
#' 
#' # -- you can also get the TSF through the modelPar object directively --
#' modelPar <- modelPar.default()
#' modelPar <- modelPar.factor(modelPar,factorFun="gf.demo",factorPar=list(0,1),factorOutlier=10,factorStd="sectorNe")
#' TSF <- Model.TSF(modelPar)
#' 
#' # -- factorPar as character string
#' TSF2 <- getTSF(TS,"gf.demo","0,1")
#' TSF2 <- getTSF(TS,"gf.demo","mean=0,sd=1")
#' TSF2 <- getTSF(TS,"gf.foo","2,3")
#' TSF2 <- getTSF(TS,"gf.bar","20,\"IF00\"")
getTSF <- function(TS,factorFun,factorPar=list(),factorDir=1,factorOutlier=3,
                   factorStd=c("none","norm","sectorNe"),
                   factorNA=c("na","mean","median","min","max","sectmean"),
                   sectorAttr=defaultSectorAttr(),
                   FactorList,
                   splitbin=100000L){
  if(!missing(FactorList)){
    factorFun <- FactorList$factorFun
    factorPar <- FactorList$factorPar
    factorName <- FactorList$factorName
    factorDir <- FactorList$factorDir
    factorOutlier <- FactorList$factorOutlier
    factorNA <- FactorList$factorNA    
    factorStd <- FactorList$factorStd 
    sectorAttr  <- FactorList$sectorAttr
  }
  factorStd <- match.arg(factorStd)
  factorNA <- match.arg(factorNA)
  if(! factorDir %in% c(1L,-1L)) stop("unsupported \"factorDir\" argument!")
  cat(paste("Function getTSF: getting the factorscore ....\n"))
  check.TS(TS)
  
  subFun <- function(TS){
    # ---- get the raw factorscore
    TSF <- getRawFactor(TS[,c("date","stockID")],factorFun,factorPar)
    # ---- adjust the direction
    TSF$factorscore <- TSF$factorscore*factorDir
    # ---- deal with the outliers
    TSF <- factor.outlier(TSF,factorOutlier)
    # ---- standardize the factorscore
    TSF <- factor.std(TSF,factorStd,sectorAttr)
    # ---- deal with the missing values
    TSF <- factor.na(TSF,factorNA)
    # ---- re-order
    TSF <- merge.x(TS,TSF,by=c("date","stockID"))
    return(TSF)
  }
  
  if(is.numeric(splitbin)){
    cutN <- nrow(TS) %/% splitbin +1
    if(cutN > 1){
      idx <- cut(1:nrow(TS), breaks= cutN ,labels=FALSE)
    } else {
      idx <- factor(rep(1, nrow(TS)))
    }
  } else if(is.character(splitbin)){
    idx <- cut(TS$date, splitbin) 
  } else {
    stop("unsupported \"splitbin\" argument!")
  }
  if(length(levels(idx))>1){
    cat(paste('Possibly dealing with a big data. The process is splited to ',length(levels(idx)),'groups.\n'))
  }
  TS_ <- data.frame(idx,TS)
  TS_ <- dplyr::group_by(TS_,idx)
  re <- dplyr::do(TS_, subFun(.))
  re <- as.data.frame(re)
  re <- dplyr::select(re,-idx)
  return(re)
}



#' @rdname getTSF
#' @export
#' @details \code{getRawFactor} return a \bold{TSF} object, with 'raw' factorscore, which has not been standardized and not been deal with missing values and outliers. Function \code{getRawFactor} is often called intermediately by function \code{getTSF}.
#' @return \code{getRawFactor} return a \bold{TSF} object, with 'raw' factorscore. 
#' @examples
#' # -- get 'raw' factorscore
#' TSF_raw <- getRawFactor(TS,"gf.pct_chg_per","20")
getRawFactor <- function (TS,factorFun,factorPar) {
  if(missing(factorPar)){
    TSF <- do.call(factorFun,c(list(TS)))
  } else if(is.list(factorPar)){
    TSF <- do.call(factorFun,c(list(TS),factorPar))    
  } else if(is.character(factorPar)){
    if(is.na(factorPar)||factorPar==""){
      funchar <- paste(factorFun,"(TS)")
    } else {
      funchar <- paste(factorFun,"(","TS,",factorPar,")")
    }    
    TSF <- eval(parse(text=funchar))
  } else {
    stop("The factorPar must be a list or a character string!")
  }
}


#' get multiFactors
#' 
#' get multiple single-factor-scores and the combined-factor-score.#' 
#' @param TS
#' @param FactorLists a list, with elements of FactorList(See detail in \code{\link{buildFactorList}}).
#' @param wgts a numeric vector with the same length of FactorLists and sum of 1(if not, the wgts will be rescaled to sum 1). If missing, then only return all the single-factor-scores, without the combined-factor-score.
#' @param factorStd_mult
#' @param factorNA_mult
#' @return  \code{getMultiFactor} return a \bold{TSF} object including cols of all the single-factor-scores, and (if param \code{wgts} not missing) the \bold{raw} combined-factor-score .
#' @note  Function \code{getMultiFactor} not only could be used to get the "raw" combined-factor-score, but also could be used as the \code{"factorFun"} parametre in function \code{getTSF} to get the "clean and standardized" combined-factor-score. See more detail in the examples.
#' @details Function \code{getMultiFactor} firstly get all the single-factor-scores and then calculating the weighted sum of them to get the combined-factor-score.
#' @export
#' @examples
#' # -- get multi-factor by FactorLists 
#' FactorLists <- list(
#'   buildFactorList(factorFun="gf.PE",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=20),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=60),
#'                   factorDir=-1),       
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=120),
#'                   factorDir=-1))
#' wgts <- c(0.25,0.25,0.25,0.25)
#' 
#' # -- 0. get the all the single-factor-scores
#' TSF <- getMultiFactor(TS,FactorLists)
#' 
#' # -- 1. get the "raw" combined-factor-score
#' TSF.multi <- getMultiFactor(TS,FactorLists,wgts)
#' 
#' # -- 2. get the "clean and standardized" combined-factor-score
#' # --- 2.1 by \code{getTSF}
#' TSF.multi2 <- getTSF(TS,factorFun="getMultiFactor",factorPar=list(FactorLists,wgts),factorOutlier=10,factorStd="sectorNe")
#' # --- 2.2 by \code{Model.TSF}
#' modelPar <- modelPar.factor(modelPar.default(),factorFun="getMultiFactor",factorPar=list(FactorLists,wgts),factorStd="sectorNe",factorDir=1)
#' TSF.multi3 <- Model.TSF(modelPar)
#' TSF.multi4 <- Model.TSF_byTS(modelPar, TS)
getMultiFactor <- function(TS,FactorLists,wgts,
                           factorStd_mult=c("none","norm","sectorNe"),
                           factorNA_mult=c("na","mean","median","min","max","sectmean"),
                           sectorAttr_mult=defaultSectorAttr()){ 
  
  factorStd_mult <- match.arg(factorStd_mult)
  factorNA_mult <- match.arg(factorNA_mult)

  # ---- get all the single-factor-scores
  for(i in 1:length(FactorLists)){
    factorFun <- FactorLists[[i]]$factorFun
    factorPar <- FactorLists[[i]]$factorPar
    factorName <- FactorLists[[i]]$factorName
    factorDir <- FactorLists[[i]]$factorDir
    factorOutlier <- FactorLists[[i]]$factorOutlier
    factorNA <- FactorLists[[i]]$factorNA    
    factorStd <- FactorLists[[i]]$factorStd 
    sectorAttr  <- FactorLists[[i]]$sectorAttr
    cat(paste("Function getMultiFactor: getting the score of factor",factorName,"....\n"))
    # ---- get the raw factorscore
    TSF <- getRawFactor(TS,factorFun,factorPar) 
    # ---- adjust the direction
    TSF$factorscore <- TSF$factorscore*factorDir
    # ---- deal with the outliers
    TSF <- factor.outlier(TSF,factorOutlier)
    # ---- standardize the factorscore 
    TSF <- factor.std(TSF,factorStd,sectorAttr)
    TSF <- factor.std(TSF,factorStd_mult,sectorAttr_mult) 
    if(factorStd=="none" && factorStd_mult=="none"){
      warning(paste("'factorStd' of factor",factorName, "is 'none'. It might make mistake when computing the combined-factorscore or testing by regression method!")) 
    }
    # ---- deal with the missing values 
    TSF <- factor.na(TSF,factorNA)
    TSF <- factor.na(TSF,factorNA_mult)
    if(factorNA=="na" && factorNA_mult=="na" ){
      warning(paste("'factorNA' of factor",factorName, "is 'na'. It might make mistake when computing the combined-factorscore or testing by regression method!")) 
    }
    # ---- merge
    TSF <- renameCol(TSF,"factorscore",factorName)
    if(i==1L){
      re <- merge.x(TS,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    } else {
      re <- merge.x(re,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    }
  }
  
  # ---- get the combi-factor-score
  factorNames <- sapply(FactorLists,"[[","factorName")
  if(!missing(wgts)){
    re <- MultiFactor2CombiFactor(TSF_M=re,wgts=wgts,factorNames=factorNames,keep_single_factors="TRUE")
  }
  
  return(re)
}


#' @rdname getMultiFactor
#' @export
getRawMultiFactor <- function(TS,FactorLists){
  for(i in 1:length(FactorLists)){
    factorFun <- FactorLists[[i]]$factorFun
    factorPar <- FactorLists[[i]]$factorPar
    factorName <- FactorLists[[i]]$factorName
    # factorDir <- FactorLists[[i]]$factorDir
    # factorOutlier <- FactorLists[[i]]$factorOutlier
    # factorNA <- FactorLists[[i]]$factorNA    
    # factorStd <- FactorLists[[i]]$factorStd 
    # sectorAttr  <- FactorLists[[i]]$sectorAttr
    cat(paste("Function getMultiFactor: getting the score of factor",factorName,"....\n"))
    # ---- get the raw factorscore
    TSF <- getRawFactor(TS,factorFun,factorPar) 
    # ---- merge
    TSF <- renameCol(TSF,"factorscore",factorName)
    if(i==1L){
      re <- merge.x(TS,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    } else {
      re <- merge.x(re,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    }
  }
  return(re)
}


#' @rdname getMultiFactor
#' @export
#' @param TSFs a \bold{TSFs} object. see /code{/link{Model.TSFs}}
#' @return \code{getMultiFactorbyTSFs} return a \bold{TSF} object including cols of all the single-factor-scores, and (if param \code{wgts} not missing) the \bold{raw} combi-factor-score, from a \bold{TSFs} list. (If param TSFs is a TSFRs object, then will return a TSFR object.)
#' @examples 
#' # -- get multi-factor by TSFs 
#' MPs <- getMPs_FactorLists(FactorLists, modelPar.default())
#' TSFs <- Model.TSFs(MPs)
#' TSF.multi5 <- getMultiFactorbyTSFs(TSFs,wgts)
getMultiFactorbyTSFs <- function(TSFs,wgts,
                                 factorStd_mult=c("none","norm","sectorNe"),
                                 factorNA_mult=c("na","mean","median","min","max","sectmean"),
                                 sectorAttr_mult=defaultSectorAttr()){
  
  factorStd_mult <- match.arg(factorStd_mult)
  factorNA_mult <- match.arg(factorNA_mult)
  
  # ---- get all the single-factor-scores
  nrows <- sapply(TSFs,NROW)
  if(!all(nrows[1]==nrows)){
    stop("NROWs of TSFs are not all equal!\n")
    print(nrows)
  }
  factorNames <- names(TSFs)
  for(i in 1:length(TSFs)){
    factorName <- factorNames[i]
    TSF <- TSFs[[i]]
    if(TRUE){ # -- factorStd & factorNA
      TSF <- factor.std(TSF,factorStd_mult, sectorAttr = sectorAttr_mult)
      TSF <- factor.na(TSF,factorNA_mult)
      if(!is.null(attr(TSF,"MP"))) {
        factorStd <- attr(TSF,"MP")$factor$factorStd
        if(factorStd=="none" && factorStd_mult=="none" ){
          warning(paste("'factorStd' of factor",factorName, "is 'none'. It might make mistake when computing the combined-factorscore or testing by regression method!")) 
        }
        factorNA <- attr(TSF,"MP")$factor$factorNA
        if(factorNA=="na" && factorNA_mult=="na"){
          warning(paste("'factorNA' of factor",factorName, "is 'na'. It might make mistake when computing the combined-factorscore or testing by regression method!")) 
        }
      }
    }
    TSF <- renameCol(TSF,"factorscore",factorNames[i])
    if(i==1L){
      # re <- TSF[, intersect(c("date","stockID","date_end","periodrtn","sector",factorNames[i]),colnames(TSF))]
      keep_cols <- is_usualcols(cols = colnames(TSF), usualcols = c(usualcols(),factorNames[i]))
      re <- TSF[, colnames(TSF)[keep_cols]]      
      
    } else {
      re <- merge.x(re,TSF[, c("date","stockID",factorNames[i])], by=c("date","stockID"))
    }
  }  
  
  # ---- get the combi-factor-score(weighted sum of all the single-factor-scores)
  if(!missing(wgts)){
    re <- MultiFactor2CombiFactor(TSF_M=re,wgts=wgts,factorNames=factorNames,keep_single_factors="TRUE")
  }
  
  return(re)
}

#' @rdname getMultiFactor
#' @param TSF_M a dataframe of 'TS & MultiFactors'
#' @param factorNames
#' @param keep_single_factors
#' @export
MultiFactor2CombiFactor <- function(TSF_M,
                                    wgts,
                                    factorNames = setdiff(colnames(TSF_M),usualcols()),
                                    keep_single_factors="TRUE"){
  # ---- get the combi-factor-score(weighted sum of all the single-factor-scores)
  TSF_mat <- TSF_M[,factorNames,drop=FALSE]
  if(!missing(wgts)){
    if(ncol(TSF_mat) != length(wgts)) {
      stop("The numbers of factors and wgts are not equal!")
    }
    if(!isTRUE(all.equal(sum(wgts),1,tolerance=0.001))){
      warning("The sum of wgts is not 1. The wgts will be rescaled! ")
      wgts <- wgts/sum(wgts)
    }  
    sumScore <- as.matrix(TSF_mat) %*% wgts
    if(keep_single_factors){
      re <- cbind(TSF_M,factorscore=sumScore)
    } else {
      re <- cbind(dplyr::select(TSF_M,-one_of(factorNames)),factorscore=sumScore)
    }
  }
  return(re)
}




MultiFactor2TSFs <- function(TSF_M,
                             factorNames = setdiff(colnames(TSF_M),usualcols())){
  
  
  
}



# --------------------  ~~ intermediately called functions ----------------
# ---- deal with the outliers of factorscore
factor.outlier <- function (TSF, factorOutlier) {
  TSF <- plyr::ddply(TSF,"date",
               function(x,outlier){  
                 outlier_u <- with(x,mean(factorscore,na.rm=TRUE)+outlier*sd(factorscore,na.rm=TRUE))
                 outlier_l <- with(x,mean(factorscore,na.rm=TRUE)-outlier*sd(factorscore,na.rm=TRUE))
                 transform(x,factorscore = ifelse(factorscore > outlier_u, outlier_u,
                                                  ifelse(factorscore < outlier_l, outlier_l, factorscore)))
               },outlier=factorOutlier)
  return(TSF)
}
# ---- standardize the factorscore
factor.std <- function (TSF,factorStd=c("none","norm","sectorNe"),sectorAttr) {
  factorStd <- match.arg(factorStd)
  if(factorStd == "norm"){ 
    TSF <- plyr::ddply(TSF,"date",transform,factorscore=scale(factorscore))      
  } else if (factorStd == "sectorNe"){
    TSF <- getSectorID(TSF,sectorAttr=sectorAttr)  
    TSF <- data.table::data.table(TSF,key=c("date","sector"))
    TSF <- TSF[,factorscore:=scale(factorscore), by = c("date","sector")]
    TSF <- as.data.frame(TSF)
  }
  return(TSF)
}
# ---- deal with the missing values of factorscore
factor.na <- function (TSF, method=c("na","mean","median","min","max","sectmean"), trim = 0.025) {
  method <- match.arg(method)
  if(method=="mean"){
    TSS <- gf.sector(TSF[,c('date','stockID')],defaultSectorAttr())
    TSS <- TSS[,c('date','stockID','sector')]
    TSF <- merge.x(TSF,TSS,by=c('date','stockID'))
    TSF <- dplyr::group_by(TSF, date, sector)
    if(is.na(trim)){
      #TSF <- plyr::ddply(TSF,"date",transform,factorscore=ifelse(is.na(factorscore),mean(factorscore,na.rm=TRUE),factorscore))
      TSF <- dplyr::mutate(TSF, factorscore=ifelse(is.na(factorscore),mean(factorscore,na.rm=TRUE),factorscore))
    }else{
      #TSF <- plyr::ddply(TSF,"date",transform,factorscore=ifelse(is.na(factorscore),mean(factorscore,na.rm=TRUE,trim=trim),factorscore))
      TSF <- dplyr::mutate(TSF, factorscore=ifelse(is.na(factorscore),mean(factorscore,na.rm=TRUE,trim=trim),factorscore))
    }
    TSF <- as.data.frame(TSF[,c('date','stockID','factorscore')])
  } else if(method=="median"){
    #TSF <- plyr::ddply(TSF,"date",transform,factorscore=ifelse(is.na(factorscore),median(factorscore,na.rm=TRUE),factorscore))
    TSS <- gf.sector(TSF[,c('date','stockID')],defaultSectorAttr())
    TSS <- TSS[,c('date','stockID','sector')]
    TSF <- merge.x(TSF,TSS,by=c('date','stockID'))
    TSF <- dplyr::group_by(TSF, date, sector)
    TSF <- dplyr::mutate(TSF,factorscore=ifelse(is.na(factorscore),median(factorscore,na.rm=TRUE),factorscore))
    TSF <- as.data.frame(TSF[,c('date','stockID','factorscore')])
  } else if(method=="min"){
    TSF <- plyr::ddply(TSF,"date",transform,factorscore=ifelse(is.na(factorscore),min(factorscore,na.rm=TRUE),factorscore))
  } else if(method=="max"){
    TSF <- plyr::ddply(TSF,"date",transform,factorscore=ifelse(is.na(factorscore),max(factorscore,na.rm=TRUE),factorscore))
  } else if(method=="sectmean"){
    TSF <- gf.ezsec(TSF)
    TSF <- dplyr::group_by(TSF, date, secID)
    if(is.na(trim)){
      TSF <- dplyr::mutate(TSF, factorscore=ifelse(is.na(factorscore),mean(factorscore,na.rm=TRUE),factorscore))
    }else{
      TSF <- dplyr::mutate(TSF, factorscore=ifelse(is.na(factorscore),mean(factorscore,na.rm=TRUE,trim=trim),factorscore))
    }
    TSF <- as.data.frame(TSF)
  }
  return(TSF)
}
# ---- get simple sector categories
gf.ezsec <- function(ts){
  ts.tmp <- subset(ts, select = c("date","stockID"))
  ts.tmp <- gf.sector(ts.tmp, sectorAttr = defaultSectorAttr())
  seclist <- list()
  # BigCycle
  seclist[[1]]<- c("ES33110000","ES33210000","ES33220000","ES33230000","ES33240000")
  #FinRealEstate
  seclist[[2]]<- c("ES33480000","ES33490000","ES33430000")
  #TMT
  seclist[[3]]<- c("ES33710000","ES33720000","ES33730000","ES33270000")
  #Comsump
  seclist[[4]]<- c("ES33280000","ES33330000","ES33340000","ES33350000","ES33460000","ES33370000","ES33450000")
  #Manufac
  seclist[[5]]<- c("ES33360000","ES33630000","ES33640000","ES33610000","ES33620000","ES33650000")
  #Others
  seclist[[6]]<- c("ES33420000","ES33410000","ES33510000")
  for(ii in 1:length(seclist)){
    V2 <- paste("ES",ii,sep = "")
    seclist[[ii]] <- as.data.frame(seclist[[ii]])
    seclist[[ii]] <- cbind(seclist[[ii]], V2)
    names(seclist[[ii]]) <- c("sector","secID")
  }
  secdf <- data.table::rbindlist(seclist)
  re <- merge(ts.tmp, secdf, by = c("sector"))
  re2 <- subset(re, select = c("date","stockID","secID"))
  re3 <- merge(ts,re2,by=c("date","stockID"))
  return(re3)
}





