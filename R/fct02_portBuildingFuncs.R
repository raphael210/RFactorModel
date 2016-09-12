

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- get portfolio object and rtn object ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


rm.sus <- function(TS){  
  check.TS(TS)
  
  TS_next <- data.frame(date=trday.nearby(TS$date,by=-1), stockID=TS$stockID)
  TS_next <- TS.getTech_ts(TS_next, funchar="istradeday4()",varname="trading")
  TS <- TS[TS_next$trading == 1, ]
  return(TS)
}


#' portfolio building and backtesting
#' 
#' @rdname PortfolioBacktest
#' @name PortfolioBacktest
#' @aliases getPort 
#' @details \code{getPort} get the \bold{Port}('portfolio') objects, subsetting from the \bold{TSF} object.
#' @param TSF a \bold{TSF} object or a \bold{TSFR} object
#' @param topN an integer vector with 2 or 1 elements, giving the rank range of the assets to be selected into the portfolio. If containing only 1 element, the the top rank of 1 will be added automatically.
#' @param topQ a numeric vector with 2 or 1 elements, giving the percentage range of the assets to be selected into the portfolio.  If containing only 1 element, the top percentage of 0 will be added automatically.
#' @param factorNA
#' @param pick.sectorNe
#' @param sectorAttr
#' @param buffer.in a numeric between 0 and 1. eg. 0.1 means that stock with rank less than topN*90\% or pct less than topQ*90\% will be incorporated coercively.
#' @param buffer.keep a numeric between 0 and 1. eg. 0.1 means that stock with rank less than topN*110\% or pct less than topQ*110\% will be kept coercively.
#' @param init_port a charactor vector of stockIDs.
#' @param backtestPar a \bold{backtestPar} object. If param backtestPar is not missing,then the other params will be extracted from backtestPar.It is usefull when the backtestPar has been initialized.  
#' @param dir a character string "long" or "short". In \code{getPort}, if "short",select from top to bottom, and vice versa.
#' @return \code{getPort} return a \bold{Port}('portfolio') objects,which are of dataframe class containing at least 2 cols("date","stockID")
#' @note Note that \code{topN} and \code{topQ} should at least have one and only have one.
#' @note Note that if use bufferring, the length of topN (or topQ) should be 1.
#' @note Note that if pick.sectorNe is TRUE, the param topN and topQ will act \bold{in the sector}. That is , for example, topN=10 means picking 10 stocks in every sectors. For that reason, if pick.sectorNe is true, topQ is a better choise than topN.
#' @author Ruifei.Yin
#' @export
#' @examples
#' modelPar <- modelPar.default()
#' TSFR <- Model.TSFR(modelPar)
#' # -- good ports
#' Lport1 <- getPort(TSFR,20)
#' Lport2 <- getPort(TSFR,c(1,20)) # the same as Lport1
#' Lport3 <- getPort(TSFR,,0.2)
#' Lport4 <- getPort(TSFR,,c(0.1,0.2))
#' # -- bad ports
#' Sport1 <- getPort(TSFR,20,dir="short")
#' Sport2 <- getPort(TSFR,c(10,20),dir="short")
#' Sport3 <- getPort(TSFR,,0.2,dir="short")
#' Sport4 <- getPort(TSFR,,c(0.1,0.2),dir="short")
#' # -- N groups of ports
#' N <- 5
#' tmp <- seq(0,1,by=1/N)
#' groups <- cbind(tmp[-(N+1)],tmp[-1])
#' port.list <- apply(groups,1,function(x)getPort(TSFR,,x))
getPort <- function(TSF, topN=NA, topQ=NA, 
                    factorNA=c("median","mean","na","min","max"),
                    pick.sectorNe=FALSE, sectorAttr=defaultSectorAttr(),
                    buffer.in=0, buffer.keep=0, init_port=NULL,
                    backtestPar,
                    dir=c("long","short")){
  factorNA <- match.arg(factorNA)
  dir <- match.arg(dir)
  if(!missing(backtestPar)){
    topN <- getbacktestPar.longshort(backtestPar,"topN")
    topQ <- getbacktestPar.longshort(backtestPar,"topQ")
    factorNA <- getbacktestPar.longshort(backtestPar,"factorNA")
    pick.sectorNe <- getbacktestPar.longshort(backtestPar,"pick.sectorNe")
    sectorAttr <- getbacktestPar.longshort(backtestPar,"sectorAttr")
    buffer.in <- getbacktestPar.longshort(backtestPar,"buffer.in")
    buffer.keep <- getbacktestPar.longshort(backtestPar,"buffer.keep")
    init_port <- getbacktestPar.longshort(backtestPar,"init_port")
  }
  check.TSF(TSF)
  TSF <- factor.na(TSF,factorNA)
  if (any(is.na(topN)) && any(is.na(topQ)) || all(!is.na(topN)) && all(!is.na(topQ))){
    stop("'topN' and 'topQ' should at least have one and only have one!")
  }
  if(length(topN) == 1L) topN_ <- c(1,topN) else topN_ <- topN
  if(length(topQ) == 1L) topQ_ <- c(0,topQ) else topQ_ <- topQ
  if(pick.sectorNe){
    TSF <- getSectorID(TSF,sectorAttr=sectorAttr)
    TSF_by <- group_by(TSF,date) %>% dplyr::mutate(cnt=n()) %>% group_by(sector,add=TRUE) %>% dplyr::mutate(cnt_sct=n())
  } else {
    TSF_by <- group_by(TSF,date) %>% dplyr::mutate(cnt=n()) 
  }
  if(dir == "long"){
    TSF_by <- dplyr::mutate(TSF_by,rnk=rank(-factorscore)) %>% dplyr::arrange(rnk)
  } else {
    TSF_by <- dplyr::mutate(TSF_by,rnk=rank(factorscore)) %>% dplyr::arrange(rnk)
  }
  
  if(all(!is.na(topN))){  ## get port by topN
    port <- dplyr::filter(TSF_by, rnk>=min(topN_)/cnt*n() & rnk<=max(topN_)/cnt*n())
  } else { ## get port by topQ
    port <- slice(TSF_by,max(1,ceiling(min(topQ_)*n())) : floor(max(topQ_)*n()))
  }
  
  # ---- buffering
  if (!(identical(buffer.in,0) && identical(buffer.keep,0))){
    timeidx <- sort(unique(TSF_by$date))
    # the initial port
    if (is.null(init_port)) { 
      init_port <- port[port$date==timeidx[1],"stockID",drop=TRUE]
    } 
    # looping
    port <- data.frame()
    for (idx in timeidx) {
      subTSF <- TSF_by[TSF_by$date==idx,]        
      if (idx == timeidx[[1]]){
        subTSF <- dplyr::mutate(subTSF,old=stockID %in% init_port)  
      } else {
        subTSF <- dplyr::mutate(subTSF,old=stockID %in% subport$stockID)
      }
      if (all(!is.na(topN))){ ## get port by topN
        if(length(topN) > 1L) stop("If use buffer, the length of topN must be 1!")
        topN.in <- round(topN*(1-buffer.in))
        topN.keep <- round(topN*(1+buffer.keep))  
        # flag: 
        # 1-coercively incorporated new stocks,
        # 2-coercively keep old stocks,
        # 3-others
        subTSF <- dplyr::mutate(subTSF,flag=ifelse(rnk<=topN.in/cnt*n(),1,ifelse(rnk<=topN.keep/cnt*n() & old,2,3)))
        subTSF <- dplyr::arrange(subTSF,flag,rnk)
        subTSF <- dplyr::mutate(subTSF,rnk_new=order(order(flag,rnk)))
        subport <- dplyr::filter(subTSF, rnk_new<=topN/cnt*n())
      } else if (all(!is.na(topQ))){ ## get port by topQ
        if(length(topQ) > 1L) stop("If use buffer, the length of topQ must be 1!")  
        topQ.in <- topQ*(1-buffer.in)
        topQ.keep <- topQ*(1+buffer.keep)
        subTSF <- dplyr::mutate(subTSF,flag=ifelse(rnk<=topQ.in*n(),1,ifelse(rnk<=topQ.keep*n() & old,2,3)))
        subTSF <- dplyr::arrange(subTSF,flag,rnk)
        subTSF <- dplyr::mutate(subTSF,rnk_new=order(order(flag,rnk)))
        subport <- slice(subTSF,1:(topQ*n()))
      }
      port <- rbind(port,subport)
    }
  }
  return(port)
}



#' @details \code{addwgt2port} add the weights to the \bold{Port} object.
#' @rdname PortfolioBacktest
#' @param wgtType a character string, giving the weighting type of portfolio,which could be "eq"(equal),"fv"(floatValue),"fvsqrt"(sqrt of floatValue) or "custom".
#' @param wgt.sectorNe a logic. If true, the wgt will be neutralized by sector.
#' @param wgtbmk
#' @param tolerance a numeric, only used when \code{wgt.sectorNe} is true. Giving the tolerance of absent sectors weights in the portfolio.
#' @return \code{addwgt2port} return a \bold{Port} object which are of dataframe class containing at least 3 cols("date","stockID","wgt").
#' @export
#' @examples 
#' # -- add wgt to port
#' port <- getPort(TSF,20)
#' port <- addwgt2port(port,"fv")
addwgt2port <- function (port,
                         wgtType= c("eq","fv","fvsqrt","custom"),
                         wgt.sectorNe=FALSE, wgtbmk="EI000300", sectorAttr=defaultSectorAttr(),
                         backtestPar,                       
                         tolerance=0.2) {  
  wgtType <- match.arg(wgtType)
  if(!missing(backtestPar)){
    wgtType <- getbacktestPar.longshort(backtestPar,"wgtType")
    wgt.sectorNe <- getbacktestPar.longshort(backtestPar,"wgt.sectorNe") 
    sectorAttr <- getbacktestPar.longshort(backtestPar,"sectorAttr")
    wgtbmk <- getbacktestPar.longshort(backtestPar,"bmk")
  }     
  if (wgtType=="custom") {
    coltest <- c("date","stockID","wgt")    
  } else {
    coltest <- c("date","stockID")
  }
  check.colnames(port,coltest)
  # ---- add weights
  if (wgtType=="eq") {
    port <- plyr::ddply(port,"date",transform,wgt=1/length(stockID))    
  } else if (wgtType %in% c("fv","fvsqrt")) {    
    port <- TS.getTech(port,variables="float_cap")
    if (wgtType=="fv") {
      port <- plyr::ddply(port,"date",transform,wgt=float_cap/sum(float_cap,na.rm=TRUE))
    } else {
      port <- plyr::ddply(port,"date",transform,wgt=sqrt(float_cap)/sum(sqrt(float_cap),na.rm=TRUE))
    }   
    port$float_cap <- NULL
  } else if (wgtType=="custom"){
    port <- port
  }
  # ---- neutrualizing wgt by sectors 
  if (wgt.sectorNe) {
    # --- get the bmk sector wgt
    datelist <- unique(port$date)    
    wgt.bmk <- getIndexCompWgt(indexID=wgtbmk,endT=datelist)      
    wgt.bmk <- getSectorID(wgt.bmk, sectorAttr=sectorAttr)
    wgt.bmk.sector <- plyr::ddply(wgt.bmk,c("date","sector"),plyr::summarize,wgt.sector=sum(wgt,na.rm=TRUE))  
    # --- merge and rescale the wgt to neutrualiezde
    port<- getSectorID(port,sectorAttr=sectorAttr)
    port <- merge(port,wgt.bmk.sector,by=c("date","sector"),all.x=TRUE)
    port[is.na(port$wgt.sector),"wgt.sector"] <- 0  # -- dealing with sectors in port but not in bmk
    port <- plyr::ddply(port,c("date","sector"),transform,wgt=wgt/sum(wgt,na.rm=TRUE)*wgt.sector)    
    if (TRUE) { # -- dealing with sectors in bmk but not in port, by rescaling wgt to sum 1.
      warning.tab <- plyr::ddply(port,"date",plyr::summarize,sum.wgt=sum(wgt,na.rm=TRUE))
      warning.tab <- subset(warning.tab, sum.wgt < 1-tolerance)
      if (nrow(warning.tab) > 0) {
        warning("There are some sectors that are in the bmk but absent in the portfolio. The weights will be rescaled! \nThe following table are the details:")
        print(warning.tab)
      }
      port <- plyr::ddply(port,"date",transform,wgt=wgt/sum(wgt,na.rm=TRUE))
    } 
  }
  return(port)
}





#' @details \code{port.substitute} redistribute the extra weight to other stocks to reduce the risk of concentration,when too much weight on single stock. 
#' 
#' TSF is used when the stocks in \code{port} is not enough to share the extra weights. In that case, stock in TSF, which is in the same sector, will be imported into the port to share the extra weights.
#' @rdname PortfolioBacktest
#' @param wgt.max a numeric or NA, giving the maximum of weight which could be set on a single stock. If NA(the default value), with no limit, return \code{port} itself without doing anything.
#' @param dir a character string of "long" or "short". In \code{port.substitute}, indicating the port direction. If "long",the port will be look as a long portfolio, and will import stocks from the top of the TSF; vise versa.
#' @return \code{port.substitute} return a \bold{Port} object
#' @note If the \code{port} and \code{TSF} originally contain the column "sector", it will be droped before the new sector imported according to the specified auguments.
#' @export
#' @examples
#' # -- reduce the risk of concentration
#' port <- port.substitute(port,TSF,wgt.max=0.08)
port.substitute <- function(port,TSF,
                            wgt.max=NA,
                            sectorAttr=defaultSectorAttr(),
                            backtestPar,
                            dir=c("long","short")){
  dir <- match.arg(dir)
  if(!missing(backtestPar)){
    wgt.max <- getbacktestPar.longshort(backtestPar,"wgt.max")
    sectorAttr <- getbacktestPar.longshort(backtestPar,"sectorAttr")
  }
  if(is.na(wgt.max)){ # 
    return(port)
  }  
  check.Port(port)
  port <- getSectorID(port,sectorAttr=sectorAttr)  
  port <- dplyr::arrange(port,date,sector,desc(wgt))  
  check.TSF(TSF)
  TSF <- getSectorID(TSF,sectorAttr=sectorAttr)
  subfun <- function(subT){
    wgt.ex <- 0
    need.TSF <- TRUE
    for (i in 1:nrow(subT)) {
      subT[i,"wgt"] <- subT[i,"wgt"] + wgt.ex
      if (subT[i,"wgt"] > wgt.max) {
        wgt.ex <- subT[i,"wgt"]-wgt.max
        subT[i,"wgt"] <- wgt.max            
      } else {      
        need.TSF <- FALSE
        break
      }  
    }
    if (need.TSF) { # -- the weight is so much that the stocks in the port is not enough to share.Need to import more stocks from the TSF. 
      subTSF <- subset(TSF,date==subT[1,"date"] & sector==subT[1,"sector"],
                       select=c("date","sector","stockID","factorscore"))
      subTSF <- subset(subTSF,!(stockID %in% subT$stockID))
      subTSF <- if(dir=="long") dplyr::arrange(subTSF,desc(factorscore)) else dplyr::arrange(subTSF,factorscore)
      N <- wgt.ex %/% wgt.max + 1
      if(N > nrow(subTSF)){
        stop(paste("There are not enough stocks in TSF to share the extra weight in sector",subTSF[1,"sector"],"on",subTSF[1,"date"],"!"))
      } else {
        subTSF <- head(subTSF,N)
        subTSF[1:N-1,"wgt"] <- wgt.max
        subTSF[N,"wgt"] <- wgt.ex %% wgt.max
        subT <- plyr::rbind.fill(subT,subTSF)
        warning(paste("Too much weight is set on single stocks in sector",subT[1,"sector"],"on",subT[1,"date"],".\n",
                      N,"more stocks are imported to share",round(wgt.ex,4),"extra weight."))
      }      
    }
    return(subT)
  }
  port <- plyr::ddply(port,c("date","sector"),subfun)
  return(port)
}




#' @details \code{port.backtest} backtest the \bold{Port} object. Get a \bold{PB}("PortfolioBacktest") object.
#' @rdname PortfolioBacktest
#' @param holdingEndT a Date object, giving the ending date of the holding portfolio 
#' @param fee.buy giving the buying fee of each assets. See \code{\link{Return.rebalancing_yrf}} for detail
#' @param fee.sell See \code{\link{Return.rebalancing_yrf}} for detail
#' @param dir a character string of "long" or "short". In \code{port.backtest}, if "long", the port will be look as asset, the fee will be cut from the asset; if "short", the port will be look as liability, the fee will be added to the liability.
#' @return \code{port.backtest} return a \bold{PB}("PortfolioBacktest") object. a list with 3 items:
#'   \itemize{ 
#'   \item rtn: a time series of portfolio return, with the class of xts.
#'   \item dailywgt: 
#'   \item rebtrade:
#'   }
#' @seealso \code{\link[QUtility]{Return.rebalancing_yrf}}
#' @export
#' @examples
#' # -- backtest the Port object
#' PB <- port.backtest(port)
port.backtest <- function(port,
                          holdingEndT=Sys.Date(), fee.buy=0, fee.sell=0,                          
                          backtestPar,
                          dir=c("long","short")){  
  dir <- match.arg(dir)
  if(!missing(backtestPar)){
    holdingEndT <- getbacktestPar.longshort(backtestPar,"holdingEndT")
    fee.buy <- getbacktestPar.fee(backtestPar,"secu")
    fee.sell <- getbacktestPar.fee(backtestPar,"secu")
  }   
  if(dir == "short"){
    fee.buy <- -fee.buy
    fee.sell <- -fee.sell
  }
  # ---- get weights
  check.colnames(port,c("date","stockID","wgt"))
  weights.df <- reshape2::dcast(port,date~stockID,value.var="wgt",fill=0)
  weights <- xts(weights.df[,-1],weights.df[,1])
  colnames(weights) <- colnames(weights.df)[-1]  
  # ---- get R
  stocks <- colnames(weights) 
  qt <- getQuote(stocks,begT=min(port$date),endT=holdingEndT,variables=c("pct_chg"))
  qt <- renameCol(qt,"pct_chg","rtn")
  R.df <- reshape2::dcast(qt,date~stockID,value.var="rtn",fill=0)
  R <- xts(R.df[,-1],R.df[,1])
  colnames(R) <- colnames(R.df)[-1]
  # ---- get PB
  re <- Return.rebalancing_yrf(R = R,weights = weights,fee.buy = fee.buy,fee.sell = fee.sell,verbose = TRUE)
  return(re) 
}


#' @details \code{getPort_throughout}, which is a wrapped function of \code{getPort}, \code{addwgt2Port}, \code{port.substitute}, get \bold{Port} object from \bold{TSF}, with further treatment.
#' @rdname PortfolioBacktest
#' @export
#' @return \code{getPort_throughout} return a \bold{Port} object, which contain the col of 'wgt'.
#' @examples
#' # -- get Port object from TSF throughout
#' Port_throut <- getPort_throughout(TSF, topN=20, wgt.max=0.8, dir="long")
getPort_throughout <- function (TSF,
                                # getPort
                                topN=NA, topQ=NA, 
                                factorNA=c("median","mean","na","min","max"),
                                pick.sectorNe=FALSE, 
                                buffer.in=0, buffer.keep=0, init_port=NULL,
                                # addwgt2port
                                wgtType= c("eq","fv","fvsqrt","custom"),
                                wgt.sectorNe=FALSE, wgtbmk="EI000300",
                                # port.substitute
                                wgt.max=NA, 
                                # sector setting
                                sectorAttr=defaultSectorAttr(),
                                
                                backtestPar,
                                dir=c("long","short")) {
  dir <- match.arg(dir)
  Port <- getPort(TSF, 
                  topN=topN, topQ=topQ,
                  factorNA=factorNA,
                  pick.sectorNe=pick.sectorNe,
                  buffer.in=buffer.in, buffer.keep=buffer.keep, init_port=init_port,
                  dir=dir, backtestPar=backtestPar)
  
  Port <- addwgt2port(Port, 
                      wgtType=wgtType, 
                      wgt.sectorNe=wgt.sectorNe,                       
                      wgtbmk=wgtbmk,
                      sectorAttr=sectorAttr, 
                      backtestPar=backtestPar)
  
  Port <- port.substitute(port=Port, TSF=TSF, 
                          wgt.max=wgt.max, 
                          sectorAttr=sectorAttr, 
                          dir=dir, backtestPar=backtestPar)
  
  return(Port)
}
  


#' @details \code{getPB}, which is a wrapped function of \code{getPort}, \code{addwgt2Port}, \code{port.substitute}, \code{port.backtest}, get \bold{PB} object from \bold{TSF} directly.
#' @rdname PortfolioBacktest
#' @export
#' @return \code{getPB} return a \bold{PB}("PortfolioBacktest") object.
#' @examples
#' # -- get PB object from TSF directly
#' PB <- getPB(TSF, topN=20, dir="long")
getPB <- function (TSF,
                   # getPort
                   topN=NA, topQ=NA, 
                   factorNA=c("median","mean","na","min","max"),
                   pick.sectorNe=FALSE, 
                   buffer.in=0, buffer.keep=0, init_port=NULL,
                   # addwgt2port
                   wgtType= c("eq","fv","fvsqrt","custom"),
                   wgt.sectorNe=FALSE, wgtbmk="EI000300",
                   # port.substitute
                   wgt.max=NA, 
                   # port.backtest
                   holdingEndT=Sys.Date(), fee.buy=0, fee.sell=fee.buy,
                   # sector setting
                   sectorAttr=defaultSectorAttr(),
                   
                   backtestPar,
                   dir=c("long","short")) {
  dir <- match.arg(dir)
  Port <- getPort(TSF, 
                  topN=topN, topQ=topQ,
                  factorNA=factorNA,
                  pick.sectorNe=pick.sectorNe,
                  buffer.in=buffer.in, buffer.keep=buffer.keep, init_port=init_port,
                  dir=dir, backtestPar=backtestPar)
  
  Port <- addwgt2port(Port, 
                      wgtType=wgtType, 
                      wgt.sectorNe=wgt.sectorNe,                       
                      wgtbmk=wgtbmk,
                      sectorAttr=sectorAttr,  
                      backtestPar=backtestPar)
  
  Port <- port.substitute(port=Port, TSF=TSF, 
                          wgt.max=wgt.max, 
                          sectorAttr=sectorAttr, 
                          dir=dir, backtestPar=backtestPar)
  
  PB <- port.backtest(Port, 
                      holdingEndT= holdingEndT, 
                      fee.buy=fee.buy, fee.sell=fee.sell,
                      dir=dir, backtestPar=backtestPar)
  return(PB)
}


# consider: port.summary(port) vs. PB.summary(PB) ?
port.summary <- function(port){
  # sector distribution
  # number of stocks
}
# consider: port.risk vs. PB.risk ?
port.risk <- function(port){
  # risk annalysis
}


#' getrtn.bmk
#' 
#' get the benchmark return series corresponding to the specific rtn series.
#' @param rtn an xts,timeSeries or zoo object of portfolio returns,which must be 1 column
#' @param bmk a character string, giving the stockID of the benchmark index,eg. "EI000300".
#' @return a xts object, giving the rtn series of the benchmark
#' @author Ruifei.Yin
#' @export
#' @family LSH-frame building functions
#' @examples
#' rtn <- zoo(rnorm(1000,0.001,0.02),as.Date("2010-01-01")+1:1000)
#' re <- getrtn.bmk(rtn,"EI000300")
getrtn.bmk <- function(rtn,bmk="EI000300"){
  #   if(datasrc=="ts"){
  #     bmk <- stockID2stockID(bmk,from="local",to="ts")
  #     qt <- getQuote_ts(stocks=bmk,begT=as.Date(start(rtn)),endT=as.Date(end(rtn)),variables=c("price","yclose"),cycle="cy_day()")
  #     bmkrtn <- qt$price/qt$yclose-1
  #     re <- xts(bmkrtn,as.Date(qt$date,tz=""))
  #   } else if (datasrc=="local"){
  qt <- getIndexQuote(stocks=bmk,begT=as.Date(start(rtn)),endT=as.Date(end(rtn)),variables="pct_chg")
  re <- xts(qt$pct_chg,qt$date)
  #   }    
  colnames(re) <- "bmk"
  return(re)
}

# getrtn.bmk2 <- function(rtn,bmk="IF00"){
#   qt <- getQuote_ts(stocks=bmk,begT=as.Date(start(rtn)),endT=as.Date(end(rtn)),variables=c("price","yclose"),cycle="cy_day()")
#   bmkrtn <- qt$price/qt$yclose-1
#   re <- xts(bmkrtn,as.Date(qt$date,tz=""))   
#   colnames(re) <- "bmk"
#   return(re)
# }



#' addrtn.hedge
#' 
#' Add the hedged return on the long and short return series.
#' 
#' Note that the rowsum of weight is not required to be equal to 1. If the portfolio is not full position, then the sum of weight could be smaller than 1; If credit and short is allowed, then the sum of weight could be smaller than 0 or larger than 1.
#' @param rtn.long an xts,timeSeries or zoo object of long portfolio returns,which must be 1 column
#' @param rtn.short an xts,timeSeries or zoo object of benchmark or short portfolio returns,which must be 1 column
#' @param rebFreq an interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param weight a time series or single-row matrix(vector) containing asset weights. If a vector,the weights time series will be computed automatically via different rebFreq. See example for detail.
#' @param fee.long a numeric, giving the Unilateral transaction fee of long asset
#' @param fee.short a numeric, giving the Unilateral transaction fee of short asset
#' @return a \bold{rtn.LSH} object of class xts,giving the return series of long,short and hedge.
#' @author Ruifei.Yin
#' @export
#' @family LSH-frame building functions
#' @examples
#' rtn.long <- zoo(rnorm(1000,0.001,0.02),as.Date("2010-01-01")+1:1000)
#' rtn.short <- rtn.long + rnorm(1000,-0.0001,0.003)
#' rebFreq <- "month"
#' # use the default weight: full position hedging, rebalancing by month
#' re <- addrtn.hedge(rtn.long,rtn.short,rebFreq,c(1,-1))
#' # 70 percent postion hedging,rebalancing by month
#' re2 <- addrtn.hedge(rtn.long,rtn.short,rebFreq,c(0.7,-0.7))
#' # use the time series weight
#' wgt.idx <- unique(as.Date(cut(zoo::index(rtn.long),"month")))
#' wgt <- xts(matrix(rep(c(0.7,-0.7),each=length(wgt.idx)),length(wgt.idx),2),wgt.idx)
#' re3 <- addrtn.hedge(rtn.long,rtn.short,weight=wgt) # the same as re2
addrtn.hedge <- function (rtn.long, rtn.short, rebFreq="month",weight=c(1,-1),fee.long=0,fee.short=0) {
  nm.long <- if(is.null(colnames(rtn.long))) "long" else colnames(rtn.long)
  nm.short <- if(is.null(colnames(rtn.short))) "short" else colnames(rtn.short)
  if(nm.long==nm.short){
    nm.long <- "long"
    nm.short <- "short"
  }
  rtn.long <- xts::try.xts(rtn.long)
  rtn.short <- xts::try.xts(rtn.short)
  rtn <- merge(rtn.long,rtn.short,all=FALSE)
  colnames(rtn) <- c(nm.long,nm.short)
  if(is.vector(weight)){ # create a weight time series via the rebFreq
    weight.index <- unique(as.Date(cut(zoo::index(rtn),rebFreq)))
    weight <- xts(matrix(rep(weight,each=length(weight.index)),length(weight.index),2),weight.index)    
  }
  colnames(weight) <- colnames(rtn)
  fee.buy <- fee.sell <- c(fee.long,fee.short)
  rtn.hedge <- Return.rebalancing_yrf(rtn,weight,fee.buy,fee.sell,warning.wgtsum=FALSE)
  rtn <- merge(rtn,rtn.hedge,all=FALSE)
  colnames(rtn) <- c(nm.long,nm.short,"hedge")
  return(rtn)
}


#' getrtn.LSH
#' 
#' get the \bold{rtn.LSH} object from the \bold{PB} object.
#' 
#' If param backtestPar is not missing,then the related params will be extracted from backtestPar.It is usefull when the backtestPar has been initialized. 
#' @param PB.L a \bold{PB} object of the long portfolio
#' @param PB.S a \bold{PB} object of the short portfolio
#' @param hedge.rebFreq giving the rebalance freq when computing the hedged rtn.An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param hedge.posi a numeric, giving the position of the hedging assets
#' @param fee.long a numeric, giving the fee of long asset
#' @param fee.short a numeric, giving the fee of short asset
#' @param backtestPar a \bold{backtestPar} object
#' @return a \bold{rtn.LSH} object of class xts,giving the return series of long,short and hedge.
#' @author Ruifei.Yin
#' @export
#' @family LSH-frame building functions
#' @examples
#' port.L <- getPort(TSF,20, dir="long")
#' port.S <- getPort(TSF,20, dir="short")
#' PB.L <- port.backtest(port.L)
#' PB.S <- port.backtest(port.S)
#' re <- getrtn.LSH(PB.L,PB.S)
getrtn.LSH <- function (PB.L, PB.S,
                        hedge.rebFreq="month", hedge.posi=1, fee.long=0, fee.short=0,
                        backtestPar) {  
  if(!missing(backtestPar)){
    hedge.rebFreq <- getbacktestPar.longshort(backtestPar,"hedge.rebFreq")
    hedge.posi <- getbacktestPar.longshort(backtestPar,"hedge.posi")
    fee.long <- getbacktestPar.fee(backtestPar,"secu")
    fee.short <- getbacktestPar.fee(backtestPar,"secu")
  }
  rtn.long <- PB.L$rtn
  rtn.short <- PB.S$rtn  
  rtn.LSH <- addrtn.hedge(rtn.long, rtn.short, hedge.rebFreq,
                          weight=c(hedge.posi,-hedge.posi),
                          fee.long, fee.short)
  return(rtn.LSH)
}

#' getrtn.LBH
#' 
#' get the \bold{rtn.LBH} object from the \bold{TSF} object.
#' 
#' If param backtestPar is not missing,then the related params will be extracted from backtestPar.It is usefull when the backtestPar has been initialized.
#' @param PB.L a \bold{PB} object of the long portfolio
#' @param bmk a character string,giving the stockID of the benchmark index,eg. "EI000300".
#' @param hedge.rebFreq giving the rebalance freq when computing the hedged rtn.An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param hedge.posi a numeric, giving the position of the hedging assets
#' @param fee.long a numeric, giving the fee of long asset
#' @param fee.short a numeric, giving the fee of short asset
#' @param backtestPar a \bold{backtestPar} object
#' @return a \bold{rtn.LBH} object of class xts,giving the return series of long,bmk and hedge.
#' @author Ruifei.Yin
#' @export
#' @family LSH-frame building functions
#' @examples
#' re <- getrtn.LBH(PB.L,"EI000300")
getrtn.LBH <- function(PB.L,bmk="EI000300",
                       hedge.rebFreq="month",hedge.posi=1,fee.long=0,fee.short=0,
                       backtestPar){
  if(!missing(backtestPar)){
    bmk <- getbacktestPar.longshort(backtestPar,"bmk")
    hedge.rebFreq <- getbacktestPar.longshort(backtestPar,"hedge.rebFreq")
    hedge.posi <- getbacktestPar.longshort(backtestPar,"hedge.posi")
    fee.long <- getbacktestPar.fee(backtestPar,"secu")
    fee.short <- getbacktestPar.fee(backtestPar,"future")
  }
  rtn.long <- PB.L$rtn
  rtn.bmk <- getrtn.bmk(rtn.long,bmk)
  rtn.LBH <- addrtn.hedge(rtn.long, rtn.bmk,hedge.rebFreq,
                          weight=c(hedge.posi,-hedge.posi),
                          fee.long,fee.short)  
  colnames(rtn.LBH) <- c("long","bmk","hedge")
  return(rtn.LBH)
}


getTurnover <- function(PB){
  PB <- PB.L
  hsl.mat <- PB$rebtrade
  buy <- hsl.mat
  
}

