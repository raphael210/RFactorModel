

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- get portfolio & portfolio process ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



#' portfolio building and backtesting
#' 
#' @rdname PortfolioBacktest
#' @name PortfolioBacktest
#' @aliases getPort 
#' @details \code{getPort} get the \bold{Port}('portfolio') objects, subsetting from the \bold{TSF} object.
#' @param TSF a \bold{TSF} object or a \bold{TSFR} object
#' @param topN an integer vector with 2 or 1 elements, giving the rank range of the assets to be selected into the portfolio. If containing only 1 element, the the top rank of 1 will be added automatically.
#' @param topQ a numeric vector with 2 or 1 elements, giving the percentage range of the assets to be selected into the portfolio.  If containing only 1 element, the top percentage of 0 will be added automatically.
#' @param sectorNe_pick NULL, "existing", or a sectorAttr.
#' @param force_in a numeric between 0 and 1. eg. 0.1 means that stock with rank less than topN*10\%  or pct less than topQ*10\% will be incorporated coercively.
#' @param buffer_keep a numeric greater than 0. eg. 0.1 means that stock with rank less than topN*110\% (\code{topN*(1+buffer_keep)}) or pct less than topQ*110\% will be kept coercively.
#' @param buffer_rate a numeric between 0 and 1
#' @param init_port a charactor vector of stockIDs.
#' @param dir a character string "long" or "short". In \code{getPort}, if "short",select from top to bottom, and vice versa.
#' @return \code{getPort} return a \bold{Port}('portfolio') objects,which are of dataframe class containing at least 2 cols("date","stockID")
#' @note Note that \code{topN} and \code{topQ} should at least have one and only have one.
#' @note Note that if use bufferring, the length of topN (or topQ) should be 1.
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
#' # -- with buffer_in_keep
#' ts <- getTS(as.Date(c("2016-03-31","2016-04-29")),indexID = "EI000016")
#' tsf <- getTSF(ts,"gf_lcfs",list("F000008"),factorDir = -1) 
#' pt <- getPort(tsf,20,force_in = 0.5,buffer_keep = 0.5)
#' pt2 <- getPort(tsf,20,force_in = 0.5,buffer_keep = 0.5,sectorNe_pick = defaultSectorAttr())
#' # -- with buffer_rate
#' pt3 <- getPort(tsf,20,buffer_rate =  0.5,sectorNe_pick =NULL)
#' pt4 <- getPort(tsf,topQ = 0.4,buffer_rate =  0.5,sectorNe_pick =NULL)
#' pt5 <- getPort(tsf,20,buffer_rate =  0.5,sectorNe_pick =defaultSectorAttr())
#' pt6 <- getPort(tsf,topQ = 0.4,buffer_rate =  0.5,sectorNe_pick =defaultSectorAttr())
getPort <- function(TSF, topN=NA, topQ=NA, 
                    sectorNe_pick=NULL,
                    force_in=0, buffer_keep=0, buffer_rate=0,init_port=NULL,
                    dir=c("long","short")){
  dir <- match.arg(dir)
  check.TSF(TSF)
  TSF <- factor_na(TSF,method="median")
  
  if(TRUE){# remove stocks due to suspending or over limit
    TSF <- rm_suspend(TSF,nearby = 1L)
    TSF <- rm_priceLimit(TSF,nearby = 1L,lim=c(-Inf,10))
  }
  
  if (any(is.na(topN)) && any(is.na(topQ)) || all(!is.na(topN)) && all(!is.na(topQ))){
    stop("'topN' and 'topQ' should at least have one and only have one!")
  }
  if(length(topN) == 1L) topN_ <- c(1,topN) else topN_ <- topN
  if(length(topQ) == 1L) topQ_ <- c(0,topQ) else topQ_ <- topQ
  if(!is.null(sectorNe_pick)){
    TSF <- getSectorID(TSF,sectorAttr=sectorNe_pick)
    TSF_by <- dplyr::group_by(TSF,date) %>% dplyr::mutate(cnt=n()) %>% dplyr::group_by(sector,add=TRUE) %>% dplyr::mutate(cnt_sct=n())
  } else {
    TSF_by <- dplyr::group_by(TSF,date) %>% dplyr::mutate(cnt=n()) 
  }
  if(dir == "long"){
    TSF_by <- dplyr::mutate(TSF_by,rnk=rank(-factorscore)) %>% dplyr::arrange(rnk)
  } else {
    TSF_by <- dplyr::mutate(TSF_by,rnk=rank(factorscore)) %>% dplyr::arrange(rnk)
  }
  
  if(all(!is.na(topN))){  ## get port by topN
    port <- dplyr::filter(TSF_by, rnk>=min(topN_)/cnt*n() & rnk<=max(topN_)/cnt*n())
  } else { ## get port by topQ
    port <- dplyr::slice(TSF_by,max(1,ceiling(min(topQ_)*n())) : floor(max(topQ_)*n()))
  }
  
  
  # ---- buffering --
  if (force_in>0 | buffer_keep>0 |buffer_rate>0){ 
    
    if(length(topN) > 1 |length(topQ)>1){
      stop("If use buffer, the length of topN and topQ must be 1!")
    } 
    timeidx <- sort(unique(TSF_by$date))
    if (is.null(init_port)) { # the initial port
      init_port <- port[port$date==timeidx[1],"stockID"][[1]]
    } 
    # looping
    port <- data.frame()
    for (idx in timeidx) {
      if (idx == timeidx[[1]]){
        old_port <- init_port  
      } else {
        old_port <- new_port$stockID
      }
      subTSF <- TSF_by[TSF_by$date==idx,]
      if(TRUE){# keep old stocks due to suspending or under limit
        sus <- is_suspend(datelist = idx,stockID = old_port,nearby = 1L,drop = TRUE)
        underLim <- is_priceLimit(datelist = idx,stockID = old_port,nearby = 1L,lim=c(-10,Inf),drop=TRUE)
        old_sus_underLim <- old_port[sus|underLim]
        if(length(old_sus_underLim>0)){
          subTSF <- dplyr::bind_rows(data.frame(date=as.Date(idx,"1970-01-01"), stockID=old_sus_underLim, rnk=0),
                                     dplyr::filter(subTSF,!stockID %in% old_sus_underLim))
          
          if(!is.null(sectorNe_pick)){
            subTSF$sector <- NULL
            subTSF <- getSectorID(subTSF,sectorAttr = sectorNe_pick)
            subTSF <- dplyr::group_by(subTSF,date,sector)
          }
          
          subTSF <- dplyr::mutate(subTSF,cnt=nrow(subTSF))
        }
      }
      subTSF <- dplyr::mutate(subTSF,old=stockID %in% old_port)
      
      cnt_in_date <- nrow(subTSF)
      if (all(!is.na(topN))){ ## get port by topN
        topQ <- topN/cnt_in_date
      } else if (all(!is.na(topQ))){ ## get port by topQ  
        topQ <- topQ
      }
      
      if(force_in>0 | buffer_keep>0 ){# ----- buffer_in_keep
        if(buffer_rate>0){
          stop("buffer_in_keep and buffer_rate can not work together!")
        }
        # flag: 
        # 0-coercively kept old stocks due to suspending or under price limit
        # 1-coercively incorporated new stocks due to force_in
        # 2-coercively kept old stocks due to buffer_keep
        # 3-others
        topQ_in <- topQ*force_in
        topQ_keep <- topQ*(1+buffer_keep)
        subTSF <- dplyr::mutate(subTSF,flag=ifelse(rnk==0,0,
                                                   ifelse(rnk<=topQ_in*n() & old==FALSE, 1,
                                                          ifelse(rnk<=topQ_keep*n() & old==TRUE,2,
                                                                 3))))
      } else if(buffer_rate > 0){ # ----  buffer_rate
        # flag: 
        # 0-coercively kept old stocks due to suspending or under price limit
        # 1-intersect of (rnk<=topQ*n() & old==TRUE)
        # 2-coercively kept old stocks due to buffer_rate
        # 3-others
        if(is.null(sectorNe_pick)){
          pt_0 <- dplyr::filter(subTSF,rnk==0)$stockID
          pt_1 <- dplyr::filter(subTSF,rnk<=topQ*n() & old==TRUE & rnk!=0)$stockID
          pt_2 <- dplyr::filter(subTSF,rnk>topQ*n() & old==TRUE)$stockID
          pt_2 <- head(pt_2,(topQ*cnt_in_date-length(pt_0)-length(pt_1))*buffer_rate)
          subTSF <- dplyr::mutate(subTSF,flag=ifelse(rnk==0,0,
                                                     ifelse(stockID %in% pt_1,1,
                                                            ifelse(stockID %in% pt_2,2,
                                                                   3))))
        } else {
          sectors <- unique(subTSF$sector)
          subTSF_ <- data.frame()
          for (sct in sectors){
            subsubTSF <- dplyr::filter(subTSF,sector==sct)
            cnt_in_sct <- nrow(subsubTSF)
            pt_0 <- dplyr::filter(subsubTSF,rnk==0)$stockID
            pt_1 <- dplyr::filter(subsubTSF,rnk<=topQ*n() & old==TRUE & rnk!=0)$stockID
            pt_2 <- dplyr::filter(subsubTSF,rnk>topQ*n() & old==TRUE)$stockID
            pt_2 <- head(pt_2,(topQ*cnt_in_sct-length(pt_0)-length(pt_1))*buffer_rate)
            subsubTSF <- dplyr::mutate(subsubTSF,flag=ifelse(rnk==0,0,
                                                             ifelse(stockID %in% pt_1,1,
                                                                    ifelse(stockID %in% pt_2,2,
                                                                           3))))
            subTSF_ <- rbind(subTSF_, subsubTSF)
          }
          subTSF <- dplyr::group_by(subTSF_,date,sector)
        }
      }
      subTSF <- dplyr::arrange(subTSF,flag,rnk)
      subTSF <- dplyr::mutate(subTSF,rnk_new=order(order(flag,rnk)))
      new_port <- dplyr::slice(subTSF,1:(topQ*n()))
      port <- rbind(port,new_port)
    }
  } 
  port <- dplyr::arrange(port,date,rnk)
  port <- as.data.frame(port)
  return(port)
}



#' @details \code{addwgt2port} add the weights to the \bold{Port} object.
#' @rdname PortfolioBacktest
#' @param wgtType a character string, giving the weighting type of portfolio,which could be "eq"(equal),"fv"(floatValue),"fvsqrt"(sqrt of floatValue) or "custom".
#' @param sectorNe_wgt NULL, "existing", or sectorAttr.
#' @param wgtbmk a index ID.
#' @param tolerance a numeric, only used when \code{sectorNe_wgt} is not null, Giving the tolerance of absent sectors weights in the portfolio.
#' @return \code{addwgt2port} return a \bold{Port} object which are of dataframe class containing at least 3 cols("date","stockID","wgt").
#' @export
#' @examples 
#' # -- add wgt to port
#' port <- getPort(TSF,20)
#' port <- addwgt2port(port,"fv")
#' port <- addwgt2port(port,"fv",max_wgt=0.08)
addwgt2port <- function (port,
                         wgtType= c("eq","fv","fvsqrt","custom","ffv","ffvsqrt"),
                         sectorNe_wgt=NULL, wgtbmk="EI000300",
                         max_wgt=NULL,                     
                         tolerance=0.2) {  
  wgtType <- match.arg(wgtType)
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
    port <- getTech(port,variables="float_cap")
    if (wgtType=="fv") {
      port <- plyr::ddply(port,"date",transform,wgt=float_cap/sum(float_cap,na.rm=TRUE))
    } else {
      port <- plyr::ddply(port,"date",transform,wgt=sqrt(float_cap)/sum(sqrt(float_cap),na.rm=TRUE))
    }   
    port$float_cap <- NULL
  } else if (wgtType=="custom"){
    port <- port
  } else if (wgtType %in% c("ffv","ffvsqrt")){
    port <- gf.free_float_sharesMV(port)
    if (wgtType=="ffv") {
      port <- plyr::ddply(port,"date",transform,wgt=factorscore/sum(factorscore,na.rm=TRUE))
    } else {
      port <- plyr::ddply(port,"date",transform,wgt=sqrt(factorscore)/sum(sqrt(factorscore),na.rm=TRUE))
    }   
    port$factorscore <- NULL
  }
  
  # ---- neutrualizing wgt by sectors 
  if (!is.null(sectorNe_wgt)) {
    # --- get the bmk sector wgt
    datelist <- unique(port$date)    
    wgt.bmk <- getIndexCompWgt(indexID=wgtbmk,endT=datelist)      
    wgt.bmk <- getSectorID(wgt.bmk, sectorAttr=sectorNe_wgt)
    wgt.bmk.sector <- plyr::ddply(wgt.bmk,c("date","sector"),plyr::summarize,wgt.sector=sum(wgt,na.rm=TRUE))  
    # --- merge and rescale the wgt to neutrualiezde
    port<- getSectorID(port,sectorAttr=sectorNe_wgt)
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
  
  # ---- port_limitwgt
  port <- port_limitwgt(port=port, max_wgt=max_wgt)
  return(port)
}




#' @rdname PortfolioBacktest
#' @param max_wgt a numeric or NA, giving the maximum of weight which could be set on a single stock. If NA(the default value), with no limit, return \code{port} itself without doing anything.
#' @export
#' @examples
#' # -- reduce the risk of concentration
#' port <- port_limitwgt(port,max_wgt=0.08)
port_limitwgt <- function(port,max_wgt=NULL){
  if(is.null(max_wgt)){  
    return(port)
  }
  check.Port(port)
  dates <- unique(port$date)
  for(dt in dates){
    wgt <- port[port$date==dt,"wgt"]
    while(any(wgt > max_wgt)){
      over_flag <- wgt>=max_wgt
      total_over <- sum((wgt-max_wgt)[over_flag])
      total_remain <- sum(wgt[!over_flag])
      wgt[over_flag] <- max_wgt
      wgt[!over_flag] <- wgt[!over_flag]*(1+total_over/total_remain)
    }
    port[port$date==dt,"wgt"] <- wgt
  }
  return(port)
}




#' @details \code{getPort_throughout}, which is a wrapped function of \code{getPort}, \code{addwgt2Port}, \code{port_limitwgt}, get \bold{Port} object from \bold{TSF}, with further treatment.
#' @rdname PortfolioBacktest
#' @export
#' @return \code{getPort_throughout} return a \bold{Port} object, which contain the col of 'wgt'.
#' @examples
#' # -- get Port object from TSF throughout
#' Port_throut <- getPort_throughout(TSF, topN=20, max_wgt=0.8, dir="long")
getPort_throughout <- function (TSF,
                                # getPort
                                topN=NA, topQ=NA, 
                                sectorNe_pick=NULL, 
                                force_in=0, buffer_keep=0, buffer_rate=0,init_port=NULL,
                                # addwgt2port
                                wgtType= "eq",
                                sectorNe_wgt=NULL, wgtbmk="EI000300",
                                max_wgt=NULL, 
                                
                                dir=c("long","short")) {
  dir <- match.arg(dir)
  Port <- getPort(TSF, 
                  topN=topN, topQ=topQ,
                  sectorNe_pick=sectorNe_pick,
                  force_in=force_in, buffer_keep=buffer_keep, buffer_rate=buffer_rate, init_port=init_port,
                  dir=dir)
  
  Port <- addwgt2port(Port, 
                      wgtType=wgtType, 
                      sectorNe_wgt=sectorNe_wgt,                       
                      wgtbmk=wgtbmk, 
                      max_wgt=max_wgt)
  
  return(Port)
}
  

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



#' port.wgt_align
#'
#' @param target_long long position's target total wgt
#' @param target_short short position's target total wgt
#'
#' @return a port
#' @export
#' @examples 
#' library(data.table)
#' port <- getIndexCompWgt("EI000300", endT = as.Date(c("2018-01-31", "2018-03-31")))
#' data.table(port)[,.(sumwgt=sum(wgt)),by=date]
#' #- wgt aligning
#' port1 <- port[20:580,]
#' data.table(port1)[,.(sumwgt=sum(wgt)),by=date]
#' port1 <- port.wgt_align(port1)
#' data.table(port1)[,.(sumwgt=sum(wgt)),by=date]
#' #- deal with the longshort port
#' port2 <- port
#' port2[200:400,"wgt"] <- -0.01
#' port2$tag <- port2$wgt>0
#' data.table(port2)[,.(sumwgt=sum(wgt)),by=.(date,tag)]
#' port2 <- port.wgt_align(port2,target_long = 1, target_short = -2)
#' data.table(port2)[,.(sumwgt=sum(wgt)),by=.(date,tag)]
port.wgt_align <- function(port, target_long=1, target_short=-1){
  newport <- port %>% dplyr::mutate(.wgt_tag=ifelse(wgt>=0,'long','short'), .wgt_target=ifelse(wgt>=0,target_long,target_short)) %>% 
    dplyr::group_by(date, .wgt_tag) %>% 
    dplyr::mutate(wgt=wgt/sum(wgt, na.rm = TRUE)* .wgt_target) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.wgt_tag,-.wgt_target) %>%
    as.data.frame()
  return(newport)
}

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- portfolio backtesting and anlalizing ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' @details \code{port.backtest} backtest the \bold{Port} object. Get a \bold{PB}("PortfolioBacktest") object.
#' @rdname PortfolioBacktest
#' @param holdingEndT a Date object, giving the ending date of the holding portfolio 
#' @param fee.buy giving the buying fee of each assets. See \code{\link[QUtility]{Return.backtesting}} for detail
#' @param fee.sell 
#' @param dir a character string of "long" or "short". In \code{port.backtest}, if "long", the port will be look as asset, the fee will be cut from the asset; if "short", the port will be look as liability, the fee will be added to the liability.
#' @param rtn_get a character string of "loop","whole","simple". "loop" and "whole" get the dailyrtn, where as "simple" get the periodrtn.
#' @return \code{port.backtest} return a \bold{PB}("PortfolioBacktest") object, a xts series of portfolio return, with the attr of 'turnover'(a xts series) and 'fee'(a vector).
#' @seealso \code{\link[QUtility]{Return.backtesting}}
#' @export
#' @examples
#' # -- backtest the Port object
#' PB <- port.backtest(port)
#' turnover <- attr(PB,"turnover")
#' fee <- attr(PB,"fee")
port.backtest <- function(port,
                          holdingEndT=Sys.Date(), fee.buy=0, fee.sell=0,
                          dir=c("long","short"),
                          rtn_get=c("loop","whole","simple"),silence=FALSE){  
  dir <- match.arg(dir)
  rtn_get <- match.arg(rtn_get)
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
  if(rtn_get=="simple"){
    if(!"periodrtn" %in% colnames(port)){
      port <- getTSR(TS=port)
    } 
    if(TRUE){# replace the last date_end with holdingEndT
      port0 <- dplyr::filter(port,date!=max(date)) 
      port1 <- dplyr::filter(port,date==max(date)) 
      port1$date_end <- holdingEndT
      port1$periodrtn <- getPeriodrtn(stockID=port1$stockID, begT=port1$date, endT=holdingEndT, drop=TRUE)
      port <- rbind(port0,port1)
    }
    R.df <- reshape2::dcast(port,date_end~stockID,value.var="periodrtn",fill=0)
    R <- xts(R.df[,-1], R.df[,1])
    colnames(R) <- colnames(R.df)[-1]
  } else if(rtn_get=="whole"){
    stocks <- colnames(weights) 
    qt <- getQuote(stocks,begT=min(port$date),endT=holdingEndT,variables=c("pct_chg"),tableName = "QT_DailyQuote")
    qt <- renameCol(qt,"pct_chg","rtn")
    R.df <- reshape2::dcast(qt,date~stockID,value.var="rtn",fill=0)
    R <- xts(R.df[,-1],R.df[,1])
    colnames(R) <- colnames(R.df)[-1]
  } else if(rtn_get=="loop"){
    datelist <- unique(port$date)
    R.df <- data.frame()
    message("Looping to get the quote data in function 'port.backtest' ....")
    pb <- txtProgressBar(style = 3)
    for (ii in 1:length(datelist)){
      stocks <- port[port$date==datelist[ii],"stockID"]
      begT <- trday.nearby(datelist = datelist[ii], by = 1L)
      endT <- as.Date(ifelse(ii==length(datelist), holdingEndT, datelist[ii+1]), "1970-01-01")
      qt <- getQuote(stocks = stocks, begT = begT, endT = endT, variables = c("pct_chg"),tableName = "QT_DailyQuote")
      qt <- renameCol(qt, "pct_chg", "rtn")
      if(ii==1L){
        R.df <- qt
      } else {
        R.df <- rbind(R.df, qt)
      }
      if(!silence){
        setTxtProgressBar(pb, ii/length(datelist))
      }
    }
    close(pb)
    R.df <- reshape2::dcast(R.df, date~stockID,value.var="rtn",fill=0)
    R <- xts(R.df[,-1],R.df[,1])
    colnames(R) <- colnames(R.df)[-1]
  }
  
  # ---- get PB
  re_tmp <- Return.backtesting(R = R,weights = weights,fee.buy = fee.buy,fee.sell = fee.sell,output=c("rtn","turnover"))
  re <- re_tmp[["rtn"]]
  attr(re,"turnover") <- re_tmp[["turnover"]]
  attr(re,"fee") <- c(fee_buy=fee.buy, fee_sell=fee.sell)
  return(re) 
}


# consider: port.summary(port) vs. PB.summary(PB) ?
# try to add port to the attr of PB.
# port.summary <- function(port){
#   # sector distribution
#   # number of stocks, maxwgt,minwgt
#   # turnover
# }
# consider: port.risk vs. PB.risk ?
# port.risk <- function(port){
#   # risk annalysis
# }

#' @export
port.turnover <- function(port){
  
  check.colnames(port, c("date","stockID","wgt"))
  datelist <- sort(unique(port$date))
  dateframe <- data.frame(date = datelist, 
                          adj_date = dplyr::lead(datelist, n = 1L))
  
  port_1 <- subset(port, date != datelist[1])
  
  port_2 <- merge(port, dateframe, by = "date")
  port_2$date <- NULL
  port_2 <- dplyr::rename(port_2, date = adj_date, old_wgt = wgt)
  port_2 <- na.omit(port_2)
  
  port_union <- merge(port_1, port_2, by = c("date","stockID"), all = TRUE)
  port_union$wgt <- ifelse(is.na(port_union$wgt),0,port_union$wgt)
  port_union$old_wgt <- ifelse(is.na(port_union$old_wgt),0,port_union$old_wgt)
  port_union$diff <- abs(port_union$wgt - port_union$old_wgt)
  
  port_turnover <- dplyr::group_by(port_union, date)
  port_turnover <- dplyr::summarise(port_turnover, turnover = sum(diff, na.rm = TRUE)/2)
  port_turnover <- as.data.frame(port_turnover)
  
  return(port_turnover)
}

#' @export
port.sector <- function(port,
                        bmk = NULL, 
                        sectorAttr = defaultSectorAttr("ind"),
                        include_rtn = FALSE){
  
  # check port 
  check.colnames(port, c("date","stockID","wgt"))
  
  # check R
  if(include_rtn){
    port <- getTSR(port)
  }
  
  # sector distribution
  port_sector <- getSectorID(port, sectorAttr = sectorAttr)
  port_sector <- dplyr::group_by(port_sector, date, sector)
  if(include_rtn){
    port_sector <- dplyr::summarise(port_sector, 
                                    wgt = sum(wgt, na.rm = TRUE),
                                    periodrtn = sum(wgt * periodrtn, na.rm = TRUE))
    port_sector <- as.data.frame(port_sector)
    
    port_sector_summary <- dplyr::group_by(port_sector, sector)
    port_sector_summary <- dplyr::summarise(port_sector_summary, 
                                            mean_wgt = mean(wgt, na.rm = TRUE),
                                            mean_rtn = mean(periodrtn, na.rm = TRUE))
  }else{
    port_sector <- dplyr::summarise(port_sector, 
                                    wgt = sum(wgt, na.rm = TRUE))
    port_sector <- as.data.frame(port_sector)
    
    port_sector_summary <- dplyr::group_by(port_sector, sector)
    port_sector_summary <- dplyr::summarise(port_sector_summary, 
                                            mean_wgt = mean(wgt, na.rm = TRUE))
  }
  
  # relative
  if(!is.null(bmk)){
    datelist <- sort(unique(port$date))
    bmk_sector <- getIndexCompWgt(indexID = bmk, endT = datelist)
    bmk_sector <- getSectorID(bmk_sector, sectorAttr = sectorAttr)
    if(include_rtn){
      bmk_sector <- getTSR(bmk_sector)
      bmk_sector <- dplyr::group_by(bmk_sector, date, sector)
      bmk_sector <- dplyr::summarise(bmk_sector, 
                                     bmk_wgt = sum(wgt, na.rm = TRUE),
                                     bmk_periodrtn = sum(wgt*periodrtn, na.rm = TRUE))
    }else{
      bmk_sector <- dplyr::group_by(bmk_sector, date, sector)
      bmk_sector <- dplyr::summarise(bmk_sector, bmk_wgt = sum(wgt, na.rm = TRUE))
    }
    bmk_sector <- as.data.frame(bmk_sector)
    
    # rela part
    port_sector <- merge(port_sector, bmk_sector, by = c("date","sector"), all = TRUE)
    port_sector$wgt <- ifelse(is.na(port_sector$wgt), 0, port_sector$wgt)
    port_sector$bmk_wgt <- ifelse(is.na(port_sector$bmk_wgt), 0, port_sector$bmk_wgt)
    port_sector$rela_wgt <- port_sector$wgt - port_sector$bmk_wgt
    
    if(include_rtn){
      port_sector$rela_rtn <- port_sector$periodrtn - port_sector$bmk_periodrtn
      # overwrite summary table
      port_sector_summary <- dplyr::group_by(port_sector, sector)
      port_sector_summary <- dplyr::summarise(port_sector_summary, 
                                              mean_wgt = mean(wgt, na.rm = TRUE),
                                              mean_rtn = mean(periodrtn, na.rm = TRUE),
                                              mean_bmk_wgt = mean(bmk_wgt, na.rm = TRUE),
                                              mean_bmk_rtn = mean(bmk_periodrtn, na.rm = TRUE),
                                              mean_rela_wgt = mean(rela_wgt, na.rm = TRUE),
                                              mean_rela_rtn = mean(rela_rtn, na.rm = TRUE))
    }else{
      # overwrite summary table
      port_sector_summary <- dplyr::group_by(port_sector, sector)
      port_sector_summary <- dplyr::summarise(port_sector_summary, 
                                              mean_wgt = mean(wgt, na.rm = TRUE),
                                              mean_bmk_wgt = mean(bmk_wgt, na.rm = TRUE),
                                              mean_rela_wgt = mean(rela_wgt, na.rm = TRUE))
    }
  }
  port_sector_summary <- as.data.frame(port_sector_summary)
  result <- list(port_sector_summary,
                 port_sector)
  return(result)
}

#' @export
port.summary <- function(port,
                         sectorAttr = defaultSectorAttr("ind"),
                         bmk = NULL,
                         include_rtn = TRUE){
  # check port 
  check.colnames(port, c("date","stockID","wgt"))
  
  # sector distribution
  port_sector_result <- port.sector(port, 
                                    bmk = bmk, 
                                    sectorAttr = sectorAttr, 
                                    include_rtn = include_rtn)
  
  # number of stocks, maxwgt,minwgt
  port_stat <- dplyr::group_by(port, date)
  port_stat <- dplyr::summarise(port_stat, num_stocks = n(), max_wgt = max(wgt, na.rm = TRUE), min_wgt = min(wgt, na.rm = TRUE), total_wgt = sum(wgt, na.rm = TRUE))
  port_stat <- as.data.frame(port_stat)
  
  # turnover
  port_turnover <- port.turnover(port)
  
  # sector summary
  
  # stat summary
  port_stat_summary <- port_stat
  port_stat_summary$date <- NULL
  port_stat_summary <- colMeans(port_stat_summary, na.rm = TRUE)
  port_stat_summary <- t(as.data.frame(port_stat_summary))
  
  # turnover summary
  port_turnover_summary <- mean(port_turnover$turnover, na.rm = TRUE)
  port_turnover_summary <- t(port_turnover_summary)
  rownames(port_turnover_summary) <- c("port_turnover_summary")
  colnames(port_turnover_summary) <- c("mean_turnover")
  
  result <- list(sector_summary = port_sector_result[[1]],
                 wgt_stat_summary = port_stat_summary,
                 turnover_summary = port_turnover_summary,
                 sector_dist = port_sector_result[[2]],
                 wgt_stat = port_stat,
                 turnover = port_turnover)
  
  return(result)
}


#' GET VOLATILITY OF PORT
#' 
#' @param port requires colnames: date, stockID and wgt.
#' @return data frame, including two columns: date, port_vol
#' @export
#' @examples 
#' port <- getIndexCompWgt("EI000300", endT = as.Date(c("2018-01-31", "2018-03-31")))
#' port.getVolatility(port)
#' 
port.getVolatility <- function(port){
  
  check.colnames(port, c("date","stockID","wgt"))
  port <- port[,c("date","stockID","wgt")]
  port <- data.table::as.data.table(port)
  
  # load data
  mdata_list <- TS.get_barra(port)
  # mtsf <- gf_sector(mdata_list$mtsf, sectorAttr = defaultSectorAttr("ind"))
  mtsf <- data.table::as.data.table(mdata_list$mTSF)
  fcov_dt <- data.table::as.data.table(mdata_list$fCov)
  sigma_dt <- data.table::as.data.table(mdata_list$sigma)
  
  # rock'n'roll
  datelist <- sort(unique(mtsf$date))
  for(i in 1:length(datelist)){
    
    date_ <- datelist[i]
    port_ <- port[date == date_]
    mtsf_ <- mtsf[date == date_]
    fcov_ <- fcov_dt[date == date_, -'date']
    sigma_ <- sigma_dt[date == date_, -'date']
    
    risk_fnames <- colnames(fcov_)
    B_mat <- as.matrix(mtsf_[,risk_fnames, with = FALSE])
    if(length(sigma_$sigma) > 1){
      S_mat <- diag(sigma_$sigma)
    }else{
      S_mat <- sigma_$sigma
    }
    
    risk_mat <- B_mat %*% as.matrix(fcov_) %*% t(B_mat) + S_mat
    wgt_ <- as.vector(port_$wgt)
    risk_ <- as.numeric(t(wgt_) %*% risk_mat %*% wgt_)
    risk_ <- sqrt(risk_)
    result_ <- data.frame('date' = date_, 'port_vol' = risk_, 
                          stringsAsFactors = FALSE)
    #   
    if(i == 1L){
      result <- result_
    }else{
      result <- rbind(result, result_)
    }
  }
  # output 
  return(result)
  
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- get rtn series ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
#' getrtn.bmk
#' 
#' get the benchmark return series corresponding to the specific rtn series.
#' @param rtn an xts,timeSeries or zoo object of portfolio returns.
#' @param bmk a character string, giving the stockID of the benchmark index,eg. "EI000300".
#' @param date_start_pad a Date value, indicating the start point of the first rtn period. If missing, padded by an approximate value.
#' @return a xts object, giving the rtn series of the benchmark
#' @author Ruifei.Yin
#' @export
#' @family LSH-frame building functions
#' @examples
#' # -- daily rtn
#' dates <- getRebDates(as.Date("2011-03-01"),as.Date("2011-04-20"),rebFreq = "day")
#' TS <- getTS(dates,stocks = "EQ000002")
#' TSR <- getTSR(TS)
#' rtn <- xts::xts(TSR$periodrtn,TSR$date_end)
#' re <- getrtn.bmk(rtn,"EI000300")
#' # -- IF00 as bmk
#' re_IF <- getrtn.bmk(rtn,"IF00")
#' # -- none daily rtn
#' dates <- getRebDates(as.Date("2010-03-01"),as.Date("2011-04-20"),rebFreq = "month")
#' TS <- getTS(dates,stocks = "EQ000002")
#' TSR <- getTSR(TS)
#' rtn <- xts::xts(TSR$periodrtn,TSR$date_end)
#' re <- getrtn.bmk(rtn,"EI000300")
#' re2 <- getrtn.bmk(rtn,"EI000300",date_start_pad=as.Date("2010-03-01"))
getrtn.bmk <- function(rtn, bmk, date_start_pad, drop=FALSE){
  
  if(substr(bmk,1,2)=="EI"){
    date_end <- zoo::index(rtn)
    if(missing(date_start_pad)){ # calculate the padding value automatically
      p_Nday <- periodicity_Ndays(rtn)
      if(p_Nday>2){
        warning("The rtn series is not daily. The start point of the first rtn period is padded by an approximate value!")
        date_start_pad <- trday.offset(min(date_end),by = lubridate::days(-round(periodicity_Ndays(date_end))), dir = -1L)
      } else { # daily rtn
        date_start_pad <- trday.nearby(min(date_end),by = -1)
      }
    }
    date_start <- c(date_start_pad, date_end[-length(date_end)])
    re <- getPeriodrtn_EI(stockID=bmk, begT=date_start, endT=date_end)
    re <- xts::xts(re$periodrtn, date_end)
    colnames(re) <- "bmk"
  } else if(substr(bmk,1,2) %in% c("IF","IC","IH")){ # - to be modified...
    p_Nday <- periodicity_Ndays(rtn)
    if(p_Nday>2){
      stop("The rtn series is not daily. can not use Index Future as bmk in the current!")
    }
    # bmk <- "IF00"
    begT <- rdate2ts(as.Date(start(rtn)))
    endT <- rdate2ts(as.Date(end(rtn)))
    str <- paste('
    SetSysParam(pn_stock(),',QT(bmk,2),');
    SetSysParam(pn_cycle(),cy_day());
                 EndT:=',endT,'+0.999;
                 N:=TradeDays(',begT,',EndT);
                 SetSysparam(pn_date(),EndT);
                 return Nday(N,"date",datetimetostr(sp_time()),
                 "Settlement",Settlement(),
                 "Prev_Settlement",Prev_Settlement());
                 ')
    tsRequire()
    re <- tsRemoteExecute(str)
    re <- plyr::ldply(re,as.data.frame)
    re <- xts::xts(re$Settlement/re$Prev_Settlement-1 ,as.Date(re$date))
    colnames(re) <- bmk
  } 
  if(!drop){
    re <- merge(rtn,re)
  }
  return(re)
}








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
#' @return a \bold{rtn.LSH} object of class xts,giving the return series of long,short and hedge, with attr of 'rebtrade' and 'fee'.
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
  rtn.hedge <- Return.backtesting(R = rtn, weights = weight, fee.buy = fee.buy, fee.sell = fee.sell, warning.wgtsum=FALSE,output = c("rtn","rebtrade"))
  rtn <- merge(rtn, rtn.hedge[["rtn"]], all=FALSE)
  colnames(rtn) <- c(nm.long,nm.short,"hedge")
  attr(rtn,"rebtrade") <- rtn.hedge[["rebtrade"]]
  attr(rtn,"fee") <- c(fee_long=fee.long, fee_short=fee.short)
  return(rtn)
}


#' getrtn.LSH
#' 
#' get the \bold{rtn.LSH} object from the \bold{PB} object.
#' 
#' @param PB.L a \bold{PB} object of the long portfolio
#' @param PB.S a \bold{PB} object of the short portfolio
#' @param hedge.rebFreq giving the rebalance freq when computing the hedged rtn.An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param hedge.posi a numeric, giving the position of the hedging assets
#' @param fee.long a numeric, giving the fee of long asset
#' @param fee.short a numeric, giving the fee of short asset
#' @return a \bold{rtn.LSH} object of class xts,giving the return series of long,short and hedge, with the attr of 'turnover'(a xts series) and 'fee'(a vector).
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
                        hedge.rebFreq="month", hedge.posi=1, fee.long=0, fee.short=0) {  
  rtn.long <- PB.L
  rtn.short <- PB.S 
  rtn.LSH <- addrtn.hedge(rtn.long, rtn.short, hedge.rebFreq,
                          weight=c(hedge.posi,-hedge.posi),
                          fee.long, fee.short)
  attr(rtn.LSH,"turnover_L") <- attr(PB.L,"turnover")
  attr(rtn.LSH,"turnover_S") <- attr(PB.S,"turnover")
  attr(rtn.LSH,"fee_L") <- attr(PB.L,"fee")
  attr(rtn.LSH,"fee_S") <- attr(PB.S,"fee")
  return(rtn.LSH)
}

#' getrtn.LBH
#' 
#' get the \bold{rtn.LBH} object from the \bold{TSF} object.
#' @param PB.L a \bold{PB} object of the long portfolio
#' @param bmk a character string,giving the stockID of the benchmark index,eg. "EI000300".
#' @param hedge.rebFreq giving the rebalance freq when computing the hedged rtn.An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param hedge.posi a numeric, giving the position of the hedging assets
#' @param fee.long a numeric, giving the fee of long asset
#' @param fee.short a numeric, giving the fee of short asset
#' @return a \bold{rtn.LBH} object of class xts,giving the return series of long,bmk and hedge, with the attr of 'turnover'(a xts series) and 'fee'(a vector).
#' @author Ruifei.Yin
#' @export
#' @family LSH-frame building functions
#' @examples
#' re <- getrtn.LBH(PB.L,"EI000300")
getrtn.LBH <- function(PB.L,bmk="EI000300",
                       hedge.rebFreq="month",hedge.posi=1,fee.long=0,fee.short=0,
                       date_start_pad){
  rtn.long <- PB.L
  rtn.bmk <- getrtn.bmk(rtn = rtn.long, bmk = bmk, date_start_pad = date_start_pad, drop = TRUE)
  rtn.LBH <- addrtn.hedge(rtn.long, rtn.bmk,hedge.rebFreq,
                          weight=c(hedge.posi,-hedge.posi),
                          fee.long,fee.short)  
  colnames(rtn.LBH) <- c("long","bmk","hedge")
  attr(rtn.LBH,"turnover_L") <- attr(PB.L,"turnover")
  attr(rtn.LBH,"fee_L") <- attr(PB.L,"fee")
  return(rtn.LBH)
}



