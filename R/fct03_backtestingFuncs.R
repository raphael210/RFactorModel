# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  backtesting with 'scatter' method --------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' chart.TSFRScatter
#' 
#' plot the \bold{TSFR} object with scatter,which demonstrating the relationship between the factorscore and periodrtn.
#' @param TSFR a \bold{TSFR} object 
#' @param Nbin the number of the groups the timespan is cut to when plotting the scatter by time series.It could also be a character of interval specification,See \code{\link{cut.Date}} for detail. The default value is "day",which means no cutting, the scatters of every date are ploted.
#' @param plotPar Optional.a \bold{plotPar} object,if not missing,then extract pars from plotPar
#' @return a list, with 2 plot objects: 
#'    \itemize{
#'    \item scatter.overall: the all span scatter
#'    \item scatter.ts: the time series scatter,which is display in diffrent facets
#'    }
#' @author Ruifei.Yin
#' @export
#' @examples 
#' modelPar <- modelPar.default()
#' TSFR <- Model.TSFR(modelPar)
#' chart.TSFRScatter(TSFR,25)
chart.TSFRScatter <- function(TSFR,Nbin="day",plotPar){
  if(!missing(plotPar)){
    Nbin <- getplotPar.TSFRScatter(plotPar,"Nbin")
  }
  check.TSFR(TSFR)
  # ---- plot the all span scatter
  data1 <- plyr::ddply(TSFR,c("stockID"),plyr::summarise,periodrtn=mean(periodrtn,na.rm=TRUE),factorscore=mean(factorscore,na.rm=TRUE))
  scatter.overall <- qplot(x=factorscore,y=periodrtn,data=data1,geom=c("point","smooth"),main="Scatter of all span")  
  # ---- plot the scatter by time seris
  TSFR$date <- (cut.Date2(TSFR$date,Nbin))
  data2 <- plyr::ddply(TSFR,c("date","stockID"),plyr::summarise,periodrtn=mean(periodrtn,na.rm=TRUE),factorscore=mean(factorscore,na.rm=TRUE))  
  scatter.ts <- qplot(x=factorscore,y=periodrtn,data=data2,facets=~date,geom=c("point","smooth"),
                      main="Scatter by date")
  return(list(scatter.overall=scatter.overall,scatter.ts=scatter.ts))
}




# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  backtesting with 'IC' method ------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' backtest.IC
#'
#' backtesting the factor with some tables and charts using the 'IC' method. 
#' 
#' When caculating the correlation,two methods "pearson" and "spearman" is used.
#' 
#' If param backtestPar and plotPar is not missing,then the related params will be extracted from them.It is usefull when the parametres has been initialized.
#' @rdname backtest.IC
#' @name backtest.IC
#' @aliases seri.IC
#' @param TSFR a \bold{TSFR} object
#' @param stat a character string,indicating the methods to compute IC,could be "pearson" or "spearman". 
#' @param backtestPar Optional.a \bold{backtestPar} object,if not missing,then extract pars from backtestPar. 
#' @return seri.IC return a xts object, which containing the IC seri 
#' @author Ruifei.Yin
#' @export
#' @examples 
#' modelPar <- modelPar.default()
#' TSFR <- Model.TSFR(modelPar)
#' re <- seri.IC(TSFR)
seri.IC <- function(TSFR,stat=c("pearson","spearman"),backtestPar){
  stat <- match.arg(stat)
  if(!missing(backtestPar)){
    stat <- getbacktestPar.IC(backtestPar,"stat")
  }
  check.TSFR(TSFR)
  TSFR <- na.omit(TSFR[,c("date","stockID","factorscore","periodrtn")])
  if(stat=="pearson"){
    IC.seri <- plyr::ddply(TSFR,"date",plyr::summarise,
                     IC=cor(periodrtn,factorscore,method="pearson",use="pairwise.complete.obs"))
  } else if(stat=="spearman"){
    IC.seri <- plyr::ddply(TSFR,"date",plyr::summarise,
                     IC=cor(periodrtn,factorscore,method="spearman",use="pairwise.complete.obs"))
  }
  re <- as.xts(IC.seri[,-1,drop=FALSE],IC.seri[,1])
  colnames(re) <- "IC"
  return(re)
}
#' @rdname backtest.IC
#' @return seri.IC.decay return a xts object of 12 cols, which containing the decayed ICs seri
#' @export
#' @examples 
#' re <- seri.IC.decay(TSFR)
seri.IC.decay <- function(TSFR,stat=c("pearson","spearman"),backtestPar){
  stat <- match.arg(stat)
  if(!missing(backtestPar)){
    stat <- getbacktestPar.IC(backtestPar,"stat")
  }
  check.TSFR_decay(TSFR)
  if(stat=="pearson"){
    IC.seri <- plyr::ddply(TSFR,"date",function(dat){
      t(cor(dat[,paste("rtn",1:12,sep="")],dat[,"factorscore"],method="pearson",use="pairwise.complete.obs"))
    })
  } else if(stat=="spearman"){
    IC.seri <- plyr::ddply(TSFR,"date",function(dat){
      t(cor(dat[,paste("rtn",1:12,sep="")],dat[,"factorscore"],method="spearman",use="pairwise.complete.obs"))
    })
  }
  re <- as.xts(IC.seri[,-1,drop=FALSE],IC.seri[,1])
  colnames(re) <- paste("IC",1:12,sep="")
  return(re)
}
#' @rdname backtest.IC
#' @return table.IC return a matirx of the statistical of the IC, containing rows: "IC.mean","IC.std","IC.IR","IC.t","IC.p","IC.hitRatio"
#' @export
#' @examples 
#' IC.table <- table.IC(TSFR)
table.IC <- function(TSFR,stat=c("pearson","spearman"),backtestPar){
  stat <- match.arg(stat)
  seri <- seri.IC(TSFR,stat,backtestPar)
  tab.annu <- PerformanceAnalytics::table.AnnualizedReturns(seri)[[1]]
  seri <- as.vector(seri)
  IC.mean <- mean(seri,na.rm=TRUE)
  IC.std <- sd(seri,na.rm=TRUE)
  IC.IR <- IC.mean/IC.std
  IC.Ttest.t <- t.test(seri)$statistic
  IC.Ttest.p <- t.test(seri)$p.value
  IC.hit <- hitRatio(seri)  
  re <- c(IC.mean, IC.std, IC.IR, IC.Ttest.t, IC.Ttest.p, IC.hit, tab.annu)
  re <- matrix(re,length(re),1)
  colnames(re) <- "IC"
  rownames(re) <- c("IC.mean","IC.std","IC.IR","IC.t","IC.p","IC.hitRatio","IC.annu","IC.std.annu","IC.IR.annu")
  return(re)
}  
#' @rdname backtest.IC
#' @return table.IC.decay return a matirx of the statistical of the decayed ICs, containing 5 rows: "IC.mean","IC.std","IC.IR","IC.t","IC.p","IC.hitRatio" and 12 cols: "IC1"-"IC12".
#' @export
#' @examples 
#' IC.table.decay <- table.IC.decay(TSFR)
table.IC.decay <- function(TSFR,stat=c("pearson","spearman"),backtestPar){
  stat <- match.arg(stat)
  seri <- seri.IC.decay(TSFR,stat,backtestPar)
  IC.mean <- colMeans(seri,na.rm=TRUE)
  IC.std <- colSds(seri,na.rm=TRUE)
  IC.IR <- IC.mean/IC.std
  IC.Ttest.t <- colStats(seri, function(x) t.test(x)$statistic)
  IC.Ttest.p <- colStats(seri, function(x) t.test(x)$p.value)
  IC.hit <- as.vector(hitRatio(seri))
  re <- t(cbind(IC.mean,IC.std,IC.IR,IC.Ttest.t,IC.Ttest.p,IC.hit))
  rownames(re) <- c("IC.mean","IC.std","IC.IR","IC.t","IC.p","IC.hitRatio")
  tab.annu <- PerformanceAnalytics::table.AnnualizedReturns(seri)
  rownames(tab.annu) <- c("IC.annu","IC.std.annu","IC.IR.annu")
  return(rbind(re,tab.annu))
}
#' @rdname backtest.IC
#' @param Nbin the number of the groups the timespan is cut to, when plotting the IC series.It could also be character of interval specification,See \code{\link{cut.Date}} for detail. The default value is "day",which means no cutting, the value of every date are ploted.
#' @param plotPar Optional.a \bold{plotPar} object,if not missing,then extract pars from plotPar
#' @return chart.IC return a ggplot object of IC time series(with its 12 months MA)
#' @export
#' @examples 
#' IC.chart <- chart.IC(TSFR,"3 month")
chart.IC <- function(TSFR,Nbin="day",stat=c("pearson","spearman"),plotPar){
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    Nbin <- getplotPar.IC(plotPar,"Nbin")
    stat <- getplotPar.IC(plotPar,"stat")
  }
  # ---- IC series
  seri <- seri.IC(TSFR=TSFR,stat=stat)    
  by <- cut.Date2(zoo::index(seri),Nbin)
  seri.aggr <- aggregate(seri,as.Date(by),mean,na.rm=TRUE)
  colnames(seri.aggr) <- "IC"
  seri.df <- data.frame(time=time(seri.aggr),zoo::coredata(seri.aggr))
  seri.melt <- reshape2::melt(seri.df,id.vars="time")  
  re <- ggplot() +
    geom_bar(data=seri.melt[,-2], aes(x=time, y=value),position="dodge",stat="identity")
  # ---- IC 12 months MA
  freq = xts::periodicity(seri)
  switch(freq$scale, 
         daily = {
           wid = 250
         }, weekly = {
           wid = 52
         }, monthly = {
           wid = 12
         }, quarterly = {
           wid = 4
         }, yearly = {
           wid = 1
         }
  )  
  if(wid >= NROW(seri)){
    warning("IC seri is shorter than 12 months, could not plot the 12 months MA!")
    re <- re + ggtitle("IC series")
  } else {
    ICma <- zoo::rollapply(as.zoo(seri),width=wid,FUN=mean,na.rm=TRUE)
    by <- cut.Date2(zoo::index(ICma),Nbin)
    ICma.aggr <- aggregate(ICma,as.Date(by),tail,1)
    colnames(ICma.aggr) <- "IC.MA"
    ICma.df <- data.frame(time=time(ICma.aggr),zoo::coredata(ICma.aggr))
    ICma.melt <- reshape2::melt(ICma.df,id.vars="time")
    re <- re + 
      geom_line(data=ICma.melt[,-2],aes(x=time,y=value),size=1) +
      ggtitle("IC series (with its 12 months MA)")
  }
  return(re)
}
#' @rdname backtest.IC
#' @return chart.IC.decay return a ggplot object of decayed ICs bar chart.
#' @export
#' @examples 
#' re <- chart.IC.decay(TSFR)
chart.IC.decay <- function(TSFR,stat=c("pearson","spearman"),plotPar){
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    stat <- getplotPar.IC(plotPar,"stat")
  }
  seri <- seri.IC.decay(TSFR,stat=stat)
  IC.mean <- colMeans(seri,na.rm=TRUE)
  dat <- data.frame(months=as.factor(1:12),IC.mean=IC.mean)
  re <- ggplot(dat,aes(x=months,y=IC.mean))+
    geom_bar(position="dodge",stat="identity")+
    ggtitle("IC decay")
  return(re)
}





# ---------------------  ~~ Multi comparison - IC --------------


#' @param TSFRs a list of object \bold{TSFR}. See \code{\link{Model.TSFR}}.
#' @return  MC.chart.IC.corr return a correlation chart of ICs of each \code{TSFR}.
#' @rdname backtest.IC
#' @export
#' @examples 
#' mp = modelPar.default()
#' factorIDs <- c("F000001","F000002","F000005")
#' FactorLists <- buildFactorLists_lcfs(factorIDs)
#' mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
#' TSR <- Model.TSR(mp)
#' TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)
#' MC.chart.IC.corr(TSFRs)
MC.chart.IC.corr <- function(TSFRs,stat=c("pearson","spearman"),plotPar){
  check.name_exist(TSFRs)
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    stat <- getplotPar.IC(plotPar,"stat")
  }
  IC.seris <- plyr::laply(TSFRs, seri.IC, stat=stat)
  rownames(IC.seris) <- names(TSFRs)
  IC.corrmat <- cor(t(IC.seris),method="pearson",use="pairwise.complete.obs")
  corrplot::corrplot.mixed(IC.corrmat,order="hclust")
  IC.corrplot <- recordPlot()
  return(IC.corrplot)
}


#' @rdname backtest.IC
#' @export
#' @examples
#' MC.table.IC(TSFRs)
MC.table.IC <- function(TSFRs,stat=c("pearson","spearman"),backtestPar){
  check.name_exist(TSFRs)
  stat <- match.arg(stat)
  if(!missing(backtestPar)){
    stat <- getbacktestPar.IC(backtestPar,"stat")
  }  
  IC.table <- plyr::laply(TSFRs,table.IC, stat=stat)
  rownames(IC.table) <- names(TSFRs)
  return(IC.table)
}


#' @param ncol a integer, specificate the number of cols of the multi-charts.
#' @rdname backtest.IC
#' @export
#' @examples 
#' MC.chart.IC(TSFRs)
MC.chart.IC <- function(TSFRs,Nbin="day",stat=c("pearson","spearman"),ncol=3, plotPar){
  check.name_exist(TSFRs)
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    Nbin <- getplotPar.IC(plotPar,"Nbin")
    stat <- getplotPar.IC(plotPar,"stat")
    ncol <- getplotPar.MC(plotPar,"ncol.IC")
  }
  NMs <- names(TSFRs)
  IC.charts <- mapply(function(x,nm){
    chart.IC(x,Nbin=Nbin,stat=stat)+  
      ggtitle(nm) +
      theme(axis.title.x= element_blank(),axis.title.y= element_blank())
  },TSFRs,NMs,SIMPLIFY = FALSE )
  IC.multicharts <- multiplot_facet(plotlist=IC.charts,ncol=ncol)
  return(IC.multicharts)
}


#' @rdname backtest.IC
#' @export
#' @examples 
#' MC.chart.IC.decay(TSFRs)
MC.chart.IC.decay <- function(TSFRs,stat=c("pearson","spearman"),ncol=3, plotPar){
  check.name_exist(TSFRs)
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    stat <- getplotPar.IC(plotPar,"stat")
    ncol <- getplotPar.MC(plotPar,"ncol.IC.decay")
  }
  NMs <- names(TSFRs)
  IC.charts.decay <- llply(TSFRs, chart.IC.decay, stat=stat)
  for(i in 1:length(IC.charts.decay)){
    IC.charts.decay[[i]] <- IC.charts.decay[[i]] +
      ggtitle(NMs[i]) +
      theme(axis.title.x= element_blank(),axis.title.y= element_blank())
  }
  IC.multicharts.decay <- multiplot_facet(plotlist=IC.charts.decay,ncol=ncol)
  return(IC.multicharts.decay)
}




# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  backtesting with 'Ngroup' method --------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' backtest.Ngroup
#'
#' backtesting the factor with some tables and charts using the 'Ngroup' method. 
#'   
#' If param backtestPar and plotPar is not missing,then the related params will be extracted from them.It is usefull when the parametres has been initialized.  
#' @rdname backtest.Ngroup
#' @name backtest.Ngroup
#' @aliases seri.Ngroup.rtn
#' @param TSFR a \bold{TSFR} object
#' @param N the number of the groups the universe is cut to
#' @param stat a character string,indicating the statistic of the return center of each group,could be "mean" or "median".
#' @param sectorNe
#' @param sectorAttr
#' @param backtestPar Optional.a \bold{backtestPar} object,if not missing,then extract pars from backtestPar.
#' @return  seri.Ngroup.rtn return a xts object, which giving the rtn seri of each group
#' @author Ruifei.Yin
#' @export
#' @examples 
#' modelPar <- modelPar.default()
#' TSFR <- Model.TSFR(modelPar)
#' re <- seri.Ngroup.rtn(TSFR,5)
seri.Ngroup.rtn <- function(TSFR,N=5,stat=c("mean","median"),
                            sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                            backtestPar){
  stat <- match.arg(stat)
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    stat <- getbacktestPar.Ngroup(backtestPar,"stat")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
    sectorAttr <- getbacktestPar.Ngroup(backtestPar,"sectorAttr")
  }
  check.TSFR(TSFR)
  TSFR <- na.omit(TSFR[,c("date","stockID","factorscore","periodrtn")])
  # ---- add the rank and groups of the factorscores 
  if(!sectorNe){
    TSFR <- data.table::data.table(TSFR,key=c("date"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by="date"]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by="date"]
  } else {
    TSFR <- getSectorID(TSFR,sectorAttr=sectorAttr)
    TSFR <- data.table::data.table(TSFR,key=c("date","sector"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by=c("date","sector")]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by=c("date","sector")]
  }
  
  # ---- rtn seri of each group
  data.table::setkeyv(TSFR,c("date","group"))
  if(stat=="mean"){
    rtn.df <- TSFR[,list(mean.rtn=mean(periodrtn)), by=c("date","group")]
  } else if(stat=="median"){
    rtn.df <- TSFR[,list(mean.rtn=median(periodrtn)), by=c("date","group")]
  }  
  rtn.df <- as.data.frame(rtn.df)
  rtn.mat <- reshape2::acast(rtn.df,date~group,value.var="mean.rtn")
  rtn.xts <- as.xts(rtn.mat,as.Date(rownames(rtn.mat),tz=""))
  colnames(rtn.xts) <- paste("Q",1:N,sep="")   
  result <- rtn.xts
  return(result)  
}
#' @rdname backtest.Ngroup
#' @param turnoverType 
#' @return seri.Ngroup.turnover return a xts, which giving the (one side) num or wgt turnover seri of each group
#' @export
#' @examples
#' re <- seri.Ngroup.turnover(TSFR,5)
seri.Ngroup.turnover <- function(TSFR,N=5,turnoverType= c("num","wgt"),
                                 sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                 backtestPar){
  turnoverType <- match.arg(turnoverType)
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    turnoverType <- getbacktestPar.Ngroup(backtestPar,"turnoverType")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
    sectorAttr <- getbacktestPar.Ngroup(backtestPar,"sectorAttr")
  }
  check.TSFR(TSFR)
  TSFR <- na.omit(TSFR[,c("date","stockID","factorscore","periodrtn")])
  # ---- add the rank and groups of the factorscores 
  if(!sectorNe){
    TSFR <- data.table::data.table(TSFR,key=c("date"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by="date"]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by="date"]    
  } else {
    TSFR <- getSectorID(TSFR,sectorAttr=sectorAttr)
    TSFR <- data.table::data.table(TSFR,key=c("date","sector"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by=c("date","sector")]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by=c("date","sector")]    
  }
  # ---- turnover seri of each group
  if(turnoverType=="num"){ # -- turnover.num
    for(i in 1:N){
      groupI <- subset(TSFR,group==i)    
      periodrtn <- reshape2::acast(groupI,date~stockID,value.var="periodrtn",fill=0)
      periodrtn <- xts(periodrtn,as.Date(rownames(periodrtn),tz=""))
      wgt.ini <- reshape2::acast(groupI,date~stockID,value.var="group",fill=0)
      wgt.ini <- wgt.ini/rowSums(wgt.ini)
      wgt.ini <- xts(wgt.ini,as.Date(rownames(wgt.ini),tz=""))         
      turnover.num <- xts::lag.xts(wgt.ini,na.pad=TRUE)-wgt.ini
      turnover.num <- turnover.num[-1,]
      turnover.num <- xts(rowSums(abs(turnover.num))/2,zoo::index(turnover.num))
      colnames(turnover.num) <- paste("Q",i,sep="")
      if(i==1L){
        seri <- turnover.num
      } else {
        seri <- merge(seri,turnover.num)
      }
    }
  } else if(turnoverType=="wgt"){ # -- turnover.wgt
    for(i in 1:N){
      groupI <- subset(TSFR,group==i)    
      periodrtn <- reshape2::acast(groupI,date~stockID,value.var="periodrtn",fill=0)
      periodrtn <- xts(periodrtn,as.Date(rownames(periodrtn),tz=""))
      wgt.ini <- reshape2::acast(groupI,date~stockID,value.var="group",fill=0)
      wgt.ini <- wgt.ini/rowSums(wgt.ini)
      wgt.ini <- xts(wgt.ini,as.Date(rownames(wgt.ini),tz=""))
      wgt.end <- wgt.ini*(1+periodrtn)          
      turnover.wgt <- xts::lag.xts(wgt.end,na.pad=TRUE)-wgt.ini
      turnover.wgt <- turnover.wgt[-1,]
      turnover.wgt <- xts(rowSums(abs(turnover.wgt))/2,zoo::index(turnover.wgt))
      colnames(turnover.wgt) <- paste("Q",i,sep="")
      if(i==1L){
        seri <- turnover.wgt
      } else {
        seri <- merge(seri,turnover.wgt)
      }
    }
  }
  re <- seri
  return(re)
}
#' @rdname backtest.Ngroup
#' @param fee a numeric, giving the (one side) fee
#' @return table.Ngroup.overall return a matrix which giving the statistics of the rtn of each group, as well as the rtn of top-bottom spread.
#' @export
#' @examples
#' re <- table.Ngroup.overall(TSFR,5,fee=0.002)
table.Ngroup.overall <- function(TSFR,N=5,stat=c("mean","median"),
                                 turnoverType=c("num","wgt"),fee=0,
                                 sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                 backtestPar){
  stat <- match.arg(stat)
  turnoverType <- match.arg(turnoverType)
  if(!missing(backtestPar)){
    fee <- getbacktestPar.fee(backtestPar,"secu")
  }
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr,backtestPar=backtestPar)
  turnoverseri <- seri.Ngroup.turnover(TSFR,N=N,turnoverType=turnoverType,sectorNe=sectorNe,sectorAttr=sectorAttr,backtestPar=backtestPar)
  # --- Ngroups
  rtnsummary <- rtn.summary(rtnseri)
  turnover.annu <- Turnover.annualized(turnoverseri)
  rtn.feefree <- rtnsummary[1,]-turnover.annu*fee*2
  rownames(rtn.feefree) <- "Annualized Return (fee cut)"
  re <- rbind(rtnsummary,turnover.annu,rtn.feefree)  
  # --- spread
  spreadseri <- rtnseri[,1]-rtnseri[,ncol(rtnseri)]
  spreadNM <- "Q1-Qn"
  colnames(spreadseri) <- spreadNM
  rtnsummary.spread <- rtn.summary(spreadseri)  
  turnover.annu.spread <- t(sum(turnover.annu[,c(1,ncol(turnover.annu))])/2)
  rownames(turnover.annu.spread) <- "Annualized Turnover"
  colnames(turnover.annu.spread) <- spreadNM
  rtn.feefree <- rtnsummary.spread[1,]-turnover.annu.spread*fee*2*2   # two side trade and two groups 
  rownames(rtn.feefree) <- "Annualized Return (fee cut)"  
  colnames(rtn.feefree) <- spreadNM
  re.spread <- rbind(rtnsummary.spread,turnover.annu.spread,rtn.feefree)
  # --- cbind
  re <- cbind(re.spread,re)  
  return(re)
}
#' @rdname backtest.Ngroup
#' @return table.Ngroup.spread return a matrix which giving the statistics of the rtn of top-bottom spread in each year.
#' @export
#' @examples
#' re <- table.Ngroup.spread(TSFR,5,fee=0.002)
table.Ngroup.spread <- function(TSFR,N=5,stat=c("mean","median"),
                                turnoverType=c("num","wgt"),fee=0,
                                sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                backtestPar){
  stat <- match.arg(stat)
  turnoverType <- match.arg(turnoverType)
  if(!missing(backtestPar)){
    fee <- getbacktestPar.fee(backtestPar,"secu")
  }
  
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr,backtestPar=backtestPar)
  turnoverseri <- seri.Ngroup.turnover(TSFR,N=N,turnoverType=turnoverType,sectorNe=sectorNe,sectorAttr=sectorAttr,backtestPar=backtestPar)
  spreadseri <- rtnseri[,1]-rtnseri[,ncol(rtnseri)]
#   colnames(spreadseri) <- "spread"  
  yearlist <- as.character(unique(lubridate::year(TSFR$date)))
  for(ii in 1:length(yearlist)) {
    yy <- yearlist[ii]
    if(NROW(spreadseri[yy])<=1 || NROW(turnoverseri[yy])<=1){
      warning(paste("Only 1 record in year",yy,". Return NULL! "))
      tsub <- NULL
    } else {
      rtnsummary <- rtn.summary(spreadseri[yy])  
      turnover.annu <- Turnover.annualized(turnoverseri[yy])
      turnover.annu <- sum(turnover.annu[,c(1,ncol(turnover.annu))])/2
      rtn.feefree <- rtnsummary[1,]-turnover.annu*fee*2*2   # two side trade and two groups 
      tsub <- rbind(rtnsummary,turnover.annu,rtn.feefree)
      rownames(tsub)[c(nrow(tsub)-1,nrow(tsub))] <- c("Annualized Turnover","Annualized Return (fee cut)")
      colnames(tsub) <- yy
    }
    if (ii==1L) {
      re <- tsub
    } else {
      re <- cbind(re,tsub)
    }
  }  
  return(re)
}
#' @rdname backtest.Ngroup
#' @param plotPar Optional.a \bold{plotPar} object,if not missing,then extract pars from plotPar
#' @return chart.Ngroup.overall return a ggplot object of "Annualized return of each group"
#' @export
#' @examples 
#' chart.Ngroup.overall(TSFR,5)
chart.Ngroup.overall <- function(TSFR,N=5,stat=c("mean","median"),
                                 sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                 plotPar){
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
    stat <- getplotPar.Ngroup(plotPar,"stat")
  }  
  tmptable <- table.Ngroup.overall(TSFR=TSFR,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr)
  rtn.annu <- tmptable[1,-1]
  rtn.annu <- data.frame(group=as.integer(substring(names(rtn.annu),2)),rtn.annu=rtn.annu)
  re <- ggplot(rtn.annu,aes(x=group,y=rtn.annu))+
    geom_bar(position="dodge",stat="identity")+
    ggtitle("Annualized return of each group")+
    geom_text(aes(label=paste(round(rtn.annu*100,1),"%",sep="")),vjust=-0.5)+
    scale_y_continuous(labels=scales::percent)
  return(re)
}
#' @rdname backtest.Ngroup
#' @param Nbin the number of the groups the timespan is cut to, when plotting the "date.grp".It could also be character of interval specification,See \code{\link{cut.Date}} for detail. the default value is "day",which means no cutting, The value of every date are ploted.
#' @return chart.Ngroup.seri_point return a ggplot object of "return time series of the groups" with geom_point
#' @export
#' @examples 
#' chart.Ngroup.seri_point(TSFR,5,"3 month")
chart.Ngroup.seri_point <- function(TSFR,N=5,Nbin="day",stat=c("mean","median"),
                                    sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                    plotPar){
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")    
    stat <- getplotPar.Ngroup(plotPar,"stat")
    Nbin <- getplotPar.Ngroup(plotPar,"Nbin")
  }
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr)
  rtnseri <- aggr.rtn(rtnseri,freq=Nbin)
  rtnseri.df <- data.frame(time=time(rtnseri),zoo::coredata(rtnseri))
  rtnseri.melt <- reshape2::melt(rtnseri.df,id.vars="time")
  rtnseri.melt$group <- as.integer(substring(rtnseri.melt$variable,2))
  re <- ggplot(rtnseri.melt,aes(x=time,y=value,size=group))+
    geom_point()+
    scale_size(range=c(1,4))+
    ggtitle("Return of each group")+
    scale_y_continuous(labels=scales::percent)
  return(re)
}
#' @rdname backtest.Ngroup
#' @return chart.Ngroup.seri_bar return a ggplot object of "return time series of the groups" with geom_bar
#' @export
#' @examples 
#' chart.Ngroup.seri_bar(TSFR,5,"3 month")
chart.Ngroup.seri_bar <- function(TSFR,N=5,Nbin="day",stat=c("mean","median"),
                                  sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                  plotPar){
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")    
    stat <- getplotPar.Ngroup(plotPar,"stat")
    Nbin <- getplotPar.Ngroup(plotPar,"Nbin")
  }  
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr)
  rtnseri <- aggr.rtn(rtnseri,freq=Nbin)
  rtnseri.df <- data.frame(time=time(rtnseri),zoo::coredata(rtnseri))
  rtnseri.melt <- reshape2::melt(rtnseri.df,id.vars="time")
  rtnseri.melt$group <- as.integer(substring(rtnseri.melt$variable,2))
  rtnseri.melt$time <- as.character(rtnseri.melt$time)
  re <- ggplot(rtnseri.melt,aes(x=group,y=value))+
    geom_bar(position="dodge",stat="identity")+
    facet_wrap(~ time, scales="free_y") +
    ggtitle("Return of each group")+
    scale_y_continuous(labels=scales::percent)
  return(re)
}
#' @rdname backtest.Ngroup
#' @return chart.Ngroup.seri_line return a ggplot object of "Cumulated return of each group" with geom_line
#' @export
#' @examples 
#' chart.Ngroup.seri_line(TSFR,5)
chart.Ngroup.seri_line <- function(TSFR,N=5,stat=c("mean","median"),
                                   sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                   plotPar){
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")    
    stat <- getplotPar.Ngroup(plotPar,"stat")
  }  
  rtnseri <- seri.Ngroup.rtn(TSFR=TSFR,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr)
  indexseri <- WealthIndex(rtnseri)
  re <- ggplot.ts.line(indexseri,main="Wealth index of each group",size=1)
  return(re)
}
#' @rdname backtest.Ngroup
#' @return chart.Ngroup.spread return and print a recordedplot object of "Performance Summary of top-bottom spread" . 
#' @export
#' @examples 
#' chart.Ngroup.spread(TSFR,5)
chart.Ngroup.spread <- function(TSFR,N=5,stat=c("mean","median"),
                                sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                plotPar){
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")    
    stat <- getplotPar.Ngroup(plotPar,"stat")
  }  
  rtnseri <- seri.Ngroup.rtn(TSFR=TSFR,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr)
  spreadseri <- rtnseri[,1]-rtnseri[,ncol(rtnseri)]
  colnames(spreadseri) <- "spread"
  re <- ggplots.PerformanceSummary(spreadseri,var.cum=list(1),var.dd=list(1),var.bar=list(1),bar.freq="day",main="Performance Summary of top-bottom spread")
}
#' @rdname backtest.Ngroup
#' @param turnoverType a character string,indicating the method to calculate the turnover,could be "num" or "wgt".
#' @param group a integer, indicating the group whose turnover be plotted
#' @return chart.Ngroup.turnover return a ggplot object of "Turnover Rate of each rebalancing point"
#' @export
#' @examples 
#' chart.Ngroup.turnover(TSFR,5)
chart.Ngroup.turnover <- function(TSFR,N=5,turnoverType=c("num","wgt"),group=1,
                                  sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                  plotPar){
  turnoverType <- match.arg(turnoverType)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")   
    turnoverType <- getplotPar.Ngroup(plotPar,"turnoverType")
  }  
  turnoverseri <- seri.Ngroup.turnover(TSFR,N=N,turnoverType=turnoverType,sectorNe=sectorNe,sectorAttr=sectorAttr)
  turnoverseri <- turnoverseri[,group,drop=FALSE]
  re <- ggplot.ts.bar(turnoverseri,main=paste("Turnover rate of group",group)) +
    theme(legend.position="none")+
    scale_y_continuous(labels=scales::percent)
  return(re)
}





# --------------------- ~~ Multi comparison - Ngroup --------------


#' @rdname backtest.Ngroup
#' @param TSFRs a list of object \bold{TSFR}. See \code{\link{Model.TSFR}}.
#' @return  MC.table.Ngroup.overall return a matrix, which giving the statistics of the top-bottom spread of each \code{TSFR}.
#' @export
#' @examples 
#' mp = modelPar.default()
#' factorIDs <- c("F000001","F000002","F000005")
#' FactorLists <- buildFactorLists_lcfs(factorIDs)
#' mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
#' TSR <- Model.TSR(mp)
#' TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)
#' MC.table.Ngroup.overall(TSFRs)
MC.table.Ngroup.overall <- function(TSFRs,N=5,stat=c("mean","median"),
                                    turnoverType=c("num","wgt"),fee=0,
                                    sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                    backtestPar){
  check.name_exist(TSFRs)
  stat <- match.arg(stat)
  turnoverType <- match.arg(turnoverType)
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    stat <- getbacktestPar.Ngroup(backtestPar,"stat")
    turnoverType <- getbacktestPar.Ngroup(backtestPar,"turnoverType")
    fee <- getbacktestPar.fee(backtestPar,"secu")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
    sectorAttr <- getbacktestPar.Ngroup(backtestPar,"sectorAttr")
  } 
  overall.table <- plyr::laply(TSFRs,function(x) {table.Ngroup.overall(TSFR=x,N=N,stat=stat,turnoverType=turnoverType,fee=fee,sectorNe=sectorNe,sectorAttr=sectorAttr)[ , 1, drop=FALSE]})
  NMs <- names(TSFRs)
  rownames(overall.table) <- NMs
  return(overall.table)
}


#' @param ncol a integer, specificate the number of cols of the multi-charts.
#' @rdname backtest.Ngroup
#' @export
#' @examples 
#' MC.chart.Ngroup.overall(TSFRs)
MC.chart.Ngroup.overall <- function(TSFRs,N=5,stat=c("mean","median"),
                                    sectorNe=FALSE,sectorAttr=defaultSectorAttr(),
                                    ncol=3,plotPar){
  check.name_exist(TSFRs)
  stat <- match.arg(stat)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
    stat <- getplotPar.Ngroup(plotPar,"stat")
    sectorNe <- getplotPar.Ngroup(plotPar,"sectorNe")
    sectorAttr <- getplotPar.Ngroup(plotPar,"sectorAttr")
    ncol <- getplotPar.MC(plotPar,"ncol.Ngroup")
  } 
  NMs <- names(TSFRs)
  Ngroup.charts <- mapply(function(x,nm){
    chart.Ngroup.overall(x,N=N,stat=stat,sectorNe=sectorNe,sectorAttr=sectorAttr)+  
      ggtitle(nm) +
      theme(axis.title.x= element_blank(),axis.title.y= element_blank())
  },TSFRs,NMs,SIMPLIFY = FALSE )
  Ngroup.multicharts <- multiplot_facet(plotlist=Ngroup.charts,ncol=ncol)
  return(Ngroup.multicharts)
}












# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  backtesting with 'regression' method ----------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

backtest.reg <- function(TSFR){
  
}

plot.reg <- function(TSFR){
  
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  backtesting with 'longshort' method ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' backtest.longshort
#' 
#' backtesting the factor with some tables and charts using the 'long-short(hedging)' method.
#' 
#' If param backtestPar and plotPar is not missing,then the related params will be extracted from them.It is usefull when the parametres has been initialized.
#' @rdname backtest.longshort
#' @name backtest.longshort
#' @aliases tables.longshort
#' @param rtn.LSH a \bold{rtn.LSH} or a \bold{rtn.LBH} object getting by function \code{\link{getrtn.LSH}} or \code{\link{getrtn.LBH}}.
#' @param hitFreq indicating the interval when computing the hitRatio of rtn. An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param backtestPar Optional. a \bold{backtestPar} object,if not missing,then extract pars from backtestPar.
#' @return  tables.longshort return a list containing some tables which giving the result of the long-short strategy backtesting.The items are:
#'  \itemize{
#'    \item summary: summary of \bold{of long,short and hedge}.
#'    \item summary.yearly: yearly summary \bold{of the hedged rtn}.
#'    \item hedge.stats: main statisticals \bold{of the hedged rtn},by different freq.
#'    \item period.stats: table showing yearly,all-span and annualized return \bold{of long,short and hedge}.
#'    \item DD.stats:table showing statistics for the worst drawdowns \bold{of the hedged rtn}.
#'  }
#' @seealso \code{\link{getrtn.LSH}},\code{\link{getrtn.LBH}}
#' @author Ruifei.Yin
#' @export
#' @examples
#' rtn.long <- xts(rnorm(1000,0.001,0.02),as.Date("2010-01-01") + 1:1000)
#' rtn.short <- rtn.long + rnorm(1000,-0.0001,0.003)
#' rebFreq <- "month"
#' rtn.LSH <- addrtn.hedge(rtn.long,rtn.short,rebFreq)
#' re <- tables.longshort(rtn.LSH)
tables.longshort <- function(rtn.LSH,hitFreq="month",backtestPar){
  if(!missing(backtestPar)){
    hitFreq <- getbacktestPar.longshort(backtestPar,"hitFreq")
  }
  rtn <- rtn.LSH
  # ---- rtn.aggr: aggreated return series(of long,short and hedge) by different freq, each being an item of a list.(note that 'rtn.aggr$day' is equal to 'rtn') 
  freq <- c("day","week","month","quarter","year")  
  rtn.aggr <- lapply(freq,function(freq){aggr.rtn(rtn,freq)})
  names(rtn.aggr) <- paste(freq,"ly",sep="")
  # ---- hedge.stats: main statisticals of the hedged rtn,by different freq
  hedge.stats <- t(plyr::laply(rtn.aggr,function(x){rtn.stats(x[,"hedge",drop=FALSE])}))
  colnames(hedge.stats) <- paste(freq,"ly",sep="")  
  # ---- period.stats: table showing the yearly,all-span and annualized return
  period.stats <- rtn.periods(rtn)
  # ---- DD.stats:table showing statistics for the worst drawdowns.
  DD.stats <- PerformanceAnalytics::table.Drawdowns(rtn$hedge)  
  # ---- summary:summary of the all over rtn
  summary <- rtn.summary(rtn,hitFreq=hitFreq)
  # ---- summary.yearly:summary of the yearly 'hedged' rtn
  summary.yearly <- t(xts::apply.yearly(rtn$hedge,rtn.summary,hitFreq=hitFreq))
  colnames(summary.yearly) <- lubridate::year(colnames(summary.yearly))
  rownames(summary.yearly) <- rownames(summary)  
  return(list(summary=summary,
              summary.yearly=summary.yearly,
              period.stats=period.stats,
              hedge.stats=hedge.stats,              
              DD.stats=DD.stats))  
}
#' @rdname backtest.longshort
#' @param bar.freq the freq of the per-period performance bar chart
#' @param plotPar Optional.a \bold{plotPar} object,if not missing,then extract pars from plotPar
#' @return chart.longshort.summary  return and print a recordedplot object, which demonstrate the performance of the return series,including wealth index chart(\bold{of long,short and hedging}),underwater chart for drawdown(\bold{of hedging}),and bars for per-period performance(\bold{of hedging})..
#' @export
#' @examples
#' chart.longshort.summary(rtn.LSH)
chart.longshort.summary <- function(rtn.LSH,bar.freq="month",plotPar){  
  if(!missing(plotPar)){
    bar.freq <- getplotPar.longshort(plotPar,"bar.freq")
  }
  re <- ggplots.PerformanceSummary(rtn.LSH,var.cum=list(c(1,2),3),var.dd=list(3),var.bar=list(3),bar.freq=bar.freq)
}
#' @rdname backtest.longshort
#' @param roll.width the width argument for rolling performance chart
#' @param roll.by the by argument for rolling performance chart
#' @return chart.longshort.rolling  return and print a recordedplot object, which include a rolling annualized returns chart,a rolling annualized standard deviation chart, and a rolling annualized sharpe ratio chart.
#' @export
#' @examples
#' chart.longshort.rolling(rtn.LSH)
chart.longshort.rolling <- function(rtn.LSH,roll.width=250,roll.by=30,plotPar){
  if(!missing(plotPar)){
    roll.width <- getplotPar.longshort(plotPar,"roll.width")
    roll.by <- getplotPar.longshort(plotPar,"roll.by")
  }
  re <- ggplots.RollingPerformance(rtn.LSH[,"hedge"],width=roll.width,by=roll.by)
}


table.turnover <- function(PB){
  
}
chart.turnover <- function(PB){
  
}






# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  multifactor comparison ------------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============




#' MC.wgt.CAPM
#' 
#' compute the wgt vector of multi-factors by CAPM model. 
#' @param TSFRs a list of object \bold{TSFR}. See \code{\link{Model.TSFR}}.
#' @param stat a character string,indicating the methods to compute IC,could be "pearson" or "spearman".
#' @param backtestPar Optional.a \bold{backtestPar} object,if not missing,then extract pars from backtestPar.
#' @param wgtmin set minimal factor weight.
#' @param wgtmax set maximal factor weight.
#' @param targetType optimization's target type, could be "return" or "risk" or "sharpe" or 'balance',default value is "sharpe".
#' @param riskaversion risk aversion parameter for "balance" target.
#' @return a factor weight vector
#' @export
#' @importFrom PortfolioAnalytics set.portfolio.moments
#' @examples
#' mp = modelPar.default()
#' factorIDs <- c("F000001","F000004","F000005","F000008")
#' FactorLists <- buildFactorLists_lcfs(factorIDs)
#' mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
#' TSR <- Model.TSR(mp)
#' TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)
#' MC.wgt.CAPM(TSFRs)
#' MC.wgt.CAPM(TSFRs,wgtmin=0.05,wgtmax=0.4,targetType='risk')  
#' MC.wgt.CAPM(TSFRs,wgtmin=0.05,wgtmax=0.4,targetType='balance',riskaversion = 10)  
MC.wgt.CAPM <- function (TSFRs,stat=c("pearson","spearman"),backtestPar,
                         wgtmin=0, wgtmax=0.5,
                         targetType=c('sharpe','return','risk','balance'),
                         riskaversion=1) {
  check.name_exist(TSFRs)
  stat <- match.arg(stat)
  targetType <- match.arg(targetType)
  if(!missing(backtestPar)){
    stat <- getbacktestPar.IC(backtestPar,"stat")
  } 
  IC.seris <- plyr::laply(TSFRs, seri.IC, stat=stat)
  rownames(IC.seris) <- names(TSFRs)
  IC.seris <- t(IC.seris)
  IC.seris <- xts::xts(IC.seris,order.by = unique(TSFRs[[1]]$date_end)[1:nrow(IC.seris)])
  
  factor.names <- colnames(IC.seris)
  pspec <- PortfolioAnalytics::portfolio.spec(assets=factor.names)
  pspec <- PortfolioAnalytics::add.constraint(portfolio=pspec, type="full_investment")
  pspec <- PortfolioAnalytics::add.constraint(portfolio=pspec, type="box", min=wgtmin, max=wgtmax)
  if(targetType=='return'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec,type='return',name='mean')
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=IC.seris, portfolio=pspec,optimize_method="ROI",trace=TRUE)
  }else if(targetType=='risk'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec,type='risk',name='var')
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=IC.seris, portfolio=pspec,optimize_method="ROI",trace=TRUE)
  }else if(targetType=='balance'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="return", name="mean")
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="risk", name="var", risk_aversion=riskaversion)
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=IC.seris, portfolio=pspec,optimize_method="ROI",trace=TRUE)
  }else if(targetType=='sharpe'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="return", name="mean")
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="risk", name="StdDev")
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=IC.seris, portfolio=pspec,optimize_method="ROI",maxSR=TRUE,trace=TRUE)
  }
  
  return(opt_ps$weights)
}







