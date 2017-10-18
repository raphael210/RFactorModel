# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# -------------------- Factor descriptive statistics --------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' Factor descriptive statistics
#' 
#' draw factor's histogram and boxplot,and summarize factor's statistics value.
#' @name factor_descriptive_statistics
#' @rdname factor_descriptive_statistics
#' @author Aming.Tao
#' @examples 
#' mp <- modelPar.default()
#' TSF <- Model.TSF(mp)
#' chart.Fct_hist(TSF)
#' chart.Fct_box(TSF)
#' chart.Fct_density(TSF)
#' re <- table.Fct_descr(TSF)
#' #~~ multiple factor ~~
#' FactorLists <- buildFactorLists_lcfs(c("F000006","F000002","F000005"))
#' TS <- Model.TS(mp)
#' mTSF <- getMultiFactor(TS,FactorLists)
#' MF.chart.Fct_hist(mTSF)
#' MF.chart.Fct_box(mTSF)
#' MF.chart.Fct_density(mTSF)
#' re2 <- MF.table.Fct_descr(mTSF)
#' @export
chart.Fct_hist <- function(TSF,bins=NULL,ncol=NULL){
  ggplot(TSF, aes(factorscore)) + 
    geom_histogram(colour = "black", fill = "white",bins = bins)+
    facet_wrap(~date,scales = "free",ncol = ncol)
}

#' @rdname factor_descriptive_statistics
#' @export
chart.Fct_density <- function(TSF){
  ggplot(TSF, aes(factorscore,color=as.factor(date))) + 
    geom_density()
}


#' @rdname factor_descriptive_statistics
#' @export
chart.Fct_box <- function(TSF){
  #facet by dates
  TSF$date <- as.factor(TSF$date)
  ggplot(TSF, aes(date,factorscore)) + 
    geom_boxplot()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#' @rdname factor_descriptive_statistics
#' @export
table.Fct_descr <- function(TSF){
  re <- TSF %>% group_by(date) %>%
    dplyr::summarise(Obs=length(factorscore),
                     NAs=sum(is.na(factorscore)),
                     min=min(factorscore,na.rm = TRUE),
                     max=max(factorscore,na.rm = TRUE),
                     mean=mean(factorscore,na.rm = TRUE),
                     median=median(factorscore,na.rm = TRUE),
                     sd=sd(factorscore,na.rm = TRUE),
                     skewness=PerformanceAnalytics::skewness(factorscore,na.rm = TRUE),
                     kurtosis=PerformanceAnalytics::kurtosis(factorscore,na.rm = TRUE))
  return(re)
}


#' chart.FctRtn_scatter
#' 
#' @export
#' @examples 
#' modelPar <- modelPar.default()
#' TSFR <- Model.TSFR(modelPar)
#' chart.FctRtn_scatter(TSFR,25)
chart.FctRtn_scatter <- function(TSFR,ncol=NULL){
  ggplot(TSFR, aes(factorscore,periodrtn)) + 
    geom_point()+
    geom_smooth()+
    facet_wrap(~date,scales = "free",ncol = ncol)
}


#' ANOVA ANALYSIS
#' 
#' @param TSF A TSF.
#' @param sectorAttr_lists A list of sectorAttr, each sectorAttr is a list.
#' @param sectorAttr_names A character vector of names, could be missing.
#' @param significant_level The ceiling of the p_value. This argument will make sense only when the value_type is p_value. Only the value under this cutting line will be considered as passing the test.
#' @param full_details Logical value. Whether return the details instead of summary. 
#' @return If all the arguments are default, the result is a table with the ANOVA test pass ratio in each sector splitting method.
#' @rdname table.Fct_anova
#' @name table.Fct_anova
#' @export
#' @author Han.Qian
#' @examples 
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' TSF <- gf.NP_YOY(TS, src = "fin")
#' sectorAttr_lists_1 <- list(list(std = 33, level = 1),
#'                            list(std = 336, level = 1))
#' sectorAttr_lists_2 <- list(list(std = 33, level = 1))
#' chart.Fct_anova(TSF, sectorAttr_lists_1)
#' chart.Fct_anova(TSF, sectorAttr_lists_2)
#' table.Fct_anova(TSF, sectorAttr_lists_1)
#' table.Fct_anova(TSF, sectorAttr_lists_2)
#' table.Fct_anova(TSF, sectorAttr_lists_1, full_details = TRUE)
#' table.Fct_anova(TSF, sectorAttr_lists_2, full_details = TRUE)
table.Fct_anova <- function(TSF, sectorAttr_lists, sectorAttr_names, 
                        significant_level = 0.05, full_details = FALSE){
  
  # ARGUMENTS CHECKING
  sec_attr_length <- length(sectorAttr_lists)
  if(missing(sectorAttr_names)){
    sectorAttr_names <- paste0("sec_",1:sec_attr_length)
  }else{
    if(length(sectorAttr_lists) != length(sectorAttr_names)){
      stop("The length of sectorAttr_names does not match the length of sectorAttr_lists")
    }
  }
  
  # GET SECTORS
  for(i in 1:sec_attr_length){
    sectorAttr_ <- sectorAttr_lists[[i]]
    TSF <- getSectorID(TSF, sectorAttr = sectorAttr_)
    TSF <- renameCol(TSF, "sector", sectorAttr_names[i])
  }
  TSF_core <- subset(TSF, select = c("date","stockID","factorscore", sectorAttr_names))
  
  # LOOP STARTS HERE  
  datelist <- unique(TSF$date)
  final_re_p <- data.frame()
  final_re_f <- data.frame()
  for( i in 1:length(datelist)){
    date_ <- datelist[i]
    TSF_subset_ <- subset(TSF_core, date == date_)
    # LOOP THROUGH COLUMNS
    for( j in 1:sec_attr_length){
      results_ <- aov(factorscore ~ TSF_subset_[,j+3], data = TSF_subset_)
      results2_ <- summary(results_)
      re_p_ <- results2_[[1]]$`Pr(>F)`[1]
      re_f_ <- results2_[[1]]$`F value`[1]
      
      if(j == 1L){
        final_re_row_p <- data.frame("date" = date_, re_p_)
        final_re_row_p <- renameCol(final_re_row_p, "re_p_", sectorAttr_names[1])
        final_re_row_f <- data.frame("date" = date_, re_f_)
        final_re_row_f <- renameCol(final_re_row_f, "re_f_", sectorAttr_names[1])
      }else{
        final_re_row_p <- cbind(final_re_row_p, re_p_)
        final_re_row_p <- renameCol(final_re_row_p, "re_p_", sectorAttr_names[j]) 
        final_re_row_f <- cbind(final_re_row_f, re_f_)
        final_re_row_f <- renameCol(final_re_row_f, "re_f_", sectorAttr_names[j])
      }
    }
    final_re_p <- rbind(final_re_p, final_re_row_p)
    final_re_f <- rbind(final_re_f, final_re_row_f)
  }
  
  # ORGANIZE AND OUTPUT
  if(full_details){
    colnames(final_re_p) <- c("date", paste0("p_value_",colnames(final_re_p)[-1]))
    colnames(final_re_f) <- c("date", paste0("f_value_",colnames(final_re_f)[-1]))
    final_final_re <- merge.x(final_re_p, final_re_f, by = "date")
  }else{
    final_re_p[,2:(sec_attr_length+1)] <- (final_re_p[,2:(sec_attr_length+1), drop = FALSE] < significant_level)
    final_final_re <- colMeans(final_re_p[,2:(sec_attr_length+1), drop = FALSE])
    final_final_re <- as.data.frame(final_final_re)
    colnames(final_final_re) <- "PassRate"
  }
  return(final_final_re)
}

#' @rdname table.Fct_anova
#' @export
chart.Fct_anova <- function(TSF, sectorAttr_lists, sectorAttr_names, 
                        significant_level = 0.05, 
                        value_type = c("p_value","f_value")){
  value_type <- match.arg(value_type)
  dat <- table.Fct_anova(TSF, sectorAttr_lists, sectorAttr_names, significant_level, full_details = TRUE)
  col_names <- colnames(dat)
  if(value_type == "p_value"){
    ind_ <- substr(col_names, 1, 4) == "p_va"
  }else if(value_type == "f_value"){
    ind_ <- substr(col_names, 1, 4) == "f_va"
  }
  the_xts <- xts::as.xts(dat[,ind_], order.by = dat$date)
  # OUT PUT
  return(ggplot.ts.line(the_xts, main = paste("ANOVA",value_type,"time series")))
}

#' @rdname table.Fct_anova
#' @export
chart.Fct_NA <- function(TSF){
  check.TSF(TSF)
  TSF <- data.table::as.data.table(TSF)
  TSF <- data.table::setkeyv(TSF, cols= "date")
  TSF <- TSF[,.(percent_NA = mean(is.na(factorscore))), by = date]
  TSF.xts <- xts::as.xts(TSF$percent_NA, order.by = TSF$date)
  result <- ggplot.ts.line(TSF.xts, main = "NA percentage", show.legend = FALSE)
  return(result)
}


#' @rdname table.Fct_anova
#' @export
MF.chart.Fct_NA <- function(mTSF){
  fnames <- guess_factorNames(mTSF)
  datelist <- unique(mTSF$date)
  result <- data.frame()
  for( i in 1:length(datelist)){
    date_ <- datelist[i]
    mTSF_ <- subset(mTSF, date == date_)
    mTSF_ <- mTSF_[,fnames, drop = FALSE]
    re_ <- as.data.frame(t(colMeans(is.na(mTSF_))))
    re_ <- cbind(date_, re_)
    result <- rbind(result, re_)
  }
  result.xts <- xts::as.xts(result[,fnames], order.by = result$date_)
  result.plot <- ggplot.ts.line(result.xts, main = "NA percentage")
  return(result.plot)
}



# ---------------------  ~~ Multi-factor - descriptive stat --------------
#' @rdname factor_descriptive_statistics
#' @export
MF.chart.Fct_hist <- function(mTSF){
  fnames <- guess_factorNames(mTSF)
  mTSF <- reshape2::melt(mTSF,id.vars=c('date','stockID'),measure.vars=fnames,variable.name = "fname",value.name = "factorscore")
  ggplot(mTSF, aes(factorscore)) + 
    geom_histogram(colour = "black", fill = "white")+
    facet_grid(date~fname,scales="free")
  
}


#' @rdname factor_descriptive_statistics
#' @export
MF.chart.Fct_density <- function(mTSF,ncol=NULL){
  fnames <- guess_factorNames(mTSF)
  mTSF <- reshape2::melt(mTSF,id.vars=c('date','stockID'),measure.vars=fnames,variable.name = "fname",value.name = "factorscore")
  ggplot(mTSF, aes(factorscore,color=fname)) + 
    geom_density()+
    facet_wrap(~date,scales="free",ncol = ncol)
}

#' @rdname factor_descriptive_statistics
#' 
#' @export
MF.chart.Fct_box <- function(mTSF,ncol=NULL){
  fnames <- guess_factorNames(mTSF)
  mTSF <- reshape2::melt(mTSF,id.vars=c('date','stockID'),measure.vars=fnames,variable.name = "fname",value.name = "factorscore")
  ggplot(mTSF, aes(fname,factorscore)) + 
    geom_boxplot()+
    facet_wrap(~date,scales = "free",ncol = ncol)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#' @rdname factor_descriptive_statistics
#' @export
MF.table.Fct_descr <- function(mTSF){
  fnames <- guess_factorNames(mTSF)
  mTSF <- reshape2::melt(mTSF,id.vars=c('date','stockID'),measure.vars=fnames,variable.name = "fname",value.name = "factorscore")
  re <- mTSF %>% group_by(date,fname) %>%
    dplyr::summarise(Obs=length(factorscore),
                     NAs=sum(is.na(factorscore)),
                     min=min(factorscore,na.rm = TRUE),
                     max=max(factorscore,na.rm = TRUE),
                     mean=mean(factorscore,na.rm = TRUE),
                     median=median(factorscore,na.rm = TRUE),
                     sd=sd(factorscore,na.rm = TRUE),
                     skewness=PerformanceAnalytics::skewness(factorscore,na.rm = TRUE),
                     kurtosis=PerformanceAnalytics::kurtosis(factorscore,na.rm = TRUE))
  return(re)
  
}



#' @name factor_descriptive_statistics
#' @param Nbin the number of the groups the timespan is cut to when plotting the scatter by time series.It could also be a character of interval specification,See \code{\link{cut.Date}} for detail. The default value is "day",which means no cutting, the scatters of every date are ploted.
#' @examples
#' # ---  raw_factor_correlation
#' RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2016-09-30'))
#' TS <- getTS(RebDates,indexID = 'EI000985')
#' factorIDs <- c("F000006","F000008","F000012")
#' FactorLists <- buildFactorLists_lcfs(factorIDs,factorRefine=refinePar_default("robust"))
#' mTSF <- getMultiFactor(TS,FactorLists = FactorLists)
#' MF.chart.Fct_corr(mTSF)
#' MF.chart.Fct_corr(mTSF,Nbin='year')
#' @export
MF.chart.Fct_corr <- function(mTSF,Nbin){
  corr <- MF.table.Fct_corr(mTSF,Nbin)
  ggplot.corr(corr)
}



#' @rdname factor_descriptive_statistics
#' @examples 
#' MF.table.Fct_corr(mTSF)
#' MF.table.Fct_corr(mTSF,Nbin='year')
#' @export
MF.table.Fct_corr <- function(mTSF,Nbin){
  
  # fnames <- setdiff(colnames(mTSF),c('date','stockID','date_end','periodrtn'))
  fnames <- guess_factorNames(mTSF)
  mTSF_by <- dplyr::group_by(mTSF[,c('date',fnames)],date)
  
  cordata <- mTSF_by %>% dplyr::do(cormat = cor(.[,fnames],method='spearman',use="pairwise.complete.obs"))
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
#' @param TSF a \bold{TSF} object
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
  TSFR <- na.omit(TSFR[,c("date_end","stockID","factorscore","periodrtn")])
  if(stat=="pearson"){
    IC.seri <- plyr::ddply(TSFR,"date_end",plyr::summarise,
                     IC=cor(periodrtn,factorscore,method="pearson",use="pairwise.complete.obs"))
  } else if(stat=="spearman"){
    IC.seri <- plyr::ddply(TSFR,"date_end",plyr::summarise,
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
#' re <- seri.IC.decay(TSF)
seri.IC.decay <- function(TSF,stat=c("pearson","spearman"),backtestPar,
                          prd_lists = list(w1=lubridate::weeks(1),
                                           w2=lubridate::weeks(2),
                                           m1=months(1),
                                           m2=months(2),
                                           m3=months(3),
                                           m6=months(6)) ){
  stat <- match.arg(stat)
  if(!missing(backtestPar)){
    stat <- getbacktestPar.IC(backtestPar,"stat")
  }
  
  # --- get the period rtns
  TSFR <- getTSR_decay(TSF, prd_lists = prd_lists)
  # --- calculate the IC seri.
  prd_names <- names(prd_lists)
  if(stat=="pearson"){
    IC.seri <- plyr::ddply(TSFR,"date",function(dat){
      t(cor(dat[, paste("prdrtn_",prd_names,sep="")],dat[,"factorscore"],method="pearson",use="pairwise.complete.obs"))
    })
  } else if(stat=="spearman"){
    IC.seri <- plyr::ddply(TSFR,"date",function(dat){
      t(cor(dat[,paste("prdrtn_",prd_names,sep="")],dat[,"factorscore"],method="spearman",use="pairwise.complete.obs"))
    })
  }
  re <- as.xts(IC.seri[,-1,drop=FALSE],IC.seri[,1])
  colnames(re) <- paste("IC_",prd_names,sep="")
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
  IC.annu <- as.vector(IC.annualized(seri)) # IC.annu = IC.mean*sqrt(N)
  seri <- as.vector(seri)
  IC.mean <- mean(seri,na.rm=TRUE)
  IC.std <- sd(seri,na.rm=TRUE)
  IC.IR <- IC.mean/IC.std
  IC.Ttest.t <- t.test(seri)$statistic
  IC.Ttest.p <- t.test(seri)$p.value
  IC.hit <- hitRatio(seri) 
  re <- c(IC.mean, IC.std, IC.IR, IC.Ttest.t, IC.Ttest.p, IC.hit, IC.annu)
  re <- matrix(re,length(re),1)
  colnames(re) <- "IC"
  rownames(re) <- c("IC_mean","IC_std","IC_IR","IC_t","IC_p","IC_hitRatio","IC_annu")
  return(re)
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
  wid <- 365/periodicity_Ndays(seri)  
  if(wid >= NROW(seri)){
    warning("IC seri is shorter than 12 months, could not plot the 12 months MA!")
    re <- re + ggtitle("IC series")
  } else {
    ICma <- zoo::rollapply(as.zoo(seri),width=wid,FUN=mean,na.rm=TRUE,align ="right")
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
#' @return chart.IC.decay return a ggplot object of decayed ICs bar chart. (You can also use \code{attr(re,"table")} to get the result table.)
#' @export
#' @examples 
#' re <- chart.IC.decay(TSF)
#' attr(re,"table") # the result table
chart.IC.decay <- function(TSF,stat=c("pearson","spearman"),backtestPar,
                           prd_lists = list(w1=lubridate::weeks(1),
                                            w2=lubridate::weeks(2),
                                            m1=months(1),
                                            m2=months(2),
                                            m3=months(3),
                                            m6=months(6))){
  stat <- match.arg(stat)
  seri <- seri.IC.decay(TSF=TSF,stat=stat,backtestPar=backtestPar,prd_lists=prd_lists)
  IC.mean <- base::colMeans(seri,na.rm=TRUE)
  IC.std <- timeSeries::colSds(seri,na.rm=TRUE)
  IC.IR <- IC.mean/IC.std
  IC.Ttest.t <- timeSeries::colStats(seri, function(x) t.test(x)$statistic)
  IC.Ttest.p <- timeSeries::colStats(seri, function(x) t.test(x)$p.value)
  IC.hit <- as.vector(hitRatio(seri))
  if(TRUE){ # annulized IC
    sqrtN <- vector()
    for (ii in 1:length(prd_lists)){
      prd <- prd_lists[[ii]]
      N <- 365/(prd/lubridate::days(1))
      sqrtN <- c(sqrtN,sqrt(N))
    }
    IC.annu <- IC.mean*sqrtN
  }
  re_table <- t(cbind(IC.mean, IC.std, IC.IR, IC.Ttest.t, IC.Ttest.p, IC.hit, IC.annu))
  rownames(re_table) <- c("IC_mean","IC_std","IC_IR","IC_t","IC_p","IC_hitRatio","IC_annu")
  
  if(TRUE){ # -- chart.IC.decay
    dat <- data.frame(decay=factor(1:ncol(seri),labels = colnames(seri)),IC_mean=IC.mean,IC_annu=IC.annu, leg_mean="IC_mean",leg_annu="IC_annu", group=1L)
    re <- ggplot(data = dat)+
      geom_bar(mapping = aes(x=decay, y=IC_mean, fill=leg_mean),position="dodge",stat="identity")+
      geom_line(mapping = aes(x=decay, y=IC_annu, fill=leg_annu, group=group),colour = "red", size = 1)+
      theme(axis.title.y= element_blank(),legend.title=element_blank())+
      ggtitle("IC decay")
  }
  attr(re,"table") <- re_table
  return(re)
}




#' @param mTSFR a \bold{mTSFR} object. See \code{\link{getMultiFactor}}.
#' @rdname backtest.IC
#' @export
#' @examples
#' mTSFR <- getMultiFactor(TSR,FactorLists)
#' MF.chart.IC(mTSFR)
MF.chart.IC <- function(mTSFR,Nbin="day",stat=c("pearson","spearman"),
                        facet_by=c("date","fname")){
  fnames <- guess_factorNames(mTSFR)
  TSFRs <- lapply(mTSFR[,fnames],function(x,mTSFR){
    as.data.frame(cbind(mTSFR[,c('date','date_end','stockID')],
                        factorscore=x,periodrtn=mTSFR[,'periodrtn']))
  },mTSFR=mTSFR)
  
  stat <- match.arg(stat)
  facet_by <- match.arg(facet_by)
  
  IC <- plyr::llply(TSFRs,seri.IC,stat=stat)
  IC <- lapply(IC,function(ts){
    df <- data.frame(date=zoo::index(ts),IC=zoo::coredata(ts))
    df$date <- cut.Date2(df$date,Nbin)
    df <- df %>% dplyr::group_by(date) %>% dplyr::summarise(IC=mean(IC,na.rm = TRUE)) %>%
      dplyr::ungroup() %>% dplyr::mutate(date=as.Date(date))
    return(df)
  })
  IC <- dplyr::bind_rows(IC,.id = 'fname')
  
  if(facet_by=='date'){
    ggplot(IC,aes(fname, IC,fill=fname)) +
      geom_bar(stat = "identity") + facet_wrap(~date)
  }else if(facet_by=='fname'){
    seri <- reshape2::dcast(IC,date~fname,value.var = 'IC')
    seri <- xts::xts(seri[,-1],order.by = seri[,1])
    wid <- round(365/periodicity_Ndays(seri))
    if(wid > NROW(seri)){
      ggplot(IC,aes(date, IC)) +
        geom_bar(stat = "identity") + facet_wrap(~fname)
    } else {
      ICma <- zoo::rollapply(zoo::as.zoo(seri),width=wid,FUN=mean,na.rm=TRUE,align='right')
      by <- cut.Date2(zoo::index(ICma),Nbin)
      ICma.aggr <- aggregate(ICma,as.Date(by),tail,1)
      ICma.aggr <- melt.ts(ICma.aggr)
      colnames(ICma.aggr) <- c("date","fname","IC.MA")
      
      ggplot(IC,aes(date, IC)) +
        geom_bar(stat = "identity")+
        geom_line(data=ICma.aggr,aes(x=date,y=IC.MA),size=1)+
        ggtitle("IC series (with its 12 months MA)")+
        facet_wrap(~fname)
    }
  }
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
  ggplot.corr(IC.corrmat)
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
  IC.charts.decay <- plyr::llply(TSFRs, chart.IC.decay, stat=stat)
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
#' re2 <- seri.Ngroup.rtn(TSFR,5,include_univ=TRUE)
seri.Ngroup.rtn <- function(TSFR,N=5,
                            include_univ=FALSE,
                            sectorNe=NULL,
                            bysector=NULL,
                            backtestPar){
  
  # ARGUMENTS CHECKING
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
  }
  check.TSFR(TSFR)
  TSFR <- na.omit(TSFR[,c("date_end","stockID","factorscore","periodrtn")]) 
  
  # ADD RANK AND GROUP
  if(is.null(sectorNe)){
    TSFR <- data.table::data.table(TSFR,key=c("date_end"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by="date_end"]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by="date_end"]
  } else {
    TSFR <- renameCol(TSFR,"date_end","date")
    TSFR <- getSectorID(TSFR,sectorAttr=sectorNe)
    TSFR <- renameCol(TSFR,"date","date_end")
    TSFR <- data.table::data.table(TSFR,key=c("date_end","sector"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by=c("date_end","sector")]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by=c("date_end","sector")]
    TSFR$sector <- NULL
  }
  
  # GET RTN
  if(is.null(bysector)){ # -- return a xts
    data.table::setkeyv(TSFR,c("date_end","group"))
    rtn.df <- TSFR[,list(mean.rtn=mean(periodrtn)), by=c("date_end","group")]
    if(include_univ){
      univ_rtn <- TSFR[,.(group = N+1, mean.rtn = mean(periodrtn)), by = "date_end"]
      rtn.df <- rbind(rtn.df, univ_rtn)
    }
    rtn.df <- as.data.frame(rtn.df)
    rtn.mat <- reshape2::acast(rtn.df,date_end~group,value.var="mean.rtn")
    rtn.xts <- xts::as.xts(rtn.mat,as.Date(rownames(rtn.mat),tz=""))
    colnames(rtn.xts) <- if(!include_univ) paste("Q",1:N,sep="") else c(paste("Q",1:N,sep=""),"univ")
    result <- rtn.xts
    
  } else { # -- return a list of xts by sector
    TSFR <- renameCol(TSFR,"date_end","date")
    TSFR <- getSectorID(TSFR,sectorAttr=bysector)
    TSFR <- renameCol(TSFR,"date","date_end")
    TSFR <- data.table::data.table(TSFR,key=c("date_end","sector","group"))
    rtn.df <- TSFR[,list(mean.rtn=mean(periodrtn)), by=c("date_end","sector","group")]
    if(include_univ){
      univ_rtn <- TSFR[,.(group = N+1, mean.rtn = mean(periodrtn)), by = c("date_end","sector")]
      rtn.df <- rbind(rtn.df, univ_rtn)
    }
    rtn.df <- as.data.frame(rtn.df)
    rtn.mat <- reshape2::acast(rtn.df,date_end~sector~group,value.var="mean.rtn")
    result <- list()
    for(ii in 1:dim(rtn.mat)[2]){
      rtn.xts <- xts::as.xts(rtn.mat[,ii,],as.Date(rownames(rtn.mat),tz=""))
      colnames(rtn.xts) <- if(!include_univ) paste("Q",1:N,sep="") else c(paste("Q",1:N,sep=""),"univ")
      result <- c(result, list(rtn.xts))
    }
    names(result) <- dimnames(rtn.mat)[[2]]
  }
  # OUTPUT 
  return(result)  
}





#' @rdname backtest.Ngroup
#' @return seri.Ngroup.turnover return a xts, which giving the (one side) num or wgt turnover seri of each group
#' @export
#' @examples
#' re <- seri.Ngroup.turnover(TSFR,5)
seri.Ngroup.turnover <- function(TSFR,N=5,
                                 sectorNe=NULL,
                                 backtestPar){
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
  }
  check.TSFR(TSFR)
  # TSFR <- na.omit(TSFR[,c("date","stockID","factorscore","periodrtn")])
  # ---- add the rank and groups of the factorscores 
  if(is.null(sectorNe)){
    TSFR <- data.table::data.table(TSFR,key=c("date"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by="date"]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by="date"]    
  } else {
    TSFR <- getSectorID(TSFR,sectorAttr=sectorNe)
    TSFR <- data.table::data.table(TSFR,key=c("date","sector"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by=c("date","sector")]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by=c("date","sector")]    
  }
  # ---- turnover seri of each group
  for(i in 1:N){
    groupI <- subset(TSFR,group==i)    
    periodrtn <- reshape2::acast(groupI,date~stockID,value.var="periodrtn",fill=0)
    periodrtn <- xts(periodrtn,as.Date(rownames(periodrtn),tz=""))
    wgt.ini <- reshape2::acast(groupI,date~stockID,value.var="group",fill=0)
    wgt.ini <- wgt.ini/rowSums(wgt.ini)
    wgt.ini <- xts(wgt.ini,as.Date(rownames(wgt.ini),tz=""))         
    turnover.num <- wgt.ini - xts::lag.xts(wgt.ini,na.pad=TRUE)
    turnover.num <- turnover.num[-1,]
    turnover.num <- xts(rowSums(abs(turnover.num))/2,zoo::index(turnover.num))
    colnames(turnover.num) <- paste("Q",i,sep="")
    if(i==1L){
      seri <- turnover.num
    } else {
      seri <- merge(seri,turnover.num)
    }
  }
  re <- seri
  return(re)
}

# inner function
seri.Ngroup.size <- function(TSFR,N=5,
                             sectorNe=NULL,
                             backtestPar){
  # ARGUMENTS CHECKING
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
  }
  check.TSFR(TSFR)
  TSFR <- TSFR[,c("date","date_end","stockID")]
  TSFR <- getTech(TSFR, variables = "mkt_cap")
  
  # ADD RANK OR GROUP
  if(is.null(sectorNe)){
    TSFR <- data.table::data.table(TSFR,key=c("date_end"))
    TSFR <- TSFR[,rank:=rank(-mkt_cap), by="date_end"]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by="date_end"]
  } else {
    TSFR <- renameCol(TSFR,"date_end","date")
    TSFR <- getSectorID(TSFR,sectorAttr=sectorNe)
    TSFR <- renameCol(TSFR,"date","date_end")
    TSFR <- data.table::data.table(TSFR,key=c("date_end","sector"))
    TSFR <- TSFR[,rank:=rank(-mkt_cap), by=c("date_end","sector")]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by=c("date_end","sector")]
  }
  
  # ORGANIZING 
  data.table::setkeyv(TSFR,c("date_end","group"))
  size.df <- TSFR[,list(mean.size=mean(mkt_cap)), by=c("date_end","group")]
  univ_size <- TSFR[,.(group = N+1, mean.size = mean(mkt_cap)), by = "date_end"]
  
  size.df <- rbind(size.df, univ_size)
  size.df <- as.data.frame(size.df)
  size.mat <- reshape2::acast(size.df,date_end~group,value.var="mean.size")
  size.xts <- xts::as.xts(size.mat,as.Date(rownames(size.mat),tz=""))
  
  colnames(size.xts) <- c(paste("Q",1:N,sep=""),"univ")
  result <- size.xts
  return(result)
}





#' @rdname backtest.Ngroup
#' @param fee a numeric, giving the (one side) fee
#' @return table.Ngroup.overall return a matrix which giving the statistics of the rtn of each group, as well as the rtn of top-bottom spread.
#' @export
#' @examples
#' re <- table.Ngroup.overall(TSFR,5,fee=0.002)
#' re2 <- table.Ngroup.overall(TSFR, rtn_type = "long-univ")
table.Ngroup.overall <- function(TSFR,N=5,
                                 sectorNe=NULL,
                                 bysector=NULL,
                                 fee=0,
                                 rtn_type = c("long-short", "long-univ"),
                                 backtestPar){
  rtn_type <- match.arg(rtn_type)
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
    fee <- getbacktestPar.fee(backtestPar,"secu")
  }
  
  if(!is.null(bysector)){ # bysector result: a simple matrix which giving the annualized rtn of each group, by sectors.
    rtnseri <- seri.Ngroup.rtn(TSFR,N=N,include_univ = FALSE,sectorNe=sectorNe,bysector=bysector,backtestPar=backtestPar)
    annu_rtn <- plyr::laply(rtnseri,Return.annualized)
    rownames(annu_rtn) <- names(rtnseri)
    re <- annu_rtn
    return(re)
  }
  
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,include_univ = TRUE,sectorNe=sectorNe,bysector = NULL, backtestPar=backtestPar)
  turnoverseri <- seri.Ngroup.turnover(TSFR,N=N,sectorNe=sectorNe,backtestPar=backtestPar)
  
  # --- Ngroups
  rtnsummary <- rtn.summary(rtnseri)
  turnover.annu <- Turnover.annualized(turnoverseri)
  univ <- 0
  turnover.annu <- cbind(turnover.annu, univ)
  rtn.feecut <- rtnsummary[1,]-turnover.annu*fee*2
  row.names(rtn.feecut) <- "Annualized Return (fee cut)"
  re <- rbind(rtnsummary, turnover.annu, rtn.feecut)  
  
  # --- spread
  if(rtn_type == "long-short"){
    spreadNM <- "Q1-Qn"
    ncol_target <- ncol(rtnseri) - 1
  }else if(rtn_type == "long-univ"){
    spreadNM <- "Q1-univ"
    ncol_target <- ncol(rtnseri)
  }
  
  spreadseri <- rtnseri[,1]-rtnseri[,ncol_target]
  colnames(spreadseri) <- spreadNM
  rtnsummary.spread <- rtn.summary(spreadseri)
  
  if(rtn_type == "long-short"){
    turnover.annu.spread <- t(sum(turnover.annu[,c(1,ncol_target)])/2)
    rtn.feecut.spread <- rtnsummary.spread[1,]-turnover.annu.spread*fee*2*2   # two side trade and two groups 
  }else if(rtn_type == "long-univ"){
    turnover.annu.spread <- t(turnover.annu[,1])
    rtn.feecut.spread <- rtnsummary.spread[1,]-turnover.annu.spread*fee*2   # two side trade 
  }
  
  rownames(turnover.annu.spread) <- "Annualized Turnover"
  colnames(turnover.annu.spread) <- spreadNM
  rownames(rtn.feecut.spread) <- "Annualized Return (fee cut)"  
  colnames(rtn.feecut.spread) <- spreadNM
  
  re.spread <- rbind(rtnsummary.spread,turnover.annu.spread,rtn.feecut.spread)
  
  # --- cbind
  re <- cbind(re.spread,re)  
  
  # --- extra part
  # beta
  group_beta <- c()
  allrtnseri <- cbind(spreadseri, rtnseri)
  for( i in 1:ncol(allrtnseri)){
    fit_ <- lm(allrtnseri[,i]~allrtnseri[,ncol(allrtnseri)])
    group_beta <- c(group_beta, fit_$coefficients[[2]])
  }
  group_beta <- t(group_beta)
  rownames(group_beta) <- "Beta"  
  colnames(group_beta) <- colnames(re)
  
  # size
  sizeseri <- seri.Ngroup.size(TSFR,N=N,sectorNe=sectorNe,backtestPar=backtestPar)
  group_cap <- t(colMeans(sizeseri))
  group_cap <- cbind(NA, group_cap)
  colnames(group_cap)[1] <- spreadNM
  row.names(group_cap) <- "Size"
  
  # --- output
  re <- rbind(re, group_beta, group_cap)
  
  return(re)
}



#' @rdname backtest.Ngroup
#' @return table.Ngroup.spread return a matrix which giving the statistics of the rtn of top-bottom spread in each year.
#' @export
#' @examples
#' re <- table.Ngroup.spread(TSFR,5,fee=0.002)
#' re2 <- table.Ngroup.spread(TSFR, rtn_type = "long-univ")
table.Ngroup.spread <- function(TSFR,N=5,
                                sectorNe=NULL,
                                fee=0,
                                rtn_type = c("long-short","long-univ"),
                                backtestPar){
  rtn_type <- match.arg(rtn_type)
  
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
    fee <- getbacktestPar.fee(backtestPar,"secu")
  }
  
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,include_univ = TRUE, sectorNe=sectorNe, bysector = NULL, backtestPar=backtestPar)
  turnoverseri <- seri.Ngroup.turnover(TSFR,N=N,sectorNe=sectorNe,backtestPar=backtestPar)
  
  if(rtn_type == "long-short"){
    spreadseri <- rtnseri[,1]-rtnseri[,ncol(rtnseri)-1]    
  }else if(rtn_type == "long-univ"){
    spreadseri <- rtnseri[,1]-rtnseri[,ncol(rtnseri)]
  }
  
  yearlist <- as.character(unique(lubridate::year(TSFR$date)))
  for(ii in 1:length(yearlist)) {
    yy <- yearlist[ii]
    if(NROW(spreadseri[yy])<=1 || NROW(turnoverseri[yy])<=1){
      warning(paste("Only 1 record in year",yy,". Return NULL! "))
      tsub <- NULL
    } else {
      rtnsummary <- rtn.summary(spreadseri[yy])  
      turnover.annu <- Turnover.annualized(turnoverseri[yy])
      if(rtn_type == "long-short"){
        turnover.annu <- t(sum(turnover.annu[,c(1,ncol(turnover.annu))])/2)
        rtn.feecut <- rtnsummary[1,]-turnover.annu*fee*2*2   # two side trade and two groups
      }else if(rtn_type == "long-univ"){
        turnover.annu <- t(turnover.annu[1,1])
        rtn.feecut <- rtnsummary[1,]-turnover.annu*fee*2   # two side trade
      }
      # beta
      fit_ <- lm(spreadseri[yy]~rtnseri[yy,"univ"])
      beta_ <- t(fit_$coefficients[[2]])
      #
      tsub <- rbind(rtnsummary,turnover.annu,rtn.feecut,beta_)
      rownames(tsub)[(nrow(tsub)-2):(nrow(tsub))] <- c("Annualized Turnover","Annualized Return (fee cut)","Beta")
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
chart.Ngroup.overall <- function(TSFR,N=5,
                                 sectorNe=NULL,
                                 bysector=NULL,
                                 plotPar
                                 ){
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
  }  
  if(is.null(bysector)){
    tmptable <- table.Ngroup.overall(TSFR=TSFR,N=N,sectorNe=sectorNe,bysector=NULL)
    rtn.annu <- tmptable[1,2:(N+1)]
    rtn.annu <- data.frame(group=as.integer(substring(names(rtn.annu),2)),rtn.annu=rtn.annu)
    re <- ggplot(rtn.annu,aes(x=group,y=rtn.annu))+
      geom_bar(position="dodge",stat="identity")+
      ggtitle("Annualized return of each group")+
      geom_text(aes(label=paste(round(rtn.annu*100,1),"%",sep="")),vjust=-0.5)+
      scale_y_continuous(labels=scales::percent)
  } else {
    tmptable <- table.Ngroup.overall(TSFR=TSFR,N=N,sectorNe = sectorNe,bysector=bysector)
    tmptable <- cbind(sector=rownames(tmptable),as.data.frame(tmptable))
    tmptable <- reshape2::melt(tmptable, id.var="sector")
    re <- ggplot(tmptable, aes(x=sector,y=variable,fill=value))+ geom_tile() +
      scale_fill_gradient2(low = 'green', high = 'red')
  }
  return(re)
}
#' @rdname backtest.Ngroup
#' @param Nbin the number of the groups the timespan is cut to, when plotting the "date.grp".It could also be character of interval specification,See \code{\link{cut.Date}} for detail. the default value is "day",which means no cutting, The value of every date are ploted.
#' @return chart.Ngroup.seri_point return a ggplot object of "return time series of the groups" with geom_point
#' @export
#' @examples 
#' chart.Ngroup.seri_point(TSFR,5,"3 month")
chart.Ngroup.seri_point <- function(TSFR,N=5,Nbin="day",
                                    sectorNe=NULL,
                                    plotPar){
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
    Nbin <- getplotPar.Ngroup(plotPar,"Nbin")
  }
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,sectorNe=sectorNe)
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
chart.Ngroup.seri_bar <- function(TSFR,N=5,Nbin="day",
                                  sectorNe=NULL,
                                  bysector=NULL,
                                  plotPar
                                  ){
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
    Nbin <- getplotPar.Ngroup(plotPar,"Nbin")
  }  
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,sectorNe=sectorNe,bysector = bysector)
  if(is.null(bysector)){
    rtn_aggr <- aggr.rtn(rtnseri,freq=Nbin)
    rtn_aggr.df <- data.frame(time=time(rtn_aggr),zoo::coredata(rtn_aggr))
    rtn_aggr.melt <- reshape2::melt(rtn_aggr.df,id.vars="time")
    rtn_aggr.melt$group <- as.integer(substring(rtn_aggr.melt$variable,2))
    rtn_aggr.melt$time <- as.character(rtn_aggr.melt$time)
    re <- ggplot(rtn_aggr.melt,aes(x=group,y=value))+
      geom_bar(position="dodge",stat="identity")+
      facet_wrap(~ time, scales="free_y") +
      ggtitle("Return of each group")+
      scale_y_continuous(labels=scales::percent)
  } else {
    rtn_aggr <- plyr::laply(rtnseri,aggr.rtn,freq=Nbin)
    dimnames(rtn_aggr)[[1]] <- names(rtnseri)
    dimnames(rtn_aggr)[[2]] <- as.character(time(aggr.rtn(rtnseri[[1]],freq=Nbin)))
    rtn_aggr.melt <- reshape2::melt(rtn_aggr,varnames =c("sector","time","group"))
    rtn_aggr.melt$time <- as.character(rtn_aggr.melt$time)
    re <- ggplot(rtn_aggr.melt,aes(x=sector,y=group,fill=value))+ geom_tile() +
      scale_fill_gradient2(low = 'green', high = 'red')+
      facet_wrap(~ time, scales="free_y") +
      ggtitle("Return of each group by sector")
  }
  return(re)
}
#' @rdname backtest.Ngroup
#' @return chart.Ngroup.seri_line return a ggplot object of "Cumulated return of each group" with geom_line
#' @export
#' @examples 
#' chart.Ngroup.seri_line(TSFR,5)
chart.Ngroup.seri_line <- function(TSFR,N=5,
                                   include_univ=TRUE,
                                   sectorNe=NULL,
                                   plotPar){
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
  }  
  rtnseri <- seri.Ngroup.rtn(TSFR=TSFR,N=N,include_univ=include_univ,sectorNe=sectorNe)
  indexseri <- WealthIndex(rtnseri)
  re <- ggplot.ts.line(indexseri,main="Wealth index of each group",size=1)
  return(re)
}





chart.Ngroup.box <- function(TSFR,N=5,Nbin="day",
                             sectorNe=NULL,
                             plotPar){
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
    Nbin <- getplotPar.Ngroup(plotPar,"Nbin")
  }  
  
  
  
  
  check.TSFR(TSFR)
  TSFR <- na.omit(TSFR[,c("date_end","stockID","factorscore","periodrtn")])
  # ---- add the rank and groups of the factorscores 
  if(is.null(sectorNe)){
    TSFR <- data.table::data.table(TSFR,key=c("date_end"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by="date_end"]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by="date_end"]
  } else {
    TSFR <- getSectorID(TSFR,sectorAttr=sectorAttr)
    TSFR <- data.table::data.table(TSFR,key=c("date_end","sector"))
    TSFR <- TSFR[,rank:=rank(-factorscore), by=c("date_end","sector")]
    TSFR <- TSFR[,group:=cut(rank,N,labels=FALSE), by=c("date_end","sector")]
  }
  

  rtn.mat <- reshape2::acast(rtn.df,date_end~group,value.var="mean.rtn")
  rtn.xts <- as.xts(rtn.mat,as.Date(rownames(rtn.mat),tz=""))
  colnames(rtn.xts) <- paste("Q",1:N,sep="")   
  result <- rtn.xts
  
  
  
  
  rtnseri <- seri.Ngroup.rtn(TSFR,N=N,sectorNe=sectorNe)
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
  
  
  
  p <- ggplot(data=TSFR,mapping = aes(x=factor(date),y=periodrtn, fill=factor(group)))+geom_boxplot()
  return(re)
  
  
}


#' @rdname backtest.Ngroup
#' @return chart.Ngroup.spread return and print a recordedplot object of "Performance Summary of top-bottom spread" . 
#' @export
#' @examples 
#' chart.Ngroup.spread(TSFR,5)
#' chart.Ngroup.spread(TSFR, rtn_type = "long-univ")
chart.Ngroup.spread <- function(TSFR,N=5,
                                sectorNe=NULL,
                                rtn_type = c("long-short", "long-univ"),
                                plotPar
                                ){
  rtn_type <- match.arg(rtn_type)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
  }  
  rtnseri <- seri.Ngroup.rtn(TSFR=TSFR,N=N,sectorNe=sectorNe,include_univ = TRUE)
  if(rtn_type == "long-short"){
    spreadseri <- rtnseri[,1]-rtnseri[,ncol(rtnseri)-1]
    colnames(spreadseri) <- "spread"
    re <- ggplots.PerformanceSummary(spreadseri,var.cum=list(1),var.dd=list(1),var.bar=list(1),bar.freq="day",main="Performance Summary of top-bottom spread")
  }else if(rtn_type == "long-univ"){
    spreadseri <- rtnseri[,1]-rtnseri[,ncol(rtnseri)]
    colnames(spreadseri) <- "spread"
    re <- ggplots.PerformanceSummary(spreadseri,var.cum=list(1),var.dd=list(1),var.bar=list(1),bar.freq="day",main="Performance Summary of top-univ spread")
  }
}
#' @rdname backtest.Ngroup
#' @param group a integer, indicating the group whose turnover be plotted
#' @return chart.Ngroup.turnover return a ggplot object of "Turnover Rate of each rebalancing point"
#' @export
#' @examples 
#' chart.Ngroup.turnover(TSFR,5)
chart.Ngroup.turnover <- function(TSFR,N=5,group=1,
                                  sectorNe=NULL,
                                  plotPar){
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")  
  }  
  turnoverseri <- seri.Ngroup.turnover(TSFR,N=N,sectorNe=sectorNe)
  turnoverseri <- turnoverseri[,group,drop=FALSE]
  re <- ggplot.ts.bar(turnoverseri,main=paste("Turnover rate of group",group)) +
    theme(legend.position="none")+
    scale_y_continuous(labels=scales::percent)
  return(re)
}




#' @param mTSFR a \bold{mTSFR} object. See \code{\link{getMultiFactor}}.
#' @rdname backtest.Ngroup
#' @export
#' @examples 
#' mTSFR <- getMultiFactor(TSR)
#' MF.chart.Ngroup.spread(mTSFR)
MF.chart.Ngroup.spread <- function(mTSFR,N=5,
                                   sectorNe=NULL,
                                   Nbin="day",
                                   facet_by=c("none","date","fname")){
  fnames <- guess_factorNames(mTSFR)
  TSFRs <- lapply(mTSFR[,fnames],function(x,mTSFR){
    as.data.frame(cbind(mTSFR[,c('date','date_end','stockID')],
                        factorscore=x,periodrtn=mTSFR[,'periodrtn']))
  },mTSFR=mTSFR)
  
  facet_by <- match.arg(facet_by)
  
  rtnseri <- plyr::llply(TSFRs,seri.Ngroup.rtn,N=N,sectorNe=sectorNe)
  rtnseri <- lapply(rtnseri,function(ts){
    rtn <- ts[,1]-ts[,ncol(ts)]
    wealth <- WealthIndex(rtn)
    spread <- data.frame(date=zoo::index(wealth),zoo::coredata(rtn),zoo::coredata(wealth))
    colnames(spread) <- c('date','rtn','wealth')
    return(spread)})
  rtnseri <- dplyr::bind_rows(rtnseri,.id = 'fname')
  
  if(facet_by=='none'){
    ggplot(rtnseri, aes(x=date, y=wealth, color=fname)) +
      geom_line(size=1) +
      coord_trans(y="log")
  }else if(facet_by=='date'){
    
    rtnseri$date <- cut.Date2(rtnseri$date,Nbin)
    rtnseri <- rtnseri %>% dplyr::group_by(fname,date) %>% dplyr::summarise(rtn=prod(1+rtn)-1) %>%
      dplyr::ungroup() %>% dplyr::mutate(date=as.Date(date))
    
    ggplot(rtnseri, aes(x=fname, y=rtn,fill=fname)) +
      geom_bar(stat = 'identity')+facet_wrap(~date)
  }else if(facet_by=='fname'){
    ggplot(rtnseri, aes(x=date, y=wealth)) +
      geom_line(size=1)+facet_wrap(~fname)+
      coord_trans(y="log")
  }
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
MC.table.Ngroup.overall <- function(TSFRs,N=5,
                                    sectorNe=NULL,
                                    fee=0,
                                    backtestPar){
  check.name_exist(TSFRs)
  if(!missing(backtestPar)){
    N <- getbacktestPar.Ngroup(backtestPar,"N")
    fee <- getbacktestPar.fee(backtestPar,"secu")
    sectorNe <- getbacktestPar.Ngroup(backtestPar,"sectorNe")
  } 
  overall.table <- plyr::laply(TSFRs,function(x) {table.Ngroup.overall(TSFR=x,N=N,fee=fee,sectorNe=sectorNe)[ , 1, drop=FALSE]})
  NMs <- names(TSFRs)
  rownames(overall.table) <- NMs
  return(overall.table)
}


#' @param ncol a integer, specificate the number of cols of the multi-charts.
#' @rdname backtest.Ngroup
#' @export
#' @examples 
#' MC.chart.Ngroup.overall(TSFRs)
MC.chart.Ngroup.overall <- function(TSFRs,N=5,
                                    sectorNe=NULL,
                                    bysector=NULL,
                                    ncol=3,plotPar
                                    ){
  check.name_exist(TSFRs)
  if(!missing(plotPar)){
    N <- getplotPar.Ngroup(plotPar,"N")
    sectorNe <- getplotPar.Ngroup(plotPar,"sectorNe")
    ncol <- getplotPar.MC(plotPar,"ncol.Ngroup")
  } 
  NMs <- names(TSFRs)
  Ngroup.charts <- mapply(function(x,nm){
    chart.Ngroup.overall(x,N=N,sectorNe=sectorNe,bysector=bysector)+  
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
  if(!is.null(attr(rtn,"turnover_L"))){
    turnover_L <- Turnover.annualized(attr(rtn,"turnover_L"))[,"avg"]
    if(!is.null(attr(rtn,"turnover_S"))){
      turnover_S <- Turnover.annualized(attr(rtn,"turnover_S"))[,"avg"]
    } else {
      turnover_S <- NA
    }
    turnover <- matrix(c(turnover_L,turnover_S,NA),nrow = 1)
    rownames(turnover) <- "Annualized Turnover"
    summary <- rbind(summary,turnover)
  }
  
  # ---- summary.yearly:summary of the yearly 'hedged' rtn
  summary.yearly <- t(xts::apply.yearly(rtn$hedge,rtn.summary,hitFreq=hitFreq))
  colnames(summary.yearly) <- lubridate::year(colnames(summary.yearly))
  if(!is.null(attr(rtn,"turnover_L"))){
    turnover.yearly <- t(xts::apply.yearly(attr(rtn,"turnover_L"),Turnover.annualized)[,"avg"])
    if(!is.null(attr(rtn,"turnover_S"))){
      turnover.yearly_S <- t(xts::apply.yearly(attr(rtn,"turnover_S"),Turnover.annualized)[,"avg"])
      turnover.yearly <- (turnover.yearly+turnover.yearly_S)/2
    }
    colnames(turnover.yearly) <- lubridate::year(colnames(turnover.yearly))
    summary.yearly <- plyr::rbind.fill.matrix(summary.yearly,turnover.yearly)
  }
  rownames(summary.yearly) <- rownames(summary)  
  return(list(summary=summary,
              summary.yearly=summary.yearly,
              period.stats=period.stats,
              hedge.stats=hedge.stats,              
              DD.stats=DD.stats))  
}




#' tables.PB
#' 
#' @param PB a PB object or a one colume rtn series.
#' @param hitFreq
#' @return a list containing some tables which giving the summary result of the PB.
#' @seealso \code{\link{tables.longshort}}
#' @export
tables.PB <- function(PB, hitFreq="month"){
  rtn <- PB
  # ---- rtn.aggr: aggreated return series by different freq, each being an item of a list.(note that 'rtn.aggr$day' is equal to 'rtn') 
  freq <- c("day","week","month","quarter","year")  
  rtn.aggr <- lapply(freq,function(freq){aggr.rtn(rtn,freq)})
  names(rtn.aggr) <- paste(freq,"ly",sep="")
  
  # ---- rtn.stats: main statisticals of the rtn,by different freq
  rtn.stats <- t(plyr::laply(rtn.aggr,function(x){rtn.stats(x)}))
  colnames(rtn.stats) <- paste(freq,"ly",sep="")  
  
  # ---- period.stats: table showing the yearly,all-span and annualized return
  period.stats <- rtn.periods(rtn)
  
  # ---- DD.stats:table showing statistics for the worst drawdowns.
  DD.stats <- PerformanceAnalytics::table.Drawdowns(rtn)  
  
  # ---- summary:summary of the all over rtn
  summary <- rtn.summary(rtn,hitFreq=hitFreq)
  if(!is.null(attr(rtn,"turnover"))){
    turnover <- Turnover.annualized(attr(rtn,"turnover"))[,"avg"]
    turnover <- matrix(c(turnover),nrow = 1)
    rownames(turnover) <- "Annualized Turnover"
    summary <- rbind(summary,turnover)
  }
  
  # ---- summary.yearly:summary of the yearly rtn
  summary.yearly <- t(xts::apply.yearly(rtn,rtn.summary,hitFreq=hitFreq))
  colnames(summary.yearly) <- lubridate::year(colnames(summary.yearly))
  if(!is.null(attr(rtn,"turnover"))){
    turnover.yearly <- t(xts::apply.yearly(attr(rtn,"turnover"),Turnover.annualized)[,"avg"])
    colnames(turnover.yearly) <- lubridate::year(colnames(turnover.yearly))
    summary.yearly <- plyr::rbind.fill.matrix(summary.yearly,turnover.yearly)
  }
  rownames(summary.yearly) <- rownames(summary)  
  return(list(summary=summary,
              summary.yearly=summary.yearly,
              period.stats=period.stats,
              rtn.stats=rtn.stats,              
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


# table.turnover <- function(PB){
#   
# }
# chart.turnover <- function(PB){
#   
# }






# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  others ------------------------
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
#' @param reg_results See \code{\link{reg.TSFR}}.
#' @return a factor weight vector
#' @export
#' @importFrom PortfolioAnalytics set.portfolio.moments
#' @examples
#' mp <- modelPar.default()
#' factorIDs <- c("F000001","F000004","F000005","F000008")
#' FactorLists <- buildFactorLists_lcfs(factorIDs)
#' mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
#' TSR <- Model.TSR(mp)
#' TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)
#' MC.wgt.CAPM(TSFRs)
#' MC.wgt.CAPM(TSFRs,wgtmin=0.05,wgtmax=0.4,targetType='risk')  
#' MC.wgt.CAPM(TSFRs,wgtmin=0.05,wgtmax=0.4,targetType='balance',riskaversion = 10) 
#' -----------------------------------------------------------------------------
#' MC.wgt.CAPM(reg_results=reg_results) 
#' MC.wgt.CAPM(wgtmin=0.05,wgtmax=0.4,targetType='balance',reg_results=reg_results) 
MC.wgt.CAPM <- function (TSFRs,stat=c("pearson","spearman"),backtestPar,
                         wgtmin=0, wgtmax=0.5,
                         targetType=c('sharpe','return','risk','balance'),
                         riskaversion=1,
                         reg_results) {
  targetType <- match.arg(targetType)
  if(missing(reg_results)){
    check.name_exist(TSFRs)
    stat <- match.arg(stat)
    targetType <- match.arg(targetType)
    if(!missing(backtestPar)){
      stat <- getbacktestPar.IC(backtestPar,"stat")
    } 
    IC.seris <- plyr::laply(TSFRs, seri.IC, stat=stat)
    rownames(IC.seris) <- names(TSFRs)
    IC.seris <- t(IC.seris)
    seris <- xts::xts(IC.seris,order.by = unique(TSFRs[[1]]$date_end)[1:nrow(IC.seris)])
  }else{
    rtn.seris <- reg_results$fRtn
    rtn.seris <- reshape2::dcast(rtn.seris,date~fname,value.var = 'frtn')
    seris <- xts::xts(rtn.seris[,-1],order.by = rtn.seris[,1])
  }
  
  require(ROI)
  factor.names <- colnames(seris)
  pspec <- PortfolioAnalytics::portfolio.spec(assets=factor.names)
  pspec <- PortfolioAnalytics::add.constraint(portfolio=pspec, type="full_investment")
  pspec <- PortfolioAnalytics::add.constraint(portfolio=pspec, type="box", min=wgtmin, max=wgtmax)
  if(targetType=='return'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec,type='return',name='mean')
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=seris, portfolio=pspec,optimize_method="ROI",trace=TRUE)
  }else if(targetType=='risk'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec,type='risk',name='var')
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=seris, portfolio=pspec,optimize_method="ROI",trace=TRUE)
  }else if(targetType=='balance'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="return", name="mean")
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="risk", name="var", risk_aversion=riskaversion)
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=seris, portfolio=pspec,optimize_method="ROI",trace=TRUE)
  }else if(targetType=='sharpe'){
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="return", name="mean")
    pspec <- PortfolioAnalytics::add.objective(portfolio=pspec, type="risk", name="StdDev")
    opt_ps <- PortfolioAnalytics::optimize.portfolio(R=seris, portfolio=pspec,optimize_method="ROI",maxSR=TRUE,trace=TRUE)
  }
  
  return(opt_ps$weights)
}



#' summary of factor-refine-methods comparing
#' 
#' @param rawTSF The TSF which contains the raw factorscore.
#' @param refinePar_lists A list of (refinePar)s, each refinePar is a list built by refinePar_default.
#' @param refinePar_names The character vector of names, could be missing.
#' @param result_type Currently supports 3 possible results : chart, table, data
#' @param group_N The argument passed into Ngroup.overall, etc.
#' @author Han.Qian
#' @export
#' @examples 
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' refinePar_lists <- list(refinePar_default(type = "none"),
#'                         refinePar_default(type = "reg"),
#'                         refinePar_default(type = "old_robust"),
#'                         refinePar_default(type = "robust"))
#' rawTSF <- gf.NP_YOY(TS, src = "fin")
#' summary.factor_refine(rawTSF, refinePar_lists)
summary.factor_refine <- function(rawTSF, refinePar_lists, refinePar_names, result_type = c("chart","table","data"), group_N = 5){
  
  # ARGUMENTS CHECKING
  result_type <- match.arg(result_type)
  # ORGANIZE TSFs
  core_mTSF <- factor_refine_MF(TSF = rawTSF,refinePar_lists = refinePar_lists,refinePar_names = refinePar_names)
  # GET RETURN
  core_mTSFR <- getTSR(core_mTSF)
  
  ### OUTPUT
  # CHART/TABLE
  if(result_type == "chart"){
    return(MF.chart.Ngroup.spread(mTSFR = core_mTSFR, N = group_N))
    # MF.chart.IC(core_mTSFR)
    # MC.chart.Ngroup.overall(mTSF2TSFs(core_mTSFR), N = group_N)
    # MC.chart.IC(mTSF2TSFs(core_mTSFR))
  }else if(result_type == "table"){
    return(MC.table.Ngroup.overall(mTSF2TSFs(core_mTSFR), N = group_N))
    # MC.table.IC(mTSF2TSFs(core_mTSFR))
  }else if(result_type == "data"){
    return(core_mTSFR)
  }
  # END
}
