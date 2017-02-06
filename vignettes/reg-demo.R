## ----setup,echo=FALSE,include=FALSE--------------------------------------
library(RFactorModel)
library(knitr)
knitr::opts_chunk$set(
  eval = FALSE,
  warning = FALSE
)

## ----eval=TRUE,echo=FALSE------------------------------------------------
ftbale <- CT_FactorLists()
ftbale <- dplyr::arrange(ftbale,factorID)
kable(ftbale)

## ----getlists_buildlocaltables-------------------------------------------
#  # get recommended factorlists
#  FactorLists <- reg.factorlists.recommend(indexID = 'EI000985')
#  
#  # build local database's regression tables
#  system.time(lcdb.build.RegTables(FactorLists=FactorLists))
#  
#  # update local database's regression tables
#  system.time(lcdb.update.RegTables(FactorLists = FactorLists))

## ----reg_result----------------------------------------------------------
#  # parameter setting
#  begT <- as.Date('2012-01-31')
#  endT <- as.Date('2016-12-31')
#  RebDates <- getRebDates(begT,endT)
#  indexID <- 'EI000985'
#  
#  # get TS,TSF,TSFR
#  TS <- getTS(RebDates,indexID)
#  FactorLists <- reg.factorlists.recommend(indexID)
#  TSF <- getMultiFactor(TS,FactorLists = FactorLists)
#  TSFR <- getTSR(TSF)
#  
#  # get regression result
#  reg_results <- reg.TSFR(TSFR)
#  
#  ## show regression result
#  # factor correlation plot
#  MC.chart.fCorr(TSF,Nbin='year')
#  # regression's rsquare plot
#  chart.reg.rsquare(reg_results)
#  # regression's rsquare table
#  table.reg.rsquare(reg_results)
#  # correlation plot of factor's return
#  MC.chart.regCorr(reg_results)
#  # pure factor's wealth index
#  chart.reg.fRtnWealthIndex(reg_results,facet = T)
#  # barplot of pure factor's return
#  chart.reg.fRtnBar(reg_results)
#  # summarise table of pure factor's return
#  table.reg.fRtn(reg_results)

## ----portOpt-------------------------------------------------------------
#  # set factor's exposure
#  fNames <- sapply(FactorLists, '[[','factorName')
#  fexp <- data.frame(fname=fNames,
#                     low=c(-0.01,-0.01,-0.01,-0.01,-0.01,-0.01),
#                     up=c(     1, 0.01,  100,  100,  2,    100))
#  # set alpha factor
#  alphaf <- c("disposition_","beta_","ln_mkt_cap_","NP_YOY" )
#  
#  # get factor return
#  fRtn <- getfRtn(RebDates,alphaf,rollavg = T,reg_results = reg_results)
#  # get factor covariance
#  fCov <- calfCov(RebDates,alphaf,rollavg=T,reg_results = reg_results)
#  
#  # Date Alignment
#  tmp.date1 <- max(min(fRtn$date),min(fCov$date))
#  tmp.date2 <- min(max(fRtn$date),max(fCov$date))
#  fRtn <- subset(fRtn,date>=tmp.date1,date<=tmp.date2)
#  fCov <- subset(fCov,date>=tmp.date1,date<=tmp.date2)
#  TSF <- subset(TSF,date>=tmp.date1,date<=tmp.date2)
#  
#  ## portfolio demo
#  # industry neutral,maximize return
#  system.time(port_opt <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='Ind'))
#  # industry and style neutral,maximize return
#  system.time(port_opt <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='IndSty',fexp=fexp,addEvent = F))
#  # industry and style neutral,risk return balance
#  system.time(port_opt <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return-risk',constr='IndSty',fexp=fexp))
#  
#  
#  # port backtest and return summary
#  portrtn <- port.backtest(port_opt,fee.buy = 0.001)
#  benchrtn <- getrtn.bmk(portrtn, bmk = "EI000905")
#  allrtn <- addrtn.hedge(portrtn,benchrtn)
#  ggplot.WealthIndex(allrtn)
#  rtn.summary(allrtn)
#  rtn.periods(allrtn)

## ----ICmethods-----------------------------------------------------------
#  #set modelPar
#  modelPar <- modelPar.default()
#  modelPar <- setmodelPar.time(modelPar,begT,endT)
#  modelPar <- setmodelPar.univ(modelPar,'EI000905')
#  
#  #get factor wgt
#  factorIDs <- c("F000006","F000008","F000013","F000017")
#  tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "mean")
#  FactorLists <- buildFactorLists(
#    buildFactorList(factorFun="gf.NP_YOY",
#                    factorPar=list(),
#                    factorDir=1),
#      buildFactorList(factorFun="gf.ln_mkt_cap",
#                    factorPar=list(),
#                    factorDir=1),
#    factorStd="norm",factorNA = "median")
#  FactorLists <- c(tmp,FactorLists)
#  MPs <- getMPs_FactorLists(FactorLists, modelPar)
#  TSFRs <- Model.TSFRs(MPs)
#  wgt <- MC.wgt.CAPM(TSFRs,wgtmin = 0.05,wgtmax = 0.3)
#  
#  #get port and backtest
#  TS <- Model.TS(modelPar)
#  TSF2 <- getMultiFactor(TS,FactorLists = FactorLists,wgts = wgt)
#  port2 <- getPort(TSF2,topN = 100,pick.sectorNe = T)
#  port2 <- port2[,c('date','stockID')]
#  port2 <- addwgt2port(port2, wgt.sectorNe = T, wgtbmk = "EI000905")
#  re2 <- port.backtest(port2,fee.buy = 0.001)
#  benchrtn2 <- getrtn.bmk(re2, bmk = "EI000905")
#  allrtn2 <- addrtn.hedge(re2,benchrtn2)
#  ggplot.WealthIndex(allrtn2)
#  rtn.summary(allrtn2)
#  rtn.periods(allrtn2)

