## ----setup,echo=FALSE,include=FALSE--------------------------------------
library(RFactorModel)
library(knitr)
knitr::opts_chunk$set(
  eval = FALSE,
  warning = FALSE
)

## ----local_factorLists,eval=TRUE,echo=FALSE------------------------------
ftbale <- CT_FactorLists()
ftbale <- dplyr::arrange(ftbale,factorID)
kable(ftbale)

## ----getfactorlist-------------------------------------------------------
#  # get recommended factorlists last year based on regression
#  begT <- as.Date('2016-12-31')
#  endT <- as.Date('2017-08-31')
#  indexID <- 'EI000985'
#  re <- reg.factorlists_recommend(indexID,begT,endT,forder = c('ln_mkt_cap_','PB_mrq_','pct_chg_per_60_','liquidity_'))
#  FactorLists <- re$FactorLists
#  # recommend result
#  kable(re$result)

## ----showfactorstat------------------------------------------------------
#  begT <- as.Date('2014-12-31')
#  endT <- as.Date('2017-08-31')
#  RebDates <- getRebDates(begT,endT)
#  TS <- getTS(RebDates,indexID)
#  
#  # remove high correlated or large proportion of missing factors
#  dropf <- c('float_cap_')
#  FactorLists <- FactorLists[sapply(FactorLists,function(x) !(x$factorName %in% dropf))]
#  
#  TSF <- na.omit(getMultiFactor(TS,FactorLists))
#  
#  MF.chart.Fct_box(TSF)
#  MF.chart.Fct_corr(TSF)
#  MF.chart.Fct_density(TSF)
#  
#  
#  #dealing with factor correlation
#  TSF <- factor_orthogon_single(TSF,y='ILLIQ',x = 'ln_mkt_cap_',sectorAttr = NULL)
#  TSF <- factor_orthogon_single(TSF,y='volatility_',x = 'liquidity_',sectorAttr = NULL)
#  TSFR <- na.omit(getTSR(TSF))
#  

## ----getlists_buildlocaltables-------------------------------------------
#  # build local database's regression tables
#  system.time(lcdb.build.RegTables(FactorLists=FactorLists))
#  
#  # update local database's regression tables
#  system.time(lcdb.update.RegTables(FactorLists = FactorLists))

## ----reg_result----------------------------------------------------------
#  # get regression result
#  reg_results <- reg.TSFR(TSFR)
#  
#  ## show regression result
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
#  RebDates <- getRebDates(as.Date('2015-01-31'),as.Date('2017-07-31'))
#  TS <- getTS(RebDates,indexID = 'EI000906')
#  tmp <- buildFactorLists(buildFactorList(factorFun = 'gf.NP_YOY',factorDir = 1,factorRefine = refinePar_default("robust")))
#  factorIDs <- c("F000006","F000008","F000013")
#  FactorLists <- buildFactorLists_lcfs(factorIDs,factorRefine = refinePar_default("robust"))
#  FactorLists <- c(tmp,FactorLists)
#  TSF <- getMultiFactor(TS,FactorLists)
#  TSFR <- getTSR(TSF)
#  re <- reg.TSFR(TSFR)
#  rtn_cov_delta <- f_rtn_cov_delta(reg_results=re)
#  fRtn <- rtn_cov_delta$fRtn
#  fCov <- rtn_cov_delta$fCov
#  constr <- constr_default(box_each = c(0,0.01))
#  constr <- addConstr_box(constr,ES33480000 = c(0,0.05),ES33490000 = c(0,0.03))
#  constr <- addConstr_group(constr,EI000300=c(0.6,0.8))
#  constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
#  conslist <- buildFactorLists_lcfs("F000002",factorRefine = refinePar_default("robust"))
#  constr <- addConstr_fctExp_style(constr,conslist,-0.1,0.1,relative = 0)
#  
#  #max return
#  port_opt4 <- getPort_opt(TSF,fRtn = fRtn,bmk="EI000300",constr = constr)
#  
#  #max return minus risk
#  obj <- object_default()
#  obj <- addObj_risk(obj)
#  port_opt5 <- getPort_opt(TSF,fRtn = fRtn,fCov=fCov,bmk="EI000300",constr = constr,obj = obj)
#  
#  # add turnover constraint
#  constr <- addConstr_turnover(constr)
#  port_opt6 <- getPort_opt(TSF,fRtn = fRtn,fCov=fCov,bmk="EI000300",constr = constr,obj = obj)
#  
#  # add trackingerror constraint
#  constr <- clearConstr(constr,'turnover')
#  constr <- addConstr_trackingerror(constr)
#  delta <- rtn_cov_delta$Delta
#  port_opt7 <- getPort_opt(TSF,fRtn = fRtn,fCov=fCov,bmk="EI000300",constr = constr,obj = obj,delta=delta)
#  
#  
#  
#  
#  # port backtest and return summary
#  rtn1 <- port.backtest(port_opt4,fee.buy = 0.001)
#  rtn2 <- port.backtest(port_opt5,fee.buy = 0.001)
#  rtn3 <- port.backtest(port_opt6,fee.buy = 0.001)
#  rtn4 <- port.backtest(port_opt7,fee.buy = 0.001)
#  bmk <- getrtn.bmk(rtn1,bmk = 'EI000300')
#  rtn1 <- addrtn.hedge(rtn1,bmk)
#  rtn2 <- addrtn.hedge(rtn2,bmk)
#  rtn3 <- addrtn.hedge(rtn3,bmk)
#  rtn4 <- addrtn.hedge(rtn4,bmk)
#  allrtn <- merge(rtn1[,'hedge'],rtn2[,'hedge'])
#  allrtn <- merge(allrtn,rtn3[,'hedge'])
#  allrtn <- merge(allrtn,rtn4[,'hedge'])
#  ggplot.WealthIndex(allrtn)
#  rtn.summary(allrtn)
#  rtn.periods(allrtn)
#  

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

