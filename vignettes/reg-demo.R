## ----setup,echo=FALSE,include=FALSE--------------------------------------
library(quantbox)
library(knitr)
knitr::opts_chunk$set(
  eval = FALSE,
  warning = FALSE
)

## ----local_factorLists,echo=TRUE-----------------------------------------
#  ftable <- CT_GroupFactorLists()
#  ftable <- sample_n(ftable[,c("factorType","groupDesc","factorFun","factorDesc","wgt")],10)
#  kable(ftable,row.names = FALSE)

## ----getfactor,warning=FALSE,echo=TRUE-----------------------------------
#  begT <- as.Date('2010-12-31')
#  endT <- as.Date('2017-08-31')
#  RebDates <- getRebDates(begT,endT,rebFreq = 'month')
#  indexID <- 'EI000985'
#  TS <- getTS(RebDates,indexID)
#  TSF <- gf.F_target_rtn(TS,con_type = "1,2")
#  TSF <- transform(TSF,stock_code=NULL)
#  TSFR <- getTSR(TSF)

## ----factorshow,warning=FALSE,echo=TRUE----------------------------------
#  kable(sample_n(TSF,15),row.names = FALSE)

## ----factorNAshow,warning=FALSE,echo=TRUE--------------------------------
#  chart.Fct_NA(TSF)

## ----fboxplotshow,warning=FALSE,echo=TRUE--------------------------------
#  TSF_sample <- TSF[TSF$date %in% sample(RebDates,5),]
#  chart.Fct_hist(TSF_sample)

## ----ficshow,warning=FALSE,echo=TRUE-------------------------------------
#  chart.IC(TSFR)

## ----fngroupoverallshow,warning=FALSE,echo=TRUE--------------------------
#  chart.Ngroup.overall(TSFR)

## ----fngroupspreadshow,warning=FALSE,echo=TRUE---------------------------
#  chart.Ngroup.spread(TSFR)

## ----mfactorstat,echo=TRUE-----------------------------------------------
#  factorIDs <- c("F000010","F000011","F000008","F000015","F000013","F000004","F000009")
#  FactorLists <- buildFactorLists_lcfs(factorIDs,factorRefine = refinePar_default('scale'))
#  mTSF <- getMultiFactor(TS,FactorLists)
#  mTSFR <- getTSR(mTSF)
#  TSFRs <- mTSF2TSFs(mTSFR)

## ----mfic,echo=TRUE------------------------------------------------------
#  MF.chart.IC(mTSFR,facet_by = 'fname')

## ----mfngroup,echo=TRUE--------------------------------------------------
#  MF.chart.Ngroup.spread(mTSFR)

## ----mfcombine,echo=TRUE-------------------------------------------------
#  #maximize IC' sharpe
#  MC.wgt.CAPM(TSFRs)
#  #minimize IC' volatility
#  MC.wgt.CAPM(TSFRs,targetType = 'risk')
#  #control single factor's exposure in combined factor
#  MC.wgt.CAPM(TSFRs,wgtmin = 0.05,wgtmax = 0.5)
#  
#  #get group factor
#  forTSF <- gf.FORECAST(TS)
#  forTSFR <- getTSR(forTSF)

## ----mfcombineIC,echo=TRUE-----------------------------------------------
#  chart.IC(forTSFR)

## ----mfcombinengroup,echo=TRUE-------------------------------------------
#  chart.Ngroup.overall(forTSFR)

## ----mfcombinespread,echo=TRUE-------------------------------------------
#  chart.Ngroup.spread(forTSFR)

## ----getlists_buildlocaltables-------------------------------------------
#  MF.chart.Fct_box(mTSF)
#  MF.chart.Fct_corr(mTSF)
#  MF.chart.Fct_density(TSF)
#  
#  
#  #dealing with factor correlation
#  TSF <- factor_orthogon_single(TSF,y='ILLIQ',x = 'ln_mkt_cap_',sectorAttr = NULL)
#  TSF <- factor_orthogon_single(TSF,y='volatility_',x = 'liquidity_',sectorAttr = NULL)
#  TSFR <- na.omit(getTSR(TSF))
#  
#  
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
#  RebDates <- getRebDates(as.Date('2014-01-31'),as.Date('2017-07-31'))
#  TS <- getTS(RebDates,indexID = 'EI000985')
#  tmp <- buildFactorLists(buildFactorList(factorFun = 'gf.NP_YOY',factorDir = 1,factorRefine = refinePar_default("scale")))
#  factorIDs <- c("F000006","F000008","F000013")
#  FactorLists <- buildFactorLists_lcfs(factorIDs,factorRefine = refinePar_default("scale"))
#  FactorLists <- c(tmp,FactorLists)
#  TSF <- getMultiFactor(TS,FactorLists)
#  TSFR <- getTSR(TSF)
#  re <- reg.TSFR(TSFR)
#  rtn_cov_delta <- f_rtn_cov_delta(reg_results=re)
#  fRtn <- rtn_cov_delta$fRtn
#  fCov <- rtn_cov_delta$fCov
#  constr <- constr_default(box_each = c(0,0.01))
#  constr <- addConstr_box(constr,ES33480000 = c(0,0.05),ES33490000 = c(0,0.03))
#  constr <- addConstr_group(constr,EI000300=c(0.8,0.95))
#  constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
#  conslist <- buildFactorLists_lcfs("F000002",factorRefine = refinePar_default("scale"))
#  constr <- addConstr_fctExp_style(constr,conslist,-0.1,1)
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
#  constr <- addConstr_turnover(constr,turnover_target = 0.25)
#  port_opt6 <- getPort_opt(TSF,fRtn = fRtn,fCov=fCov,bmk="EI000300",constr = constr,obj = obj)
#  
#  # add trackingerror constraint
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
#  tmp <- buildFactorLists_lcfs(factorIDs)
#  FactorLists <- buildFactorLists(
#    buildFactorList(factorFun="gf.NP_YOY",
#                    factorPar=list(),
#                    factorDir=1),
#      buildFactorList(factorFun="gf.ln_mkt_cap",
#                    factorPar=list(),
#                    factorDir=1))
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
#  allrtn2sum <- tables.longshort(allrtn2)
#  ggplot.WealthIndex(allrtn2)
#  rtn.summary(allrtn2)
#  rtn.periods(allrtn2)

