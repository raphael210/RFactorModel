# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  build and update local db -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
factorIDs <- c("F000006","F000008","F000014","F000016","F000017","F000018")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
FactorLists <- buildFactorLists(
  buildFactorList(factorFun="gf.ln_mkt_cap",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="gf.NP_YOY",
                  factorPar=list(),
                  factorDir=1),
  factorStd="norm",factorNA = "median")
FactorLists <- c(tmp,FactorLists)

system.time(lcdb.build.RegTables(FactorLists=FactorLists))
#system.time(lcdb.update.RegTables(as.Date('2011-06-01'),endT=as.Date('2013-01-01'),FactorLists))



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  optimization demo -----------------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
begT <- as.Date('2009-01-31')
endT <- Sys.Date()-1
RebDates <- getRebDates(begT,endT)
indexID <- 'EI000985'
TS <- getTS(RebDates,indexID)

factorIDs <- c("F000006","F000014","F000015","F000016","F000017")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "mean",factorOutlier = 0.01)
FactorLists <- buildFactorLists(
  buildFactorList(factorFun="gf.ln_mkt_cap",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="gf.NP_YOY",
                  factorPar=list(),
                  factorDir=1),
  factorStd="norm",factorNA = "mean",factorOutlier = 0.01)
FactorLists <- c(tmp,FactorLists)
TSF <- getMultiFactor(TS,FactorLists = FactorLists)

TSFR <- getTSR(TSF)
reg_results <- reg.TSFR(TSFR)

#show regression result
MC.chart.fCorr(TSF,Nbin='year')
chart.reg.rsquare(reg_results)
table.reg.rsquare(reg_results)

MC.chart.regCorr(reg_results)
chart.reg.fRtnWealthIndex(reg_results,facet = T)
table.reg.fRtn(reg_results)
chart.reg.fRtnBar(reg_results)

#get factor return
fNames <- sapply(FactorLists, '[[','factorName')
fexp <- data.frame(fname=fNames,
                   low=c(-0.01,-0.01,-0.01,-0.01,-0.01,-0.01,-0.01),
                   up=c(1,0.01,100,100,100,2,100))

alphaf <- c("disposition_","beta_","IVR_","ln_mkt_cap_","NP_YOY" )

system.time(fRtn <- getfRtn(RebDates,alphaf,rollavg = T,reg_results = reg_results))
system.time(fCov <- calfCov(RebDates,alphaf,rollavg=T,reg_results = reg_results))


system.time(port_ind <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='Ind'))
system.time(port_indsty <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='IndSty',fexp=fexp))

system.time(port_ind_NE <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='Ind',addEvent = F))
system.time(port_indsty_NE <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='IndSty',fexp=fexp,addEvent = F))

TSF <- TSF[TSF$date>min(RebDates),]
system.time(port_ind <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return-risk',constr='Ind'))
system.time(port_indsty <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return-risk',constr='IndSty',fexp=fexp))

newport1 <- port_ind[port_ind$date== max(RebDates),]
newport2 <- port_indsty[port_indsty$date== max(RebDates),]

port4 <- port_ind[port_ind$date!= max(RebDates),]
port4 <- port_indsty[port_indsty$date!= max(RebDates),]
portrtn4 <- port.backtest(port4,fee.buy = 0.001)
benchrtn4 <- getrtn.bmk(portrtn4, bmk = "EI000905")
allrtn4 <- addrtn.hedge(portrtn4,benchrtn4)
ggplot.WealthIndex(allrtn4)
rtn.summary(allrtn4)
rtn.periods(allrtn4)

allrtn4 <- getrtn.LBH(portrtn4,bmk = "EI000905")
allrtn2 <- getrtn.LBH(re2,bmk = "EI000905")
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  traditional way demo -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
RebDates <- getRebDates(as.Date('2010-01-31'),as.Date('2016-10-31'))
TS <- getTS(RebDates,indexID = 'EI000905')
factorIDs <- c("F000006","F000008","F000013","F000015")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "mean")
FactorLists <- buildFactorLists(
  buildFactorList(factorFun="gf.NP_YOY",
                  factorPar=list(),
                  factorDir=1),
  factorStd="norm",factorNA = "median")
FactorLists <- c(tmp,FactorLists)
tmp <- length(FactorLists)
TSF2 <- getMultiFactor(TS,FactorLists = FactorLists,wgts = rep(1/tmp,tmp))
port2 <- getPort(TSF2,topN = 100,pick.sectorNe = T)
port2 <- port2[,c('date','stockID')]
port2 <- addwgt2port(port2, wgt.sectorNe = T, wgtbmk = "EI000905")
re2 <- port.backtest(port2,fee.buy = 0.001)
benchrtn2 <- getrtn.bmk(re2, bmk = "EI000905")
allrtn2 <- addrtn.hedge(re2,benchrtn2)
ggplot.WealthIndex(allrtn2)
rtn.summary(allrtn2)
rtn.periods(allrtn2)








