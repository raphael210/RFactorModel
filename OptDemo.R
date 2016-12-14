# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  build and update local db -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
factorIDs <- c("F000002","F000006","F000008","F000012","F000013","F000014","F000015")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
FactorLists <- buildFactorLists(
  buildFactorList(factorFun="gf.NP_YOY",
                  factorPar=list(),
                  factorDir=1),
  factorStd="norm",factorNA = "median")
FactorLists <- c(tmp,FactorLists)

system.time(build.lcdb.RegTables(FactorLists))
#system.time(update.lcdb.RegTables(FactorLists))



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  optimization demo -------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
RebDates <- getRebDates(as.Date('2010-01-31'),as.Date('2016-10-31'))
TS <- getTS(RebDates,indexID = 'EI000985')

factorIDs <- c("F000002","F000006","F000008","F000012","F000013","F000014","F000015")
tmp <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "mean")
FactorLists <- buildFactorLists(
  buildFactorList(factorFun="gf.NP_YOY",
                  factorPar=list(),
                  factorDir=1),
  buildFactorList(factorFun="gf.ILLIQ",
                  factorPar=list(),
                  factorDir=1),
  buildFactorList(factorFun="gf.disposition",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="gf.volatility",
                  factorPar=list(),
                  factorDir=-1),
  factorStd="norm",factorNA = "mean")
FactorLists <- c(tmp,FactorLists)
TSF <- getMultiFactor(TS,FactorLists = FactorLists)

TSFR <- getTSR(TSF)


reg_results <- reg.TSFR(TSFR)

#get factor return
fNames <- sapply(FactorLists, '[[','factorName')
fexp <- data.frame(fname=fNames,
                   low=c(-0.01,-0.01,-0.01,-0.01,-0.01,-0.01,-0.01,-0.01),
                   up=c(0.01,100,100,0.01,100,0.01,100,100))

alphaf <- c("PB_mrq_","pct_chg_per_60_","liquidity","IVR","NP_YOY")
TF <- data.frame(date=rep(RebDates,each=length(alphaf)),
                  fname=rep(alphaf,length(RebDates)))


dure <- months(1)
system.time(fRtn <- getfRtn(TF,dure,rollavg=F))
system.time(fCov <- calfCov(TF,dure,rollavg=F))

fRtn <- fRtn[1:length(alphaf),c('fname','frtn')]
fRtn$frtn <- 0.2

system.time(port_ind <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='Ind'))
system.time(port_indsty <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='IndSty',fexp=fexp))

system.time(port_ind_NE <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='Ind',addEvent = F))
system.time(port_indsty_NE <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return',constr='IndSty',fexp=fexp,addEvent = F))



system.time(port_ind <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return-risk',constr='Ind'))
system.time(port_indsty <- OptWgt(TSF,alphaf = alphaf,fRtn,fCov,target = 'return-risk',constr='IndSty',fexp=fexp))




port4 <- port_indsty_NE
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



factorFun <- 'gf.beta'
factorPar <- ''
factorID='F000014'                     
begT = as.Date("1990-01-01")
endT = Sys.Date()
splitNbin = "month"

con <- db.local()

loopT <- dbGetQuery(con,"select distinct tradingday from QT_FactorScore order by tradingday")[[1]]
loopT <- loopT[loopT>=rdate2int(begT) & loopT<=rdate2int(endT)]    
loopT.L <- split(loopT,cut(intdate2r(loopT),splitNbin))

subfun <- function(Ti){
  cat(paste(" ",min(Ti),"to",max(Ti)," ...\n"))
  dates <- paste(Ti,collapse=",")
  TS <- dbGetQuery(con,paste("select TradingDay as date, ID as stockID from QT_FactorScore where TradingDay in (",dates,")"))
  TS$date <- intdate2r(TS$date)    
  TSF <- getTSF(TS,factorFun,factorPar)
  TSF$date <- rdate2int(TSF$date)
  colnames(TSF) <- c('date','stockID',factorID)
  
  for(Tij in Ti){ # update the factorscore day by day.
    #     Tij <- Ti[1]
    # cat(paste(" ",Tij))
    dbWriteTable(con,"yrf_tmp",TSF[TSF$date==Tij,],overwrite=TRUE,append=FALSE,row.names=FALSE)
    qr <- paste("UPDATE QT_FactorScore
                SET ",factorID,"= (SELECT ",factorID," FROM yrf_tmp WHERE yrf_tmp.stockID =QT_FactorScore.ID) 
                WHERE QT_FactorScore.ID = (SELECT stockID FROM yrf_tmp WHERE yrf_tmp.stockID =QT_FactorScore.ID)
                and QT_FactorScore.TradingDay =",Tij)
    res <- dbSendQuery(con,qr)
    dbClearResult(res)  
  }   
  gc()
}  

cat(paste("Function lcfs.add: updateing factor score of",factorID,".... \n"))
plyr::l_ply(loopT.L, subfun, .progress = plyr::progress_text(style=3))   
dbDisconnect(con)



