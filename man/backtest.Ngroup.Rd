% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct03_backtestingFuncs.R
\name{backtest.Ngroup}
\alias{backtest.Ngroup}
\alias{seri.Ngroup.rtn}
\alias{seri.Ngroup.spread}
\alias{seri.Ngroup.turnover}
\alias{seri.Ngroup.size}
\alias{table.Ngroup.overall}
\alias{table.Ngroup.spread}
\alias{chart.Ngroup.overall}
\alias{chart.Ngroup.seri_point}
\alias{chart.Ngroup.violin}
\alias{chart.Ngroup.box}
\alias{chart.Ngroup.seri_bar}
\alias{chart.Ngroup.seri_line}
\alias{chart.Ngroup.spread}
\alias{chart.Ngroup.turnover}
\alias{chart.Ngroup.size}
\alias{MF.chart.Ngroup.spread}
\alias{MF.chart.Ngroup.size_spread}
\alias{MC.table.Ngroup.overall}
\alias{MC.chart.Ngroup.overall}
\alias{MC.chart.Ngroup.spread}
\alias{MC.chart.Ngroup.size_spread}
\title{backtest.Ngroup}
\usage{
seri.Ngroup.rtn(TSFR, N = 5, relative = FALSE, include_univ = FALSE,
  sectorNe = NULL, bysector = NULL)

seri.Ngroup.spread(TSFR, N = 5, sectorNe = NULL,
  rtn_type = c("long-short", "long-univ"))

seri.Ngroup.turnover(TSFR, N = 5, sectorNe = NULL)

seri.Ngroup.size(TSFR, N = 5, include_univ = FALSE, sectorNe = NULL,
  fl = fl_cap(log = TRUE, var = "float_cap"))

table.Ngroup.overall(TSFR, N = 5, relative = FALSE, sectorNe = NULL,
  bysector = NULL, fee = 0, rtn_type = c("long-short", "long-univ"))

table.Ngroup.spread(TSFR, N = 5, sectorNe = NULL, fee = 0,
  rtn_type = c("long-short", "long-univ"))

chart.Ngroup.overall(TSFR, N = 5, relative = TRUE, sectorNe = NULL,
  bysector = NULL)

chart.Ngroup.seri_point(TSFR, N = 5, relative = TRUE, Nbin = "day",
  sectorNe = NULL)

chart.Ngroup.violin(TSFR, N = 5, sectorNe = NULL, jitter = TRUE)

chart.Ngroup.box(TSFR, N = 5, sectorNe = NULL)

chart.Ngroup.seri_bar(TSFR, N = 5, relative = TRUE, Nbin = "day",
  sectorNe = NULL, bysector = NULL)

chart.Ngroup.seri_line(TSFR, N = 5, relative = TRUE, include_univ = TRUE,
  sectorNe = NULL)

chart.Ngroup.spread(TSFR, N = 5, sectorNe = NULL,
  rtn_type = c("long-short", "long-univ"))

chart.Ngroup.turnover(TSFR, N = 5, group = 1, sectorNe = NULL)

chart.Ngroup.size(TSFR, N = 5, include_univ = TRUE, sectorNe = NULL,
  fl = fl_cap(log = TRUE, var = "float_cap"))

MF.chart.Ngroup.spread(mTSFR, N = 5, sectorNe = NULL,
  rtn_type = c("long-short", "long-univ"), relative = FALSE,
  facet_by = c("none", "date", "fname"), Nbin = "year")

MF.chart.Ngroup.size_spread(mTSFR, N = 5, type = c("long-short",
  "long-univ"), fl = fl_cap(log = TRUE, var = "float_cap"))

MC.table.Ngroup.overall(TSFRs, N = 5, sectorNe = NULL, fee = 0,
  rtn_type = c("long-short", "long-univ"))

MC.chart.Ngroup.overall(TSFRs, N = 5, relative = TRUE, sectorNe = NULL,
  bysector = NULL, ncol = NULL)

MC.chart.Ngroup.spread(TSFRs, N = 5, sectorNe = NULL,
  rtn_type = c("long-short", "long-univ"), relative = FALSE,
  fillNA = TRUE, facet = FALSE)

MC.chart.Ngroup.size_spread(TSFRs, N = 5, type = c("long-short",
  "long-univ"), fl = fl_cap(log = TRUE, var = "float_cap"))
}
\arguments{
\item{TSFR}{a \bold{TSFR} object}

\item{N}{the number of the groups the universe is cut to}

\item{sectorNe}{NULL, "existing", or a sectorAttr}

\item{fee}{a numeric, giving the (one side) fee}

\item{Nbin}{the number of the groups the timespan is cut to, when plotting the "date.grp".It could also be character of interval specification,See \code{\link{cut.Date}} for detail. the default value is "day",which means no cutting, The value of every date are ploted.}

\item{group}{a integer, indicating the group whose turnover be plotted}

\item{mTSFR}{a \bold{mTSFR} object. See \code{\link{getMultiFactor}}.}

\item{TSFRs}{a list of object \bold{TSFR}. See \code{\link{Model.TSFR}}.}

\item{ncol}{a integer, specificate the number of cols of the multi-charts.}

\item{sectorAttr}{NULL, "existing", or a sectorAttr}
}
\value{
seri.Ngroup.rtn return a xts object, which giving the rtn seri of each group

seri.Ngroup.spread return a xts, which giving the spread return seri of "long-short" or "long-univ"

seri.Ngroup.turnover return a xts, which giving the (one side) num or wgt turnover seri of each group

seri.Ngroup.size return a xts, which giving the mean market-cap seri of each group.

table.Ngroup.overall return a matrix which giving the statistics of the rtn of each group, as well as the rtn of top-bottom spread.

table.Ngroup.spread return a matrix which giving the statistics of the rtn of top-bottom spread in each year.

chart.Ngroup.overall return a ggplot object of "Annualized return of each group"

chart.Ngroup.seri_point return a ggplot object of "return time series of the groups" with geom_point

chart.Ngroup.seri_bar return a ggplot object of "return time series of the groups" with geom_bar

chart.Ngroup.seri_line return a ggplot object of "Cumulated return of each group" with geom_line

chart.Ngroup.spread return and print a recordedplot object of "Performance Summary of top-bottom spread" .

chart.Ngroup.turnover return a ggplot object of "Turnover Rate of each rebalancing point"

chart.Ngroup.turnover return a line chart of "Mean mkt-cap of each group at each rebalancing point"

MC.table.Ngroup.overall return a matrix, which giving the statistics of the top-bottom spread of each \code{TSFR}.
}
\description{
backtesting the factor with some tables and charts using the 'Ngroup' method.
}
\examples{
modelPar <- modelPar.default()
TSFR <- Model.TSFR(modelPar)
re <- seri.Ngroup.rtn(TSFR,5)
re2 <- seri.Ngroup.rtn(TSFR,5,include_univ=TRUE)
re <- seri.Ngroup.spread(TSFR,5)
re <- seri.Ngroup.turnover(TSFR,5)
re <- seri.Ngroup.size(tsf,fl=buildFactorList("gf.amt"))
chart.Ngroup.size(tsf,fl = buildFactorList("gf.amt",factorPar = list(log=TRUE)))
re <- table.Ngroup.overall(TSFR,5,fee=0.002)
re2 <- table.Ngroup.overall(TSFR, rtn_type = "long-univ")
re <- table.Ngroup.spread(TSFR,5,fee=0.002)
re2 <- table.Ngroup.spread(TSFR, rtn_type = "long-univ")
chart.Ngroup.overall(TSFR,5)
chart.Ngroup.seri_point(TSFR,5,"3 month")
chart.Ngroup.seri_bar(TSFR,5,"3 month")
chart.Ngroup.seri_line(TSFR,5)
chart.Ngroup.spread(TSFR,5)
chart.Ngroup.spread(TSFR, rtn_type = "long-univ")
chart.Ngroup.turnover(TSFR,5)
mTSFR <- getMultiFactor(TSR)
MF.chart.Ngroup.spread(mTSFR)
mp = modelPar.default()
factorIDs <- c("F000001","F000002","F000005")
FactorLists <- buildFactorLists_lcfs(factorIDs)
mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
TSR <- Model.TSR(mp)
TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)
MC.table.Ngroup.overall(TSFRs)
MC.chart.Ngroup.overall(TSFRs)
}
\author{
Ruifei.Yin
}
