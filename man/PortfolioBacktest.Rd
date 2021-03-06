% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct02_portBuildingFuncs.R
\name{PortfolioBacktest}
\alias{PortfolioBacktest}
\alias{getPort}
\alias{addwgt2port}
\alias{port_limitwgt}
\alias{getPort_throughout}
\alias{port.backtest}
\title{portfolio building and backtesting}
\usage{
getPort(TSF, topN = NA, topQ = NA, sectorNe_pick = NULL, force_in = 0,
  buffer_keep = 0, buffer_rate = 0, init_port = NULL, dir = c("long",
  "short"))

addwgt2port(port, wgtType = c("eq", "fv", "fvsqrt", "custom", "ffv",
  "ffvsqrt"), sectorNe_wgt = NULL, wgtbmk = "EI000300", max_wgt = NULL,
  tolerance = 0.2)

port_limitwgt(port, max_wgt = NULL)

getPort_throughout(TSF, topN = NA, topQ = NA, sectorNe_pick = NULL,
  force_in = 0, buffer_keep = 0, buffer_rate = 0, init_port = NULL,
  wgtType = "eq", sectorNe_wgt = NULL, wgtbmk = "EI000300",
  max_wgt = NULL, dir = c("long", "short"))

port.backtest(port, holdingEndT = Sys.Date(), fee.buy = 0, fee.sell = 0,
  dir = c("long", "short"), rtn_get = c("loop", "whole", "simple"),
  silence = FALSE)
}
\arguments{
\item{TSF}{a \bold{TSF} object or a \bold{TSFR} object}

\item{topN}{an integer vector with 2 or 1 elements, giving the rank range of the assets to be selected into the portfolio. If containing only 1 element, the the top rank of 1 will be added automatically.}

\item{topQ}{a numeric vector with 2 or 1 elements, giving the percentage range of the assets to be selected into the portfolio.  If containing only 1 element, the top percentage of 0 will be added automatically.}

\item{sectorNe_pick}{NULL, "existing", or a sectorAttr.}

\item{force_in}{a numeric between 0 and 1. eg. 0.1 means that stock with rank less than topN*10\%  or pct less than topQ*10\% will be incorporated coercively.}

\item{buffer_keep}{a numeric greater than 0. eg. 0.1 means that stock with rank less than topN*110\% (\code{topN*(1+buffer_keep)}) or pct less than topQ*110\% will be kept coercively.}

\item{buffer_rate}{a numeric between 0 and 1}

\item{init_port}{a charactor vector of stockIDs.}

\item{dir}{a character string "long" or "short". In \code{getPort}, if "short",select from top to bottom, and vice versa.}

\item{wgtType}{a character string, giving the weighting type of portfolio,which could be "eq"(equal),"fv"(floatValue),"fvsqrt"(sqrt of floatValue) or "custom".}

\item{sectorNe_wgt}{NULL, "existing", or sectorAttr.}

\item{wgtbmk}{a index ID.}

\item{max_wgt}{a numeric or NA, giving the maximum of weight which could be set on a single stock. If NA(the default value), with no limit, return \code{port} itself without doing anything.}

\item{tolerance}{a numeric, only used when \code{sectorNe_wgt} is not null, Giving the tolerance of absent sectors weights in the portfolio.}

\item{holdingEndT}{a Date object, giving the ending date of the holding portfolio}

\item{fee.buy}{giving the buying fee of each assets. See \code{\link[QUtility]{Return.backtesting}} for detail}

\item{fee.sell}{}

\item{rtn_get}{a character string of "loop","whole","simple". "loop" and "whole" get the dailyrtn, where as "simple" get the periodrtn.}

\item{dir}{a character string of "long" or "short". In \code{port.backtest}, if "long", the port will be look as asset, the fee will be cut from the asset; if "short", the port will be look as liability, the fee will be added to the liability.}
}
\value{
\code{getPort} return a \bold{Port}('portfolio') objects,which are of dataframe class containing at least 2 cols("date","stockID")

\code{addwgt2port} return a \bold{Port} object which are of dataframe class containing at least 3 cols("date","stockID","wgt").

\code{getPort_throughout} return a \bold{Port} object, which contain the col of 'wgt'.

\code{port.backtest} return a \bold{PB}("PortfolioBacktest") object, a xts series of portfolio return, with the attr of 'turnover'(a xts series) and 'fee'(a vector).
}
\description{
portfolio building and backtesting
}
\details{
\code{getPort} get the \bold{Port}('portfolio') objects, subsetting from the \bold{TSF} object.

\code{addwgt2port} add the weights to the \bold{Port} object.

\code{getPort_throughout}, which is a wrapped function of \code{getPort}, \code{addwgt2Port}, \code{port_limitwgt}, get \bold{Port} object from \bold{TSF}, with further treatment.

\code{port.backtest} backtest the \bold{Port} object. Get a \bold{PB}("PortfolioBacktest") object.
}
\note{
Note that \code{topN} and \code{topQ} should at least have one and only have one.

Note that if use bufferring, the length of topN (or topQ) should be 1.
}
\examples{
modelPar <- modelPar.default()
TSFR <- Model.TSFR(modelPar)
# -- good ports
Lport1 <- getPort(TSFR,20)
Lport2 <- getPort(TSFR,c(1,20)) # the same as Lport1
Lport3 <- getPort(TSFR,,0.2)
Lport4 <- getPort(TSFR,,c(0.1,0.2))
# -- bad ports
Sport1 <- getPort(TSFR,20,dir="short")
Sport2 <- getPort(TSFR,c(10,20),dir="short")
Sport3 <- getPort(TSFR,,0.2,dir="short")
Sport4 <- getPort(TSFR,,c(0.1,0.2),dir="short")
# -- N groups of ports
N <- 5
tmp <- seq(0,1,by=1/N)
groups <- cbind(tmp[-(N+1)],tmp[-1])
port.list <- apply(groups,1,function(x)getPort(TSFR,,x))
# -- with buffer_in_keep
ts <- getTS(as.Date(c("2016-03-31","2016-04-29")),indexID = "EI000016")
tsf <- getTSF(ts,"gf_lcfs",list("F000008"),factorDir = -1) 
pt <- getPort(tsf,20,force_in = 0.5,buffer_keep = 0.5)
pt2 <- getPort(tsf,20,force_in = 0.5,buffer_keep = 0.5,sectorNe_pick = defaultSectorAttr())
# -- with buffer_rate
pt3 <- getPort(tsf,20,buffer_rate =  0.5,sectorNe_pick =NULL)
pt4 <- getPort(tsf,topQ = 0.4,buffer_rate =  0.5,sectorNe_pick =NULL)
pt5 <- getPort(tsf,20,buffer_rate =  0.5,sectorNe_pick =defaultSectorAttr())
pt6 <- getPort(tsf,topQ = 0.4,buffer_rate =  0.5,sectorNe_pick =defaultSectorAttr())
# -- add wgt to port
port <- getPort(TSF,20)
port <- addwgt2port(port,"fv")
port <- addwgt2port(port,"fv",max_wgt=0.08)
# -- reduce the risk of concentration
port <- port_limitwgt(port,max_wgt=0.08)
# -- get Port object from TSF throughout
Port_throut <- getPort_throughout(TSF, topN=20, max_wgt=0.8, dir="long")
# -- backtest the Port object
PB <- port.backtest(port)
turnover <- attr(PB,"turnover")
fee <- attr(PB,"fee")
}
\seealso{
\code{\link[QUtility]{Return.backtesting}}
}
\author{
Ruifei.Yin
}
