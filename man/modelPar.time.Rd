% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct00_modelsettingFuncs.R
\name{modelPar.time}
\alias{modelPar.time}
\alias{setmodelPar.time}
\title{modelPar.time}
\usage{
modelPar.time(modelPar = modelPar.default(), begT = Sys.Date() -
  lubridate::years(1), endT = Sys.Date() - 1, rebFreq = "month",
  shiftby = 0, dure = NULL, dates = NULL)

setmodelPar.time(modelPar, begT, endT, rebFreq, shiftby, dure, dates)
}
\arguments{
\item{modelPar}{a \bold{modelPar} object}

\item{begT}{the begin date}

\item{endT}{the end date}

\item{rebFreq}{an interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s". See \code{\link{cut.Date}} for detail.}

\item{shiftby}{a integer,how many days the rebalancing date be shifted foreward}

\item{dates}{an arbitrary vector of trading date, with class of Date. If param \code{dates} is not null, then all the other params will be invalidated.}
}
\value{
a \bold{modelPar} object
}
\description{
set the rebalancing date related parametres of the model
}
\details{
The different between \code{modelPar.time} and \code{setmodelPar.time} is that the all pars of the former have default values, and the latter have not. The former is ussually used to to build the modelPar integrally, and the latter is ussually used to set the parametre individually.
}
\seealso{
\code{\link{getRebDates}}

Other modelPar setting functions: \code{\link{modelPar.default}},
  \code{\link{modelPar.factor}},
  \code{\link{modelPar.title}}, \code{\link{modelPar.univ}}
}
\author{
Ruifei.Yin
}
