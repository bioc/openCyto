% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singletGate.R
\name{gate_singlet}
\alias{gate_singlet}
\title{Creates a singlet polygon gate using the prediction bands from a robust linear model}
\usage{
gate_singlet(
  x,
  area = "FSC-A",
  height = "FSC-H",
  sidescatter = NULL,
  prediction_level = 0.99,
  subsample_pct = NULL,
  wider_gate = FALSE,
  filterId = "singlet",
  maxit = 5,
  ...
)
}
\arguments{
\item{x}{a \code{\link[flowCore:flowFrame-class]{flowFrame}} object}

\item{area}{character giving the channel name that records the signal
intensity as peak area}

\item{height}{character giving the channel name that records the signal
intensity as peak heightchannel name of height}

\item{sidescatter}{character giving an optional channel name for the
sidescatter signal. By default, ignored.}

\item{prediction_level}{a numeric value between 0 and 1 specifying the level
to use for the prediction bands}

\item{subsample_pct}{a numeric value between 0 and 1 indicating the percentage
of observations that should be randomly selected from \code{x} to construct
the gate. By default, no subsampling is performed.}

\item{wider_gate}{logical value. If \code{TRUE}, the prediction bands used to
construct the singlet gate use the robust fitted weights, which increase
prediction uncertainty, especially for large FSC-A. This leads to wider gates,
which are sometimes desired.}

\item{filterId}{the name for the filter that is returned}

\item{maxit}{the limit on the number of IWLS iterations}

\item{...}{additional arguments (not used)}
}
\value{
a \code{\link[flowCore]{polygonGate}} object with the singlet gate
}
\description{
We construct a singlet gate by applying a robust linear model. By default, we model the forward-scatter height
(FSC-H)as a function of forward-scatter area (FSC-A). If \code{sidescatter}
is given, forward-scatter height is as a function of \code{area} +
\code{sidescatter} + \code{sidescatter / area}.
}
\details{
Because \code{rlm} relies on iteratively reweighted least
squares (IRLS), the runtime to construct a singlet gate is dependent in part
on the number of observations in \code{x}. To improve the runtime, we provide
an option to subsample randomly a subset of \code{x}. A percentage of
observations to subsample can be given in \code{subsample_pct}. By default, no
subsampling is applied.
}
