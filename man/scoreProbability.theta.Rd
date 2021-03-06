% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemResponseTheory.R
\name{scoreProbability.theta}
\alias{scoreProbability.theta}
\title{Calculate the probability of each possible score for the given items and a specific theta}
\usage{
scoreProbability.theta(item_pars, theta)
}
\arguments{
\item{item_pars}{a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL}

\item{theta}{a double. An ability level}
}
\value{
a matrix of doubles, items by scores, containing the probability of each possible score for the given items and a specific theta
}
\description{
Calculate the probability of each possible score for the given items and a specific theta
}
\examples{
  simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
  item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
  dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
  theta = 2.0
  scoreProb = scoreProbability.theta(item_pars, theta)
  options(digits=8, scipen=8)
  > scoreProb       0           1           2           3           4           5           6
  item001 0.054534047 0.279500656 0.665965296          NA          NA          NA          NA
  item002 0.077870790 0.307631523 0.614497687          NA          NA          NA          NA
  item003 0.208115753 0.278064930 0.513819317          NA          NA          NA          NA
  item004 0.194203063 0.176087171 0.197053893 0.242477093 0.190178780          NA          NA
  item005 0.250122827 0.155418381 0.181922731 0.242986174 0.169549887          NA          NA
  item006 ...391 total items/rows
}
