% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemResponseTheory.R
\name{scoreProbability.scores.3pl}
\alias{scoreProbability.scores.3pl}
\title{Calculate the probability of the given item/score combinations for a range of thetas}
\usage{
scoreProbability.scores.3pl(item_pars, thetas, scores)
}
\arguments{
\item{item_pars}{a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL}

\item{thetas}{a vector of doubles. Various ability levels}

\item{scores}{a vector of integers. A score for each item.  Must be same length as nrow(items).}
}
\value{
a matrix of doubles, items by thetas, containing the probability of the given item/score combinations for a range of thetas
}
\description{
Calculate the probability of the given item/score combinations for a range of thetas
}
\examples{
  simulation = readRDS(system.file("example/internal/gpc3pl-optimal.rds", package = "CATSimulator"))
  item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
  dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
  thetas = seq(-2, 2, by = 0.5)
  scores = rep(0, nrow(item_pars)) # sample(0:1, nrow(item_pars), replace = TRUE)
  scoreProb = scoreProbability.scores.3pl(item_pars, thetas, scores)
  options(digits=8, scipen=8)
  > scoreProb     -2       -1.5         -1       -0.5          0        0.5          1         1.5           2
  item042 0.93258889 0.91498381 0.89330699 0.86690713 0.83518019 0.79765543 0.75410271 0.704646782 0.649862455
  item043 0.97800364 0.96830755 0.95453608 0.93518101 0.90837694 0.87200583 0.82399418 0.762870413 0.688542983
  item044 0.70350254 0.65577175 0.60467498 0.55118453 0.49648327 0.44186606 0.38861989 0.337905469 0.290662804
  item045 0.95313713 0.92332093 0.87698279 0.80845126 0.71418415 0.59666976 0.46690424 0.341467387 0.234881989
  item048 0.97620356 0.96403739 0.94599523 0.91965584 0.88207156 0.83015274 0.76155526 0.676063643 0.576947540
  item049 ...277 total items/rows
}
