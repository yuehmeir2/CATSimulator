% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemResponseTheory.R
\name{itemInformation.gpc}
\alias{itemInformation.gpc}
\title{Calculate the information value of each item, based on the difficulty of the item vs the theta ability level.}
\usage{
itemInformation.gpc(item_pars, NC, theta)
}
\arguments{
\item{item_pars}{a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL}

\item{NC}{a vector of integers, one per row/item in item_pars, indicating the number of scoring categories for the item.}

\item{theta}{a double. An ability level}
}
\value{
a vector of doubles. An information value for each item based on the given theta ability
}
\description{
Calculate the information value of each item, based on the difficulty of the item vs the theta ability level.
}
\examples{
  simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
  item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
  dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
  NC = itempool$NC[itempool$MODEL == "GPC"]
  theta = 2.0
  itemInf = itemInformation.gpc(item_pars, NC, theta)
  options(digits=8, scipen=8)
  > itemInf
  item001     item002     item003     item004     item005     item006     item007     item008     item009     item010     item011
  0.106811860 0.099854278 0.404301595 0.381726626 0.345638483 0.390878165 0.879889531 1.338459278 0.300843342 0.306951683 0.098634121
  item012     item013     item014     item015     item016     item017     item018     item019     item020     item021     item022
  0.048480077 0.187423184 0.081043093 0.230168816 0.088666049 0.070758492 0.113358294 0.554698200 0.074621191 0.102712791 0.128576732
  item023     item024     item025     item026     item027     item028     item029     item030     item031     item032     item033
  0.100973865 0.160066422 0.246199688 0.011415987 0.997733849 1.609718293 0.107723968 0.389730835 0.581546083 0.258518968 0.111703177
  item034     item035     item036     item037     item038     item039     item040     item041     item046     item047     item061
  0.048911785 0.088373824 0.037252174 0.257251632 0.693060921 0.994148681 0.108422774 0.125033103 0.556370070 0.170363798 0.515111483
  item062     item074     item076     item085     item094     item137     item174     item211     item216     item230     item231
  0.797755937 0.680314723 0.414549846 0.413245770 0.146860489 0.369058561 0.517985766 0.298505817 0.774725278 0.482406447 0.166592254
  item235 ...114 total values
}
