% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contentBalancingMethod.R
\name{cbm.wpm.sort}
\alias{cbm.wpm.sort}
\title{Sort the given items by their value to the simulee according to the Weighted Penalty Method (wpm) algorithm, with the best item first.}
\usage{
cbm.wpm.sort(eligibleModuleIndices, simulee, simulation)
}
\arguments{
\item{eligibleModuleIndices}{A vector of itempool indices eligible for selection.}

\item{simulee}{A data.frame containing at least one simulee, the first row will be used.}

\item{simulation}{An object defining the test that is being run.}
}
\value{
The eligibleModuleIndices, sorted by their value to the simulee.
}
\description{
The `wpm` algorithm uses a composite sort key to order the items:
* The primary key is the progress of each constraint on the item, relative to its lower/upper bound
* The secondary key is the information value of each item
}
\examples{
  simulation = initSimulation(readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator")))
  simulee = initSimulee(generateSimuleesByNormal(1)(), simulation)
  eligibleModuleIndices = 1:nrow(simulation$modules)
  balancedModuleIndices = cbm.wpm.sort(eligibleModuleIndices, simulee, simulation)
}
