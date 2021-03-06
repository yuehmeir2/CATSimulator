% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulator.R
\name{runSimulation}
\alias{runSimulation}
\title{Generate simulees and run them through the given simulation.}
\usage{
runSimulation(
  simulation,
  simuleeGenerator,
  progressCallback = NULL,
  showProgress = F
)
}
\arguments{
\item{simulation}{An object defining the test to be run.}

\item{simuleeGenerator}{A generator function that will generate one simulee each time it is called.}

\item{progressCallback}{A callback function that takes a single parameter, the number of completed simulees.}
}
\value{
A data.frame of generated simulees that completed the simulation.
}
\description{
Generate simulees and run them through the given simulation.
}
\examples{
  options(warn=-1)
  simulation = read_simulation("inst/input_files/many_panels/test.json")
  #saveRDS(simulation, "inst/example/internal/many_panels.rds")
  simulation$control$solver = list(name = "cbc", external = T, mipGap = 0.0001, timeout = 1000, verbose = F)
  simuleeGenerator = generateSimuleesByTrueTheta(-2:2, 10001:10005)
  result = runSimulation(simulation, simuleeGenerator)
}
