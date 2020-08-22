#' Filter the given items down to the best N rows, as defined by the simulation.
#'
#' @param balancedModuleIndices A vector of indices, which are candidates for selection sorted by their value to the simulee.
#' @param simulation An object defining the test that is being run.
#' @return The balancedModuleIndices, filtered to the best N values.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   balancedModuleIndices = sample(1:nrow(simulation$modules), 4)
#'   candidateModuleIndices = iec.filter(balancedModuleIndices, simulation)
#' @export
iec.filter <- function (balancedModuleIndices, simulation) {
  if (is.null(simulation$control$iecPars)) {
    iecPars = 1
  } else {
    iecPars = min(simulation$control$iecPars, length(balancedModuleIndices))
  }

  return(balancedModuleIndices[1:iecPars])
}

#' Select a single index randomly from a given vector of indices.
#'
#' @param candidateModuleIndices A vector of indices, which are candidates for selection.
#' @return The single randomly chosen index from candidateModuleIndices.
#' @examples
#'   candidateModuleIndices = sample(1:9, 4)
#'   selectedIndex = iec.select(candidateModuleIndices)
#' @export
iec.select <- function (candidateModuleIndices) {
  random = ceiling(runif(1) * length(candidateModuleIndices))
  return(candidateModuleIndices[random])
}
