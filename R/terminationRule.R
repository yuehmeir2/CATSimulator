#' Returns whether the given simulee has completed the simulation.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return A logical true/false depending on whether the simulee has reached the end of the simulation.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   simulee = generateSimulees(numSimulees = 1, mean = 0.0, sd = 1.5)
#'   terminated = tr.isTerminated(simulee, simulation)
#' @export
tr.isTerminated <- function (simulee, simulation) {
  switch(simulation$control$terminationRule,
         asap = return(tr.asap.isTerminated(simulee, simulation)),
         csem = return(tr.csem.isTerminated(simulee, simulation))
  )

  # Algorithm unknown???
  stop("terminationRule not found: ", simulation$control$terminationRule)
  return(NULL)
}

#' Returns whether the given simulee has completed the simulation.
#'
#' The `asap` algorithm will try to end the test as soon as possible after the
#' minimum test length has been reached.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return A logical true/false depending on whether the simulee has reached the end of the simulation.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   simulee = generateSimulees(numSimulees = 1, mean = 0.0, sd = 1.5)
#'   terminated = tr.asap.isTerminated(simulee, simulation)
#' @export
tr.asap.isTerminated <- function (simulee, simulation) {
  # Stop as soon as possible once the simulee has reached the minimum test length
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  return(assignedItemCount >= simulation$control$minItems)
}

#' Returns whether the given simulee has completed the simulation.
#'
#' The `csem` algorithm will try to end the test as soon as possible after the
#' minimum test length has been reached and the csem of the latest item is below
#' a target value.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return A logical true/false depending on whether the simulee has reached the end of the simulation.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   simulee = generateSimulees(numSimulees = 1, mean = 0.0, sd = 1.5)
#'   terminated = tr.csem.isTerminated(simulee, simulation)
#' @export
tr.csem.isTerminated <- function (simulee, simulation) {
  # Stop if csem reaches termination value between min/max test length
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  if (assignedItemCount < simulation$control$minItems) {
    return(FALSE)
  } else if (assignedItemCount >= simulation$control$maxItems) {
    return(TRUE)
  } else {
    return(simulee$test$CSEM[assignedItemCount] <= simulation$control$terminationValue)
  }
}
