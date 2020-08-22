#' Calculate stats analyzing the quality of the Item Exposure Control aspect of
#' a completed simulation.
#'
#' @param simulation An object defining the test that was run.
#' @param output A tibble of data from simulees that have completed the simulation.
#' @return An object containing multiple stats and tables showing the quality
#'   of the completed simulation.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   result = runSimulation(simulation, generateSimuleesByNormal(5))
#'   result$iec = getResult.IEC(simulation, result$output)
#' @export
getResult.IEC <- function(simulation, output) {
  simuleeIds = unique(output$SIM_ID)

  itemExposureCounts = vapply(simulation$itempool$ITEM_ID, function(itemId) {
    sum(output$ITEM_ID == itemId)
  }, as.integer(0), USE.NAMES = FALSE)

  result.iec = list()
  result.iec$itemExposureCounts = itemExposureCounts
  result.iec$itemMaxExposure = max(itemExposureCounts) / length(simuleeIds)
  result.iec$itemsNeverUsed = sum(itemExposureCounts %in% 0) / nrow(simulation$itempool)

  return(result.iec)
}

#' Calculate stats analyzing the quality of the IRT Ability Estimation aspect
#' of a completed simulation.
#'
#' @param simulation An object defining the test that was run.
#' @param output A tibble of data from simulees that have completed the simulation.
#' @return An object containing multiple stats and tables showing the quality
#'   of the completed simulation.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   result = runSimulation(simulation, generateSimuleesByNormal(5))
#'   result$irt = getResult.IRT(simulation, result$output)
#' @export
getResult.IRT <- function(simulation, output) {
  simuleeIds = unique(output$SIM_ID)
  simuleeLastRows = vapply(simuleeIds, function(simuleeId) {
    max(which(output$SIM_ID == simuleeId))
  }, as.integer(0), USE.NAMES = FALSE)

  trueTheta = output$TRUE_THETA[simuleeLastRows]
  finalTheta = output$THETA[simuleeLastRows]
  finalCsem = output$CSEM[simuleeLastRows]

  result.irt = list()
  result.irt$bias = sum(trueTheta - finalTheta) / length(simuleeIds)
  result.irt$correlation = cor(finalTheta, trueTheta)
  result.irt$csem = mean(finalCsem)
  result.irt$mse = mean((trueTheta - finalTheta)^2)

  result.irt$m_reliability = 1.0 - mean(finalCsem)^2

  return(result.irt)
}

#' Calculate stats analyzing the quality of the Item Selection aspect
#' of a completed simulation.
#'
#' @param simulation An object defining the test that was run.
#' @param output A tibble of data from simulees that have completed the simulation.
#' @return An object containing multiple stats and tables showing the quality
#'   of the completed simulation.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   result = runSimulation(simulation, generateSimuleesByNormal(5))
#'   result$isr = getResult.ISR(simulation, result$output)
#' @export
getResult.ISR <- function(simulation, output) {
  simuleeIds = unique(output$SIM_ID)

  simuleeTestLengths = vapply(simuleeIds, function(simuleeId) {
    sum(output$SIM_ID == simuleeId)
  }, as.integer(0), USE.NAMES = FALSE)

  # Calculate %simulees that reached the maximum test length
  result.isr = list()
  result.isr$testLengthMean = mean(simuleeTestLengths)
  result.isr$testLengthReachMax = sum(simuleeTestLengths >= simulation$control$maxItems) / length(simuleeIds)

  if (("constraints" %in% names(simulation)) && ("content" %in% names(simulation$constraints))) {
    # Calculate the number of constraint violations for each simulee
    simuleeConsMinItems = sapply(simuleeTestLengths, function(testLength) {
      floor(simulation$constraints$content$LOWER * testLength)
    })
    simuleeConsMaxItems = sapply(simuleeTestLengths, function(testLength) {
      ceiling(simulation$constraints$content$UPPER * testLength)
    })
    simuleeConsAssignedItemCounts = sapply(simuleeIds, function(simuleeId) {
      outputRows = which(output$SIM_ID == simuleeId)
      assignedItemIndices = match(output$ITEM_ID[outputRows], simulation$itempool$ITEM_ID)
      vapply(simulation$constraints$content$ITEM_INDICES, function(consItemIndices) {
        length(intersect(consItemIndices, assignedItemIndices))
      }, as.integer(0), USE.NAMES = FALSE)
    })

    nConstraints = nrow(simulation$constraints$content)

    simuleeConsViolations = sapply(1:length(simuleeIds), function(s) {
      # s = 5
      sapply(1:nConstraints, function(c) {
        # c = 3
        if (simuleeConsAssignedItemCounts[c,s] < simuleeConsMinItems[c]) {
          # Below minimum items, return negative diff, cap at -5
          return(max(-5, simuleeConsAssignedItemCounts[c,s] - simuleeConsMinItems[c]))
        } else if (simuleeConsAssignedItemCounts[c,s] > simuleeConsMaxItems[c]) {
          # Above maximum items, return positive diff, cap at +5
          return(min(5, simuleeConsAssignedItemCounts[c,s] - simuleeConsMaxItems[c]))
        } else {
          # Within min/max items, return 0
          return(0)
        }
      })
    })

    # Calculate %simulees that had zero violations across the whole test
    result.isr$testOnTarget = sum(colSums(simuleeConsViolations == 0) == nConstraints) / length(simuleeIds)

    # Calculate (#items by constraints) how many simulees had that #items for each constraint
    consAssnDistr = sapply(0:simulation$control$maxItems, function(numItems) {
      # numItems = 1
      sapply(1:nrow(simulation$constraints$content), function(c) {
        sum(simuleeConsAssignedItemCounts[c,] == numItems)
      })
    })
    colnames(consAssnDistr) = c(0:simulation$control$maxItems)
    result.isr$consAssignedItemCountDistr = add_column(as_tibble(consAssnDistr),
      CONS_ID = simulation$constraints$content$CONS_ID,
      MIN_N = floor(simulation$constraints$content$LOWER * simulation$control$minItems),
      MAX_N = ceiling(simulation$constraints$content$UPPER * simulation$control$maxItems),
      .before = 1)

    # Calculate (#violations by constraints) how many simulees had that #violations for each constraint
    consViolations = sapply(-5:5, function(numViolations) {
      # numViolations = -4
      sapply(1:nrow(simulation$constraints$content), function(c) {
        sum(simuleeConsViolations[c,] == numViolations)
      }) / length(simuleeIds)
    })
    colnames(consViolations) = c("-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4", "+5")
    result.isr$consViolations = add_column(as_tibble(consViolations),
      CONS_ID = simulation$constraints$content$CONS_ID,
      LOWER = simulation$constraints$content$LOWER,
      UPPER = simulation$constraints$content$UPPER,
      .before = 1)
  }

  return(result.isr)
}
