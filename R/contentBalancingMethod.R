#' Sort the given items by their value to the simulee, with the best item first.
#'
#' @param eligibleModuleIndices A vector of item module indices, which are eligible for selection.
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return The eligibleModuleIndices, sorted by their value to the simulee.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByNormal(1)(), simulation)
#'   eligibleModuleIndices = 1:nrow(simulation$modules)
#'   balancedModuleIndices = cbm.sort(eligibleModuleIndices, simulee, simulation)
#' @export
cbm.sort <- function (eligibleModuleIndices, simulee, simulation) {
  switch(simulation$control$contentBalancing,
         maxinf = return(cbm.maxinf.sort(eligibleModuleIndices, simulee, simulation)),
         quota = return(cbm.quota.sort(eligibleModuleIndices, simulee, simulation)),
         wpm = {
           if (simulation$control$oneItemModules) {
             # Call a specialized version of wpm, optimized for single item modules.
             return(cbm.wpm1.sort(eligibleModuleIndices, simulee, simulation))
           } else {
             # Call the general version of wpm, which is slower but works for any data.
             return(cbm.wpm.sort(eligibleModuleIndices, simulee, simulation))
           }
         },
         return(cbm.new.sort(eligibleModuleIndices, simulee, simulation))
  )

  # Algorithm unknown???
  stop("contentBalancing not found: ", simulation$control$contentBalancing)
  return(NULL)
}

#' Sort the given items by their value to the simulee according to max information, with the best item first.
#'
#' @param eligibleModuleIndices A vector of itempool indices eligible for selection.
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return The eligibleModuleIndices, sorted by their value to the simulee.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-adaptive-maxinf.rds", package = "CATSimulator")))
#'   simulation$control$contentBalancing = "maxinf"
#'   simulee = initSimulee(generateSimuleesByNormal(1)(), simulation)
#'   eligibleModuleIndices = 1:nrow(simulation$modules)
#'   balancedModuleIndices = cbm.maxinf.sort(eligibleModuleIndices, simulee, simulation)
#' @export
cbm.maxinf.sort <- function (eligibleModuleIndices, simulee, simulation) {
  # Get the selection theta
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  if (assignedItemCount < 1) {
    # No assigned items or theta yet.  Use start theta
    theta = simulation$control$startTheta
  } else {
    theta = simulee$test$THETA[assignedItemCount]
  }

  # Get the information value of each item
  itemInf = itemInformation(simulation$item_pars, theta)
  moduleInf = vapply(simulation$modules$ITEM_INDICES, function(moduleItemIndices) {
    return(mean(itemInf[moduleItemIndices]))
  }, as.numeric(0))

  # Return the eligibleModuleIndices ordered by information, so the highest information items are first
  return(eligibleModuleIndices[order(moduleInf[eligibleModuleIndices], decreasing = TRUE)])
}

#' Sort the given items by their value to the simulee according to the quota algorithm, with the best item first.
#'
#' The `quota` algorithm uses a composite sort key to order the items:
#' * The primary key is the progress of each constraint on the item, relative to its lower/upper bound
#' * The secondary key is the information value of each item
#'
#' @param eligibleModuleIndices A vector of itempool indices eligible for selection.
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return The eligibleModuleIndices, sorted by their value to the simulee.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-adaptive-quota.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByNormal(1)(), simulation)
#'   eligibleModuleIndices = 1:nrow(simulation$modules)
#'   balancedModuleIndices = cbm.quota.sort(eligibleModuleIndices, simulee, simulation)
#' @export
cbm.quota.sort <- function (eligibleModuleIndices, simulee, simulation) {
  # Get constraint targets
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  testLength = max(simulation$control$minItems, assignedItemCount+1)
  consMinItems = floor(simulation$constraints$content$LOWER * testLength)
  consMaxItems = ceiling(simulation$constraints$content$UPPER * testLength)

  # Calculate ABC progress value for each constraint
  if (assignedItemCount < 1) {
    assignedItemIndices = integer()
  } else {
    assignedItemIndices = simulee$test$ITEM_INDEX[1:assignedItemCount]
  }
  consProgress = character(nrow(simulation$constraints$content))
  for (i in 1:length(consProgress)) {
    consAssignedItemCount = length(intersect(simulation$constraints$content$ITEM_INDICES[[i]], assignedItemIndices))
    if (consAssignedItemCount >= consMaxItems[i]) {
      consProgress[i] = "C"
    } else if (consAssignedItemCount < consMinItems[i]) {
      consProgress[i] = "A"
    } else {
      consProgress[i] = "B"
    }
  }

  # moduleConsProgress is the combined ABC progress for each constraint on each item in the module
  moduleConsProgress = vapply(simulation$modules$ITEM_INDICES, function(moduleItemIndices) {
    moduleConsIndices = unique(unlist(simulation$itempool$CONS_INDICES[moduleItemIndices]))
    if (any(consProgress[moduleConsIndices] == "C")) {
      return("C")
    } else if (any(consProgress[moduleConsIndices] == "A")) {
      return("A")
    } else {
      return("B")
    }
  }, as.character(0), USE.NAMES = FALSE)

  # Get the selection theta
  if (assignedItemCount < 1) {
    # No assigned items or theta yet.  Use start theta
    theta = simulation$control$startTheta
  } else {
    theta = simulee$test$THETA[assignedItemCount]
  }

  # module information is the mean of the information value of each item in the module
  itemInf = itemInformation(simulation$item_pars, theta)
  moduleInf = vapply(simulation$modules$ITEM_INDICES, function(moduleItemIndices) {
    return(mean(itemInf[moduleItemIndices]))
  }, as.numeric(0))

  # Return the sort order by consProgress first, and inf second, so the highest information items are first
  return(eligibleModuleIndices[order(moduleConsProgress[eligibleModuleIndices], -moduleInf[eligibleModuleIndices], decreasing = FALSE)])
}

#' @export
# simulation = list(control = list(consWeight = seq(1, 0, length.out = 20)))
# assignedItemCount = 7
# consWeight = cbm.wpm.consWeight(assignedItemCount, simulation)
cbm.wpm.consWeight <- function(assignedItemCount, simulation) {
  if (!"consWeight" %in% names(simulation$control)) {
    return(NA)
  }

  if (assignedItemCount < length(simulation$control$consWeight)) {
    return(simulation$control$consWeight[assignedItemCount+1])
  } else {
    return(simulation$control$consWeight[length(simulation$control$consWeight)])
  }
}

#' @export
# simulation = list(control = list(infWeight = seq(0, 1, length.out = 20)))
# assignedItemCount = 7
# infWeight = cbm.wpm.infWeight(assignedItemCount, simulation)

cbm.wpm.infWeight <- function(assignedItemCount, simulation) {
  if (!"infWeight" %in% names(simulation$control)) {
    return(NA)
  }

  if (assignedItemCount < length(simulation$control$infWeight)) {
    return(simulation$control$infWeight[assignedItemCount+1])
  } else {
    return(simulation$control$infWeight[length(simulation$control$infWeight)])
  }
}

# simulation = list(control = list(usePrevalence = FALSE))
# usePrevalence = cbm.wpm.usePrevalence(simulation)
cbm.wpm.usePrevalence <- function (simulation) {
  if ("usePrevalence" %in% names(simulation$control)) {
    return(as.logical(simulation$control$usePrevalence))
  } else {
    return(TRUE)
  }
}

#' Sort the given items by their value to the simulee according to the Weighted Penalty Method (wpm) algorithm, with the best item first.
#'
#' The `wpm1` algorithm is the same algorithm as `wpm`, but optimized to run faster for tests containing only discrete single item modules.
#' @seealso cbm.wpm.sort for general details.
#'
#' @param eligibleModuleIndices A vector of itempool indices eligible for selection.
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return The eligibleModuleIndices, sorted by their value to the simulee.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByNormal(1)(), simulation)
#'   eligibleModuleIndices = 1:nrow(simulation$modules)
#'   balancedModuleIndices = cbm.wpm.sort(eligibleModuleIndices, simulee, simulation)
#' @export
cbm.wpm1.sort <- function (eligibleModuleIndices, simulee, simulation) {
  # Calculate a few intermediate values
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  testLength = max(simulation$control$minItems, assignedItemCount+1)
  if (assignedItemCount < 1) {
    assignedItemIndices = integer()
  } else {
    assignedItemIndices = simulee$test$ITEM_INDEX[1:assignedItemCount]
  }
  consAssignedItemCount = vapply(simulation$constraints$content$ITEM_INDICES, function(consItemIndices) {
    return(length(intersect(consItemIndices, assignedItemIndices)))
  }, as.integer(0))
  consMinItems = floor(simulation$constraints$content$LOWER * testLength)
  consMaxItems = ceiling(simulation$constraints$content$UPPER * testLength)
  consMid = (simulation$constraints$content$LOWER + simulation$constraints$content$UPPER) / 2.0
  if (cbm.wpm.usePrevalence(simulation)) {
    consPrevalence = vapply(simulation$constraints$content$ITEM_INDICES, function(consItemIndices) {
      return(length(consItemIndices) / nrow(simulation$itempool))
    }, as.numeric(0))
    testLengthRemaining = max(0, testLength - assignedItemCount)
    consProportion = (consAssignedItemCount + (consPrevalence * testLengthRemaining)) / testLength
  } else {
    consProportion = consAssignedItemCount / testLength
  }
  consX = consProportion - consMid
  consD = pmin(-0.01, simulation$constraints$content$LOWER - consMid)
  consA = pmax(0.01, simulation$constraints$content$UPPER - consMid)
  consPenaltyA = (1.0 / (2.0 * consD) * (consX * consX) + (consD / 2.0)) * simulation$constraints$content$WEIGHT
  consPenaltyB = consX * simulation$constraints$content$WEIGHT
  consPenaltyC = (1.0 / (2.0 * consA) * (consX * consX) + (consA / 2.0)) * simulation$constraints$content$WEIGHT

  ### Calculate the color & cons penalty of each module
  g = nrow(simulation$modules)
  moduleColor = character(g)
  moduleConsPenalty = numeric(g)

  # to get moduleConsPenalty[1:poolSize]
  n = nrow(simulation$constraints$content)
  moduleConsIndices = simulation$itempool$CONS_INDICES[unlist(simulation$modules$ITEM_INDICES)]

  for (moduleIndex in eligibleModuleIndices) {
    moduleConsCount <- rep(0, n)
    moduleConsCount [moduleConsIndices[[moduleIndex]]] <- 1

    consProgress = rep("C", n)
    consPenalty = consPenaltyC

    A_At = which(consAssignedItemCount < consMinItems)
    consProgress[A_At] = "A"
    consPenalty[A_At] <- consPenaltyA[A_At]

    B_At = which(consAssignedItemCount + moduleConsCount <= consMaxItems)
    consProgress[B_At] = "B"
    consPenalty[B_At] <- consPenaltyB[B_At]

    # Calculate the ABCD progress value (color) for the current module
    ABC = consProgress[moduleConsIndices[[moduleIndex]]]

    if (any(ABC == "A")) {
      if (any(ABC == "C")) {
        moduleColor[moduleIndex] = "C" # Orange - AC or ABC
      } else {
        moduleColor[moduleIndex] = "A" # Green - A or AB
      }
    } else {
      if (any(ABC == "C")) {
        moduleColor[moduleIndex] = "D" # Red - BC or C
      } else {
        moduleColor[moduleIndex] = "B" # Yellow - B
      }
    }

    moduleConsPenalty[moduleIndex] = sum(consPenalty[moduleConsIndices[[moduleIndex]]])
  }

  # Normalize the cons penalty values
  minPen = min(moduleConsPenalty[eligibleModuleIndices], 1000000)
  maxPen = max(moduleConsPenalty[eligibleModuleIndices], -1000000)
  if ((maxPen - minPen) < 0.00000001) {
    moduleConsPenalty = ifelse(maxPen <= 0, 0.25, 0.75)
  } else {
    moduleConsPenalty = (moduleConsPenalty - minPen) / (maxPen - minPen)
  }

  ### Calculate the information & inf penalty of each module
  if (assignedItemCount < 1) {
    # No assigned items or theta yet.  Use start theta
    theta = simulation$control$startTheta
  } else {
    theta = simulee$test$THETA[assignedItemCount]
  }
  moduleInf = itemInformation(simulation$item_pars, theta)

  # Normalize the inf penalty values
  maxInf = max(moduleInf, 0.00000001)
  moduleInfPenalty = -moduleInf * moduleInf / maxInf / maxInf

  ### Combine the moduleConsPenalty & moduleInfPenalty, then sort by moduleColor & modulePenalty
  consWeight = cbm.wpm.consWeight(assignedItemCount, simulation)
  infWeight = cbm.wpm.infWeight(assignedItemCount, simulation)
  modulePenalty = (consWeight * moduleConsPenalty) + (infWeight * moduleInfPenalty)

  # Return the sort order by moduleColor first and modulePenalty second, least penalized items are first in each module
  return(eligibleModuleIndices[order(moduleColor[eligibleModuleIndices], modulePenalty[eligibleModuleIndices], decreasing = FALSE)])
}

#' Sort the given items by their value to the simulee according to the Weighted Penalty Method (wpm) algorithm, with the best item first.
#'
#' The `wpm` algorithm uses a composite sort key to order the items:
#' * The primary key is the progress of each constraint on the item, relative to its lower/upper bound
#' * The secondary key is the information value of each item
#'
#' @param eligibleModuleIndices A vector of itempool indices eligible for selection.
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return The eligibleModuleIndices, sorted by their value to the simulee.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByNormal(1)(), simulation)
#'   eligibleModuleIndices = 1:nrow(simulation$modules)
#'   balancedModuleIndices = cbm.wpm.sort(eligibleModuleIndices, simulee, simulation)
#' @export
cbm.wpm.sort <- function (eligibleModuleIndices, simulee, simulation) {
  # Calculate a few intermediate values
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  testLength = max(simulation$control$minItems, assignedItemCount+1)
  if (assignedItemCount < 1) {
    assignedItemIndices = integer()
  } else {
    assignedItemIndices = simulee$test$ITEM_INDEX[1:assignedItemCount]
  }
  consAssignedItemCount = vapply(simulation$constraints$content$ITEM_INDICES, function(consItemIndices) {
    return(length(intersect(consItemIndices, assignedItemIndices)))
  }, as.integer(0))
  consMinItems = floor(simulation$constraints$content$LOWER * testLength)
  consMaxItems = ceiling(simulation$constraints$content$UPPER * testLength)
  consMid = (simulation$constraints$content$LOWER + simulation$constraints$content$UPPER) / 2.0
  if (cbm.wpm.usePrevalence(simulation)) {
    consPrevalence = vapply(simulation$constraints$content$ITEM_INDICES, function(consItemIndices) {
      return(length(consItemIndices) / nrow(simulation$itempool))
    }, as.numeric(0))
    testLengthRemaining = max(0, testLength - assignedItemCount)
    consProportion = (consAssignedItemCount + (consPrevalence * testLengthRemaining)) / testLength
  } else {
    consProportion = consAssignedItemCount / testLength
  }
  consX = consProportion - consMid
  consD = pmin(-0.01, simulation$constraints$content$LOWER - consMid)
  consA = pmax(0.01, simulation$constraints$content$UPPER - consMid)
  consPenaltyA = (1.0 / (2.0 * consD) * (consX * consX) + (consD / 2.0)) * simulation$constraints$content$WEIGHT
  consPenaltyB = consX * simulation$constraints$content$WEIGHT
  consPenaltyC = (1.0 / (2.0 * consA) * (consX * consX) + (consA / 2.0)) * simulation$constraints$content$WEIGHT

  ### Calculate the color & cons penalty of each module
  moduleColor = character(nrow(simulation$modules))
  moduleConsPenalty = numeric(nrow(simulation$modules))
  for (moduleIndex in eligibleModuleIndices) {
    # moduleIndex = 1
    moduleConsIndices = c(simulation$itempool$CONS_INDICES[simulation$modules$ITEM_INDICES[[moduleIndex]]], recursive = TRUE)

    # Calculate ABC progress value for each constraint
    consProgress = vapply(1:nrow(simulation$constraints$content), function(consIndex) {
      # consIndex = 13
      moduleConsCount = sum(moduleConsIndices == consIndex)
      if ((consAssignedItemCount[consIndex] < consMinItems[consIndex])
          && ((consAssignedItemCount[consIndex] + moduleConsCount) <= consMaxItems[consIndex])) {
        return("A")
      } else if ((consAssignedItemCount[consIndex] + moduleConsCount) <= consMaxItems[consIndex]) {
        return("B")
      } else {
        return("C")
      }
    }, as.character(0))

    # Calculate the ABCD progress value (color) for the current module
    if (any(consProgress[moduleConsIndices] == "A")) {
      if (any(consProgress[moduleConsIndices] == "C")) {
        moduleColor[moduleIndex] = "C" # Orange - AC or ABC
      } else {
        moduleColor[moduleIndex] = "A" # Green - A or AB
      }
    } else {
      if (any(consProgress[moduleConsIndices] == "C")) {
        moduleColor[moduleIndex] = "D" # Red - BC or C
      } else {
        moduleColor[moduleIndex] = "B" # Yellow - B
      }
    }

    # Calculate the penalty value for each constraint
    consPenalty = vapply(1:nrow(simulation$constraints$content), function(consIndex) {
      # consIndex = 13
      if (consProgress[consIndex] == "A") {
        consPenaltyA[consIndex]
      } else if (consProgress[consIndex] == "B") {
        consPenaltyB[consIndex]
      } else { # consProgress[consIndex] == "C"
        consPenaltyC[consIndex]
      }
    }, as.numeric(0))

    # Calculate the penalty value for the module
    # TODO: If a cons is on 2+ items, should it be included 2+ times?
    moduleConsPenalty[moduleIndex] = sum(consPenalty[unique(moduleConsIndices)])
  }
  # Normalize the cons penalty values
  minPen = min(moduleConsPenalty[eligibleModuleIndices], 1000000)
  maxPen = max(moduleConsPenalty[eligibleModuleIndices], -1000000)
  if ((maxPen - minPen) < 0.00000001) {
    moduleConsPenalty = ifelse(maxPen <= 0, 0.25, 0.75)
  } else {
    moduleConsPenalty = (moduleConsPenalty - minPen) / (maxPen - minPen)
  }

  ### Calculate the information & inf penalty of each module
  if (assignedItemCount < 1) {
    # No assigned items or theta yet.  Use start theta
    theta = simulation$control$startTheta
  } else {
    theta = simulee$test$THETA[assignedItemCount]
  }
  itemInf = itemInformation(simulation$item_pars, theta)
  # module information is the mean information value of each item in the module
  moduleInf = vapply(simulation$modules$ITEM_INDICES, function(moduleItemIndices) {
    return(mean(itemInf[moduleItemIndices]))
  }, as.numeric(0))
  # Normalize the inf penalty values
  maxInf = max(moduleInf, 0.00000001)
  moduleInfPenalty = -moduleInf * moduleInf / maxInf / maxInf

  ### Combine the moduleConsPenalty & moduleInfPenalty, then sort by moduleColor & modulePenalty
  consWeight = cbm.wpm.consWeight(assignedItemCount, simulation)
  infWeight = cbm.wpm.infWeight(assignedItemCount, simulation)
  modulePenalty = (consWeight * moduleConsPenalty) + (infWeight * moduleInfPenalty)

  # Return the sort order by moduleColor first and modulePenalty second, least penalized items are first in each module
  return(eligibleModuleIndices[order(moduleColor[eligibleModuleIndices], modulePenalty[eligibleModuleIndices], decreasing = FALSE)])
}
