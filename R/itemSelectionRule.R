# source("R/contentBalancingMethod.R")
# source("R/itemExposureControl.R")
# source("R/solver.R")
# source("R/terminationRule.R")

#' Returns the next items that should be assigned to the simulee, selected
#' from the given simulation.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @param stage A number for the stage of the test to be selected
#' @return A list containing the selected moduleIndex, plus other values useful for understanding the selection.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByTrueTheta(-2, 10001)(), simulation)
#'   stage = 1
#'   selection = isr.select(simulee, simulation, stage)
#' @export
isr.select <- function (simulee, simulation, stage) {
  switch(simulation$control$itemSelectionRule,
         adaptive = return(isr.adaptive.select(simulee, simulation)),
         linear = return(isr.linear.select(simulee, simulation)),
         multistage = return(isr.multistage.select(simulee, simulation, stage)),
         optimal = return(isr.optimal.select(simulee, simulation))
  )

  # Algorithm unknown???
  stop("itemSelectionRule not found: ", simulation$control$itemSelectionRule)
  return(NULL)
}

#' isr.checkIncompletePassage
#'
#' @examples
#'   options(warn=-1)
#'   simulation = initSimulation(readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByTrueTheta(-2, 10001)(), simulation)
#'   eligibleModuleIndices = NULL
#'   prevSlot = 1
#'   prevSlotPsgId = isr.checkIncompletePassage(prevSlot, simulee, simulation)
isr.checkIncompletePassage <- function (prevSlot, eligibleModuleIndices, simulee, simulation) {
  # If no previous item assigned, or passages not defined, it cannot belong to a passage
  if ((prevSlot < 1) || (!"PSG_ID" %in% names(simulation$itempool)) || (!"passage" %in% names(simulation$constraints))) {
    return(NULL)
  }

  # Get the passage id for the item in the previous slot
  prevSlotItemIndex = simulee$test$ITEM_INDEX[prevSlot]
  prevSlotPsgId = simulation$itempool$PSG_ID[prevSlotItemIndex]

  # If prevSlotPsgId has NA as its value, return NULL instead
  if (is.na(prevSlotPsgId)) return(NULL)

  # Count the number of items associated with the psgId assigned to the simulee
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  psgLower = simulation$constraints$passage$LOWER[which(simulation$constraints$passage$PSG_ID == prevSlotPsgId)]
  psgMinCount = floor(psgLower * max(assignedItemCount, simulation$control$minItems))
  assignedPsgItemCount = sum(simulation$itempool$PSG_ID[simulee$test$ITEM_INDEX[seq_len(prevSlot)]] == prevSlotPsgId, na.rm = T)

  # If the passage is incomplete, return its id so we keep assigning items to that passage
  if (assignedPsgItemCount < psgMinCount) {
    return(prevSlotPsgId)
  }

  # If there are more eligible items (from the optimal solution) belonging to the previous passage, return its id so we keep assigning items to that passage
  # This is necessary for variable-length passages, where we have reached the min but the solver assigns more than min items
  if (length(eligibleModuleIndices) > 0) {
    eligiblePsgIds = simulation$modules$PSG_ID[eligibleModuleIndices]
    eligiblePsgIds = eligiblePsgIds[!is.na(eligiblePsgIds)]
    if (any(eligiblePsgIds == prevSlotPsgId)) {
      return(prevSlotPsgId)
    }
  }

  # If none of these things were true, the passage is finished.  Return null so other items/psgs can be freely assigned.
  return(NULL)
}

#' isr.filterByConstraintControl
#'
#' @examples
#'   options(warn=-1)
#'   simulation = initSimulation(readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator")))
#'   slot = 1
#'   incompletePsgId = NULL
#'   eligibleModuleIndices = 1:nrow(simulation$modules)
#'   filteredIndices = isr.filterByConstraintControl(eligibleModuleIndices, incompletePsgId, slot, simulation)
isr.filterByConstraintControl <- function (eligibleModuleIndices, incompletePsgId, slot, simulation) {
  constraintControls = simulation$control$constraintControl

  # Which constraint controls apply to the slot being selected?
  bySlot = vapply(constraintControls, function(control) {
    # control = constraintControls[[2]]
    slot %in% control$onSlots
  }, as.logical(0), USE.NAMES = FALSE)
  activeControls = constraintControls[bySlot]

  for (control in activeControls) {
    # control = activeControls[[1]]
    if ("requireAll" %in% names(control)) {
      # ALL constraints must appear on at least one item in the module.
      # Does not have to be all on the same item.
      keepModule = vapply(eligibleModuleIndices, function(moduleIndex) {
        # moduleIndex = 25
        moduleConsIds = c(simulation$itempool$CONS_IDS[simulation$modules$ITEM_INDICES[[moduleIndex]]], recursive = T)
        all(control$requireAll %in% moduleConsIds)
      }, as.logical(0), USE.NAMES = FALSE)
      eligibleModuleIndices = eligibleModuleIndices[keepModule]
    }
    if ("requireAny" %in% names(control)) {
      # AT LEAST ONE constraint must appear on at least one item in the module.
      # ALL constraints must appear on at least one item in the module.
      # Does not have to be all on the same item.
      keepModule = vapply(eligibleModuleIndices, function(moduleIndex) {
        # moduleIndex = 25
        moduleConsIds = c(simulation$itempool$CONS_IDS[simulation$modules$ITEM_INDICES[[moduleIndex]]], recursive = T)
        any(control$requireAny %in% moduleConsIds)
      }, as.logical(0), USE.NAMES = FALSE)
      eligibleModuleIndices = eligibleModuleIndices[keepModule]
    }
    if ("block" %in% names(control)) {
      # NONE of the constraints may appear on ANY item in the module
      excludeModule = vapply(eligibleModuleIndices, function(moduleIndex) {
        # moduleIndex = 25
        moduleConsIds = c(simulation$itempool$CONS_IDS[simulation$modules$ITEM_INDICES[[moduleIndex]]], recursive = T)
        any(control$block %in% moduleConsIds)
      }, as.logical(0), USE.NAMES = FALSE)
      eligibleModuleIndices = eligibleModuleIndices[!excludeModule]
    }
    if (("blockIfNoPassage" %in% names(control)) && is.null(incompletePsgId)) {
      # NONE of the constraints may appear on ANY item in the module
      excludeModule = vapply(eligibleModuleIndices, function(moduleIndex) {
        # moduleIndex = 25
        moduleConsIds = c(simulation$itempool$CONS_IDS[simulation$modules$ITEM_INDICES[[moduleIndex]]], recursive = T)
        any(control$blockIfNoPassage %in% moduleConsIds)
      }, as.logical(0), USE.NAMES = FALSE)
      eligibleModuleIndices = eligibleModuleIndices[!excludeModule]
    }
  }
  return(eligibleModuleIndices)
}

#' isr.getEligibleModules
#'
#' @examples
#'   options(warn=-1)
#'   simulation = initSimulation(readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByTrueTheta(-2, 10001)(), simulation)
#'   slot = 1
#'   blocksReleased = TRUE
#'   eligibleModuleIndices = isr.getEligibleModules(slot, blocksReleased, simulee, simulation)
isr.getEligibleModules <- function (slot, blocksReleased, simulee, simulation) {
  # Get an initial set of eligible modules that fit within the remaining test length
  if (simulation$control$oneItemModules) {
    eligibleModuleIndices = 1:nrow(simulation$modules)
  } else {
    maxLength = simulation$control$maxItems - slot + 1
    eligibleModuleIndices = which(vapply(simulation$modules$ITEM_IDS, length, as.integer(0)) <= maxLength)
  }

  # Filter out blocked/assigned items
  if (blocksReleased) {
    # Exclude only assigned items
    if (slot <= 1) {
      excludeItemIndices = integer(0)
    } else {
      excludeItemIndices = simulee$test$ITEM_INDEX[1:slot]
    }
  } else {
    # Filter out blocked/assigned items
    excludeItemIndices = simulee$blockedItemIndices
  }
  if (length(excludeItemIndices) > 0) {
    # Eligible modules must not contain any excluded item ids
    excludeModule = vapply(eligibleModuleIndices, function(moduleIndex) {
      # moduleIndex = 25
      any(simulation$modules$ITEM_INDICES[moduleIndex] %in% excludeItemIndices)
    }, as.logical(0), USE.NAMES = FALSE)
    if (any(excludeModule)) {
      eligibleModuleIndices = eligibleModuleIndices[!excludeModule]
    }
  }

  # Filter out blocked/assigned passages
  incompletePsgId = isr.checkIncompletePassage(slot - 1, NULL, simulee, simulation)
  if ("PSG_ID" %in% names(simulation$modules)) {
    if (!is.null(incompletePsgId)) {
      # Exclude every passage except the incomplete passage.
      # Select only from that passage until it is completed
      excludeModule = is.na(simulation$modules$PSG_ID[eligibleModuleIndices]) | simulation$modules$PSG_ID[eligibleModuleIndices] != incompletePsgId
      if (any(excludeModule)) {
        eligibleModuleIndices = eligibleModuleIndices[!excludeModule]
      }
    } else if (!blocksReleased && (length(simulee$blockedPsgIds) > 0)) {
      # Exclude blocked passages
      excludeModule = simulation$modules$PSG_ID[eligibleModuleIndices] %in% simulee$blockedPsgIds
      if (any(excludeModule)) {
        eligibleModuleIndices = eligibleModuleIndices[!excludeModule]
      }
    }
  }

  # Filter out based on constraintControl
  if ("constraintControl" %in% names(simulation$control)) {
    # Filter the eligible modules based on constraint control: requiredAll, requiredAny and blocked
    eligibleModuleIndices = isr.filterByConstraintControl(eligibleModuleIndices, incompletePsgId, slot, simulation)
  }

  # simulation$itempool[unlist(simulation$modules$ITEM_INDICES[eligibleModuleIndices]),]
  # simulation$itempool$ITEM_ID[unlist(simulation$modules$ITEM_INDICES[eligibleModuleIndices])]
  # simulation$itempool$CONS_IDS[unlist(simulation$modules$ITEM_INDICES[eligibleModuleIndices])]
  return(eligibleModuleIndices)
}

#' Returns the next items that should be assigned to the simulee, selected
#' from the given simulation.
#'
#' The `adaptive` algorithm uses content balancing and item exposure algorithms
#' to select the best item in the item pool for the given simulee.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return A list containing the selected moduleIndex, plus other values useful for understanding the selection.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByTrueTheta(-2, 10001)(), simulation)
#'   selection = isr.adaptive.select(simulee, simulation)
#' @export
isr.adaptive.select <- function (simulee, simulation) {
  if (tr.isTerminated(simulee, simulation)) {
    # Return a null moduleIndex if the test is complete
    return(list(moduleIndex = NULL))
  }

  # Determine which modules are eligible to be selected
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  eligibleModuleIndices = isr.getEligibleModules(assignedItemCount + 1, !is.null(simulee$iecReleasedAt), simulee, simulation)

  # If no items are eligible, release the IEC blocked items and try again
  if ((length(eligibleModuleIndices) < 1) && is.null(simulee$iecReleasedAt)) {
    simulee$iecReleasedAt = assignedItemCount
    eligibleModuleIndices = isr.getEligibleModules(assignedItemCount + 1, TRUE, simulee, simulation)
  }

  # If there are still no items eligible, we cannot continue
  if (length(eligibleModuleIndices) < 1) {
    stop("no eligible item modules for simulee: ", simulee$id, ", blockedItemIds: ", paste(simulee$blockedItemIds, collapse=","))
    return(NULL)
  }

  # Sort the items so the best are first.
  balancedModuleIndices = cbm.sort(eligibleModuleIndices, simulee, simulation)

  # Filter the balanced item modules to a list of a few candidates
  candidateModuleIndices = iec.filter(balancedModuleIndices, simulation)

  # Select a single item module from the candidates
  selectedIndex = iec.select(candidateModuleIndices)

  selection = list(moduleIndex = selectedIndex,
                   candidateModuleIndices = candidateModuleIndices,
                   iecReleasedAt = simulee$iecReleasedAt)
  if ("PSG_ID" %in% names(simulation$modules)) {
    selection$psgId = simulation$modules$PSG_ID[selectedIndex]
  }
  return(selection)
}

#' Returns the next items that should be assigned to the simulee, selected
#' from the given simulation.
#'
#' The `linear` algorithm return the first unassigned item in the item pool.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return A list containing the selected moduleIndex, plus other values useful for understanding the selection.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-linear.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByTrueTheta(-2, 10001)(), simulation)
#'   selection = isr.linear.select(simulee, simulation)
#' @export
isr.linear.select <- function (simulee, simulation) {
  if (tr.isTerminated(simulee, simulation)) {
    # Return a null moduleIndex if the test is complete
    return(list(moduleIndex = NULL))
  }

  # Determine which modules are eligible to be selected
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  eligibleModuleIndices = isr.getEligibleModules(assignedItemCount + 1, !is.null(simulee$iecReleasedAt), simulee, simulation)

  # If no items are eligible, release the IEC blocked items and try again
  if ((length(eligibleModuleIndices) < 1) && is.null(simulee$iecReleasedAt)) {
    simulee$iecReleasedAt = assignedItemCount
    eligibleModuleIndices = isr.getEligibleModules(assignedItemCount + 1, TRUE, simulee, simulation)
  }

  # If there are still no items eligible, we cannot continue
  if (length(eligibleModuleIndices) < 1) {
    stop("no eligible item modules for simulee: ", simulee$id, ", blockedItemIds: ", paste(simulee$blockedItemIds, collapse=","))
    return(NULL)
  }

  # Return first item
  selectedIndex = head(eligibleModuleIndices, 1)

  selection = list(moduleIndex = selectedIndex,
                   candidateModuleIndices = selectedIndex)
  if ("PSG_ID" %in% names(simulation$modules)) {
    selection$psgId = simulation$modules$PSG_ID[selectedIndex]
  }
  return(selection)
}

isr.multistage.cutGroup <- function (theta, cutThetas) {
  if (length(cutThetas) > 0) {
    for (i in 1:length(cutThetas)) {
      if (theta < cutThetas[i]) {
        return(i)
      }
    }
  }
  return(length(cutThetas)+1)
}

#' Returns the next items that should be assigned to the simulee, selected
#' from the given simulation.
#'
#' The `multistage` algorithm selects the item module with the best information for
#' the current stage, and return all the items within that module
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @param stage A number for the stage of the test to be selected
#' @return A list containing the selected moduleIndex, plus other values useful for understanding the selection.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/internal/shallow-multistage.rds", package = "CATSimulator")))
#'   simulee = initSimulee(generateSimuleesByTrueTheta(-2, 10001)(), simulation)
#'   stage = 1
#'   selection = isr.multistage.select(simulee, simulation, stage)
#' @export
isr.multistage.select <- function (simulee, simulation, stage) {
  if (tr.isTerminated(simulee, simulation)) {
    # Return a null moduleIndex if the test is complete
    return(list(moduleIndex = NULL))
  }

  # Determine which modules are eligible to be selected
  if ("panels" %in% names(simulation)) {
    # If panels are defined, restrict the eligibleModuleIndices to the simulee's panel
    eligibleModuleIndices = unlist(simulation$panels$MODULE_INDICES[simulation$panels$PANEL_ID == simulee$panel])
  } else {
    # Otherwise all modules are initially eligible
    eligibleModuleIndices = 1:nrow(simulation$modules)
  }

  # Do stage-specific filtering
  if ("STAGE" %in% names(simulation$modules)) {
    # Eligible modules must be associated with the current stage
    eligibleModuleIndices = Filter(function(eligibleIndex) {
      stage == simulation$modules$STAGE[[eligibleIndex]]
    }, eligibleModuleIndices)
  }

  # If mst$path is defined, restrict the eligibleModuleIndices to only match prescribed paths
  if (!is.null(simulation$mst$active_path) | !is.null(simulation$mst$path)) {
    # What is the simulee's path so far?
    assignedPath = unique(simulee$test$MODULE_INDEX[!is.na(simulee$test$MODULE_INDEX)])

    # Which of the simulation's configured paths match (start with) the simulee's path so far?
    if (!is.null(simulation$mst$active_path)) {
      pathSet = simulation$mst$ACTIVE_PATH_INDICES
    } else {
      pathSet = simulation$mst$PATH_INDICES
    }
    if (length(assignedPath) < 1) {
      eligiblePaths = pathSet
    } else {
      eligiblePaths = Filter(function(path) {
        all.equal(assignedPath, path[1:length(assignedPath)])
      }, pathSet)
    }

    # Restrict the eligible indices to the eligible paths
    eligibleModuleIndices = intersect(eligibleModuleIndices, vapply(eligiblePaths, function(path) {
      as.integer(path[length(assignedPath)+1])
    }, as.integer(0), USE.NAMES = FALSE))
  }

  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))
  if (assignedItemCount < 1) {
    # No assigned items or theta yet.  Use start theta
    theta = simulation$control$startTheta
  } else {
    theta = simulee$test$THETA[assignedItemCount]
  }
  cutThetas = simulation$control[[paste0("cutThetaStage", stage)]]
  if (!is.null(cutThetas) && length(cutThetas) > 0) {
    cutGroup = isr.multistage.cutGroup(theta, cutThetas)
    eligibleModuleIndices = eligibleModuleIndices[simulation$modules$MODULE_SLOT[eligibleModuleIndices] == cutGroup]
  }

  if (length(eligibleModuleIndices) < 1) {
    message = paste0("no eligible item modules for simulee: ", simulee$id, ", stage: ", stage)
    if(!is.null(simulation$mst)) {
      message = paste0(message, ", panel: ", simulee$panel, ", assignedPath: ", paste(assignedPath, collapse=","))
    }
    stop(message)
    return(NULL)
  }

  # Sort the item modules so the best are first.
  # Module information is the mean information each item in the module.
  itemInf = itemInformation(simulation$item_pars, theta)
  eligibleModuleInf = vapply(simulation$modules$ITEM_INDICES[eligibleModuleIndices], function(moduleItemIndices) {
    return(mean(itemInf[moduleItemIndices]))
  }, as.numeric(0))
  balancedModuleIndices = eligibleModuleIndices[order(eligibleModuleInf, decreasing = TRUE)]

  # Filter the balanced items to a list of a few candidates
  candidateModuleIndices = iec.filter(balancedModuleIndices, simulation)

  # Select a single item from the candidates
  selectedIndex = iec.select(candidateModuleIndices)

  # Return all the items in the module
  selection = list(moduleIndex = selectedIndex,
                   candidateModuleIndices = candidateModuleIndices,
                   iecReleasedAt = simulee$iecReleasedAt)
  if ("PSG_ID" %in% names(simulation$modules)) {
    selection$psgId = simulation$modules$PSG_ID[selectedIndex]
  }
  return(selection)
}

getNormalizedModuleInformation <- function(modules, theta, simulation) {
  # Module information is the mean item information of the items within that module
  itemInf = itemInformation(simulation$item_pars, theta)
  moduleInf = vapply(modules$ITEM_INDICES, function(moduleItemIndices) {
    return(mean(itemInf[moduleItemIndices]))
  }, as.numeric(0))

  # Normalize the information values before returning
  return(moduleInf / max(moduleInf))
}

#' Returns the next items that should be assigned to the simulee, selected
#' from the given simulation.
#'
#' The `optimal` algorithm uses information value of each item as the objective,
#' then uses lpSolveAPI to find a complete test solution that maximizes overall test
#' information while still meeting blueprint constraints.  Then it selects the max
#' information item from the unassigned items in the solution.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test that is being run.
#' @return A list containing the selected moduleIndex, plus other values useful for understanding the selection.
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator")))
#'   simulation$control$solver = list(name = "cbc", external = F, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   simulation$control$solver = list(name = "gurobi", external = F, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   simulee = generateSimuleesByTrueTheta(-2, 10001)()
#'   selection = isr.optimal.select(simulee, simulation)
#' @export
isr.optimal.select <- function (simulee, simulation) {
  if (tr.isTerminated(simulee, simulation)) {
    # Return a null moduleIndex if the test is complete
    return(list(moduleIndex = NULL))
  }
  assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))

  # Setup an empty lp problem object
  lp = solver.initProblem(paste0(ifelse("test_name" %in% names(simulation), simulation$test_name, "catSimulator"), ".slot", assignedItemCount+1))

  # Define primary variables for modules to be selected
  if (("optimalObjective" %in% names(simulation$control)) && (simulation$control$optimalObjective == "none")) {
    # Use zeroes for the primary objective coefficients.  The constraints will control the solution.
    moduleInf = rep(0, nrow(simulation$modules))
  } else {
    # Use normalized information for the primary objective coefficients.  The constraints will control the solution.
    theta = ifelse(assignedItemCount < 1, simulation$control$startTheta, simulee$test$THETA[assignedItemCount])
    moduleInf = getNormalizedModuleInformation(simulation$modules, theta, simulation)
    # If information weights are configured, weight the module information
    infCoefWeight = cbm.wpm.infWeight(assignedItemCount, simulation)
    if (!is.na(infCoefWeight)) {
      moduleInf = moduleInf * (infCoefWeight / simulation$control$minItems)
    }
  }
  solver.addPrimaryVariables(lp, simulation$modules, moduleInf)

  # Add constraints for test length, and to require modules that were already assigned
  solver.addTestLengthConstraint(lp, simulation$modules, simulation$control$minItems)
  solver.addRequiredModulesConstraint(lp, simulation$modules, unique(simulee$test$MODULE_INDEX[!is.na(simulee$test$MODULE_INDEX)]))

  # Add enemy code constraints if simulation$modules$ENEMY_CODES is defined
  if ("ENEMY_CODES" %in% names(simulation$modules)) {
    # Get a list of all unique enemy codes
    enemyCodes = sort(unique(unlist(simulation$modules$ENEMY_CODES, recursive = T)))
    if (length(enemyCodes) > 0) {
      solver.addEnemyCodeConstraints(lp, simulation$modules, enemyCodes)
    }
  }

  # Add content constraints if simulation$constraints$content is defined
  if (("constraints" %in% names(simulation)) && ("content" %in% names(simulation$constraints))) {
    simulation$constraints$content$MIN_ITEMS = floor(simulation$constraints$content$LOWER * simulation$control$minItems)
    simulation$constraints$content$MAX_ITEMS = ceiling(simulation$constraints$content$UPPER * simulation$control$minItems)
    # Use normalized cons weight for the soft constraint penalty coefficients
    simulation$constraints$content$PENALTY = -simulation$constraints$content$WEIGHT / max(simulation$constraints$content$WEIGHT)
    # If constraint weights are configured, weight the constraint penalties
    consCoefWeight = cbm.wpm.consWeight(assignedItemCount, simulation)
    if (!is.na(consCoefWeight)) {
      simulation$constraints$content$PENALTY = simulation$constraints$content$PENALTY * (consCoefWeight / simulation$control$minItems)
    }
    solver.addContentConstraints(lp, simulation$modules, simulation$constraints$content)
  }

  # If add passage constraints
  if (("constraints" %in% names(simulation)) && ("passage" %in% names(simulation$constraints))) {
    simulation$constraints$passage$MIN_ITEMS = floor(simulation$constraints$passage$LOWER * simulation$control$minItems)
    simulation$constraints$passage$MAX_ITEMS = ceiling(simulation$constraints$passage$UPPER * simulation$control$minItems)
    solver.addPassageConstraints(lp, simulation$modules, simulation$constraints$passage, simulation$control$minPassages, simulation$control$maxPassages)
  }

  # If simulation$tif is defined, add tif constraints
  if ("tif" %in% names(simulation)) {
    simulation$tif$MODULE_INF = lapply(simulation$tif$THETA, function(theta) {
      # theta = 0
      itemInfAtTheta = itemInformation(simulation$item_pars, theta)
      moduleInfAtTheta = vapply(simulation$modules$ITEM_INDICES, function(moduleItemIndices) {
        return(mean(itemInfAtTheta[moduleItemIndices]))
      }, as.numeric(0))
      return(moduleInfAtTheta / simulation$control$minItems)
    })
    solver.addTIFConstraints(lp, simulation$modules, simulation$tif)
  }

  # Solve and get solution
  tryCatch({
    solution = solver.solveAndGetSolution(lp, simulation$modules, simulation$control$solver)
  },
  error = function(err) {
    stop("no solution found for simulee: ", simulee$id, " slot: ", (assignedItemCount+1), " assignedItemIds: ", paste(simulee$test$ITEM_INDEX, collapse=","), ", solver: ", err$message)
    return(NULL)
  },
  finally = {
    solver.disposeProblem(lp)
  })

  # Reality check! Make sure the solution is the correct length and results in the correct test length
  if (length(solution) != nrow(simulation$modules)) {
    stop("invalid solution! simulee: ", simulee$id, " slot: ", (assignedItemCount+1), " numModules: ", nrow(simulation$modules), " solutionLength: ", length(solution), ", solution: ", paste(solution, collapse=","))
  }
  solutionTestLength = sum(vapply(simulation$modules$ITEM_INDICES[solution > 0], function(moduleItemInd) {
    return(length(moduleItemInd))
  }, as.integer(0)))
  if (solutionTestLength != simulation$control$minItems) {
    stop("invalid solution! simulee: ", simulee$id, " slot: ", (assignedItemCount+1), " testLength: ", simulation$control$minItems, " solutionTestLength: ", solutionTestLength, ", solution: ", paste(solution, collapse=","))
  }

  # The eligible modules for selection is the un-assigned portion of the solution.
  eligibleModuleIndices = setdiff(which(solution == 1), unique(simulee$test$MODULE_INDEX[1:assignedItemCount]))

  # Filter the eligible modules based on constraint control: requiredAll, requiredAny and blocked
  incompletePsgId = isr.checkIncompletePassage(assignedItemCount, eligibleModuleIndices, simulee, simulation)
  if ("constraintControl" %in% names(simulation$control)) {
    eligibleModuleIndices = isr.filterByConstraintControl(eligibleModuleIndices, incompletePsgId, assignedItemCount+1, simulation)
  }
  # If the prev item belongs to a passage that is incomplete, filter the eligible modules to only include the incomplete passage
  if (!is.null(incompletePsgId)) {
    # Exclude every passage except the incomplete passage.
    # Select only from that passage until it is completed
    excludeModule = is.na(simulation$modules$PSG_ID[eligibleModuleIndices]) | simulation$modules$PSG_ID[eligibleModuleIndices] != incompletePsgId
    if (any(excludeModule)) {
      eligibleModuleIndices = eligibleModuleIndices[!excludeModule]
    }
  }
  # If no modules are eligible for the current slot despite an optimal solution being found, stop with an error.
  if (length(eligibleModuleIndices) < 1) {
    stop("no eligible item modules for simulee: ", simulee$id, " slot ", (assignedItemCount+1), " with solution: ", paste(solution, collapse=","))
    return(NULL)
  }

  # Sort the remaining eligibleModuleIndices by information value, so the highest information items are first
  balancedModuleIndices = eligibleModuleIndices[order(moduleInf[eligibleModuleIndices], decreasing = TRUE)]

  # Filter the balanced item modules to a list of a few IEC candidates
  candidateModuleIndices = iec.filter(balancedModuleIndices, simulation)

  # Select a single item module from the IEC candidates
  selectedIndex = iec.select(candidateModuleIndices)
  selection = list(moduleIndex = selectedIndex,
                   candidateModuleIndices = selectedIndex)
  if ("PSG_ID" %in% names(simulation$modules)) {
    selection$psgId = simulation$modules$PSG_ID[selectedIndex]
  }
  return(selection)
}
