# source("R/input.R")
# source("R/itemResponseTheory.R")
# source("R/itemSelectionRule.R")
# source("R/abilityEstimator.R")
# source("R/result.R")

#' Return a generator function that will generate one simulee each time it is called.
#' The true theta of the generated simulees will follow a normal distribution.
#' Once the specified number of simulees has been reached, no more simulees will be generated.
#'
#' @param numSimulees The number of simulees that should be generated.
#' @param mean The mean of the true theta for the generated simulees.
#' @param sd The standard deviation of the true theta for the generated simulees.
#' @param randomSeeds A vector of integers. The source of random seeds for each simulee.
#' @return A generator function that will generate one simulee each time it is called.
#' @examples
#'   simuleeGenerator = generateSimuleesByNormal(numSimulees = 5, mean = 0.5, sd = 1.2, randomSeeds = 10001:10003)
#'   simulee = simuleeGenerator()
#' @export
generateSimuleesByNormal <- function (numSimulees, mean = 0, sd = 1, randomSeeds = NULL, min_value = -Inf, max_value = Inf) {
  counter <- 0
  if (is.null(randomSeeds)) {
    randomSeeds <- runif(numSimulees, 10000, .Machine$integer.max - 3000)
  }
  # trueThetas <- as.double(rnorm(numSimulees, mean, sd))

  # Return a generator function that will create and return one simulee per call
  # The generator function will return NULL once it reaches the requested number of simulees (numSimulees)
  return(function() {
    if (counter >= numSimulees) {
      return(NULL)
    }
    counter <<- counter + 1

    # Return one simulee, using the next randomSeed value to generate a trueTheta
    randomSeed = randomSeeds[((counter - 1) %% length(randomSeeds)) + 1]
    set.seed(randomSeed)
    return(list(
      id = sprintf("sim%05d", counter),
      randomSeed = randomSeed,
      trueTheta = rnorm(1, mean, sd) %>% as.double() %>% pmin(max_value) %>% pmax(min_value)
    ))
  })
}

#' Return a generator function that will generate one simulee each time it is called.
#' The true theta of the generated simulees will come from the next value in the given vector.
#' Once all true thetas have been consumed, no more simulees will be generated.
#'
#' @param trueThetas A vector of doubles. The source of true thetas for each simulee.
#' @param randomSeeds A vector of integers. The source of random seeds for each simulee.
#' @return A generator function that will generate one simulee each time it is called.
#' @examples
#'   simuleeGenerator = generateSimuleesByTrueTheta(-2:2, 10001:10003)
#'   simulee = simuleeGenerator()
#' @export
generateSimuleesByTrueTheta <- function (trueThetas, randomSeeds = NULL) {
  counter <- 0
  if (is.null(randomSeeds)) {
    randomSeeds <- runif(length(trueThetas), 10000, .Machine$integer.max - 3000)
  }

  # Return a generator function that will create and return one simulee per call
  # The generator function will return NULL once it reaches the requested number of simulees (length(trueThetas))
  return(function() {
    if (counter >= length(trueThetas)) {
      return(NULL)
    }
    counter <<- counter + 1

    # Return one simulee, using the next trueTheta and randomSeed value
    return(list(
      id = sprintf("sim%03d", counter),
      randomSeed = randomSeeds[((counter - 1) %% length(randomSeeds)) + 1],
      trueTheta = as.double(trueThetas[counter])
    ))
  })
}

#' initSimulation
#'
#' @examples
#'   options(warn=-1)
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   simulation = initSimulation(simulation)
#' @import tidyverse
#' @export
initSimulation <- function (simulation) {
  require(tidyverse)
  # Generate 'item_pars' matrix including only the IRT params portion of the itempool
  if(!"item_pars" %in% names(simulation)) {
    simulation$item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
    dimnames(simulation$item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
  }

  # Generate modules
  if ((!"modules" %in% names(simulation)) || (("generateModules" %in% names(simulation$control)) && simulation$control$generateModules)) {
    # Create an empty "modules" tibble, if necessary
    if (!"modules" %in% names(simulation)) {
      simulation$modules = tibble(
        MODULE_ID = character(),
        ITEM_IDS = list()
      )
    }

    # Generate a row for each item which does not already belong to a module.
    orphanItem = rep(TRUE, nrow(simulation$itempool))
    if (nrow(simulation$modules) > 0) {
      for (i in 1:nrow(simulation$modules)) {
        orphanItem[match(simulation$modules$ITEM_IDS[[i]], simulation$itempool$ITEM_ID)] = FALSE
      }
    }
    orphanItemInds = which(orphanItem)
    if (length(orphanItemInds) > 0) {
      simulation$modules = add_row(simulation$modules,
        MODULE_ID = simulation$itempool$ITEM_ID[orphanItemInds],
        ITEM_IDS = lapply(orphanItemInds, function(orphanItemInds) { return(c(simulation$itempool$ITEM_ID[orphanItemInds])) })
      )
    }
  }

  # Check if all modules are one-item modules.
  # Set a flag that can be used to enable "one-item only" performance optimizations
  if (!("oneItemModules" %in% names(simulation$control))) {
    simulation$control$oneItemModules = !any(vapply(simulation$modules$ITEM_IDS, length, as.integer(0)) > 1)
  }

  # Add 'ITEM_INDICES' to the modules, indicating the items associated with each module
  if(!"ITEM_INDICES" %in% names(simulation$modules)) {
    simulation$modules$ITEM_INDICES = lapply(simulation$modules$ITEM_IDS, function(itemIds) {
      match(itemIds, simulation$itempool$ITEM_ID)
    })
  }

  # Add 'PSG_ID' to the modules, indicating the passage associated with the items in each module
  if ("PSG_ID" %in% names(simulation$itempool)) {
    # Create an empty "PSG_ID" column, if necessary
    if (!"PSG_ID" %in% names(simulation$modules)) {
      simulation$modules$PSG_ID = as.character(NA)
    }
    noPsgIdModuleInds = which(is.na(simulation$modules$PSG_ID))
    if (any(noPsgIdModuleInds)) {
      simulation$modules$PSG_ID[noPsgIdModuleInds] = vapply(noPsgIdModuleInds, function(moduleInd) {
        # moduleInd = 1
        itemPsgIds = unique(simulation$itempool$PSG_ID[simulation$modules$ITEM_INDICES[[moduleInd]]])
        if (length(itemPsgIds) < 1) {
          return(NA)
        } else if (length(itemPsgIds) == 1) {
          return(itemPsgIds[[1]])
        } else {
          stop("module ", simulation$modules$MODULE_ID[[moduleInd]], " has too many PSG_IDs: ", paste(itemPsgIds, collapse=","))
          return(NA)
        }
      }, as.character(0))
    }
  }

  # Add 'ENEMY_CODES' to the modules, indicating modules with overlapping or conflicting items
  # This field is currently only needed by optimal item selection rules
  if((simulation$control$itemSelectionRule == "optimal") && (!"ENEMY_CODES" %in% names(simulation$modules))) {
    # initialize an empty column
    simulation$modules$ENEMY_CODES = list(character(0))
    # inspect all modules with >1 item.
    multiItemModuleInds = which(vapply(simulation$modules$ITEM_INDICES, length, as.integer(0)) > 1)
    for (moduleInd in multiItemModuleInds) {
      # moduleInd = 3
      otherModuleInds = seq_len(nrow(simulation$modules))
      otherModuleInds = otherModuleInds[otherModuleInds != moduleInd]
      for (otherModuleInd in otherModuleInds) {
        # otherModuleInd = 4
        overlapItemIds = intersect(simulation$modules$ITEM_IDS[[moduleInd]], simulation$modules$ITEM_IDS[[otherModuleInd]])
        # if any ids overlap, add the current MODULE_ID as an enemy code in both modules
        if (length(overlapItemIds) > 1) {
          simulation$modules$ENEMY_CODES[[moduleInd]] = sort(unique(c(simulation$modules$ENEMY_CODES[[moduleInd]], simulation$modules$MODULE_ID[[moduleInd]])))
          simulation$modules$ENEMY_CODES[[otherModuleInd]] = sort(unique(c(simulation$modules$ENEMY_CODES[[otherModuleInd]], simulation$modules$MODULE_ID[[moduleInd]])))
        }
      }
    }
  }

  if("constraints" %in% names(simulation)) {
    if("content" %in% names(simulation$constraints)) {
      # Add 'TYPE' to the content constraints if missing, or fill in missing values if defined
      if (!"TYPE" %in% names(simulation$constraints$content)) {
        simulation$constraints$content$TYPE = rep("H", nrow(simulation$constraints$content))
      } else {
        simulation$constraints$content$TYPE[is.na(simulation$constraints$content$TYPE)] = "H"
      }

      # Add 'CONS_INDICES' to the itempool, indicating the content constraints associated with each item
      if(!"CONS_INDICES" %in% names(simulation$itempool)) {
        simulation$itempool$CONS_INDICES = lapply(simulation$itempool$CONS_IDS, function(consIds) {
          match(consIds, simulation$constraints$content$CONS_ID)
        })
      }

      # Add 'ITEM_INDICES' to the content constraints, indicating the items associated with each content constraint
      if(!"ITEM_INDICES" %in% names(simulation$constraints$content)) {
        simulation$constraints$content$ITEM_INDICES = lapply(1:nrow(simulation$constraints$content), function(consIndex) {
          return(which(vapply(simulation$itempool$CONS_INDICES, function(itemConsIndices) {
            return(consIndex %in% itemConsIndices)
          }, as.logical(0))))
        })
      }

      # Add 'MODULE_LENGTH' to the content constraints, indicating how many items in each module associated with each content constraint
      # This field is currently only needed by optimal item selection rules
      if((simulation$control$itemSelectionRule == "optimal") && (!"MODULE_LENGTH" %in% names(simulation$constraints$content))) {
        simulation$constraints$content$MODULE_LENGTH = lapply(1:nrow(simulation$constraints$content), function(consIndex) {
          # consIndex = 1
          moduleLength = vapply(1:nrow(simulation$modules), function(moduleIndex) {
            # moduleIndex = 2
            length(intersect(simulation$constraints$content$ITEM_INDICES[[consIndex]], simulation$modules$ITEM_INDICES[[moduleIndex]]))
          }, as.integer(0))
        })
      }
    }

    if("passage" %in% names(simulation$constraints)) {
      # Add 'ITEM_INDICES' to the passage constraints, indicating the items associated with each passage
      if(!"ITEM_INDICES" %in% names(simulation$constraints$passage)) {
        simulation$constraints$passage$ITEM_INDICES = lapply(simulation$constraints$passage$PSG_ID, function(psgId) {
          # psgId = simulation$constraints$passage$PSG_ID[[1]]
          return(which(simulation$itempool$PSG_ID == psgId))
        })
      }

      # Add 'MODULE_LENGTH' to the passage constraints, indicating how many items in each module associated with each passage
      # This field is currently only needed by optimal item selection rules
      if((simulation$control$itemSelectionRule == "optimal") && (!"MODULE_LENGTH" %in% names(simulation$constraints$passage))) {
        simulation$constraints$passage$MODULE_LENGTH = lapply(1:nrow(simulation$constraints$passage), function(psgIndex) {
          # psgIndex = 1
          moduleLength = vapply(1:nrow(simulation$modules), function(moduleIndex) {
            # moduleIndex = 2
            length(intersect(simulation$constraints$passage$ITEM_INDICES[[psgIndex]], simulation$modules$ITEM_INDICES[[moduleIndex]]))
          }, as.integer(0))
        })
      }
    }
  }

  # Add 'MODULE_INDICES' to the panels, indicating the modules associated with each panel
  if (("panels" %in% names(simulation)) && !("MODULE_INDICES" %in% names(simulation$panels))) {
    simulation$panels$MODULE_INDICES = lapply(simulation$panels$MODULE_IDS, function(moduleIds) {
      match(moduleIds, simulation$modules$MODULE_ID)
    })
  }

  # Add 'PATH_INDICES' to mst$path
  if ("mst" %in% names(simulation)) {
    if (("active_path" %in% names(simulation$mst)) && !("ACTIVE_PATH_INDICES" %in% names(simulation$mst))) {
      simulation$mst$ACTIVE_PATH_INDICES = lapply(simulation$mst$active_path, function(path) {
        match(path, simulation$modules$MODULE_ID)
      })
    }
    if (("path" %in% names(simulation$mst)) && !("PATH_INDICES" %in% names(simulation$mst))) {
      simulation$mst$PATH_INDICES = lapply(simulation$mst$path, function(path) {
        match(path, simulation$modules$MODULE_ID)
      })
    }
  }

  return(simulation)
}

#' initSimulee
#'
#' @examples
#'   simulation = initSimulation(readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator")))
#'   simulee = generateSimuleesByTrueTheta(-2, 10001)()
#'   simulee = initSimulee(simulee, simulation)
#' @import tidyverse
#' @export
initSimulee <- function (simulee, simulation) {
  # Initialize the simulee... pre-allocate room for the max test length
  require(tidyverse)
  simulee$test = tibble(
    MODULE_INDEX = as.integer(rep(NA, simulation$control$maxItems)),
    ITEM_INDEX = as.integer(NA),
    # ITEM_DIFFICULTY = as.double(NA),
    SCORE = as.integer(NA),
    THETA = as.double(NA),
    CSEM = as.double(NA),
    ESTIMATOR = as.character(NA)
  )
  if ("PSG_ID" %in% names(simulation$modules)) {
    simulee$test = add_column(simulee$test, .after = "MODULE_INDEX",
      PSG_ID = as.character(NA)
    )
  }

  simulee$blockedItemIndices = integer(0)
  simulee$blockedPsgIds = character(0)

  # If panels are defined, assign a panel to the simulee
  if ("panels" %in% names(simulation)) {
    panelWeight = NULL
    if ("PANEL_WEIGHT" %in% names(simulation$panels)) {
      panelWeight = simulation$panels$PANEL_WEIGHT
    }
    simulee$panel <- sample(simulation$panels$PANEL_ID, size = 1, prob = panelWeight)
  }

  return(simulee)
}

#' Generate simulees and run them through the given simulation.
#'
#' @param simulation An object defining the test to be run.
#' @param simuleeGenerator A generator function that will generate one simulee each time it is called.
#' @param progressCallback A callback function that takes a single parameter, the number of completed simulees.
#' @return A data.frame of generated simulees that completed the simulation.
#' @examples
#'   options(warn=-1)
#'   simulation = read_simulation("inst/input_files/many_panels/test.json")
#'   #saveRDS(simulation, "inst/example/internal/many_panels.rds")
#'   simulation$control$solver = list(name = "cbc", external = T, mipGap = 0.0001, timeout = 1000, verbose = F)
#'   simuleeGenerator = generateSimuleesByTrueTheta(-2:2, 10001:10005)
#'   result = runSimulation(simulation, simuleeGenerator)
#' @import tidyverse
#' @export
runSimulation <- function(simulation, simuleeGenerator, progressCallback = NULL, showProgress = F) {
  if (is.null(simulation)) {
    return(NULL)
  }

  if (showProgress)
    cat("Simulation start at ", format(Sys.time(), "%X"), "\n")

  simulation = initSimulation(simulation)

  require(tidyverse)
  result = list(output = tibble(
    SIM_ID = character(),
    SEED = integer(),
    TRUE_THETA = double(),
    MODULE_ID = character(),
    ITEM_ID = character(),
    # ITEM_DIFFICULTY = double(),
    SCORE = integer(),
    THETA = double(),
    CSEM = double(),
    ESTIMATOR = character()
  ))
  if ("PSG_ID" %in% names(simulation$modules)) {
    result$output = add_column(result$output, .after = "MODULE_ID",
      PSG_ID = character()
    )
  }

  completedCounter = 0
  while (!is.null(simulee <- simuleeGenerator())) {
    # Run the simulee
    completedSimulee = runSimulee(simulee, simulation)

    # Append the completed rows of the simulee$test to the combined result$output
    completedRows = 1:sum(!is.na(completedSimulee$test$ITEM_INDEX))
    result$output = add_row(result$output,
      SIM_ID = completedSimulee$id,
      SEED = completedSimulee$randomSeed,
      TRUE_THETA = completedSimulee$trueTheta,
      MODULE_ID = simulation$modules$MODULE_ID[completedSimulee$test$MODULE_INDEX[completedRows]],
      ITEM_ID = simulation$itempool$ITEM_ID[completedSimulee$test$ITEM_INDEX[completedRows]],
      # ITEM_DIFFICULTY = completedSimulee$test$ITEM_DIFFICULTY[completedRows],
      SCORE = completedSimulee$test$SCORE[completedRows],
      THETA = completedSimulee$test$THETA[completedRows],
      CSEM = completedSimulee$test$CSEM[completedRows],
      ESTIMATOR = completedSimulee$test$ESTIMATOR[completedRows]
    )
    if ("PSG_ID" %in% names(simulation$modules)) {
      result$output$PSG_ID[result$output$SIM_ID == completedSimulee$id] = completedSimulee$test$PSG_ID[completedRows]
    }
    completedCounter = completedCounter + 1

    # If there is a progress callback, report progress; otherwise, printout progress on console
    if (!is.null(progressCallback)) {
      progressCallback(completedCounter)
    }
    if (showProgress)
      if (completedCounter %% 10 == 1)
        cat(".")

  }

  result$output <- result$output %>%
    left_join(simulation$itempool %>% select("ITEM_ID", starts_with("PAR_"))) %>%
    select("SIM_ID", "SEED", "TRUE_THETA", "MODULE_ID", "ITEM_ID", starts_with("PAR_"), everything())

  if (showProgress)
    cat("Simulation end at ", format(Sys.time(), "%X"), "\n")

  result$irt = getResult.IRT(simulation, result$output)
  result$isr = getResult.ISR(simulation, result$output)
  result$iec = getResult.IEC(simulation, result$output)

  if (showProgress)
    cat("Rsult processing finished at ", format(Sys.time(), "%X"), "\n")

  return(result)
}

#' Run a single simulee through the given simulation.
#'
#' @param simulee A data.frame containing at least one simulee, the first row will be used.
#' @param simulation An object defining the test to be run.
#' @return An updated version of the simulee data.frame, with the test completed.
#' @examples
#'   options(warn=-1)
#'   simulation = initSimulation(read_simulation("inst/input_files/many_panels/test.json"))
#'   simulation$control$solver = list(name = "cbc", external = T, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   simulee = generateSimuleesByTrueTheta(-2, 10001)()
#'   simuleeOut = runSimulee(simulee, simulation)
#' @import tidyverse
#' @export
runSimulee <- function(simulee, simulation) {
  require(tidyverse)

  # print(simulee$id)
  simulee = initSimulee(simulee, simulation)

  # Select items, answer/score items, repeat... until the test is finished.
  stage = 0
  repeat {
    stage = stage + 1
    assignedItemCount = sum(!is.na(simulee$test$ITEM_INDEX))

    # Set the random seed for each selection
    set.seed(simulee$randomSeed + (10 * assignedItemCount) + ifelse(assignedItemCount < 1, 0, simulee$test$SCORE[assignedItemCount]))

    # Try to select more items
    selection = isr.select(simulee, simulation, stage)
    # print(selection)
    if (is.null(selection)) {
      return(NULL)
    }
    if (is.null(selection$moduleIndex)) {
      # If no items were returned, the test is finished.
      break
    }

    # Assign the selected items to the simulee
    firstRowToAssign = assignedItemCount+1
    lastRowToAssign = assignedItemCount+length(simulation$modules$ITEM_INDICES[[selection$moduleIndex]])
    simulee$test$MODULE_INDEX[firstRowToAssign:lastRowToAssign] = selection$moduleIndex
    simulee$test$ITEM_INDEX[firstRowToAssign:lastRowToAssign] = simulation$modules$ITEM_INDICES[[selection$moduleIndex]]
    # simulee$test$ITEM_DIFFICULTY[firstRowToAssign:lastRowToAssign] = simulation$itempool$PAR_2[simulee$test$ITEM_INDEX[firstRowToAssign:lastRowToAssign]]

    # Block all selected and candidate items, and all selected passage ids, so they are not eligible for future selection
    if ("candidateModuleIndices" %in% names(selection)) {
      simulee$blockedItemIndices = c(simulee$blockedItemIndices, simulation$modules$ITEM_INDICES[selection$candidateModuleIndices], recursive = TRUE)
    }
    simulee$blockedItemIndices = sort(unique(c(simulee$blockedItemIndices, simulation$modules$ITEM_INDICES[selection$moduleIndex], recursive = TRUE)))
    if (("PSG_ID" %in% names(simulation$modules)) && ("psgId" %in% names(selection))) {
      simulee$test$PSG_ID[firstRowToAssign:lastRowToAssign] = selection$psgId
      simulee$blockedPsgIds = sort(unique(c(simulee$blockedPsgIds, selection$psgId, recursive = TRUE)))
    }

    # Track if and where IEC blocked items were released. Do not clear the blockedItemIndices, we keep collecting but ignore them.
    if (("iecReleasedAt" %in% names(selection)) && !("iecReleasedAt" %in% names(simulee))) {
      simulee$iecReleasedAt = selection$iecReleasedAt
    }

    # Answer/score each item that was selected.
    simulee$test$SCORE[firstRowToAssign:lastRowToAssign] = generateScores(simulation$item_pars[simulee$test$ITEM_INDEX[firstRowToAssign:lastRowToAssign],,drop=FALSE], simulee$trueTheta)

    # Calculate theta/csem based on all assigned items/scores
    item_parsToEstimate = simulation$item_pars[simulee$test$ITEM_INDEX[1:lastRowToAssign],,drop=FALSE]
    scoresToEstimate = simulee$test$SCORE[1:lastRowToAssign]
    abilityEstimate = estimateAbility(item_parsToEstimate, scoresToEstimate, simulation)
    simulee$test$THETA[lastRowToAssign] = abilityEstimate$theta
    simulee$test$CSEM[lastRowToAssign] = abilityEstimate$csem
    simulee$test$ESTIMATOR[lastRowToAssign] = abilityEstimate$abilityEstimator
  }

  return (simulee)
}
