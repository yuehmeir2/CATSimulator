#' Destroys the given lp object, cleaning up all associated resources
#'
#' @import glpkAPI
solver.disposeProblem <- function (lp) {
  require(glpkAPI)
  delProbGLPK(lp)
}

#' Returns a new lp object
#'
#' @import glpkAPI
solver.initProblem <- function (probname = "CATSimulator") {
  require(glpkAPI)
  lp = initProbGLPK()
  setProbNameGLPK(lp, pname = probname)
  return(lp)
}

#' Adds primary variables to the given lp, creating a column for each module in the pool
#'
#' @examples
#'   modules = simulation$modules
#' @import glpkAPI
solver.addPrimaryVariables <- function (lp, modules, moduleInf) {
  # Create columns for each module being added
  firstModuleInd = addColsGLPK(lp, nrow(modules))
  moduleInd = seq.int(from = firstModuleInd, length.out = nrow(modules))

  # Each primary column is a binary variable using the module information as a coefficient
  setColsNamesGLPK(lp, moduleInd, cnames = modules$MODULE_ID)
  setColsKindGLPK(lp, moduleInd, kind = rep(GLP_BV, nrow(modules))) # GLP_BV = binary variables
  # setColsBndsGLPK(lp, moduleInd, type = rep(GLP_DB, nrow(modules)), lb = rep(0, nrow(modules)), ub = rep(1, nrow(modules))) # GLP_DB = double bound variable (>= and <=)
  setObjCoefsGLPK(lp, moduleInd, obj_coef = moduleInf)

  # This function must be called so that 'findColGLPK' and 'findRowGLPK' will work
  createIndexGLPK(lp)
}

#' Adds test length constraints to the given lp, based on the given test length
#'
#' @import glpkAPI
solver.addTestLengthConstraint <- function (lp, modules, testLength) {
  moduleLength = vapply(modules$ITEM_INDICES, function(moduleItemInd) {
    return(length(moduleItemInd))
  }, as.integer(0))

  # Create a row for the constraint being added
  rowInd = addRowsGLPK(lp, 1)
  firstModuleInd = findColGLPK(lp, cname = modules$MODULE_ID[[1]])
  moduleInd = seq.int(from = firstModuleInd, length.out = nrow(modules))

  setRowNameGLPK(lp, rowInd, rname = "TestLength")
  setMatRowGLPK(lp, rowInd, len = length(moduleInd), ind = moduleInd, val = moduleLength)
  setRowBndGLPK(lp, rowInd, type = GLP_FX, lb = testLength, ub = testLength) # GLP_FX = fixed constraint
}

#' Adds required module constraints to the given lp, based on the given list of modules already assigned
#'
#' @import glpkAPI
solver.addRequiredModulesConstraint <- function (lp, modules, reqModuleInd) {
  if (length(reqModuleInd) > 0) {
    # Create a row for the constraint being added
    rowInd = addRowsGLPK(lp, 1)
    firstModuleInd = findColGLPK(lp, cname = modules$MODULE_ID[[1]])
    # Use the firstModuleInd as an offset to translate the modules ind to solver column ind
    reqModuleInd = firstModuleInd + reqModuleInd - 1

    # Primary columns are always first, so we should be able to assume reqModuleIndices do not need an offset
    setRowNameGLPK(lp, rowInd, rname = "Required")
    setMatRowGLPK(lp, rowInd, len = length(reqModuleInd), ind = reqModuleInd, val = rep(1, length(reqModuleInd)))
    setRowBndGLPK(lp, rowInd, type = GLP_FX, lb = length(reqModuleInd), ub = length(reqModuleInd)) # GLP_FX = fixed constraint
  }
}

#' Adds enemy code constraints to the given lp, based on the given modules
#'
#' @examples
#'   modules = simulation$modules
#'   enemyCodes = sort(unique(unlist(modules$ENEMY_CODES, recursive = T)))
#' @import glpkAPI
solver.addEnemyCodeConstraints <- function (lp, modules, enemyCodes) {
  # Create a row for each enemy code being added
  firstRowInd = addRowsGLPK(lp, length(enemyCodes))
  firstModuleInd = findColGLPK(lp, cname = modules$MODULE_ID[[1]])
  moduleInd = seq.int(from = firstModuleInd, length.out = nrow(modules))

  for (i in seq_len(length(enemyCodes))) {
    # i = 1
    rowInd = firstRowInd + i - 1
    consInd = moduleInd[which(vapply(modules$ENEMY_CODES, function(moduleEnemyCodes) { return(enemyCodes[[i]] %in% moduleEnemyCodes) }, as.logical(0), USE.NAMES = FALSE))]

    setRowNameGLPK(lp, rowInd, rname = paste0("Enemy.", enemyCodes[[i]]))
    setMatRowGLPK(lp, rowInd, len = length(consInd), ind = consInd, val = rep(1, length(consInd)))
    setRowBndGLPK(lp, rowInd, type = GLP_UP, lb = 1, ub = 1) # GLP_UP = upper bound constraint (<=)
  }
}

#' Adds content constraints to the given lp, based on the given content constraints table
#'
#' @examples
#'   modules = simulation$modules
#'   contentCons = simulation$constraints$content
#' @import glpkAPI
solver.addContentConstraints <- function (lp, modules, contentCons) {
  # Create rows for the constraints being added
  firstRowInd = addRowsGLPK(lp, nrow(contentCons) * 2)
  firstModuleInd = findColGLPK(lp, cname = modules$MODULE_ID[[1]])
  moduleInd = seq.int(from = firstModuleInd, length.out = nrow(modules))

  for (i in seq_len(nrow(contentCons))) {
    # i = 1
    # modules$MODULE_ID[which(contentCons$MODULE_LENGTH[[i]] != 0)]

    # Check for empty constraint
    if (all(contentCons$MODULE_LENGTH[[i]] == 0)) {
      if (contentCons$LOWER[[i]] > 0) {
        # Impossible test if there are zero items to meet this requirement.
        stop("infeasible constraint: '", contentCons$CONS_ID[[i]], "' has no associated items")
      }
      # No items is fine if no items are required!  Just skip.
      break
    }

    # Add lower bound constraint
    consInd = moduleInd[which(contentCons$MODULE_LENGTH[[i]] != 0)]
    consCoef = contentCons$MODULE_LENGTH[[i]][which(contentCons$MODULE_LENGTH[[i]] != 0)]
    if (contentCons$TYPE[[i]] == "S") {
      # Create a new integer variable to indicate how far below the soft lower bound
      penColInd = addColsGLPK(lp, 1)
      setColNameGLPK(lp, penColInd, cname = paste0(contentCons$CONS_ID[[i]], ".LB"))
      setColKindGLPK(lp, penColInd, kind = GLP_IV) # GLP_IV = integer variables
      setColBndGLPK(lp, penColInd, type = GLP_LO, lb = 0, ub = 0) # GLP_LO = lower bound variable (>=)
      setObjCoefGLPK(lp, penColInd, obj_coef = contentCons$PENALTY[[i]])

      consInd = c(consInd, penColInd)
      consCoef = c(consCoef, 1)
    }

    rowInd = firstRowInd + (i - 1) * 2
    setRowNameGLPK(lp, rowInd, rname = paste0(contentCons$CONS_ID[[i]], ".LB"))
    setMatRowGLPK(lp, rowInd, len = length(consInd), ind = consInd, val = consCoef)
    setRowBndGLPK(lp, rowInd, type = GLP_LO, lb = contentCons$MIN_ITEMS[[i]], ub = contentCons$MIN_ITEMS[[i]]) # GLP_LO = lower bound constraint (>=)

    # Add upper bound constraint
    consInd = moduleInd[which(contentCons$MODULE_LENGTH[[i]] != 0)]
    consCoef = contentCons$MODULE_LENGTH[[i]][which(contentCons$MODULE_LENGTH[[i]] != 0)]
    if (contentCons$TYPE[[i]] == "S") {
      # Create a new integer variable to indicate how far above the soft upper bound
      penColInd = addColsGLPK(lp, 1)
      setColNameGLPK(lp, penColInd, cname = paste0(contentCons$CONS_ID[[i]], ".UB"))
      setColKindGLPK(lp, penColInd, kind = GLP_IV) # GLP_IV = integer variables
      setColBndGLPK(lp, penColInd, type = GLP_LO, lb = 0, ub = 0) # GLP_LO = lower bound variable (>=)
      setObjCoefGLPK(lp, penColInd, obj_coef = contentCons$PENALTY[[i]])

      consInd = c(consInd, penColInd)
      consCoef = c(consCoef, -1)
    }

    rowInd = firstRowInd + (i - 1) * 2 + 1
    setRowNameGLPK(lp, rowInd, rname = paste0(contentCons$CONS_ID[[i]], ".UB"))
    setMatRowGLPK(lp, rowInd, len = length(consInd), ind = consInd, val = consCoef)
    setRowBndGLPK(lp, rowInd, type = GLP_UP, lb = contentCons$MAX_ITEMS[[i]], ub = contentCons$MAX_ITEMS[[i]]) # GLP_UP = upper bound constraint (<=)
  }
}

#' Adds passage constraints to the given lp, based on the given passage constraints table
#'
#' @examples
#'   modules = simulation$modules
#'   passageCons = simulation$constraints$passage
#'   minPassages = simulation$control$minPassages
#'   maxPassages = simulation$control$maxPassages
#' @import glpkAPI
solver.addPassageConstraints <- function (lp, modules, passageCons, minPassages = NULL, maxPassages = NULL) {
  # Create rows for the constraints being added
  firstRowInd = addRowsGLPK(lp, nrow(passageCons) * 2)
  firstModuleInd = findColGLPK(lp, cname = modules$MODULE_ID[[1]])
  moduleInd = seq.int(from = firstModuleInd, length.out = nrow(modules))

  psgColInd = integer()
  for (i in seq_len(nrow(passageCons))) {
    # i = 1
    # modules$MODULE_ID[which(passageCons$MODULE_LENGTH[[i]] != 0)]

    # Create a new binary variable to indicate whether the passage has been selected
    newPsgColInd = addColsGLPK(lp, 1)
    setColNameGLPK(lp, newPsgColInd, cname = passageCons$PSG_ID[[i]])
    setColKindGLPK(lp, newPsgColInd, kind = GLP_BV) # GLP_BV = binary variables
    # setColBndGLPK(lp, newPsgColInd, type = GLP_DB, lb = 0, ub = 1) # GLP_DB = double bound variable (>= and <=)
    setObjCoefGLPK(lp, newPsgColInd, obj_coef = 0) # flag variable only, should not contribute to to the objective function
    psgColInd = c(psgColInd, newPsgColInd)

    consInd = c(moduleInd[which(passageCons$MODULE_LENGTH[[i]] != 0)], newPsgColInd)

    # Add lower bound constraint
    consCoef = c(passageCons$MODULE_LENGTH[[i]][which(passageCons$MODULE_LENGTH[[i]] != 0)], -passageCons$MIN_ITEMS[[i]])
    rowInd = firstRowInd + (i - 1) * 2
    setRowNameGLPK(lp, rowInd, rname = paste0(passageCons$PSG_ID[[i]], ".LB"))
    setMatRowGLPK(lp, rowInd, len = length(consInd), ind = consInd, val = consCoef)
    setRowBndGLPK(lp, rowInd, type = GLP_LO, lb = 0, ub = 0) # GLP_LO = lower bound constraint (>=)

    # Add upper bound constraint
    consCoef = c(passageCons$MODULE_LENGTH[[i]][which(passageCons$MODULE_LENGTH[[i]] != 0)], -passageCons$MAX_ITEMS[[i]])
    rowInd = firstRowInd + (i - 1) * 2 + 1
    setRowNameGLPK(lp, rowInd, rname = paste0(passageCons$PSG_ID[[i]], ".UB"))
    setMatRowGLPK(lp, rowInd, len = length(consInd), ind = consInd, val = consCoef)
    setRowBndGLPK(lp, rowInd, type = GLP_UP, lb = 0, ub = 0) # GLP_UP = upper bound constraint (<=)
  }

  if (!is.null(minPassages) || !is.null(maxPassages)) {
    if (is.null(minPassages)) { minPassages = maxPassages }
    if (is.null(maxPassages)) { maxPassages = minPassages }

    # Create a row for the min/max number of passages, using the indicator column variables created for each passage
    firstRowInd = addRowsGLPK(lp, 2)
    consCoef = rep(1, length(psgColInd))

    # Add lower bound constraint
    setRowNameGLPK(lp, firstRowInd, rname = "NumPsg.LB")
    setMatRowGLPK(lp, firstRowInd, len = length(psgColInd), ind = psgColInd, val = consCoef)
    setRowBndGLPK(lp, firstRowInd, type = GLP_LO, lb = minPassages, ub = minPassages) # GLP_LO = lower bound constraint (>=)

    # Add upper bound constraint
    setRowNameGLPK(lp, firstRowInd + 1, rname = "NumPsg.UB")
    setMatRowGLPK(lp, firstRowInd + 1, len = length(psgColInd), ind = psgColInd, val = consCoef)
    setRowBndGLPK(lp, firstRowInd + 1, type = GLP_UP, lb = maxPassages, ub = maxPassages) # GLP_UP = upper bound constraint (<=)
  }
}

#' Adds tif constraints to the given lp, based on the given tif table
#'
#' @examples
#'   modules = simulation$modules
#'   tif = simulation$tif
#' @import glpkAPI
solver.addTIFConstraints <- function (lp, modules, tif) {
  # Create rows for the tif being added
  firstRowInd = addRowsGLPK(lp, nrow(tif) * 2)
  firstModuleInd = findColGLPK(lp, cname = modules$MODULE_ID[[1]])
  moduleInd = seq.int(from = firstModuleInd, length.out = nrow(modules))

  for (i in 1:nrow(tif)) {
    # i = 1
    # Add lower bound constraint
    rowInd = firstRowInd + (i - 1) * 2
    setRowNameGLPK(lp, rowInd, rname = paste0(tif$TIF_ID[[i]], ".LB"))
    setMatRowGLPK(lp, rowInd, len = length(moduleInd), ind = moduleInd, val = tif$MODULE_INF[[i]])
    setRowBndGLPK(lp, rowInd, type = GLP_LO, lb = tif$LOWER[[i]], ub = tif$LOWER[[i]]) # GLP_LO = lower bound constraint (>=)

    # Add upper bound constraint
    rowInd = firstRowInd + (i - 1) * 2 + 1
    setRowNameGLPK(lp, rowInd, rname = paste0(tif$TIF_ID[[i]], ".UB"))
    setMatRowGLPK(lp, rowInd, len = length(moduleInd), ind = moduleInd, val = tif$MODULE_INF[[i]])
    setRowBndGLPK(lp, rowInd, type = GLP_UP, lb = tif$UPPER[[i]], ub = tif$UPPER[[i]]) # GLP_UP = upper bound constraint (<=)
  }
  # print(getColsKindGLPK(lp, 1:getNumColsGLPK(lp)))
}

#' Returns the solution for the given lp, using the solverConfig to choose which tool to solve the problem
#'
#' @examples
#'   modules = simulation$modules
#'   solverConfig = simulation$control$solver
#' @import glpkAPI
solver.solveAndGetSolution <- function (lp.glpk, modules, solverConfig) {
  # Write lp to disk for debugging, if requested

  termOutGLPK(GLP_OFF)
  setObjDirGLPK(lp.glpk, GLP_MAX)

  if (("verbose" %in% names(solverConfig)) && as.logical(solverConfig$verbose)) {
    writeLPGLPK(lp.glpk, fname="glpkAPI.lp")
    writeMPSGLPK(lp.glpk, fmt = GLP_MPS_FILE, fname="glpkAPI.mps")
  }

  if ("external" %in% names(solverConfig)) {
    solveExternal = as.logical(solverConfig$external)
  } else {
    solveExternal = TRUE # default
  }

  if ("name" %in% names(solverConfig)) {
    solverName = solverConfig$name
  } else {
    solverName = "glpk" # default
  }

  firstModuleInd = findColGLPK(lp.glpk, cname = modules$MODULE_ID[[1]])
  moduleInd = seq.int(from = firstModuleInd, length.out = nrow(modules))

  if (solveExternal) {
    if (solverName == "clp" || solverName == "cbc") {
      solution.clp = solver.solveAndGetSolution.clp.external(lp.glpk, solverConfig)
      # clp solutions contain a dense vector of selected indices only.
      # solver.solveAndGetSolution.clp.external pads this back out to the largest non-zero value, but we may need more zeroes
      if (length(solution.clp) < length(moduleInd)) {
        solution.clp = c(solution.clp, rep(0, length(moduleInd) - length(solution.clp)))
      }
      return(solution.clp[moduleInd])
    } else if (solverName == "cplex") {
      solution.cplex = solver.solveAndGetSolution.cplex.external(lp.glpk, solverConfig)
      return(solution.cplex[moduleInd])
    } else if (solverName == "glpk") {
      solution.glpk = solver.solveAndGetSolution.glpk.external(lp.glpk, solverConfig)
      return(solution.glpk[moduleInd])
    } else if (solverName == "lpsolve") {
      solution.lpsolve = solver.solveAndGetSolution.lpsolve.external(lp.glpk, solverConfig)
      return(solution.lpsolve[moduleInd])
    }
  } else {
    if (solverName == "clp" || solverName == "cbc") {
      solution.clp = solver.solveAndGetSolution.clp.internal(lp.glpk, solverConfig)
      return(solution.clp[moduleInd])
    } else if (solverName == "cplex") {
      solution.cplex = solver.solveAndGetSolution.cplex.internal(lp.glpk, solverConfig)
      return(solution.cplex[moduleInd])
    } else if (solverName == "glpk") {
      solution.glpk = solver.solveAndGetSolution.glpk.internal(lp.glpk, solverConfig)
      return(solution.glpk[moduleInd])
    } else if (solverName == "lpsolve") {
      solution.lpsolve = solver.solveAndGetSolution.lpsolve.internal(lp.glpk, solverConfig)
      return(solution.lpsolve[moduleInd])
    } else if (solverName == "gurobi") {
      solution.gurobi = solver.solveAndGetSolution.gurobi.internal(lp.glpk, solverConfig)
      return(solution.gurobi[moduleInd])
    }
  }

  # Algorithm unknown???
  stop("solver not found: ", solverName)
  return(NULL)
}

#' Returns the solution for the given lp, solving it externally using the "cbc" terminal command
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "clp", external = T, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.clp = solver.solveAndGetSolution.clp.external(lp.glpk, solverConfig)
solver.solveAndGetSolution.clp.external <- function (lp.glpk, solverConfig) {
  # Write the problem to the temp folder
  probFilename = file.path(tempdir(), "clp-prob.lp") # tempdir
  solnFilename = file.path(tempdir(), "clp-soln.txt") # getwd
  setObjDirGLPK(lp.glpk, GLP_MAX)
  writeLPGLPK(lp.glpk, fname = probFilename)

  if ("cmd" %in% names(solverConfig)) {
    solverCmd = solverConfig$cmd
  } else {
    solverCmd = "cbc" # default
  }
  if ("cmdOpts" %in% names(solverConfig)) {
    solverCmdOpts = unlist(strsplit(solverConfig$cmdOpts, split = "\\s+"))
  } else {
    # Set config params
    if ("mipGap" %in% names(solverConfig)) {
      mipGap = as.double(solverConfig$mipGap)
    } else {
      mipGap = 0.01 # default
    }
    if ("timeout" %in% names(solverConfig)) {
      timeout = as.integer(solverConfig$timeout / 1000)
    } else {
      timeout = 10 # default to 10 seconds
    }
    if ("verbose" %in% names(solverConfig)) {
      logLevel = ifelse(as.logical(solverConfig$verbose), 2, 1)
    } else {
      logLevel = 1 # default to only errors
    }
    solverCmdOpts = c("-cuts", "root", "-cost", "priorities", "-mipstart", "x0", "-allow", mipGap, "-seconds", timeout, "-log", logLevel) # default
  }

  # cbc glpkAPI.lp -cuts root -cost priorities -mipstart x0 -allow 0.01 -seconds 10 -log 1 -solve -solution cbcSOL.txt
  solverCmdArgs = c(probFilename, solverCmdOpts, "-solve", "-solution", solnFilename)
  solverCmdResult = system2(solverCmd, solverCmdArgs, stdout = T, stderr = T, wait = T)

  if ("status" %in% names(attributes(solverCmdResult))) {
    returnCode.clp = as.integer(attributes(solverCmdResult)$status)
  } else {
    returnCode.clp = 0 # if successful, $status will be undefined and solverCmdResult be a vector of stdout lines
  }
  if (returnCode.clp != 0) {
    stop("return ", returnCode.clp)
    return(NULL)
  }

  status.clp = readLines(solnFilename, 1)
  if (!startsWith(trimws(status.clp), "Optimal")) {
    stop("status ", status.clp)
    return(NULL)
  }

  # Get solution and return
  solutionDF = read.table(solnFilename, skip = 1, stringsAsFactors = FALSE)
  # return(as.integer(solutionDF[,3]))
  solution.clp = rep(0, solutionDF[[nrow(solutionDF),1]]+1)
  solution.clp[solutionDF[,1]+1] = solutionDF[,3]
  return(solution.clp)
}

#' Returns the solution for the given lp, solving it internally using the "clpAPI" package Simplex algorithm
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "clp", external = F, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.clp = solver.solveAndGetSolution.clp.internal(lp.glpk, solverConfig)
solver.solveAndGetSolution.clp.internal <- function (lp.glpk, solverConfig) {
  if (!requireNamespace("clpAPI", quietly = TRUE)) {
    stop("Package \"clpAPI\" needed for this function to work. Please install it or choose another solver.", call. = FALSE)
  }

  # Initialize the clp problem object and set the log level
  lp.clp = initProbCLP()
  if ("verbose" %in% names(solverConfig)) {
    verbose = as.logical(solverConfig$verbose)
  } else {
    verbose = F # default to only errors (glpk normally defaults to GLP_MSG_ALL)
  }
  setLogLevelCLP(lp.clp, ifelse(verbose, 4, 0)) # 0: nothing, 4: verbose

  # Convert lp.glpk to clp format
  probFilename = file.path(tempdir(), "clp-prob.mps")
  setObjDirGLPK(lp.glpk, GLP_MAX)
  writeMPSGLPK(lp.glpk, fmt = GLP_MPS_FILE, fname = probFilename)
  readMPSCLP(lp.clp, fname = probFilename)

  # Set more config params
  # if ("mipGap" %in% names(solverConfig)) {
  #   mipGap = as.double(solverConfig$mipGap)
  #   setMIPParmGLPK(MIP_GAP, mipGap)
  # } # default to 0.0 (glpk normally defaults to 0.0 to search until an optimal solution is found)

  if ("timeout" %in% names(solverConfig)) {
    timeout = as.integer(solverConfig$timeout / 1000)
  } else {
    timeout = 10 # default to 10 seconds (glpk normally defaults to INT_MAX, which is effectively no timeout)
  }
  setMaximumSecondsCLP(lp.clp, timeout)

  # Solve the lp, then clean up the temp solver object
  # setMIPParmGLPK(PRESOLVE, GLP_ON)
  setObjDirCLP(lp.clp, -1) # -1: maximize
  returnCode.clp = solveInitialCLP(lp.clp)
  status.clp = getSolStatusCLP(lp.clp)
  solution.clp = getColPrimCLP(lp.clp)
  delProbCLP(lp.clp) # TODO: This should probably be in a try/finally

  # Get solution and return
  if (returnCode.clp != 0) {
    stop("return ", returnCode.clp)
    return(NULL)
  }
  if (status.clp != 0) { # 0: optimal
    stop("status ", status.clp)
    return(NULL)
  }
  return(as.integer(round(solution.clp)))
}

#' Returns the solution for the given lp, solving it externally using the "cbc" terminal command
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "cplex", external = T, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.cplex = solver.solveAndGetSolution.cplex.external(lp.glpk, solverConfig)
solver.solveAndGetSolution.cplex.external <- function (lp.glpk, solverConfig) {
  # Write the problem to the temp folder
  probFilename = file.path(tempdir(), "cplex-prob.lp") # tempdir
  solnFilename = file.path(tempdir(), "cplex-soln.sol") # getwd
  cmdsFilename = file.path(tempdir(), "cplex-cmds.txt")
  setObjDirGLPK(lp.glpk, GLP_MAX)
  writeLPGLPK(lp.glpk, fname = probFilename)

  # Existing solution file MUST be removed, otherwise the solver will ask "overwrite? [y/n]" and we cannot answer
  if (file.exists(solnFilename)) {
    file.remove(solnFilename)
  }

  # Write the command file that cplex will execute
  solverCmdOpts = paste0("read ", probFilename)
  if ("cmdOpts" %in% names(solverConfig)) {
    solverCmdOpts = c(solverCmdOpts, solverConfig$cmdOpts)
  } else {
    # Set config params. See CPLEX_Studio_Community1210/doc/html/en-US/CPLEX/Parameters/topics/introListAlpha.html
    if ("mipGap" %in% names(solverConfig)) {
      solverCmdOpts = c(solverCmdOpts, paste0("mip tolerances mipgap ", as.double(solverConfig$mipGap)))
    } else {
      solverCmdOpts = c(solverCmdOpts, "mip tolerances mipgap 0.01") # default to 1%
    }
    if ("timeout" %in% names(solverConfig)) {
      solverCmdOpts = c(solverCmdOpts, paste0("set timelimit ", as.integer(solverConfig$timeout / 1000)))
    } else {
      solverCmdOpts = c(solverCmdOpts, "set timelimit 10") # default to 10 seconds
    }
    if ("verbose" %in% names(solverConfig)) {
      solverCmdOpts = c(solverCmdOpts, paste0("mip display ", ifelse(as.logical(solverConfig$verbose), 1, 0)))
    } else {
      solverCmdOpts = c(solverCmdOpts, "mip display 0") # default to only errors
    }
  }
  solverCmdOpts = c(solverCmdOpts, "mipopt", paste0("write ", solnFilename))
  writeLines(solverCmdOpts, cmdsFilename)

  if ("cmd" %in% names(solverConfig)) {
    solverCmd = solverConfig$cmd
  } else {
    solverCmd = "cplex" # default
  }

  # cplex -f cplex-cmds.txt
  solverCmdArgs = c("-f", cmdsFilename)
  solverCmdResult = system2(solverCmd, solverCmdArgs, stdout = T, stderr = T, wait = T)

  # CPlex always returns a 0 status code.  Cannot use that value to determine if an error occurred.
  # Instead look for lines with "CPLEX Error" in the solverCmdResult
  errorLines = grep("CPLEX Error", solverCmdResult, value = T)
  if (length(errorLines) > 0) {
    stop(paste(errorLines, collapse = "\t"))
    return(NULL)
  }

  # Get solution and return
  solutionXML = xml2::as_list(xml2::read_xml(solnFilename))
  solutionVars = solutionXML$CPLEXSolution$variables
  solution.cplex = vapply(solutionVars, function(var) {
    # "value" has type <chr>.  as.integer("0.9999") => 0, so we need to use round()
    as.integer(round(as.numeric(attr(var, "value"))))
  }, as.integer(0), USE.NAMES = FALSE)
  return(solution.cplex)
}

#' Returns the solution for the given lp, solving it internally using the "cplexAPI" package
#' install.packages("cplexAPI", configure.args = "--with-cplex-dir=/path/to/CPLEX_Studio_Community129/cplex", verbose = T)
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "cplex", external = F, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.cplex = solver.solveAndGetSolution.cplex.internal(lp.glpk, solverConfig)
solver.solveAndGetSolution.cplex.internal <- function (lp.glpk, solverConfig) {
  if (!requireNamespace("cplexAPI", quietly = TRUE)) {
    stop("Package \"cplexAPI\" needed for this function to work. Please install it or choose another solver.", call. = FALSE)
  }

  # ensure that env.cplex exists and is open
  if(!exists("env.cplex")) {
    message("creating env.cplex <- openEnvCPLEX()")
    assign("env.cplex", openEnvCPLEX(), envir = .GlobalEnv)
  }

  # Initialize the clp problem object and set the log level
  lp.cplex = initProbCPLEX(env.cplex)
  if ("verbose" %in% names(solverConfig)) {
    verbose = as.logical(solverConfig$verbose)
  } else {
    verbose = F # default to only errors (glpk normally defaults to GLP_MSG_ALL)
  }
  # mip display 0: nothing, 1: verbose
  setIntParmCPLEX(env.cplex, CPX_PARAM_MIPDISPLAY, ifelse(as.logical(solverConfig$verbose), 1, 0))

  # Convert lp.glpk to cplex format
  probFilename = file.path(tempdir(), "cplex-prob.mps")
  setObjDirGLPK(lp.glpk, GLP_MAX)
  writeMPSGLPK(lp.glpk, fmt = GLP_MPS_FILE, fname = probFilename)
  readCopyProbCPLEX(env.cplex, lp.cplex, fname = probFilename)

  # Set more config params
  if ("mipGap" %in% names(solverConfig)) {
    mipGap = as.double(solverConfig$mipGap)
  } else {
    mipGap = 0.01
  }
  # mip tolerances mipgap 0.01: default to 1%
  setDblParmCPLEX(env.cplex, CPXPARAM_MIP_Tolerances_MIPGap, mipGap)
  if ("timeout" %in% names(solverConfig)) {
    timeout = as.integer(solverConfig$timeout / 1000)
  } else {
    timeout = 10
  }
  # set timelimit 10: default to 10 seconds
  setDblParmCPLEX(env.cplex, CPXPARAM_TimeLimit, timeout)

  # Solve the lp, then clean up the temp solver object
  # setMIPParmGLPK(PRESOLVE, GLP_ON)
  setObjDirCPLEX(env.cplex, lp.cplex, CPX_MAX)
  returnCode.cplex = mipoptCPLEX(env.cplex, lp.cplex) # TODO: Check this with return_codeCPLEX, status_codeCPLEX
  solution.cplex = solutionCPLEX(env.cplex, lp.cplex)
  delProbCPLEX(env.cplex, lp.cplex) # TODO: This should probably be in a try/finally

  # Inspect solution and return
  if (class(solution.cplex) == "cplexError") {
    stop(getErrorStrCPLEX(attr(solution.cplex, "errnum")))
    return(NULL)
  }

  return(as.integer(round(solution.cplex$x)))
}

#' Returns the solution for the given lp, solving it externally using the "glpsol" terminal command
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "glpk", external = T, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.glpk = solver.solveAndGetSolution.glpk.external(lp.glpk, solverConfig)
solver.solveAndGetSolution.glpk.external <- function (lp.glpk, solverConfig) {
  # Write the problem to the temp folder
  probFilename = file.path(tempdir(), "glpk-prob.lp") # tempdir
  solnFilename = file.path(tempdir(), "glpk-soln.txt") # getwd
  setObjDirGLPK(lp.glpk, GLP_MAX)
  writeLPGLPK(lp.glpk, fname = probFilename)

  if ("cmd" %in% names(solverConfig)) {
    solverCmd = solverConfig$cmd
  } else {
    solverCmd = "glpsol" # default
  }
  if ("cmdOpts" %in% names(solverConfig)) {
    solverCmdOpts = unlist(strsplit(solverConfig$cmdOpts, split = "\\s+"))
  } else {
    # Set config params
    if ("mipGap" %in% names(solverConfig)) {
      mipGap = as.double(solverConfig$mipGap)
    } else {
      mipGap = 0.01 # default
    }
    if ("timeout" %in% names(solverConfig)) {
      timeout = as.integer(solverConfig$timeout / 1000)
    } else {
      timeout = 10 # default to 10 seconds
    }
    if ("verbose" %in% names(solverConfig)) {
      logLevel = ifelse(as.logical(solverConfig$verbose), 2, 1)
    } else {
      logLevel = 1 # default to only errors
    }
    solverCmdOpts = c("--mipgap", mipGap, "--tmlim", timeout) #, "-log", logLevel) # default
  }

  useMIP = any(getColsKindGLPK(lp.glpk, 1:getNumColsGLPK(lp.glpk)) != GLP_BV) # GLP_BV = binary variables
  if (useMIP) {
    # glpsol --lp glpkAPI.lp --mipgap 0.01 --tmlim 10 --intopt --max --write glpkSOL.txt
    solverCmdOpts = c(solverCmdOpts, "--intopt")
  } else {
    # glpsol --lp glpkAPI.lp --mipgap 0.01 --tmlim 10 --nomip --max --write glpkSOL.txt
    solverCmdOpts = c(solverCmdOpts, "--nomip")
  }
  solverCmdArgs = c("--lp", probFilename, solverCmdOpts, "--max", "--write", solnFilename)
  solverCmdResult = system2(solverCmd, solverCmdArgs, stdout = T, stderr = T, wait = T)

  if ("status" %in% names(attributes(solverCmdResult))) {
    returnCode.glpk = as.integer(attributes(solverCmdResult)$status)
  } else {
    returnCode.glpk = 0 # if successful, $status will be undefined and solverCmdResult be a vector of stdout lines
  }
  if (returnCode.glpk != 0) {
    stop("return ", returnCode.glpk)
    return(NULL)
  }

  # First 8 lines of the solution file are text defining the shape and status of the solution
  solnLines = readLines(solnFilename)
  # TODO: Is there a better way to parse the solution status?  The "s" line value "o" does not seem to mean "optimal" or "f" mean "feasible"
  status.glpk = solnLines[[5]]
  if (!grepl("OPTIMAL", status.glpk, fixed=T)) { # line 5 should contain the status of the solution
    stop("status ", status.glpk)
    return(NULL)
  }

  # Get solution and return
  solnDesc = unlist(strsplit(solnLines[startsWith(solnLines, "s")], split = "\\s+"))
  if (solnDesc[[2]] == "bas") {
    # solnDesc format 'nomip' (Simplex)
    # s => header, format, nrow, ncol, ??, ??, objValue
    ## s bas 28 21 f f -0.0512773247308822
    # j => column, colIndex, colType, colValue, objValue
    ## j 1 b 1 0
    solution.glpk = strsplit(solnLines[startsWith(solnLines, "j")], split = "\\s+")
    solution.glpk = vapply(solution.glpk, function(line) {
      return(as.integer(round(as.numeric(line[[4]]))))
    }, as.integer(0), USE.NAMES = FALSE)
  } else {
    ## solnDesc format 'mip' (Branch&Cut)
    ## s => solution, format, nrow, ncol, ??, objValue
    ## j => column, colIndex, colValue
    # c Problem:
    # c Rows:       21
    # c Columns:    30
    # c Non-zeros:  450
    # c Status:     OPTIMAL
    # c Objective:  obj = 8.020016672 (MAXimum)
    # c
    # s bas 21 30 f f 8.02001667222322
    # i 1 s 10 3.5527136788005e-15
    # i 2 b 4.35394544152488 0
    # i 3 b 4.35394544152488 0
    # j 1 l 0 -2.94209101525666e-15
    # j 2 b 0.646054558475116 0
    # j 3 u 1 -1.11022302462516e-15
    # e o f
    solution.glpk = strsplit(solnLines[startsWith(solnLines, "j")], split = "\\s+")
    solution.glpk = vapply(solution.glpk, function(line) {
      return(as.integer(round(as.numeric(line[[3]]))))
    }, as.integer(0), USE.NAMES = FALSE)
  }
  return(solution.glpk)
}

#' Returns the solution for the given lp, solving it internally using the "glpkAPI" package Simplex algorithm
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "glpk", external = F, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.glpk = solver.solveAndGetSolution.glpk.internal(lp.glpk, solverConfig)
#' @import glpkAPI
solver.solveAndGetSolution.glpk.internal.nomip <- function (lp.glpk, solverConfig) {
  require(glpkAPI)

  # Set config params
  if ("verbose" %in% names(solverConfig)) {
    verbose = as.logical(solverConfig$verbose)
  } else {
    verbose = F # default to only errors (glpk normally defaults to GLP_MSG_ALL)
  }
  setMIPParmGLPK(MSG_LEV, ifelse(verbose, GLP_MSG_ALL, GLP_MSG_ERR))

  # if ("mipGap" %in% names(solverConfig)) {
  #   mipGap = as.double(solverConfig$mipGap)
  #   setMIPParmGLPK(MIP_GAP, mipGap)
  # } # default to 0.0 (glpk normally defaults to 0.0 to search until an optimal solution is found)

  if ("timeout" %in% names(solverConfig)) {
    timeout = as.integer(solverConfig$timeout)
  } else {
    timeout = 10000 # default to 10 seconds in MS (glpk normally defaults to INT_MAX, which is effectively no timeout)
  }
  setMIPParmGLPK(TM_LIM, timeout)

  # Solve
  # setMIPParmGLPK(PRESOLVE, GLP_ON)
  setObjDirGLPK(lp.glpk, GLP_MAX)
  returnCode.glpk = solveSimplexGLPK(lp.glpk) # Solve MIP Problem with the Simplex Method (glp_simplex)
  if (!returnCode.glpk %in% c(0, GLP_ETMLIM, GLP_EMIPGAP)) {
    stop(return_codeGLPK(returnCode.glpk), " (return ", returnCode.glpk, ")")
    return(NULL)
  }

  status.glpk = getSolStatGLPK(lp.glpk)
  if (!status.glpk %in% c(GLP_FEAS, GLP_OPT)) {
    stop(status_codeGLPK(status.glpk), " (status ", status.glpk, ")")
    return(NULL)
  }

  # Get solution and return
  solution.glpk = getColsPrimGLPK(lp.glpk)
  return(as.integer(round(solution.glpk)))
}

#' Returns the solution for the given lp, solving it internally using the "glpkAPI" package Branch&Cut algorithm
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "glpk", external = F, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.glpk = solver.solveAndGetSolution.glpk.internal(lp.glpk, solverConfig)
#' @import glpkAPI
solver.solveAndGetSolution.glpk.internal <- function (lp.glpk, solverConfig) {
  require(glpkAPI)

  # Set config params
  if ("verbose" %in% names(solverConfig)) {
    verbose = as.logical(solverConfig$verbose)
  } else {
    verbose = F # default to only errors (glpk normally defaults to GLP_MSG_ALL)
  }
  setMIPParmGLPK(MSG_LEV, ifelse(verbose, GLP_MSG_ALL, GLP_MSG_ERR))

  if ("mipGap" %in% names(solverConfig)) {
    mipGap = as.double(solverConfig$mipGap)
    setMIPParmGLPK(MIP_GAP, mipGap)
  } # default to 0.0 (glpk normally defaults to 0.0 to search until an optimal solution is found)

  if ("timeout" %in% names(solverConfig)) {
    timeout = as.integer(solverConfig$timeout)
  } else {
    timeout = 10000 # default to 10 seconds in MS (glpk normally defaults to INT_MAX, which is effectively no timeout)
  }
  setMIPParmGLPK(TM_LIM, timeout)

  # Solve
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  setObjDirGLPK(lp.glpk, GLP_MAX)
  returnCode.glpk = solveMIPGLPK(lp.glpk) # Solve MIP Problem with the Branch-and-Cut Method (glp_intopt)
  if (!returnCode.glpk %in% c(0, GLP_ETMLIM, GLP_EMIPGAP)) {
    stop(return_codeGLPK(returnCode.glpk), " (return ", returnCode.glpk, ")")
    return(NULL)
  }

  status.glpk = mipStatusGLPK(lp.glpk)
  if (!status.glpk %in% c(GLP_FEAS, GLP_OPT)) {
    stop(status_codeGLPK(status.glpk), " (status ", status.glpk, ")")
    return(NULL)
  }

  # Get solution and return
  solution.glpk = mipColsValGLPK(lp.glpk)
  return(solution.glpk)
}

#' Returns the solution for the given lp, solving it externally using the "lp_solve" terminal command
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "lpsolve", external = T, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.lpsolve = solver.solveAndGetSolution.lpsolve.external(lp.glpk, solverConfig)
solver.solveAndGetSolution.lpsolve.external <- function (lp.glpk, solverConfig) {
  # Write the problem to the temp folder
  probFilename = file.path(tempdir(), "lpsolve-prob.mps")
  setObjDirGLPK(lp.glpk, GLP_MAX)
  writeMPSGLPK(lp.glpk, fmt = GLP_MPS_FILE, fname = probFilename)

  if ("cmd" %in% names(solverConfig)) {
    solverCmd = solverConfig$cmd
  } else {
    solverCmd = "lp_solve" # default
  }
  if ("cmdOpts" %in% names(solverConfig)) {
    solverCmdOpts = unlist(strsplit(solverConfig$cmdOpts, split = "\\s+"))
  } else {
    # Set config params
    if ("mipGap" %in% names(solverConfig)) {
      mipGap = as.double(solverConfig$mipGap)
    } else {
      mipGap = 0.01 # default
    }
    if ("timeout" %in% names(solverConfig)) {
      timeout = as.integer(solverConfig$timeout / 1000)
    } else {
      timeout = 10 # default to 10 seconds
    }
    if ("verbose" %in% names(solverConfig)) {
      logLevel = ifelse(as.logical(solverConfig$verbose), 5, 3)
    } else {
      logLevel = 3 # default to only errors
    }
    solverCmdOpts = c("-g", mipGap, "-timeout", timeout, paste0("-v", logLevel)) # default
  }

  # lp_solve -lp glpkAPI.lp -ga 0.01 -timeout 10 -max
  solverCmdArgs = c("-fmps", probFilename, solverCmdOpts, "-max")
  solverCmdResult = system2(solverCmd, solverCmdArgs, stdout = T, stderr = T, wait = T)

  if ("status" %in% names(attributes(solverCmdResult))) {
    returnCode.lpsolve = as.integer(attributes(solverCmdResult)$status)
  } else {
    returnCode.lpsolve = 0 # if successful, $status will be undefined and solverCmdResult be a vector of stdout lines
  }
  if (returnCode.lpsolve != 0) {
    stop("return ", returnCode.lpsolve, " - ", solverCmdResult[[length(solverCmdResult)]])
    return(NULL)
  }

  # Get solution and return
  solutionStart = which(solverCmdResult == "Actual values of the variables:")+1
  solution.lpsolve = strsplit(solverCmdResult[solutionStart:length(solverCmdResult)], split = "\\s+")
  return(vapply(solution.lpsolve, function(line) {
    return(as.integer(round(as.numeric(line[[2]]))))
  }, as.integer(0), USE.NAMES = FALSE))
}

#' Returns the solution for the given lp, solving it internally using the "lpSolveAPI" package
#'
#' @examples
#'   lp.glpk = initProbGLPK()
#'   readLPGLPK(lp.glpk, fname="glpkAPI.lp")
#'   solverConfig = list(name = "lpsolve", external = F, mipGap = 0.0001, timeout = 1000, verbose = T)
#'   solution.lpsolve = solver.solveAndGetSolution.lpsolve.internal(lp.glpk, solverConfig)
solver.solveAndGetSolution.lpsolve.internal <- function (lp.glpk, solverConfig) {
  if (!requireNamespace("lpSolveAPI", quietly = TRUE)) {
    stop("Package \"lpSolveAPI\" needed for this function to work. Please install it or choose another solver.", call. = FALSE)
  }

  # Convert lp.glpk to lpsolve format
  probFilename = file.path(tempdir(), "lpsolve-prob.mps")
  setObjDirGLPK(lp.glpk, GLP_MAX)
  writeMPSGLPK(lp.glpk, fmt = GLP_MPS_FILE, fname = probFilename)
  lp.lpsolve = read.lp(probFilename, type = "freemps")

  # Set config params
  # if ("verbose" %in% names(solverConfig)) {
  #   verbose = as.logical(solverConfig$verbose)
  # } else {
  #   verbose = F # default to only errors (glpk normally defaults to GLP_MSG_ALL)
  # }
  # setLogLevelCLP(lp.clp, ifelse(verbose, 4, 0)) # 0: nothing, 4: verbose

  # if ("mipGap" %in% names(solverConfig)) {
  #   mipGap = as.double(solverConfig$mipGap)
  #   setMIPParmGLPK(MIP_GAP, mipGap)
  # } # default to 0.0 (glpk normally defaults to 0.0 to search until an optimal solution is found)

  # if ("timeout" %in% names(solverConfig)) {
  #   timeout = as.integer(solverConfig$timeout / 1000)
  # } else {
  #   timeout = 10 # default to 10 seconds (glpk normally defaults to INT_MAX, which is effectively no timeout)
  # }
  # setMaximumSecondsCLP(lp.clp, timeout)

  # Solve the lp, then clean up the temp solver object
  lp.control(lp.lpsolve, sense = "max", epsint = 0.1, mip.gap = c(0.1, 0.05))
  status.lpsolve = solve(lp.lpsolve)
  solution.lpsolve = get.variables(lp.lpsolve)
  delete.lp(lp.lpsolve) # TODO: This should probably be in a try/finally

  # Get solution and return
  if (status.lpsolve != 0) {
    stop("status ", status.lpsolve)
    return(NULL)
  }
  return(solution.lpsolve)
}
