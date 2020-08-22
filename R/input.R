
#' Read in test_config.json
#'
#' @examples
#'   test_config_file = "inst/input_files/many_panels/test.json"
#'   test_config = read_test_config(test_config_file)
#' @export
read_test_config = function(test_config_file) {
  test_config = jsonlite::read_json(test_config_file, simplifyVector = T, simplifyDataFrame = F, simplifyMatrix = F, flatten = T)

  return(test_config)
}

#' Read in constraint.csv as content_constraint tibble
#'
#' @examples
#'   constraint_file = "inst/input_files/many_panels/constraint.csv"
#'   content_constraint = read_content_constraint(constraint_file)
#' @import tidyverse
#' @export
read_content_constraint = function(constraint_file) {
  if (is.null(constraint_file)) {
    return(NULL)
  }

  # read in the constraint csv
  require(tidyverse)
  constraint_col_types = cols(
    CONS_ID = col_character(),
    WEIGHT = col_double(),
    LOWER = col_double(),
    UPPER = col_double(),
    LABEL = col_character()
  )
  content_constraint = read_csv(constraint_file, col_types = constraint_col_types, progress = FALSE)
  return(content_constraint)
}

#' Read in item.csv as item tibble
#'
#' @examples
#'   constraint_file = "inst/input_files/many_panels/constraint.csv"
#'   content_constraint = read_content_constraint(constraint_file)
#'
#'   item_file = "inst/input_files/many_panels/metadata.csv"
#'   item_constraint_file = "inst/input_files/many_panels/item_constraint.csv"
#'   item = read_item(item_file, item_constraint_file, content_constraint)
#' @import tidyverse
#' @export
read_item = function(item_file, item_constraint_file = NULL, content_constraint = NULL) {
  if (is.null(item_file)) {
    return(NULL)
  }

  # read in the item csv
  require(tidyverse)
  item_col_types = cols(
    ITEM_ID = col_character(),
    MODEL = col_character(),
    NC = col_integer(),
    PAR_1 = col_double(),
    PAR_2 = col_double(),
    PAR_3 = col_double(),
    PAR_4 = col_double(),
    PAR_5 = col_double(),
    PAR_6 = col_double(),
    PAR_7 = col_double(),
    PAR_8 = col_double(),
    PAR_9 = col_double()
  )
  suppressWarnings(item <- read_csv(item_file, col_types = item_col_types, progress = FALSE))

  if (is.null(item_constraint_file)) {
    # add empty CONS_IDS column so that it can be assumed to always exist
    item = item %>%
      add_column(
        CONS_IDS = list(NULL) # or list(list()) if we want an empty list instead of null?
      )
  } else {
    # read in the item constraint csv
    item_constraint_col_types = cols(
      ITEM_ID = col_character(),
      CONS_ID = col_character()
    )
    item_constraint = read_csv(item_constraint_file, col_types = item_constraint_col_types, progress = FALSE)

    # validate that all item_constraints refer to a defined item
    undefined_item_constraint = item_constraint %>%
      filter(!(ITEM_ID %in% item$ITEM_ID))
    if (nrow(undefined_item_constraint) > 0) {
      stop("item_constraint contains undefined ITEM_ID: ", paste(unique(undefined_item_constraint$ITEM_ID), collapse=","))
    }
    # validate that all item_constraints refer to a defined constraint
    if (!is.null(content_constraint)) {
      undefined_item_constraint = item_constraint %>%
        filter(!(CONS_ID %in% content_constraint$CONS_ID))
      if (nrow(undefined_item_constraint) > 0) {
        stop("item_constraint contains undefined CONS_ID: ", paste(unique(undefined_item_constraint$CONS_ID), collapse=","))
      }
    }

    # add CONS_IDS list column to the item tibble
    item = item %>%
      left_join(
        item_constraint %>% group_by(across(-CONS_ID)) %>%
          summarise(CONS_IDS = list(CONS_ID), .groups = "drop"),
        by = "ITEM_ID"
      )
  }

  return(item)
}

#' Read in module.csv as module tibble
#'
#' @examples
#'   item_file = "inst/input_files/many_panels/metadata.csv"
#'   item = read_item(item_file)
#'
#'   module_file = "inst/input_files/many_panels/module.csv"
#'   module = read_module(module_file, item)
#' @import tidyverse
#' @export
read_module = function(module_file, item = NULL) {
  require(tidyverse)
  if (is.null(module_file)) {
    return(NULL)
  }

  # read in the module csv
  module_col_types = cols(
    MODULE_ID = col_character(), # TODO: Rename this to MODULE_ID
    STAGE = col_integer(),
    MODULE_SLOT = col_integer(),
    ITEM_ID = col_character()
  )
  module = read_csv(module_file, col_types = module_col_types, progress = FALSE)

  # validate that all modules refer to a defined item
  if (!is.null(item)) {
    undefined_item = module %>%
      filter(!(ITEM_ID %in% item$ITEM_ID))
    if (nrow(undefined_item) > 0) {
      stop("module contains undefined ITEM_ID: ", paste(unique(undefined_item$ITEM_ID), collapse=","))
    }
  }

  # convert module tibble from long-form many ITEM_ID rows to grouped as one ITEM_IDS list column per module
  module = module %>% group_by(across(-ITEM_ID)) %>%
    summarise(ITEM_IDS = list(ITEM_ID), .groups = 'drop') %>%
    arrange(STAGE, MODULE_SLOT, MODULE_ID)

  return(module)
}

#' Read in panel.csv as panel tibble
#'
#' @examples
#'   module_file = "inst/input_files/many_panels/module.csv"
#'   module = read_module(module_file)
#'
#'   panel_file = "inst/input_files/many_panels/panel.csv"
#'   panel = read_panel(panel_file, module)
#' @import tidyverse
#' @export
read_panel = function(panel_file, module = NULL) {
  require(tidyverse)
  if (is.null(panel_file)) {
    return(NULL)
  }

  # read in the module csv
  panel_col_types = cols(
    PANEL_ID = col_character(),
    MODULE_ID = col_character() # TODO: Rename this to MODULE_ID
  )
  panel = read_csv(panel_file, col_types = panel_col_types, progress = FALSE)

  # validate that all panels refer to a defined module
  if (!is.null(module)) {
    undefined_module = panel %>%
      filter(!(MODULE_ID %in% module$MODULE_ID))
    if (nrow(undefined_module) > 0) {
      stop("panel contains undefined MODULE_ID: ", paste(unique(undefined_module$MODULE_ID), collapse=","))
    }
  }

  # convert panel tibble from long-form many MODULE_ID rows to grouped as one MODULE_IDS list column per panel
  panel = panel %>% group_by(across(-MODULE_ID)) %>%
    summarise(MODULE_IDS = list(MODULE_ID), .groups = 'drop')

  return(panel)
}

#' Read in test_config.json and all input csvs to instantiate a simulation object
#'
#' @examples
#'   test_config_file = "inst/input_files/many_panels/test.json"
#'   simulation = read_simulation(test_config_file)
#' @export
read_simulation = function(test_config_file) {
  # First read the main config file
  test_config_dir = dirname(test_config_file)
  simulation = read_test_config(test_config_file)

  # Now read each of the csv files and attach to the simulation

  if (!is.null(simulation$input$content_constraint_file)) {
    simulation$constraints = list(content = read_content_constraint(
      constraint_file = file.path(test_config_dir, simulation$input$content_constraint_file)
    ))
  }
  if (!is.null(simulation$input$item_file)) {
    item_constraint_file = NULL
    if (!is.null(simulation$input$item_constraint_file)) {
      item_constraint_file = file.path(test_config_dir, simulation$input$item_constraint_file)
    }
    simulation$itempool = read_item(
      item_file = file.path(test_config_dir, simulation$input$item_file),
      item_constraint_file,
      simulation$constraints$content
    )
  }
  if (!is.null(simulation$input$module_file)) {
    simulation$modules = read_module(
      module_file = file.path(test_config_dir, simulation$input$module_file),
      simulation$itempool
    )
  }
  if (!is.null(simulation$input$panel_file)) {
    simulation$panels = read_panel(
      panel_file = file.path(test_config_dir, simulation$input$panel_file),
      simulation$modules
    )
  }

  # And finally return the simulation object
  return(simulation)
}
