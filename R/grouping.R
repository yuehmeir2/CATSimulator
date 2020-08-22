
#' make a group column for a tibble
#'
#' @param data a tibble data set
#' @param id_col the id column of the data set for grouping
#' @param group_by_col the column used for grouping
#' @param group_method the grouping methods include "category", "equal_interval", "equal_number",
#' "cut_by_breaks", and "percent_rank", which utilize some functions provided by ggplot2 and dplyr.
#' @param ... Arguments passed on to different group_method. For the method "cut_by_breaks", breaks must
#' be provided. The variable "labels" are commonly used by the "equal_interval", "equal_number", and
#' "cut_by_breaks".
#' @return A tibble containing id_col and group column.
#' @examples
#'   data = iris %>% mutate(id = str_c("id_", 1:nrow(.)))
#'   gen_group(data = data, id_col = "id", group_by_col = "Species", group_method = "category")
#'   gen_group(data = data, id_col = "id", group_by_col = "Sepal.Length",
#'            group_method = "equal_interval", labels = c("low", "middle", "high"))
#'   gen_group(data = data, id_col = "id", group_by_col = "Sepal.Length",
#'            group_method = "equal_number", labels = c("low", "middle", "high"))
#'   gen_group(data = data, id_col = "id", group_by_col = "Sepal.Length",
#'            group_method = "cut_by_breaks", breaks = c(0, 5.1, 6.5, 100), labels = c("low", "middle", "high"))
#'   gen_group(data = data, id_col = "id", group_by_col = "Sepal.Length", group_method = "percent_rank")
#' @export

gen_group = function(data, id_col, group_by_col, group_method = "category", ...) {

  require(ggplot2)
  # get distinct rows of id_col and group_by_col

  data <- data %>% select(all_of(id_col), all_of(group_by_col)) %>% distinct()

  # if any duplicated id_col, use the last row.

  if (any(duplicated(data[[id_col]])))
    data <- data %>% group_by(!!as.name(id_col)) %>% group_map(~tail(.x, 1)) %>% bind_rows()

  # grouping with categorical column

  if (group_method == "category") return(data %>% set_names(c(id_col, "group")))

  # grouping with "quantile"

  # if (!exists("quantile_probs")) quantile_probs = seq(0.25, 0.75, 0.25)

  # grouping with numerical column

  dots = list(...)

  if (group_method == "equal_interval") {
      return(data %>%
        mutate(group = cut_interval(data[[group_by_col]], n = length(dots$labels), labels = dots$labels)) %>%
        select(id_col, "group"))
  }

  if (group_method == "equal_number")
      return(data %>%
        mutate(group = cut_number(.[[group_by_col]], n = length(dots$labels), labels = dots$labels)) %>%
        select(id_col, "group"))

  if (group_method == "cut_by_breaks" && "breaks" %in% names(dots))
      return(data %>%
        mutate(group = cut(.[[group_by_col]], labels = dots$labels)) %>%
        select(id_col, "group"))

  if (group_method == "percent_rank")
      return(data %>%
             mutate(group = str_c(round(percent_rank(.[[group_by_col]])*100, 0), "%")) %>%
             select(id_col, "group"))

  return()
}
