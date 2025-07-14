# R/app_helpers.R

#' Assemble a list for the hypernode YAML from a list of Shiny inputs
#'
#' @param inputs A named list, e.g. list(
#'   hypernode_name = "foo",
#'   cellular_units = list(
#'     list(model_name="A", biomass=1, population=1, initial_count=10),
#'     ...
#'   ),
#'   boundary_metabolites = c("met1","met2"),
#'   fba_upper_bound = 1000,
#'   fba_lower_bound = 0,
#'   volume = 1,
#'   background_met = 0
#' )
#' @return A list ready for `yaml::write_yaml()`
#' @noRd
build_yaml_list <- function(inputs) {
  list(
    hypernode_name       = inputs$hypernode_name,
    cellular_units       = inputs$cellular_units,
    boundary_metabolites = inputs$boundary_metabolites,
    fba_upper_bound      = inputs$fba_upper_bound,
    fba_lower_bound      = inputs$fba_lower_bound,
    volume               = inputs$volume,
    background_met       = inputs$background_met
  )
}
