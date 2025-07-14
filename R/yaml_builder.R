#’ Build the full configuration YAML
#’
#’ @param unit_cfgs  List of per‐model skeletons (from reactive unit_cfgs())  
#’ @param global_cfg List of global settings (from reactive global_cfg())  
#’ @param inputs     Named list of all your input values (e.g. shiny::reactiveValuesToList(input))
#’ @param all_bounds Character vector of selected boundary metabolites
#’ @return A character string of the rendered YAML
#’ @export
build_hypernode_yaml <- function(unit_cfgs, global_cfg, inputs, all_bounds) {
  # helper to coalesce NULL → default
  co <- function(x, default) if (!is.null(x)) x else default

  n <- length(unit_cfgs)

  # 1) capacity check
  per_mod_cells <- vapply(seq_len(n), function(i) {
    dup_i  <- co(inputs[[paste0("p_dup_",   i)]], global_cfg$population$dup)
    init_i <- co(inputs[[paste0("init_",    i)]], global_cfg$initial_count)
    dup_i * init_i
  }, numeric(1))
  total_cells <- sum(per_mod_cells)
  if (total_cells > global_cfg$cell_density) {
    stop(sprintf(
      "Total cells (%.0f) exceed capacity (%.0f)", 
      total_cells, global_cfg$cell_density
    ))
  }

  # 2) per‐model entries
  yaml_units <- lapply(seq_len(n), function(i) {
    base <- unit_cfgs[[i]]

    readOr <- function(prefix, default) {
      co(inputs[[paste0(prefix, "_", i)]], default)
    }

    # biomass
    bm <- list(
      max  = readOr("b_max",  global_cfg$biomass$max),
      mean = readOr("b_mean", global_cfg$biomass$mean),
      min  = readOr("b_min",  global_cfg$biomass$min)
    )

    # population
    pd <- list(
      starv = readOr("p_starv", global_cfg$population$starv),
      dup   = readOr("p_dup",   global_cfg$population$dup),
      death = readOr("p_death", global_cfg$population$death)
    )

    # initial count
    init <- readOr("init", global_cfg$initial_count)

    list(
      model_name    = base$model_name,
      label         = base$label,
      biomass       = bm,
      population    = pd,
      initial_count = init
    )
  })

  # 3) assemble and render
  full <- list(
    boundary_metabolites = all_bounds,
    cellular_units       = yaml_units
  )
  yaml::as.yaml(full, indent.mapping.sequence = TRUE)
}

