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


#' Write a Hypernode YAML to disk
#'
#' @param yaml_txt   A character string containing the YAML.
#' @param out_dir    Path to the working directory.
#' @param hypernode  Name of the hypernode (used as filename prefix).
#' @return           The full path to the file that was written.
#' @throws           Error if `out_dir` does not exist or `hypernode` is empty.
#' @export
write_hypernode_yaml <- function(yaml_txt, out_dir, hypernode) {
  if (!dir.exists(out_dir)) {
    stop("Working directory not found: ", out_dir)
  }
  if (!nzchar(hypernode)) {
    stop("Hypernode name cannot be empty")
  }

  # ensure config/ subfolder exists
  config_dir <- file.path(out_dir, "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # 1) write the YAML
  yaml_fname <- paste0(hypernode, ".yaml")
  yaml_path  <- file.path(config_dir, yaml_fname)
  writeLines(yaml_txt, yaml_path)

  # 2) write the placeholder initial_data.csv
  csv_path <- file.path(config_dir, "initial_data.csv")
  # single header line, semicolon-delimited
  writeLines("i; init; init.gen;", csv_path)

  return(list(
    yaml = yaml_path,
    initial_data = csv_path
  ))
}



