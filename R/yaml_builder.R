#' Build the full configuration YAML
#'
#' @param eff_cfgs   List risultante di effective_cfgs()  (override + fallback)
#' @param global_cfg Lista delle impostazioni globali   (serve solo per il controllo capacità)
#' @param all_bounds Character vector dei boundary metaboliti selezionati
#' @return Stringa YAML completa
#' @export
build_hypernode_yaml <- function(eff_cfgs, global_cfg, all_bounds) {

  n <- length(eff_cfgs)

  ## 1) Controllo capacità: dup * init  -------------------------------------------------
  per_mod_cells <- vapply(seq_len(n), function(i) {
    cfg <- eff_cfgs[[i]]
    cfg$population$dup * cfg$initial_count
  }, numeric(1))

  total_cells <- sum(per_mod_cells)
  if (total_cells > global_cfg$cell_density) {
    stop(sprintf("Total cells (%.0f) exceed capacity (%.0f)",
                 total_cells, global_cfg$cell_density))
  }

  ## 2) Costruzione voci per-modello  ---------------------------------------------------
  yaml_units <- lapply(seq_len(n), function(i) {
    cfg <- eff_cfgs[[i]]

    list(
      model_name    = cfg$model_name,
      label         = cfg$label,
      biomass       = cfg$biomass,
      population    = cfg$population,
      initial_count = cfg$initial_count
    )
  })

  ## 3) Assemble-and-render + debug print  ----------------------------------------------
  full_yaml <- list(
    boundary_metabolites = all_bounds,
    cellular_units       = yaml_units
  )

  message("[DBG] YAML structure to be rendered ⤵︎")
  message(jsonlite::toJSON(full_yaml, pretty = TRUE, auto_unbox = TRUE))

  yaml::as.yaml(full_yaml, indent.mapping.sequence = TRUE)
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



