# R/simulation.R

#' Simulation UI Module
#'
#' @param id Module namespace id
#' @return A tabPanel for hypernode selection and simulation configuration
#' @export
simulationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Simulation",
    div(class = "sim-card",
			div(class = "logo-hero-banner",
					tags$img(src = "Logo_QBio.png", alt = "Logo", class = "hero-logo"),
					tags$h1("Model Analysis", class = "hero-title")
				),
      ## Directory selector
      uiOutput(ns("dir_selector")),

      ## Model list
      uiOutput(ns("model_list")),

      ## Simulation configuration
      uiOutput(ns("sim_controls"))
    )
  )
}

#' Simulation Server Module
#'
#' @param id Module namespace id
#' @return Server logic for hypernode selection and simulation config
#' @export
simulationServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


		project_root <- getOption("epimodFBAfunctionsGUI.user_proj",
				                      normalizePath("~"))
		                        
    # Setup for directory chooser
    # Setup for directory chooser: allow both your Home and your R-project
    roots <- c(
      Home    = "~",   # your real home folder
      Project = project_root    # your RStudio project directory
    )
    shinyFiles::shinyDirChoose(
      input       = input,
      id          = "hypernode_dir",    # must exactly match the UI’s id
      roots       = roots,
      session     = session,
      defaultRoot = "Project",           
      defaultPath = ""                  # start at top of that root
    )

    # Reactive flags
    dir_valid  <- shiny::reactiveVal(FALSE)
    models     <- shiny::reactiveVal(character())
    reset_flag <- shiny::reactiveVal(FALSE)
    session_files <- reactiveValues(proj = NULL, nproj = NULL)
    cfg_root <- reactiveVal(NULL)   # path alla configurazione corrente
		need_regenerate <- shiny::reactiveVal(FALSE)
		old_struct      <- shiny::reactiveVal(NULL)

	    # ──────────────────────────────────────────────────────────────────────
    # ①  CACHE PER-MODEL DATA 
    # ──────────────────────────────────────────────────────────────────────
    node_data <- reactiveVal(list())
    
    
		## ──────────────────────────────────────────────────────────────
		##   Helpers “a colonna singola”  (solo background_conc)
		## ──────────────────────────────────────────────────────────────
		make_bounds_single <- function(df, mdl) {
			if (!nrow(df)) return(
				data.frame(reaction        = character(),
				           background_conc = numeric(),
				           stringsAsFactors = FALSE))

			sub <- df[df$FBAmodel == mdl, , drop = FALSE]
			if (!nrow(sub)) return(
				data.frame(reaction        = character(),
				           background_conc = numeric(),
				           stringsAsFactors = FALSE))

			## somma i contributi quando la reazione compare più volte -----------
			agg <- tapply(sub$background_conc, sub$reaction, sum)

			data.frame(
				reaction        = names(agg),
				background_conc = as.numeric(agg),
				stringsAsFactors = FALSE
			)
		}

		## scrive il CSV partendo dal DF “single-column” ------------------------
		## ════════════════════════════════════════════════════════════════════════════
		##  Riscrive il CSV ma preserva tutte le colonne che NON stiamo editando
		##  (p. es. ‘system_volume’).  Chiave = reaction + FBAmodel.
		## ════════════════════════════════════════════════════════════════════════════
		rebuild_bounds_csv_single <- function(csv_path, slot_name) {

			## 1) snapshot dell’eventuale file già presente ────────────────────────────
			old_df <- if (file.exists(csv_path))
				          read.csv(csv_path, stringsAsFactors = FALSE)
				        else NULL

			## 2) dataframe “long” ricostruito dalla cache (come prima) ────────────────
			long <- purrr::map_dfr(names(node_data()), function(mdl) {
				wide <- node_data()[[mdl]][[slot_name]]
				if (!nrow(wide)) return(NULL)

				data.frame(
				  reaction        = wide$reaction,
				  FBAmodel        = mdl,
				  background_conc = wide$background_conc,
				  stringsAsFactors = FALSE
				)
			})

			if (!nrow(long)) return()               # niente da scrivere

			## 3) Re-innesto delle colonne extra (se presenti) ─────────────────────────
			if (!is.null(old_df)) {
				extra_cols <- setdiff(names(old_df),
				                      c("reaction", "FBAmodel", "background_conc"))
				if (length(extra_cols)) {
				  long <- merge(long,
				                old_df[ , c("reaction", "FBAmodel", extra_cols), drop = FALSE],
				                by      = c("reaction", "FBAmodel"),
				                all.x   = TRUE,
				                sort    = FALSE)
				}
			}

			## 4) Ordine colonne: come nel file originale, se lo conosciamo ────────────
			if (!is.null(old_df)) {
				# mantieni solo le colonne che realmente esistono in ‘long’
				keep <- names(old_df)[ names(old_df) %in% names(long) ]
				long <- long[ , keep, drop = FALSE]
			}

			## 5) Scrivi il CSV definitivo ─────────────────────────────────────────────
			write.csv(long, csv_path, row.names = FALSE, quote = FALSE)
		}

		get_cfg_root <- function() {
			isolate(cfg_root()) %||%
				shinyFiles::parseDirPath(roots, input$hypernode_dir)
		}
		
		# ─────────────────────────────────────────────────────────────────────────────
		#  Salva snapshot in .default_conf
		# ─────────────────────────────────────────────────────────────────────────────
		save_default_conf <- function(base_dir) {
			cfg_dir  <- file.path(base_dir, "config")
			yml_file <- list.files(cfg_dir, "\\.ya?ml$", full.names = TRUE)[1]
			yml      <- if (length(yml_file)) yaml::read_yaml(yml_file) else list()
			bm_defs  <- yml$boundary_metabolites %||% character()
			met_ids  <- gsub("[^A-Za-z0-9_]", "_", bm_defs)

			default_dir <- file.path(base_dir, ".default_conf")
			if (!dir.exists(default_dir)) dir.create(default_dir, recursive = TRUE)

			# copia i CSV GUI-temp
			for(slot in c("proj","nproj")) {
				src <- session_files[[slot]]; dst <- file.path(default_dir, basename(src))
				if (file.exists(src)) file.copy(src, dst, overwrite = TRUE)
			}

			# costruisci la lista dei valori
			bc_list <- setNames(
				lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				bm_defs
			)
			snap <- list(
				times      = list(
				  initial_time = input$i_time,
				  final_time   = input$f_time,
				  step_size    = input$s_time
				),
				tolerances = list(atol = input$atol, rtol = input$rtol),
				boundary_concentrations = bc_list,
				system_parameters = list(
				  projected_upper_bound = input$fba_ub,
				  projected_lower_bound = input$fba_lb,
				  background_met  = input$background_met,
				  background_met_lb  = input$background_met_lb,
				  volume          = input$sys_volume,
				  cell_density    = input$cell_density
				),
				models = lapply(node_data(), function(info) list(
				  params          = info$params,
				  population      = info$population,
				  initial_biomass = info$initial_biomass
				))
			)

			# salva JSON + YAML
			jsonlite::write_json(snap,
				file.path(default_dir, "default_values.json"),
				auto_unbox = TRUE, pretty = TRUE
			)
			yaml::write_yaml(snap,
				file.path(default_dir, "default_values.yaml")
			)
			message("[save_default_conf] snapshot in .default_conf aggiornata")
		}

		# ─────────────────────────────────────────────────────────────────────────────
		#  Carica snapshot da .default_conf (se esiste)
		# ─────────────────────────────────────────────────────────────────────────────
		load_default_conf <- function(base_dir) {
			default_dir <- file.path(base_dir, ".default_conf")
			if (!dir.exists(default_dir)) return()  # niente da fare

			# 1) copia indietro i CSV GUI-temp
			for(slot in c("proj","nproj")) {
				src <- file.path(default_dir, basename(session_files[[slot]]))
				dst <- session_files[[slot]]
				if (file.exists(src)) file.copy(src, dst, overwrite = TRUE)
			}

			# 2) ricarica node_data sui bounds
			proj_df  <- read.csv(session_files$proj,  stringsAsFactors = FALSE)
			nproj_df <- read.csv(session_files$nproj, stringsAsFactors = FALSE)
			nd <- lapply(models(), function(mdl) {
				list(
				  params          = node_data()[[mdl]]$params,
				  population      = node_data()[[mdl]]$population,
				  initial_biomass = node_data()[[mdl]]$initial_biomass,
				  projected       = make_bounds_single(proj_df, mdl),
				  nonprojected    = make_bounds_single(nproj_df, mdl)
				)
			})
			names(nd) <- models()
			node_data(nd)

			# 3) ricarica i numericInputs dal JSON
			def <- jsonlite::fromJSON(file.path(default_dir, "default_values.json"))
			updateNumericInput(session, "i_time", value = def$times$initial_time)
			updateNumericInput(session, "f_time", value = def$times$final_time)
			updateNumericInput(session, "s_time", value = def$times$step_size)
			updateNumericInput(session, "atol",   value = def$tolerances$atol)
			updateNumericInput(session, "rtol",   value = def$tolerances$rtol)
			for(id in names(def$boundary_concentrations)) {
				updateNumericInput(
				  session,
				  paste0("conc_", gsub("[^A-Za-z0-9_]","_", id)),
				  value = def$boundary_concentrations[[id]]
				)
			}
			sp <- def$system_parameters
			updateNumericInput(session, "fba_ub",       value = sp$projected_upper_bound)
			updateNumericInput(session, "fba_lb",       value = sp$projected_lower_bound)
			updateNumericInput(session, "background_met",value = sp$background_met)
			updateNumericInput(session, "background_met_lb",value = sp$background_met_lb)
			updateNumericInput(session, "sys_volume",   value = sp$volume)
			updateNumericInput(session, "cell_density", value = sp$cell_density)

			message("[load_default_conf] valori caricati da .default_conf")
		}



		# —————————————————————————————————————————————————————————————
		#  rebuild node_data + create GUI-temp bounds files on valid dir
		# —————————————————————————————————————————————————————————————
		observeEvent(dir_valid(), ignoreInit = TRUE, {
			req(dir_valid())

			base_dir <- get_cfg_root()

			cfg_dir  <- file.path(base_dir, "config")
			out_dir  <- file.path(base_dir, "output")

			# ----- create / overwrite temp copies ---------------------------
			session_files$proj  <- file.path(out_dir, "ub_bounds_projected_gui.csv")
			session_files$nproj <- file.path(out_dir, "non_projected_bounds_gui.csv")

			file.copy(file.path(out_dir, "ub_bounds_projected.csv"),
						    session_files$proj,  overwrite = TRUE)
			file.copy(file.path(out_dir, "non_projected_bounds.csv"),
						    session_files$nproj, overwrite = TRUE)

			# ----- YAML ------------------------------------------------------
			yml_file <- list.files(cfg_dir, "\\.ya?ml$", full.names = TRUE)[1]
			yml      <- if (length(yml_file)) yaml::read_yaml(yml_file) else NULL

			yaml_tbl <- if (!is.null(yml$cellular_units)) {
				do.call(rbind, lapply(yml$cellular_units, function(u)
				  data.frame(
				    model      = u$model_name,
				    bioMax     = u$biomass$max,
				    bioMean    = u$biomass$mean,
				    bioMin     = u$biomass$min,
				    starv      = u$population$starv,
				    dup        = u$population$dup,
				    death      = u$population$death,
	         	mu_max     = u$mu_max      %||% 1,   
				    population = u$initial_count,
				    stringsAsFactors = FALSE
				  )))
			} else data.frame()

			# ----- read the GUI-temp bounds files --------------------------
			proj_df  <- if (file.exists(session_files$proj))  read.csv(session_files$proj,
						                                                     stringsAsFactors = FALSE) else data.frame()
			nproj_df <- if (file.exists(session_files$nproj)) read.csv(session_files$nproj,
						                                                     stringsAsFactors = FALSE) else data.frame()


			info_list <- lapply(models(), function(mdl) {

				row    <- yaml_tbl[yaml_tbl$model == mdl, , drop = FALSE]

				params <- if (nrow(row))
						        as.list(row[1, c("bioMax","bioMean","bioMin",
						                          "starv","dup","death","mu_max")])
						      else
						        as.list(setNames(rep(NA,7),
						                 c("bioMax","bioMean","bioMin",
						                   "starv","dup","death","mu_max")))

				init_b <- if (nrow(row)) row$bioMean[1] else NA

				list(
					params          = params,
					population      = if (nrow(row)) row$population[1] else NA,
					projected       = make_bounds_single(proj_df , mdl),
					nonprojected    = make_bounds_single(nproj_df, mdl),
					initial_biomass = init_b
				)
			})


			names(info_list) <- models()
			node_data(info_list)
			
			sync_setup(yml)   # <— registra observer una sola volta

			load_default_conf(base_dir)

		})

		## ────────────────────────────────────────────────────────────────────
		##  CARTELLA HYPERNODE / CONFIG - gestione avanzata
		## ────────────────────────────────────────────────────────────────────
		observeEvent(input$hypernode_dir, {
			root_path <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			if (length(root_path) == 0) return()

			required <- c("biounits", "config", "gen", "output", "petri_net", "src")
			has_all_subdirs <- function(p) all(dir.exists(file.path(p, required)))

			proceed_with_config <- function(cfg_path) {
				cfg_root(cfg_path)          # memorizza il nuovo path
				dir_valid(TRUE)
				mdl <- list.dirs(file.path(cfg_path, "biounits"),
				                 full.names = FALSE, recursive = FALSE)
				models(mdl)
			}

			## ── ①  se hanno già la struttura richiesta (e non è .base) -----------
			if (has_all_subdirs(root_path) && basename(root_path) != ".base") {
				message("[DEBUG] single config folder selected: ", root_path)
				proceed_with_config(root_path)
				return()
			}

			## ── ②  altrimenti siamo su un hypernode: elenco configurazioni valide ---
			subdirs <- list.dirs(root_path, full.names = TRUE, recursive = FALSE)
			valid   <- subdirs[vapply(subdirs, has_all_subdirs, logical(1))]

			## escludi sempre la cartella nascosta .base
			valid <- valid[basename(valid) != ".base"]

			## ────────────────────────────────────────────────────────────────────────
			## DEBUG: stampo cosa ho trovato in `valid`
			## ────────────────────────────────────────────────────────────────────────
			message("[DEBUG] root_path: ", root_path)
			message("[DEBUG] all subdirs: ", paste(basename(subdirs), collapse = ", "))
			message("[DEBUG] valid configs (full paths): ", paste(valid, collapse = ", "))
			message("[DEBUG] valid config names: ", paste(basename(valid), collapse = ", "))
			## ────────────────────────────────────────────────────────────────────────

			if (!length(valid)) {
				showModal(modalDialog(
				  title   = "Invalid Directory",
				  HTML("<p>The selected folder is not a hypernode nor contains valid configurations.</p>"),
				  easyClose = TRUE, footer = modalButton("OK")
				))
				return()
			}

			conf_names  <- basename(valid)
			names(valid) <- conf_names
			new_conf_id <- "__NEW_CONFIG__"

			showModal(modalDialog(
				title   = "Select configuration",
				selectInput(ns("choose_cfg"), "Configuration:",
				            choices = c(conf_names, "➕ Create new…" = new_conf_id)),
				footer = tagList(
				  modalButton("Cancel"),
				  actionButton(ns("confirm_cfg"), "Continue", class = "btn-primary")
				),
				easyClose = FALSE
			))

observeEvent(input$confirm_cfg, ignoreInit = TRUE, {
  removeModal()
  sel <- input$choose_cfg
  message("[DEBUG confirm_cfg] user selected: '", sel, "'")

  if (is.null(sel)) {
    message("[DEBUG confirm_cfg] sel is NULL, returning")
    return()
  }

  # ── Ricalcola lista di configurazioni valide ─────────────────────────────
  subdirs    <- list.dirs(root_path, full.names = TRUE, recursive = FALSE)
  valid_dirs <- subdirs[vapply(subdirs, has_all_subdirs, logical(1))]
  valid_dirs <- valid_dirs[!basename(valid_dirs) %in% c(".base", ".default_conf")]
  names(valid_dirs) <- basename(valid_dirs)
  message("[DEBUG confirm_cfg] current valid configs: ",
          paste(names(valid_dirs), collapse = ", "))

  # ── Configurazione esistente? ───────────────────────────────────────────
  if (sel != new_conf_id) {
    if (!sel %in% names(valid_dirs)) {
      showNotification("Configurazione non valida.", type = "error")
      message("[DEBUG confirm_cfg] sel not in valid_dirs: ", sel)
      return()
    }
    message("[DEBUG confirm_cfg] loading existing config: ", sel,
            " → ", valid_dirs[[sel]])
    proceed_with_config(valid_dirs[[sel]])
    return()
  }

  # ── Altrimenti: clonazione da .base ─────────────────────────────────────
  base_dir <- file.path(root_path, ".base")
  if (!has_all_subdirs(base_dir)) {
    showNotification("La cartella nascosta '.base' manca o è invalida.",
                     type = "error")
    return()
  }

  # Chiedo il nome della nuova config
  default_name <- sprintf("conf_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
  showModal(modalDialog(
    title     = "New configuration",
    textInput(ns("new_cfg_name"), "Name:", value = default_name),
    footer    = tagList(
      modalButton("Cancel"),
      actionButton(ns("create_cfg"), "Create", class = "btn-primary")
    ),
    easyClose = FALSE
  ))

  observeEvent(input$create_cfg, ignoreInit = TRUE, {
    cfg_name <- trimws(input$new_cfg_name)
    message("[DEBUG create_cfg] requested new config name: '", cfg_name, "'")
    if (cfg_name == "" || grepl("[/\\\\]", cfg_name)) {
      showNotification("Invalid name.", type = "error")
      message("[DEBUG create_cfg] invalid name, aborting")
      return()
    }

    new_dir <- file.path(root_path, cfg_name)
    if (dir.exists(new_dir)) {
      showNotification("A configuration with that name already exists.",
                       type = "error")
      message("[DEBUG create_cfg] target dir already exists: ", new_dir)
      return()
    }

    # Clono da .base
    message("[DEBUG create_cfg] creating new configuration at: ", new_dir)
    dir.create(new_dir)
    file.copy(
      list.files(base_dir, full.names = TRUE),
      new_dir, recursive = TRUE
    )

    # Rinomina *_base → *_<cfg_name>
    files <- list.files(new_dir, recursive = TRUE, full.names = TRUE)
    for (f in files) {
      old <- basename(f)
      new <- sub("_base", paste0("_", cfg_name), old, fixed = TRUE)
      if (new != old) {
        file.rename(f, file.path(dirname(f), new))
        message("[DEBUG create_cfg] renamed ", old, " → ", new)
      }
    }

    # Rigenero il .fbainfo per la nuova config
    {
    
      showModal(modalDialog(
        title  = NULL,
        tagList(
          div(style="text-align:center;",
              img(src = "running.png", height = "400px", alt = "Running…")),
          br(),
          div("Your new configuration is being prepared. Please wait while the system completes the setup...",
              style = "text-align:center; font-weight:bold;")
        ),
        footer    = NULL,
        easyClose = FALSE,
        size      = "l"
      ))

      gen_dir    <- file.path(new_dir, "gen")
      net_file   <- list.files(
        file.path(new_dir, "petri_net"),
        "\\.PNPRO$", full.names = TRUE
      )
      trans_file <- list.files(
        file.path(new_dir, "src"),
        "\\.cpp$", full.names = TRUE
      )[1]
      fba_files  <- list.files(
        file.path(new_dir, "biounits"),
        "\\.txt$", full.names = TRUE, recursive = TRUE
      )

      message("[SIM] Rigenero il .fbainfo per la nuova config…")
      epimodFBAfunctions::model_generation_GUI(
        net_fname         = net_file,
        transitions_fname = trans_file,
        fba_fname         = fba_files,
        output_dir        = gen_dir
      )
      message("[SIM] .fbainfo rigenerato con successo")
    }

    removeModal()
    proceed_with_config(new_dir)
  })
	})
}, ignoreNULL = TRUE)



    # Reset handler
		observeEvent(input$btn_reset, {
			reset_flag(TRUE)

			unlink(c(session_files$proj, session_files$nproj), force = TRUE)
			session_files$proj <- session_files$nproj <- NULL

			node_data(list())     # svuota cache
			models(character())   # svuota elenco modelli
			dir_valid(FALSE)
		})

    
		# ------------------------------------------------------------------
		#  EXPORT: user chooses folder *and* filename for current *_gui.csv
		# ------------------------------------------------------------------
		shinyFiles::shinyDirChoose(
			input       = input,
			id          = "export_dir",
			roots       = roots,
			session     = session,
			defaultRoot = "Home"
		)

		# store chosen directory until user confirms filename
		export_dest <- reactiveVal(NULL)

		# 1) user picks the destination directory ---------------------------
		observeEvent(input$export_dir, {
			dir_chosen <- shinyFiles::parseDirPath(roots, input$export_dir)
			if (length(dir_chosen) == 0) return()

			export_dest(dir_chosen)   # remember for step-2

			showModal(
				modalDialog(
				  title = "Save bounds as …",
				  textInput(ns("export_fname"),
				            "Base filename (without extension)",
				            value = "bounds"),
				  footer = tagList(
				    modalButton("Cancel"),
				    actionButton(ns("confirm_export"), "Save", class = "btn-primary")
				  ),
				  easyClose = TRUE
				)
			)
		})
		
		# ------------------------------------------------------------------
		#  LOAD: replace *_gui.csv with a user-selected CSV file
		# ------------------------------------------------------------------
		shinyFiles::shinyFileChoose(
			input, id = "load_proj",
			roots = roots, session = session, filetypes = "csv"
		)

		# --- carica non-projected -------------------------------------------
		shinyFiles::shinyFileChoose(
			input, id = "load_nproj",
			roots = roots, session = session, filetypes = "csv"
		)

		# handler projected
		observeEvent(input$load_proj, {
			src <- shinyFiles::parseFilePaths(roots, input$load_proj)$datapath
			if (length(src) && !is.na(src))
				if (file.copy(src, session_files$proj, overwrite = TRUE)) {
				  refresh_bounds()
				  showNotification("Projected bounds file loaded.", type = "message")
				}
		})

		# handler non-projected
		observeEvent(input$load_nproj, {
			src <- shinyFiles::parseFilePaths(roots, input$load_nproj)$datapath
			if (length(src) && !is.na(src))
				if (file.copy(src, session_files$nproj, overwrite = TRUE)) {
				  refresh_bounds()
				  showNotification("Non-projected bounds file loaded.", type = "message")
				}
		})


		## helper: refresh node_data() after either file is loaded -----------
		refresh_bounds <- function() {
			proj_df  <- if (file.exists(session_files$proj))
				            read.csv(session_files$proj,  stringsAsFactors = FALSE) else data.frame()
			nproj_df <- if (file.exists(session_files$nproj))
				            read.csv(session_files$nproj, stringsAsFactors = FALSE) else data.frame()

			nd <- node_data()
			for (mdl in names(nd)) {
				nd[[mdl]]$projected    <- make_bounds_single(proj_df , mdl)
				nd[[mdl]]$nonprojected <- make_bounds_single(nproj_df, mdl)
			}
			node_data(nd)
		}



		# 2) user confirms filename -----------------------------------------
		observeEvent(input$confirm_export, {
			fname <- trimws(input$export_fname)
			if (fname == "" || is.null(export_dest())) {
				showNotification("Please enter a file name.", type = "error", duration = 3)
				return()
			}

			fname <- gsub("[^A-Za-z0-9_\\-]", "_", fname)
			dest  <- export_dest()
			ok1   <- file.copy(session_files$proj,
				                 file.path(dest, paste0(fname, "_projected.csv")),
				                 overwrite = TRUE)
			ok2   <- file.copy(session_files$nproj,
				                 file.path(dest, paste0(fname, "_non_projected.csv")),
				                 overwrite = TRUE)

			removeModal()
			showNotification(
				if (ok1 && ok2) paste("Bounds saved as", fname, "_*.csv in", dest)
				               else "Copy failed – check permissions.",
				type     = if (ok1 && ok2) "message" else "error",
				duration = 4
			)
			export_dest(NULL)
		})



    # Render reset button (only when dir_valid is TRUE)
    output$reset_btn_ui <- shiny::renderUI({
      if (dir_valid()) {
        shiny::actionButton(
          inputId = ns("btn_reset"),
          label   = NULL,
          icon    = shiny::icon("redo"),
          class   = "btn-sm btn-outline-secondary"
        )
      }
    })

		# Directory selector UI
		output$dir_selector <- renderUI({
			if (dir_valid() && !reset_flag()) {
				path <- get_cfg_root()
				div(class = "sim-section-card directory",

				  # Current directory display
				  h5(icon("folder-open"), "Hypernode Directory", class = "sim-section-title"),
				  div(class = "selected-dir d-flex align-items-center",
				    strong("Current:", class = "me-2 text-secondary"),
				    span(basename(path), class = "badge bg-primary text-white fs-6")
				  ),

				  # ↳ Reset directory button
				  div(class = "mt-3 text-right",
				    actionButton(
				      ns("btn_reset"),
				      NULL,
				      icon = icon("redo"),
				      class = "btn-reset-sim"
				    )
				  ),
				div(class = "mt-2 d-flex gap-2",
					actionButton(
						ns("save_configuration"),
						label = tagList(icon("download"), "Save Configuration…"),
						class = "btn btn-sm text-white",
						style = "
							background-color: #2980b9;
							border-color:     #1f5f8b;
							color:            #fff;
						"
					),
					actionButton(
						ns("load_configuration"),
						label = tagList(icon("upload"), "Load Configuration…"),
						class = "btn btn-sm text-white",
						style = "
							background-color: #2980b9;
							border-color:     #1f5f8b;
							color:            #fff;
						"
					)
				)


				  
				)
			} else {
				reset_flag(FALSE)
				div(class = "sim-section-card directory",
				  h5(icon("folder-open"), "Choose Hypernode Directory For Model Analysis Simulation", class = "sim-section-title"),
				  div(class = "sim-dir-selector mt-3",
				    shinyFiles::shinyDirButton(
				      id    = ns("hypernode_dir"),
				      label = "Browse…",
				      title = "Choose hypernode folder",
				      icon  = icon("folder-open"),
				      class = "btn btn-primary btn-sim-dir shinyDirButton"
				    )
				  )
				)
			}
		})

    # Render model list
		# Model list UI
		output$model_list <- renderUI({
			if (dir_valid()) {
				div(
				  id    = ns("sim_models_section"),
				  class = "sim-section-card models",

				  # Header
				  h5(icon("cubes"), "Models", class = "sim-section-title"),

				  # Scrollable list-group
				  div(
				    id    = ns("sim_model_list"),
				    class = "sim-model-list list-group",
				    lapply(models(), function(m) {
				      actionLink(
				        inputId = ns(paste0("model_", m)),
				        label   = tagList(icon("cube"), span(m, class = "model-label")),
				        class   = "list-group-item list-group-item-action model-link"
				      )
				    })
				  ),
				)
			}
		})


		# ── helper: default from node_data()  (unchanged) ---------------------------
		get_default_conc <- function(met) {
			nd <- isolate(node_data())
			if (is.null(nd) || !length(nd)) return(0)
			for (mdl in names(nd)) {
				df <- nd[[mdl]]$projected
				if (!nrow(df)) next
				idx <- which(df$reaction == paste0("EX_", met, "_r"))
				if (length(idx)) {
				  val <- df$background_conc[idx[1]]
				  if (!is.na(val)) return(val)
				}
			}
			0
		}

		# ── utility: write new value into *all* models ------------------------------
		propagate_to_all_models <- function(reaction, new_val) {
			nd    <- node_data()
			dirty <- FALSE
			for (mdl in names(nd)) {
				df  <- nd[[mdl]]$projected
				idx <- which(df$reaction == reaction)
				if (length(idx) && !isTRUE(all.equal(df$background_conc[idx], new_val))) {
				  message("[SYNC] ", reaction, " for ", mdl, " ← ", new_val)
				  df$background_conc[idx] <- new_val
				  nd[[mdl]]$projected     <- df
				  dirty <- TRUE
				}
			}
			if (dirty) {
				node_data(nd)                              # trigger reactives
				rebuild_bounds_csv_single(session_files$proj, "projected")
			}
		}

		# ──  main synchronisation registrator  --------------------------------------
		sync_setup <- function(yml) {
			bm_defs <- yml$boundary_metabolites %||% character()
			met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)

			if (!exists(".sync_reg", envir = session$userData))
				session$userData$.sync_reg <- character()

			for (i in seq_along(bm_defs)) {
				## copy-in-loop to avoid late binding
				local({
				  met       <- bm_defs[i]
				  met_id    <- met_ids[i]
				  input_id  <- paste0("conc_", met_id)
				  reaction  <- paste0("EX_", met, "_r")

				  if (input_id %in% session$userData$.sync_reg) return(NULL)

				  # A) numericInput  → all models
				  observeEvent(input[[input_id]], ignoreInit = TRUE, {
				    new_val <- input[[input_id]]
				    message("[EDIT] ", input_id, " → ", new_val)
				    propagate_to_all_models(reaction, new_val)
				  }, ignoreNULL = TRUE)

				  # B) any model bounds → numericInput
				  observeEvent(node_data(), {
				    isolate({
				      nd <- node_data()
				      for (mdl in names(nd)) {
				        df <- nd[[mdl]]$projected
				        idx <- which(df$reaction == reaction)
				        if (length(idx)) {
				          val <- df$background_conc[idx[1]]
				          cur <- isolate(input[[input_id]])
				          if (!isTRUE(all.equal(cur, val))) {
				            message("[SYNC] update ", input_id, " ← ", val)
				            updateNumericInput(session, input_id, value = val)
				          }
				          break
				        }
				      }
				    })
				  }, ignoreInit = TRUE)

				  session$userData$.sync_reg <- c(session$userData$.sync_reg, input_id)
				}) # /local
			}
		}


# ─────────────────────────────────────────────────────────────────────────────

		# Simulation configuration inputs & Run handler
		output$sim_controls <- renderUI({
			if (!dir_valid()) return(NULL)
			ns    <- session$ns
			base  <- get_cfg_root()
			cfg_dir <- file.path(base, "config")
			
			# 1) Read boundary metabolites from YAML
			yml_f   <- list.files(cfg_dir, "\\.ya?ml$", full.names = TRUE)[1]
			yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
			bm_defs <- yml$boundary_metabolites %||% character()
			
			# 2) Read system defaults from JSON
			json_f <- file.path(cfg_dir, "boundary_conditions.json")
			jsn    <- if (file.exists(json_f)) jsonlite::fromJSON(json_f) else list()
			default_fba_ub       <- jsn$projected_upper_bound  %||% 1000
			default_fba_lb       <- jsn$projected_lower_bound  %||%  1000
			default_background   <- jsn$background_met   			 %||% 1000
			default_background_lb   <- jsn$background_met_lb   %||% 1000
			default_volume       <- jsn$volume            		 %||% 0.001
			default_cell_density <- jsn$cell_density      		 %||% 1e10


			div(class = "sim-section-card config",
				
				# Section header
				h5(icon("cogs"), "Simulation Configuration", class = "sim-section-title"),
				
				# Times row
				fluidRow(
				  column(4, numericInput(ns("i_time"), "Initial Time (h)",
				                         value = 0, min = 0, width = "30%")),
				  column(4, numericInput(ns("f_time"), "Final Time (h)",
				                         value = 10, min = 0, width = "30%")),
				  column(4, numericInput(ns("s_time"), "Step Size (h)",
				                         value = 1, min = 0.0001, width = "30%"))
				),
				
				# Tolerances row
				fluidRow(
				  column(4, numericInput(ns("atol"), "Absolute Tolerance",
				                         value = 1e-6, min = 0, width = "30%")),
				  column(4, numericInput(ns("rtol"), "Relative Tolerance",
				                         value = 1e-6, min = 0, width = "30%"))
				),
				
				# ──────────── Boundary Metabolite Concentrations ────────────
				if (length(bm_defs)) {
				  met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
				  tagList(
				    hr(),
				    h5("Boundary Metabolite Concentrations (mmol)", class = "sim-section-title"),
				    div(style = "max-height:300px; overflow-y:auto; padding-right:10px;",
				      lapply(seq_along(bm_defs), function(i) {
				        met    <- bm_defs[i]
				        met_id <- met_ids[i]
				        numericInput(
				          ns(paste0("conc_", met_id)),
				          label = met,
				          value = get_default_conc(met),
				          min   = 0,
				          width = "30%"
				        )
				      })
				    )
				  )
				},
			## ──────────────── System Parameters (temporarily hidden) ────────────────
					div(
						hr(),
						h5(
							"System Parameters",
							span(
								icon("question-circle"), 
								style = "cursor:help; margin-left:8px;",
								title = "Default system parameters."
							),
							class = "sim-section-title"
						),
						fluidRow(
							column(4,
								numericInput(ns("fba_ub"), "Projected Upper Bound",
								             value = default_fba_ub, min = 0, width = "60%")
							),
							column(4,
								numericInput(ns("fba_lb"), "Projected Lower Bound",
								             value = default_fba_lb, width = "60%")
							),
							column(4,
								numericInput(ns("background_met"), "Non Projected Upper Bound",
								             value = default_background, min = 0, width = "60%")
							),
							column(4,
								numericInput(ns("background_met_lb"), "Non Projected Upper Bound",
								             value = default_background, min = 0, width = "60%")
							)
						),
						fluidRow(
							column(4,
								numericInput(ns("sys_volume"), "System Volume (mL)",
								             value = default_volume, min = 0, width = "60%")
							),
							column(4,
								numericInput(ns("cell_density"), "Initial Cell Density (cells/mL)",
								             value = default_cell_density, min = 0, width = "60%")
							)
						)
					),
				# Run button row
				fluidRow(
				  column(12,
				    div(class = "text-center mt-3",
				      actionButton(ns("btn_run_sim"), "Run Simulation",
				                   class = "btn-run-sim px-5")
				    )
				  )
				)
			)
		})

		## ─────────────────────────────────────────────────────────────
		##  RUN SIMULATION & OPTIONAL REGENERATION
		## ─────────────────────────────────────────────────────────────

		pre_check_models <- function() {
			regenerate_needed <- FALSE
			base   <- get_cfg_root()
			nd     <- node_data()
			for (mdl in names(nd)) {
				biounit_d <- file.path(base, "biounits", mdl)
				txt_file  <- list.files(biounit_d, "_model\\.txt$", full.names = TRUE)[1]
				if (!file.exists(txt_file)) next
				lines <- readLines(txt_file)
				params <- nd[[mdl]]$params
				# expected on lines 5,6,7
				new_vals <- c(as.character(params$bioMax),
				              as.character(params$bioMean),
				              as.character(params$bioMin))
				old_vals <- lines[5:7]
				if (!all(old_vals == new_vals)) {
				  # patch the file
				  lines[5:7] <- new_vals
				  writeLines(lines, txt_file)
				  message(sprintf("[DEBUG] Patched %s lines 5–7: %s", basename(txt_file), paste(new_vals, collapse="/")))
				  regenerate_needed <- TRUE
				}
			}
			regenerate_needed
		}
		
		# 1) Utility: patch JSON, optionally regenerate model, then run analysis
		run_simulation <- function(regenerate = FALSE) {
		
			regen_from_params <- pre_check_models()
			if (regen_from_params) {
				message("[DEBUG] FBA models out of sync with GUI parameters → forcing regeneration")
				regenerate <- TRUE
				showNotification("Detected parameter changes → regenerating FBA models", type="warning")
			}
			base       <- get_cfg_root()
			config_dir <- file.path(base, "config")
			petri_dir  <- file.path(base, "petri_net")
			src_dir    <- file.path(base, "src")
			biounits   <- file.path(base, "biounits")
			gen_dir    <- file.path(base, "gen")

			# A) Patch boundary_conditions.json
			bc_path <- file.path(config_dir, "boundary_conditions.json")
			if (file.exists(bc_path)) {
				bc <- jsonlite::fromJSON(bc_path, simplifyVector = FALSE)
				bc$volume       <- input$sys_volume
				bc$cell_density <- input$cell_density
				jsonlite::write_json(bc, bc_path, auto_unbox = TRUE, pretty = TRUE)
			}

			# B) Regenerate FBA‐model if requested
			if (regenerate) {
				showModal(modalDialog(
				  title     = NULL,
				  tagList(
				    div(style="text-align:center;", img(src="running.png", height="400px")),
				    br(),
				    div("Regenerating model… please wait", style="text-align:center; font-weight:bold;")
				  ),
				  footer    = NULL, easyClose = FALSE, size = "l"
				))

				# Patch C++ stub
				cpp_files <- list.files(src_dir, "\\.cpp$", full.names = TRUE)
				if (length(cpp_files)) {
				  gui_cpp <- cpp_files[[1]]
				  lines   <- readLines(gui_cpp)
				  lines   <- sub("double V = .*?;",
				                 sprintf("double V = %g;", input$sys_volume),
				                 lines)
				  lines   <- sub("long long int delta = .*?;",
				                 sprintf("long long int delta = %g;", input$cell_density),
				                 lines)
				  writeLines(lines, gui_cpp)
				}

				# model_generation_GUI
				net_file   <- list.files(petri_dir, "\\.PNPRO$", full.names = TRUE)
				trans_file <- list.files(src_dir, "\\.cpp$",   full.names = TRUE)[1]
				fba_txts   <- list.files(biounits, "\\.txt$",  full.names = TRUE, recursive = TRUE)
				epimodFBAfunctions::model_generation_GUI(
				  net_fname         = net_file,
				  transitions_fname = trans_file,
				  fba_fname         = fba_txts,
				  output_dir        = gen_dir
				)
				removeModal()
			}

			# C) Build GUI‐snapshot YAML
			hn_name   <- basename(dirname(base))
			cfg_name  <- basename(base)
			hypernode <- paste(hn_name, cfg_name, sep = "_")

			orig_yml_path <- list.files(config_dir, "\\.ya?ml$", full.names = TRUE)[1]
			gui_yml       <- if (length(orig_yml_path)) yaml::read_yaml(orig_yml_path) else list()

			gui_yml$simulation <- list(
				initial_time       = input$i_time,
				final_time         = input$f_time,
				step_size          = input$s_time,
				absolute_tolerance = input$atol,
				relative_tolerance = input$rtol
			)
			bm_defs <- gui_yml$boundary_metabolites %||% character()
			if (length(bm_defs)) {
				met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
				gui_yml$simulation$boundary_concentrations <- setNames(
				  lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				  bm_defs
				)
			}
			gui_yml$simulation$system_parameters <- list(
				fba_upper_bound = input$fba_ub,
				fba_lower_bound = input$fba_lb,
				background_met  = input$background_met,
				volume          = input$sys_volume,
				cell_density    = input$cell_density
			)

			gui_yaml_path <- file.path(config_dir, paste0(hypernode, "_gui.yaml"))
			yaml::write_yaml(gui_yml, gui_yaml_path)

			# D) Launch analysis
			showModal(modalDialog(
				title     = NULL,
				tagList(
				  div(style="text-align:center;", img(src="running.png", height="400px")),
				  br(),
				  div("Running simulation and analysis…", style="text-align:center; font-weight:bold;")
				),
				footer    = NULL, easyClose = FALSE, size = "l"
			))

			tryCatch({
				debug_paths <- c(
				  gen    = file.path(base, "gen"),
				  config = config_dir,
				  src    = file.path(base, "src"),
				  output = file.path(base, "output")
				)
				fba_files  <- list.files(file.path(base, "biounits"), "\\.txt$", full.names = TRUE, recursive = TRUE)
				user_files <- c(
				  file.path(config_dir, "population_parameters.csv"),
				  file.path(base, "gen",    paste0(hypernode, ".fbainfo")),
				  file.path(base, "output", "ub_bounds_projected_gui.csv"),
				  file.path(base, "output", "non_projected_bounds_gui.csv")
				)

				epimodFBAfunctions::model_analysis_GUI(
				  paths           = debug_paths,
				  hypernode_name  = hypernode,
				  debug_solver    = FALSE,
				  i_time          = input$i_time,
				  f_time          = input$f_time,
				  s_time          = input$s_time,
				  atol            = input$atol,
				  rtol            = input$rtol,
				  fba_fname       = fba_files,
				  user_files      = user_files,
				  volume          = base
				)

				removeModal()
				showModal(modalDialog(
				  title = "✅ Simulation & Analysis Complete",
				  tagList(
				    div(style="text-align:center;", img(src="success.png", height="400px")),
				    br(),
				    div("Your model has been generated and analyzed successfully.", style="text-align:center; font-weight:bold;"),
				    br(),
				    div("What next?", style="text-align:center;")
				  ),
				  footer = tagList(
				    actionButton(ns("btn_visualize"), "Visualize Results", class="btn-primary"),
				    actionButton(ns("btn_new_sim"),   "New Simulation",   class="btn-secondary")
				  ),
				  easyClose = FALSE, size = "l"
				))
				session$userData$last_hypernode <- base

			}, error = function(err) {
				removeModal()
				showModal(modalDialog(
				  title = "❌ Simulation Error",
				  tagList(
				    div(style="text-align:center;", img(src="error.png", height="400px")),
				    br(),
				    div(paste("An error occurred:", err$message), style="text-align:center; color:red; font-weight:bold;")
				  ),
				  easyClose = TRUE, footer = modalButton("Close"), size = "l"
				))
			}, finally = {
				save_default_conf(base)
				unlink(c(session_files$proj, session_files$nproj), force = TRUE)
				unlink(gui_yaml_path, force = TRUE)
				unlink(list.files(src_dir, "*_gui\\.(R|cpp)$", full.names = TRUE), force = TRUE)
			})
		}

		# 2) Run button → if sys-params changed ask; otherwise run directly
observeEvent(input$btn_run_sim, {
  bc_path <- file.path(get_cfg_root(), "config", "boundary_conditions.json")
  old_bc  <- if (file.exists(bc_path)) jsonlite::fromJSON(bc_path) else list()
  old_vol <- old_bc$volume; old_den <- old_bc$cell_density

  if (need_regenerate() ||
      (!is.null(old_vol) && (old_vol != input$sys_volume || old_den != input$cell_density))) {
    showModal(modalDialog(
      title = "Regeneration Required",
      "You have changed either system or model parameters.  The FBA models must be regenerated before simulation. Continue?",
      footer = tagList(
        modalButton("No"),
        actionButton(ns("confirm_regen"), "Yes, regenerate", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  } else {
    run_simulation(FALSE)
  }
})

observeEvent(input$confirm_regen, ignoreInit = TRUE, {
  removeModal()
  need_regenerate(FALSE)    # clear flag
  run_simulation(TRUE)
})



 		  # ── modals for each bacteria ─────────────────────────────────────────
		observe({
			req(dir_valid())

			lapply(models(), function(mdl) {
				local({

				  mdl2 <- mdl
				  observeEvent(input[[paste0("model_", mdl2)]], ignoreInit = TRUE, {

				    ## snapshot dati correnti -------------------------------------------------
				    info0     <- node_data()[[mdl2]]
				    params    <- info0$params
				    pop       <- info0$population
				    proj      <- info0$projected
				    nonproj   <- info0$nonprojected   # nuovo unico DF

				    ## ID univoci per output --------------------------------------------------
				    param_id  <- paste0("param_",    mdl2)
				    proj_id   <- paste0("proj_tbl_", mdl2)
				    nonpr_id  <- paste0("nonpr_tbl_", mdl2)
				    save_id   <- paste0("save_",     mdl2)

				    message("[DEBUG] Opening modal for model: ", mdl2)

				    ## mostro il modal --------------------------------------------------------
				    showModal(
				      tags$div(
				        id = "modelDetailModal",
				        tags$head(
				          tags$style(HTML("
				            #modelDetailModal .modal-content {background:#f5f5f5;border-radius:8px;overflow:hidden;}
				            #modelDetailModal .modal-header,#modelDetailModal .modal-footer {background:#27ae60;color:#fff;border:none;padding:1rem 1.5rem;}
				            #modelDetailModal .modal-title {font-size:2rem;font-weight:700;text-align:center;position:relative;margin-bottom:0.5rem;color:#fff;}
				            #modelDetailModal .modal-title::after {content:'';display:block;width:80px;height:4px;background:#2ecc71;margin:0.5rem auto 0;border-radius:2px;}
				            #modelDetailModal .modal-body {padding:1.5rem;color:#333;}
				            #modelDetailModal .btn-default {background:transparent;color:#fff;border:1px solid #fff;}
				            #modelDetailModal .btn-default:hover {background:rgba(255,255,255,0.1);}
				            #modelDetailModal .modal-save-btn {background:#2ecc71;border-color:#27ae60;color:#fff;}
				            #modelDetailModal .modal-save-btn:hover {background:#27ae60;}
				          "))
				        ),
				        modalDialog(
				          title     = paste("Model:", mdl2),
				          size      = "l",
				          easyClose = FALSE,
				          class     = "modal-model modal-model-detail",

				          # ── Initial Biomass --------------------------------------------------
				          {
				            init_val <- info0$initial_biomass %||% params$bioMean
				            div(class = "modelgen-subcard",
				              h5("Biomass data", class = "subcard-title"),
				              fluidRow(
				                column(6,
				                  numericInput(
				                    ns(paste0("bio_init_", mdl2)),
				                    label = "Initial Biomass",
				                    value = init_val,
				                    min   = 0, step = 0.01, width = "30%"
				                  )
				                )
				              )
				            )
				          },
				          hr(class = "modal-divider"),

				          # ── Parameters -------------------------------------------------------
				          div(class = "modelgen-subcard",
				            fluidRow(
				              column(12,
				                h5("Parameters", class = "subcard-title"),
				                DT::dataTableOutput(ns(param_id))
				              )
				            )
				          ),
				          hr(class = "modal-divider"),

				          # ── Bounds (2 tab) ---------------------------------------------------
				          div(class = "modelgen-subcard",
				            h5("Exchange reaction bounds", class = "subcard-title"),
				            div(class = "modal-tabs",
				              tabsetPanel(
				                type = "tabs",
				                tabPanel("Projected",
				                         DT::dataTableOutput(ns(proj_id))),
				                tabPanel("Non-projected",
				                         DT::dataTableOutput(ns(nonpr_id)))
				              )
				            )
				          ),

				          footer = tagList(
				            modalButton("Close"),
				            actionButton(ns(save_id), "Save changes",
				                         class = "btn-primary modal-save-btn")
				          )
				        )
				      )
				    ) # /showModal

				    ## ───────── DT: parameters  ─────────
						output[[param_id]] <- DT::renderDataTable({

							## helper: se il valore non è scalare, prende il primo elemento
							scalar_num <- function(x) {
								out <- suppressWarnings(as.numeric(x[1]))
								if (length(out) == 0 || is.na(out)) NA_real_ else out
							}

							vals <- vapply(params, scalar_num, numeric(1))

							df <- data.frame(
								Parameter   = c(names(params), "initial_count"),
								Value       = c(vals, pop),
								check.names = FALSE,
								stringsAsFactors = FALSE
							)

							DT::datatable(
								df,
								rownames  = FALSE,
								editable  = list(target = "cell",
												         disable = list(columns = 0)),   # blocca la colonna “Parameter”
								options   = list(dom = "t", paging = FALSE, ordering = FALSE)
							)
						}, server = TRUE)

						param_proxy <- DT::dataTableProxy(param_id, session)


				    ## edit parameters
						## ── Edit Parameters & Trigger FBA Regeneration ──────────────────────────────
						observeEvent(input[[paste0(param_id, "_cell_edit")]], ignoreInit = TRUE, {
							edit    <- input[[paste0(param_id, "_cell_edit")]]
							row     <- edit$row
							new_val <- suppressWarnings(as.numeric(edit$value))
							if (is.na(new_val)) {
								showNotification("Value must be numeric", type = "error")
								return()
							}

							isolate({
								# 1) Update in-memory cache
								nd    <- node_data()
								info1 <- nd[[mdl2]]
								fields <- names(info1$params)
								name   <- fields[row]
								info1$params[[name]] <- new_val
								nd[[mdl2]] <- info1
								node_data(nd)
							})

							# 2) Overwrite lines 5–7 in the corresponding *_model.txt under biounits/<model>/
							base      <- get_cfg_root()
							biounit_d <- file.path(base, "biounits", mdl2)
							txt_file  <- list.files(biounit_d, "_model\\.txt$", full.names = TRUE)[1]
							if (length(txt_file) == 1 && file.exists(txt_file)) {
								lines <- readLines(txt_file)
								# assume lines[5]=bioMax, [6]=bioMean, [7]=bioMin
								lines[5] <- as.character(info1$params$bioMax)
								lines[6] <- as.character(info1$params$bioMean)
								lines[7] <- as.character(info1$params$bioMin)
								writeLines(lines, txt_file)
								message("[DEBUG] Patched ", basename(txt_file), " (bioMax/bioMean/bioMin)")
							}

							# 3) Flag that FBA regeneration is required
							need_regenerate(TRUE)
						#	showNotification("Model parameters changed → FBA model regeneration required", type = "warning")

							# 4) Update the DataTable in the modal
							scalar_num <- function(x) {
								out <- suppressWarnings(as.numeric(x[1]))
								if (length(out)==0 || is.na(out)) NA_real_ else out
							}
							upd <- node_data()[[mdl2]]
							vals2 <- vapply(upd$params, scalar_num, numeric(1))
							df2 <- data.frame(
								Parameter = c(names(upd$params), "initial_count"),
								Value     = c(vals2, upd$population),
								check.names = FALSE,
								stringsAsFactors = FALSE
							)
							DT::replaceData(param_proxy, df2, resetPaging = FALSE, rownames = FALSE)
						})

						## ───────── DT: bounds  ─────────
						opts <- list(dom = "ft",
												 paging = FALSE,
												 ordering = FALSE,
												 scrollY = 300,
												 scrollCollapse = TRUE)

						lock <- list(target = "cell",
												 disable = list(columns = 0))      # blocca la colonna “reaction”

						#───────────────────────────────────────────────────────────────────
						# 1) PROJECTED
						#    se l'intera colonna ‘lower’ (→ _r) o ‘upper’ (→ _f) è NA
						#    la togliamo, così non compaiono direzioni inesistenti.
						#───────────────────────────────────────────────────────────────────
						proj_disp <- proj   # copia di lavoro

						if ("lower" %in% names(proj_disp) && all(is.na(proj_disp$lower)))
							proj_disp$lower <- NULL
						if ("upper" %in% names(proj_disp) && all(is.na(proj_disp$upper)))
							proj_disp$upper <- NULL

						# ─────────────────────────────────────────────────────────────
						#  UTILITIES  — filtri di sicurezza: mai mostrare righe “vuote”
						# ─────────────────────────────────────────────────────────────
						clean_bounds_df <- function(df) {
							# tieni solo le righe che esistono davvero nel CSV:
							#   • background_conc non NA
							#   • reaction non stringa vuota
							df <- df[ !(is.na(df$background_conc) | df$reaction == ""), ,
												drop = FALSE ]
							rownames(df) <- NULL
							df
						}

						# ─────────────────────────────────────────────────────────────
						#  RENDER DELLE TAB  — sempre in sync con node_data()
						# ─────────────────────────────────────────────────────────────
						output[[proj_id]] <- DT::renderDataTable({
							# 1) prendi lo stato attuale dalla cache reattiva
							current <- node_data()[[mdl2]]$projected

							# 2) eventuali colonne tutte-NA da rimuovere (forward / reverse mancanti)
							if ("lower" %in% names(current) && all(is.na(current$lower)))
								current$lower <- NULL
							if ("upper" %in% names(current) && all(is.na(current$upper)))
								current$upper <- NULL

							DT::datatable(
								clean_bounds_df(current),
								rownames = FALSE,
								editable = "cell",
								options  = opts
							)
						}, server = TRUE)

						output[[nonpr_id]] <- DT::renderDataTable({
							current <- node_data()[[mdl2]]$nonprojected
							DT::datatable(
								clean_bounds_df(current),
								rownames = FALSE,
								editable = "cell",
								options  = opts
							)
						}, server = TRUE)


						# proxy per gli update immediati
						proj_proxy  <- DT::dataTableProxy(proj_id,  session)
						nonpr_proxy <- DT::dataTableProxy(nonpr_id, session)

						## ---- projected ----
						observeEvent(input[[paste0(proj_id, "_cell_edit")]], ignoreInit = TRUE, {
							ed  <- input[[paste0(proj_id, "_cell_edit")]]
							row <- ed$row
							col <- ed$col + 1        # data-frame column (1-based)

							isolate({
								nd   <- node_data()
								info <- nd[[mdl2]]
								info$projected[row, col] <- as.numeric(ed$value)   # 1) update local model
								nd[[mdl2]] <- info
								node_data(nd)                                     # push to reactives

								# ── se abbiamo toccato background_conc ────────────────────────────────
								if (names(info$projected)[col] == "background_conc") {
									reac <- info$projected$reaction[row]
									newv <- as.numeric(ed$value)
									message("[DT-EDIT] ", reac, " ← ", newv, " (model ", mdl2, ")")

									# 2) propaga a *tutti* i modelli + CSV
									propagate_to_all_models(reac, newv)

									# 3) aggiorna il numericInput corrispondente al metabolita
									met     <- sub("^EX_", "", sub("_r$", "", reac))
									met_id  <- gsub("[^A-Za-z0-9_]", "_", met)
									inp_id  <- paste0("conc_", met_id)
									cur_val <- isolate(input[[inp_id]])
									if (!isTRUE(all.equal(cur_val, newv))) {
										message("[SYNC] update ", inp_id, " ← ", newv)
										updateNumericInput(session, inp_id, value = newv)
									}
								}
							})

							# 4) refresh DT e salva CSV
							DT::replaceData(proj_proxy,
												      node_data()[[mdl2]]$projected,
												      resetPaging = FALSE, rownames = FALSE)
							rebuild_bounds_csv_single(session_files$proj, "projected")
						})


						## ---- non-projected ----
						observeEvent(input[[paste0(nonpr_id, "_cell_edit")]], ignoreInit = TRUE, {
							ed  <- input[[paste0(nonpr_id, "_cell_edit")]]
							row <- ed$row; col <- ed$col + 1
							isolate({
								nd   <- node_data()
								info <- nd[[mdl2]]
								info$nonprojected[row, col] <- as.numeric(ed$value)
								nd[[mdl2]] <- info
								node_data(nd)
							})
							DT::replaceData(nonpr_proxy,
												      node_data()[[mdl2]]$nonprojected,
												      resetPaging = FALSE, rownames = FALSE)
							rebuild_bounds_csv_single(session_files$nproj, "nonprojected")
						})


				    ## save button (initial biomass) -----------------------------------
				    observeEvent(input[[save_id]], ignoreInit = TRUE, {
				      ib_id   <- paste0("bio_init_", mdl2)
				      new_bio <- input[[ib_id]]
				      if (!is.null(new_bio)) {
				        nd          <- node_data()
				        info_sav    <- nd[[mdl2]]
				        info_sav$initial_biomass <- new_bio
				        nd[[mdl2]]  <- info_sav
				        node_data(nd)
				      }
				      removeModal()
				    })

				  }) # /observeEvent click modello
				})   # /local
			})     # /lapply
		})       # /outer observe




		# 1) When user clicks “Save Configuration…”, ask for a name
		observeEvent(input$save_configuration, {
			showModal(
				modalDialog(
				  title = "Name This Configuration",
				  textInput(ns("config_name"), "Configuration name"),
				  footer = tagList(
				    modalButton("Cancel"),
				    actionButton(ns("confirm_save_config"), "Save", class = "btn-primary")
				  ),
				  easyClose = FALSE
				)
			)
		})

		## ─────────────────────────────────────────────────────────────────────────────
		## 2) Quando confermano → salva la configurazione corrente in param_configs/
		## ─────────────────────────────────────────────────────────────────────────────
		observeEvent(input$confirm_save_config, {
			req(input$config_name)
			cfg_name <- trimws(input$config_name)
			if (cfg_name == "") {
				showNotification("Please enter a name.", type = "error")
				return()
			}
			## 1) Sanitise per filesystem
			cfg_name <- gsub("[^A-Za-z0-9_\\-]", "_", cfg_name)

			## 2) Individua la root dell’hypernode (parent di cfg_01, conf_002_new, ecc.)
			cfg_dir      <- get_cfg_root()             # es: /.../minimal_doublet/cfg_01
			hypernode_dir<- dirname(cfg_dir)           # es: /.../minimal_doublet

			## 3) Crea param_configs/ accanto a cfg_01
			param_root <- file.path(hypernode_dir, "param_configs")
			if (!dir.exists(param_root)) dir.create(param_root, recursive = TRUE)

			## 4) Crea la sottocartella per questa parametrizzazione
			save_dir <- file.path(param_root, cfg_name)
		#	if (dir.exists(save_dir)) {
		#		showNotification("A configuration with that name already exists.", type = "error")
		#		return()
		#	}
			dir.create(save_dir)

			## 5) Copia i 2 file GUI‐temp
			file.copy(session_files$proj,
				        file.path(save_dir, basename(session_files$proj)))
			file.copy(session_files$nproj,
				        file.path(save_dir, basename(session_files$nproj)))

			## 6) Costruisci l’oggetto R con parametri e boundary
			cfg <- list(
				times = list(
				  initial_time = input$i_time,
				  final_time   = input$f_time,
				  step_size    = input$s_time
				),
				tolerances = list(
				  atol = input$atol,
				  rtol = input$rtol
				)
			)
			# Boundary concentrations
			yml_f   <- list.files(file.path(cfg_dir, "config"),
				                    "\\.ya?ml$", full.names = TRUE)[1]
			yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
			bm_defs <- yml$boundary_metabolites %||% character()
			if (length(bm_defs)) {
				met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
				cfg$boundary_concentrations <- setNames(
				  lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				  bm_defs
				)
			}
			# System parameters
			cfg$system_parameters <- list(
				projected_upper_bound = input$fba_ub,
				projected_lower_bound = input$fba_lb,
				background_met  = input$background_met,
				background_met_lb  = input$background_met_lb,
				volume          = input$sys_volume,
				cell_density    = input$cell_density
			)
			# Per‐model parameters
			nd <- node_data()
			cfg$models <- lapply(names(nd), function(mdl) {
				info <- nd[[mdl]]
				list(
				  model_name      = mdl,
				  parameters      = info$params,
				  population      = info$population,
				  initial_biomass = info$initial_biomass
				)
			})

			## 7) Scrivi JSON + YAML dentro save_dir
			jsonlite::write_json(cfg,
				                   file.path(save_dir, "config_values.json"),
				                   auto_unbox = TRUE, pretty = TRUE)
			yaml::write_yaml(cfg,
				               file.path(save_dir, paste0("config_", cfg_name, ".yaml")))

			removeModal()
			showNotification(paste("Configuration saved as", cfg_name),
				               type = "message")
		})

		## ─────────────────────────────────────────────────────────────────────────────
		## 1) Click “Load Configuration…” → elenco configurazioni in param_configs/
		## ─────────────────────────────────────────────────────────────────────────────
		observeEvent(input$load_configuration, {
			req(dir_valid())
			# 1) root della config corrente e hypernode
			cfg_dir       <- get_cfg_root()        # es: .../minimal_doublet/cfg_01
			hypernode_dir <- dirname(cfg_dir)      # es: .../minimal_doublet

			# 2) cartella param_configs/
			param_root <- file.path(hypernode_dir, "param_configs")
			if (!dir.exists(param_root)) {
				showNotification("Nessuna parametrizzazione salvata trovata.", type = "warning")
				return()
			}

			# 3) elenco delle sottocartelle (una per ciascuna parametrizzazione)
			configs <- basename(list.dirs(param_root, full.names = TRUE, recursive = FALSE))
			if (length(configs) == 0) {
				showNotification("Nessuna parametrizzazione salvata trovata.", type = "warning")
				return()
			}

			showModal(modalDialog(
				title   = "Load Saved Parametrization",
				selectInput(ns("choose_config"), "Select parametrization:", choices = configs),
				footer  = tagList(
				  modalButton("Cancel"),
				  actionButton(ns("confirm_load_config"), "Load", class = "btn-primary")
				),
				easyClose = FALSE
			))
		})

		## ─────────────────────────────────────────────────────────────────────────────
		## 2) Conferma caricamento → applica la parametrizzazione
		## ─────────────────────────────────────────────────────────────────────────────
		observeEvent(input$confirm_load_config, {
			req(input$choose_config)
			cfg_name      <- input$choose_config
			cfg_dir       <- get_cfg_root()
			hypernode_dir <- dirname(cfg_dir)
			param_root    <- file.path(hypernode_dir, "param_configs")
			load_dir      <- file.path(param_root, cfg_name)

			# 1) Copia i 2 CSV GUI‐temp nella sessione
			for (slot in c("proj", "nproj")) {
				dest <- session_files[[slot]]
				src  <- file.path(load_dir, basename(dest))
				if (file.exists(src)) {
				  file.copy(src, dest, overwrite = TRUE)
				}
			}

			# 2) Rilettura dei CSV appena copiati
			proj_df  <- if (file.exists(session_files$proj))
				            read.csv(session_files$proj,  stringsAsFactors = FALSE) else data.frame()
			nproj_df <- if (file.exists(session_files$nproj))
				            read.csv(session_files$nproj, stringsAsFactors = FALSE) else data.frame()

			# helper “colonna singola”
			make_bounds_single <- function(df, mdl) {
				sub <- df[df$FBAmodel == mdl, , drop = FALSE]
				if (!nrow(sub) || !"background_conc" %in% names(sub))
				  return(data.frame(reaction=character(), background_conc=numeric()))
				agg <- tapply(sub$background_conc, sub$reaction, sum)
				data.frame(reaction=names(agg), background_conc=as.numeric(agg), stringsAsFactors=FALSE)
			}

			# 3) Ricostruisci node_data() con i nuovi bounds
			info_list <- lapply(models(), function(mdl) {
				list(
				  params          = node_data()[[mdl]]$params,
				  population      = node_data()[[mdl]]$population,
				  initial_biomass = node_data()[[mdl]]$initial_biomass,
				  projected       = make_bounds_single(proj_df, mdl),
				  nonprojected    = make_bounds_single(nproj_df, mdl)
				)
			})
			names(info_list) <- models()
			node_data(info_list)

			# 4) Ripristina gli input dal JSON salvato
			json_path <- file.path(load_dir, "config_values.json")
			if (file.exists(json_path)) {
				vals <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

				# tempi & tolleranze
				updateNumericInput(session, "i_time", value = vals$times$initial_time)
				updateNumericInput(session, "f_time", value = vals$times$final_time)
				updateNumericInput(session, "s_time", value = vals$times$step_size)
				updateNumericInput(session, "atol",   value = vals$tolerances$atol)
				updateNumericInput(session, "rtol",   value = vals$tolerances$rtol)

				# boundary concentrations
				yml_f   <- list.files(file.path(cfg_dir, "config"), "\\.ya?ml$", full.names = TRUE)[1]
				yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
				bm_defs <- yml$boundary_metabolites %||% character()
				met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
				for (i in seq_along(bm_defs)) {
				  updateNumericInput(
				    session,
				    paste0("conc_", met_ids[i]),
				    value = vals$boundary_concentrations[[ bm_defs[i] ]]
				  )
				}

				# system parameters (nascosti)
				sp <- vals$system_parameters
				updateNumericInput(session, "fba_ub",       value = sp$projected_upper_bound)
				updateNumericInput(session, "fba_lb",       value = sp$projected_lower_bound)
				updateNumericInput(session, "background_met", value = sp$background_met)
				updateNumericInput(session, "background_met_lb", value = sp$background_met_lb)
				updateNumericInput(session, "sys_volume",   value = sp$volume)
				updateNumericInput(session, "cell_density", value = sp$cell_density)

				# parametri per modello
				if (!is.null(vals$models)) {
				  nd <- node_data()
				  for (m in vals$models) {
				    mdl <- m$model_name
				    if (mdl %in% names(nd)) {
				      nd[[mdl]]$params          <- m$parameters
				      nd[[mdl]]$population      <- m$population
				      nd[[mdl]]$initial_biomass <- m$initial_biomass
				    }
				  }
				  node_data(nd)
				}
			}

			removeModal()
			showNotification(paste("Parametrization loaded:", cfg_name), type = "message")
		})

		# ── 5) Handle modal choices ────────────────────────────────────────────────
		# Visualize Results

 		rootSess <- session
    while (!is.null(rootSess$parent)) rootSess <- rootSess$parent

    # 1) Visualize Results
    observeEvent(input$btn_visualize, {
      removeModal()
      updateNavlistPanel(rootSess, "main_nav", selected = "Data Visualization")
      dir_valid(FALSE); reset_flag(TRUE); models(character())
    })

    # 2) New Simulation
    observeEvent(input$btn_new_sim, {
      removeModal()
      dir_valid(FALSE); reset_flag(TRUE); models(character())
    })


  })
}
