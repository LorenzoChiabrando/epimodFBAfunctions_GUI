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
		rebuild_bounds_csv_single <- function(csv_path, slot_name) {

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

			if (nrow(long))
				write.csv(long, csv_path, row.names = FALSE, quote = FALSE)
		}



		# —————————————————————————————————————————————————————————————
		#  rebuild node_data + create GUI-temp bounds files on valid dir
		# —————————————————————————————————————————————————————————————
		observeEvent(dir_valid(), ignoreInit = TRUE, {
			req(dir_valid())

			base_dir <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
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

		})



    # Observe directory selection
    shiny::observeEvent(input$hypernode_dir, {
      path <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
      if (length(path) == 0) return()

      # Validate subdirectories
      required <- c("biounits", "config", "gen", "output", "petri_net", "src")
      missing  <- required[!base::dir.exists(file.path(path, required))]
      if (length(missing) == 0) {
        dir_valid(TRUE)
        mdl <- list.dirs(file.path(path, "biounits"), full.names = FALSE, recursive = FALSE)
        models(mdl)
      } else {
        dir_valid(FALSE)
        models(character())
				showModal(modalDialog(
					title   = "Invalid Directory",
					HTML(paste0(
						"<p>The selected folder is not a valid hypernode because the following subfolders are missing:</p>",
						"<ul>", paste0("<li>", missing, "</li>", collapse = ""), "</ul>"
					)),
					easyClose = TRUE,
					footer    = modalButton("OK"),
					class     = "modal-sim-invalid"
				))
      }
    })

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
				path <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
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
				  
				#	div(class = "mt-2 d-flex gap-2",
				#			shinyFiles::shinyDirButton(
				#				id    = ns("export_dir"),
				#				label = "Save bounds…",
				#				title = "Copy the *_gui.csv files to a folder you choose",
				#				icon  = icon("download"),
				#				class = "btn btn-outline-secondary btn-sm"
				#			),
				#			shinyFiles::shinyFilesButton(        # ←--- correct name
				#				id    = ns("load_proj"),
				#				label = "Load projected…",
				#				title = "Pick a CSV to replace ub_bounds_projected_gui.csv",
				#				multiple = FALSE,
				#				icon  = icon("file-upload"),
				#				class = "btn btn-outline-secondary btn-sm"
				#			),
				#			shinyFiles::shinyFilesButton(        # ←--- correct name
				#				id    = ns("load_nproj"),
				#				label = "Load not-projected…",
				#				title = "Pick a CSV to replace ub_bounds_not_projected_gui.csv",
				#				multiple = FALSE,
				#				icon  = icon("file-upload"),
				#				class = "btn btn-outline-secondary btn-sm"
				#			)
				#	)

				)
			}
		})


		# Simulation configuration inputs & Run handler
		output$sim_controls <- renderUI({
			if (!dir_valid()) return(NULL)
			ns    <- session$ns
			base  <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			cfg_dir <- file.path(base, "config")
			
			# 1) Read boundary metabolites from YAML
			yml_f   <- list.files(cfg_dir, "\\.ya?ml$", full.names = TRUE)[1]
			yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
			bm_defs <- yml$boundary_metabolites %||% character()
			
			# 2) Read system defaults from JSON
			json_f <- file.path(cfg_dir, "boundary_conditions.json")
			jsn    <- if (file.exists(json_f)) jsonlite::fromJSON(json_f) else list()
			default_fba_ub       <- jsn$fba_upper_bound  %||% 1000
			default_fba_lb       <- jsn$fba_lower_bound  %||% -1000
			default_background   <- jsn$background_met    %||% 1000
			default_volume       <- jsn$volume            %||% 0.001
			default_cell_density <- jsn$cell_density      %||% 1e10

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
				          value = 0,
				          min   = 0,
				          width = "30%"
				        )
				      })
				    )
				  )
				},
				
			## ──────────────── System Parameters (temporarily hidden) ────────────────
			# ──────────────── System Parameters (hidden) ────────────────
# instead of directly emitting the inputs, wrap them in a conditionalPanel
# whose JavaScript condition is always false:
conditionalPanel(
  condition = "false", 
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
        numericInput(ns("fba_ub"), "FBA Upper Bound (mmol/h)",
                     value = default_fba_ub, min = 0, width = "60%")
      ),
      column(4,
        numericInput(ns("fba_lb"), "FBA Lower Bound (mmol/h)",
                     value = default_fba_lb, width = "60%")
      ),
      column(4,
        numericInput(ns("background_met"), "Background met (mmol)",
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

		## ─────────────────────────────────────────────────────────────────────────────
##  RUN SIMULATION BUTTON
## ─────────────────────────────────────────────────────────────────────────────
observeEvent(input$btn_run_sim, {

  ## 0)  Modal “Running…”
  showModal(
    modalDialog(
      title  = NULL,
      tagList(
        div(style = "text-align:center;",
            img(src = "running.png", height = "400px", alt = "Running…")),
        br(),
        div("Running simulation and analysis…",
            style = "text-align:center; font-weight:bold;")
      ),
      footer    = NULL,
      easyClose = FALSE,
      size      = "l"
    )
  )

  tryCatch({

    ## ── 1) Paths / YAML ─────────────────────────────────────────────────────
    base       <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
    config_dir <- file.path(base, "config")
    hypernode  <- basename(base)

    yml_file <- list.files(config_dir, "\\.ya?ml$", full.names = TRUE)[1]
    orig_yml <- if (length(yml_file)) yaml::read_yaml(yml_file) else list()
    gui_yml  <- orig_yml

    ## ── 2) Simulation section in YAML ───────────────────────────────────────
    gui_yml$simulation <- list(
      initial_time       = input$i_time,
      final_time         = input$f_time,
      step_size          = input$s_time,
      absolute_tolerance = input$atol,
      relative_tolerance = input$rtol
    )

    ## ── 3) Boundary metabolite concentrations ───────────────────────────────
    bm_defs <- orig_yml$boundary_metabolites %||% character()
    if (length(bm_defs)) {
      met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
      gui_yml$simulation$boundary_concentrations <-
        setNames(
          lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
          bm_defs
        )
    }

    ## ── 4) System parameters ───────────────────────────────────────────────
    gui_yml$simulation$system_parameters <- list(
      fba_upper_bound = input$fba_ub,
      fba_lower_bound = input$fba_lb,
      background_met  = input$background_met,
      volume          = input$sys_volume,
      cell_density    = input$cell_density
    )

    ## ── 5) Aggiorna cellular_units (parametri + initial_biomass) ────────────
    if (!is.null(orig_yml$cellular_units)) {
      nd <- node_data()
      for (i in seq_along(orig_yml$cellular_units)) {
        unit <- orig_yml$cellular_units[[i]]
        mdl  <- unit$model_name
        if (mdl %in% names(nd)) {
          info <- nd[[mdl]]
          orig_yml$cellular_units[[i]]$biomass$max   <- info$params$bioMax
          orig_yml$cellular_units[[i]]$biomass$mean  <- info$params$bioMean
          orig_yml$cellular_units[[i]]$biomass$min   <- info$params$bioMin
          orig_yml$cellular_units[[i]]$population$starv <- info$params$starv
          orig_yml$cellular_units[[i]]$population$dup   <- info$params$dup
          orig_yml$cellular_units[[i]]$population$death <- info$params$death
          orig_yml$cellular_units[[i]]$mu_max           <- info$params$mu_max
          orig_yml$cellular_units[[i]]$initial_count    <- info$population
          orig_yml$cellular_units[[i]]$initial_biomass  <-
            info$initial_biomass %||% info$params$bioMean
        }
      }
      gui_yml$cellular_units <- orig_yml$cellular_units
    }

    ## ── 6) Scrivi lo snapshot GUI YAML ──────────────────────────────────────
    gui_yaml_path <- file.path(config_dir, paste0(hypernode, "_gui.yaml"))
    yaml::write_yaml(gui_yml, gui_yaml_path)

    ## ── 7) Lancio model_analysis_GUI ───────────────────────────────────────
    epimodFBAfunctions::model_analysis_GUI(
      paths = c(gen    = file.path(base, "gen"),
                config = config_dir,
                src    = file.path(base, "src"),
                output = file.path(base, "output")),
      hypernode_name = hypernode,
      debug_solver   = FALSE,
      i_time         = input$i_time,
      f_time         = input$f_time,
      s_time         = input$s_time,
      atol           = input$atol,
      rtol           = input$rtol,
      fba_fname      = list.files(file.path(base, "biounits"),
                                  "\\.txt$", full.names = TRUE, recursive = TRUE),
      user_files     = c(
        file.path(config_dir, "population_parameters.csv"),
        file.path(base, "gen",    paste0(hypernode, ".fbainfo")),
        file.path(base, "output", "ub_bounds_projected_gui.csv"),
        file.path(base, "output", "non_projected_bounds_gui.csv")
      ),
      volume = base
    )

    ## ── 8) Success → modal ─────────────────────────────────────────────────
    removeModal()
    showModal(
      modalDialog(
        title = "✅ Simulation & Analysis Complete",
        tagList(
          div(style = "text-align:center;",
              img(src = "success.png", height = "400px", alt = "Success")),
          br(),
          div("Your model has been generated and analyzed successfully.",
              style = "text-align:center; font-weight:bold;"),
          br(),
          div("What would you like to do next?",
              style = "text-align:center;")
        ),
        footer = tagList(
          actionButton(ns("btn_visualize"), "Visualize Results",
                       class = "btn-primary"),
          actionButton(ns("btn_new_sim"),   "New Simulation",
                       class = "btn-secondary")
        ),
        easyClose = FALSE,
        size      = "l"
      )
    )

    session$userData$last_hypernode <- base

  }, error = function(err) {

    ## ── ERROR ──────────────────────────────────────────────────────────────
    removeModal()
    showModal(
      modalDialog(
        title = "❌ Simulation Error",
        tagList(
          div(style = "text-align:center;",
              img(src = "error.png", height = "400px", alt = "Error")),
          br(),
          div(paste("An error occurred during simulation:", err$message),
              style = "text-align:center; color:red; font-weight:bold;")
        ),
        easyClose = TRUE,
        footer    = modalButton("Close"),
        size      = "l"
      )
    )

  }, finally = {

    ## ── CLEAN-UP (_gui files & stubs) ──────────────────────────────────────
    try({
      ## 1) csv temporanei
      unlink(c(session_files$proj,
               session_files$nproj),
             force = TRUE)

      ## 2) gui-yaml
      unlink(gui_yaml_path, force = TRUE)

      ## 3) mu_max csv
      unlink(file.path(config_dir, "mu_max_values_gui.csv"), force = TRUE)

      ## 4) stubs R/C++
      src_dir   <- file.path(base, "src")
      r_stubs   <- list.files(src_dir, "*_gui\\.R$",  full.names = TRUE)
      cpp_stubs <- list.files(src_dir, "*_gui\\.cpp$", full.names = TRUE)
      if (length(r_stubs))   unlink(r_stubs,   force = TRUE)
      if (length(cpp_stubs)) unlink(cpp_stubs, force = TRUE)
    }, silent = TRUE)

  }) ## /tryCatch

}) ## /observeEvent btn_run_sim


 		  # ── modals for each bacteria ─────────────────────────────────────────
		# ── modals for each bacteria ────────────────────────────────────────
		observe({
			req(dir_valid())

			lapply(models(), function(mdl) {
				local({

				  mdl2 <- mdl

				  # ——————————————————————————————————————————————
				  #  Clic sul nome del modello  →  apre il modal
				  # ——————————————————————————————————————————————
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
						# ─────────────────────────────────────────────────────────────
						#  A) RENDER DELLA TAB DEL PARAMETRI  (bloc-co SOSTITUTIVO)
						# ─────────────────────────────────────────────────────────────
						output[[param_id]] <- DT::renderDataTable({

							## helper: se il valore non è scalare, prendi il primo elemento
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
				    observeEvent(input[[paste0(param_id, "_cell_edit")]], ignoreInit = TRUE, {
				      edit    <- input[[paste0(param_id, "_cell_edit")]]
				      row     <- edit$row
				      new_val <- suppressWarnings(as.numeric(edit$value))
				      if (is.na(new_val)) {
				        showNotification("Value must be numeric", type = "error")
				        return()
				      }
				      isolate({
				        nd    <- node_data()
				        info1 <- nd[[mdl2]]
				        fields <- c(names(info1$params), "initial_count")
				        name   <- fields[row]
				        if (name %in% names(info1$params)) {
				          info1$params[[name]] <- new_val
				        } else {
				          info1$population <- new_val
				        }
				        nd[[mdl2]] <- info1
				        node_data(nd)
				      })
				      upd <- node_data()[[mdl2]]
				      df2 <- data.frame(
				        Parameter = c(names(upd$params), "initial_count"),
				        Value     = c(unlist(upd$params, use.names = FALSE),
				                      upd$population),
				        check.names = FALSE, stringsAsFactors = FALSE
				      )
							## ricostruisci il data-frame in modo sicuro
							scalar_num <- function(x) {
								out <- suppressWarnings(as.numeric(x[1]))
								if (length(out) == 0 || is.na(out)) NA_real_ else out
							}
							vals2 <- vapply(upd$params, scalar_num, numeric(1))

							df2 <- data.frame(
								Parameter   = c(names(upd$params), "initial_count"),
								Value       = c(vals2, upd$population),
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
							row <- ed$row; col <- ed$col + 1   # col 2 = background_conc
							isolate({
								nd   <- node_data()
								info <- nd[[mdl2]]
								info$projected[row, col] <- as.numeric(ed$value)
								nd[[mdl2]] <- info
								node_data(nd)
							})
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
		## 2)  Quando confermano → salva la configurazione corrente
		## ─────────────────────────────────────────────────────────────────────────────
		observeEvent(input$confirm_save_config, {
			req(input$config_name)
			cfg_name <- trimws(input$config_name)
			if (cfg_name == "") {
				showNotification("Please enter a name.", type = "error")
				return()
			}
			## sanitise per filesystem
			cfg_name <- gsub("[^A-Za-z0-9_ -]", "_", cfg_name)

			## cartelle di destinazione --------------------------------------------------
			base_dir      <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			settings_root <- file.path(base_dir, "settings")
			if (!dir.exists(settings_root)) dir.create(settings_root)
			save_dir <- file.path(settings_root, cfg_name)
			if (dir.exists(save_dir)) {
				showNotification("A configuration with that name already exists.",
				                 type = "error")
				return()
			}
			dir.create(save_dir)

			## copia i 2 file GUI-temp ---------------------------------------------------
			file.copy(session_files$proj, file.path(save_dir,
				                                      basename(session_files$proj)))
			file.copy(session_files$nproj, file.path(save_dir,
				                                       basename(session_files$nproj)))

			## 1) snapshot tempi & tolleranze -------------------------------------------
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

			## 2) snapshot concentrazioni boundary --------------------------------------
			yml_f <- list.files(file.path(base_dir, "config"),
				                  "\\.ya?ml$", full.names = TRUE)[1]
			yml   <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
			bm_defs <- yml$boundary_metabolites %||% character()
			if (length(bm_defs)) {
				met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
				cfg$boundary_concentrations <- setNames(
				  lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				  bm_defs
				)
			}

			## 3) system parameters ------------------------------------------------------
			cfg$system_parameters <- list(
				fba_upper_bound = input$fba_ub,
				fba_lower_bound = input$fba_lb,
				background_met  = input$background_met,
				volume          = input$sys_volume,
				cell_density    = input$cell_density
			)

			## 4) parametri per-modello (+ initial_biomass) -----------------------------
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

			## 5) salva JSON + YAML ------------------------------------------------------
			jsonlite::write_json(cfg,
				                   file.path(save_dir, "config_values.json"),
				                   auto_unbox = TRUE, pretty = TRUE)
			yaml::write_yaml(cfg,
				               file.path(save_dir, paste0("config_", cfg_name, ".yaml")))

			removeModal()
			showNotification(paste("Configuration saved as", cfg_name), type = "message")
		})


		## ─────────────────────────────────────────────────────────────────────────────
		## 1)  Click “Load Configuration…” → elenco configurazioni disponibili
		## ─────────────────────────────────────────────────────────────────────────────
		observeEvent(input$load_configuration, {
			req(dir_valid())
			base_dir      <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			settings_root <- file.path(base_dir, "settings")
			configs <- if (dir.exists(settings_root))
				           basename(list.dirs(settings_root, full.names = TRUE,
				                              recursive = FALSE))
				         else character()
			if (!length(configs)) {
				showNotification("No saved configurations found.", type = "warning")
				return()
			}
			showModal(modalDialog(
				title = "Load Saved Configuration",
				selectInput(ns("choose_config"), "Select configuration:", choices = configs),
				footer = tagList(
				  modalButton("Cancel"),
				  actionButton(ns("confirm_load_config"), "Load Configuration",
				               class = "btn-primary")
				),
				easyClose = FALSE
			))
		})


	## ─────────────────────────────────────────────────────────────────────────────
## 2)  Conferma caricamento  →  applica configurazione
## ─────────────────────────────────────────────────────────────────────────────
observeEvent(input$confirm_load_config, {
  req(input$choose_config)
  cfg_name <- input$choose_config

  base_dir <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
  load_dir <- file.path(base_dir, "settings", cfg_name)

  ## ── 1) Copia i 2 CSV GUI-temp nella sessione corrente ---------------------
  for (slot in c("proj", "nproj")) {
    dest <- session_files[[slot]]
    src  <- file.path(load_dir, basename(dest))
    file.copy(src, dest, overwrite = TRUE)
  }

  ## ── 2) Rilettura dei CSV appena copiati -----------------------------------
  proj_df  <- if (file.exists(session_files$proj))
                read.csv(session_files$proj,  stringsAsFactors = FALSE)
              else data.frame()

  nproj_df <- if (file.exists(session_files$nproj))
                read.csv(session_files$nproj, stringsAsFactors = FALSE)
              else data.frame()

  ## ── 3) Helper “colonna singola” (solo background_conc) --------------------
  make_bounds_single <- function(df, mdl) {
    sub <- df[df$FBAmodel == mdl, , drop = FALSE]
    if (!nrow(sub) || !"background_conc" %in% names(sub))
      return(data.frame(reaction        = character(),
                        background_conc = numeric(),
                        stringsAsFactors = FALSE))

    agg <- tapply(sub$background_conc, sub$reaction, sum)

    data.frame(
      reaction        = names(agg),
      background_conc = as.numeric(agg),
      stringsAsFactors = FALSE
    )
  }

  ## ── 4) Ricostruisci node_data() con i nuovi bounds ------------------------
  models_list <- models()

  info_list <- lapply(models_list, function(mdl) {
    list(
      ## cache invariata
      params          = node_data()[[mdl]]$params,
      population      = node_data()[[mdl]]$population,
      initial_biomass = node_data()[[mdl]]$initial_biomass,

      ## nuovi bounds (una sola colonna)
      projected       = make_bounds_single(proj_df , mdl),
      nonprojected    = make_bounds_single(nproj_df, mdl)
    )
  })
  names(info_list) <- models_list
  node_data(info_list)

  ## ── 5) Ripristina gli input dall’eventuale JSON ---------------------------
  json_path <- file.path(load_dir, "config_values.json")
  if (file.exists(json_path)) {

    vals <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

    ## tempi / tolleranze
    updateNumericInput(session, "i_time", value = vals$times$initial_time)
    updateNumericInput(session, "f_time", value = vals$times$final_time)
    updateNumericInput(session, "s_time", value = vals$times$step_size)
    updateNumericInput(session, "atol",   value = vals$tolerances$atol)
    updateNumericInput(session, "rtol",   value = vals$tolerances$rtol)

    ## boundary metabolite concentrations
    yml_f   <- list.files(file.path(base_dir, "config"),
                          "\\.ya?ml$", full.names = TRUE)[1]
    yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
    bm_defs <- yml$boundary_metabolites %||% character()
    met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
    for (i in seq_along(bm_defs)) {
      updateNumericInput(session,
                         paste0("conc_", met_ids[i]),
                         value = vals$boundary_concentrations[[ bm_defs[i] ]])
    }

    ## system parameters (restano se presenti negli input nascosti)
    sp <- vals$system_parameters
    updateNumericInput(session, "fba_ub",         value = sp$fba_upper_bound)
    updateNumericInput(session, "fba_lb",         value = sp$fba_lower_bound)
    updateNumericInput(session, "background_met", value = sp$background_met)
    updateNumericInput(session, "sys_volume",     value = sp$volume)
    updateNumericInput(session, "cell_density",   value = sp$cell_density)

    ## parametri per-modello
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
  showNotification(paste("Loaded configuration:", cfg_name), type = "message")
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
