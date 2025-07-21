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
    session_files <- reactiveValues(proj = NULL, nproj_f = NULL, nproj_r = NULL)
    
	    # ──────────────────────────────────────────────────────────────────────
    # ①  CACHE PER-MODEL DATA 
    # ──────────────────────────────────────────────────────────────────────
    node_data <- reactiveVal(list())
    
    
			# ----- helpers to build wide lower/upper per reaction ----------
		make_bounds_proj <- function(df, mdl) {
		  if (!nrow(df)) return(data.frame(reaction=character(), lower=numeric(), upper=numeric()))
		  sub <- df[df$FBAmodel == mdl, ]
		  sub$dir      <- ifelse(grepl("_r$", sub$reaction), "lower", "upper")
		  sub$reaction <- sub("_([rf])$", "", sub$reaction)
		  lower <- aggregate(upper_bound ~ reaction, sub[sub$dir=="lower", ], `[`, 1)
		  upper <- aggregate(upper_bound ~ reaction, sub[sub$dir=="upper", ], `[`, 1)
		  names(lower)[2] <- "lower"; names(upper)[2] <- "upper"
		  merge(lower, upper, all = TRUE)
		}

		make_bounds_f <- function(df, mdl) {
		  if (!nrow(df)) return(data.frame(reaction=character(), lower=numeric(), upper=numeric()))
		  sub <- df[df$FBAmodel == mdl, ]
		  if (!nrow(sub)) return(data.frame(reaction=character(), lower=numeric(), upper=numeric()))
		  sub$reaction <- sub("_f$", "", sub$reaction)
		  # pick the correct flux column
		  flux_col <- if ("upper_bound" %in% names(sub)) "upper_bound" else if ("background_met" %in% names(sub)) "background_met" else {
		    stop("Forward file missing both upper_bound and background_met columns")
		  }
		  agg <- tapply(sub[[flux_col]], sub$reaction, sum)
		  data.frame(
		    reaction = names(agg),
		    lower    = numeric(length(agg)),
		    upper    = as.numeric(agg),
		    stringsAsFactors = FALSE
		  )
		}
		
			make_bounds_r <- function(df, mdl) {
				# df = GUI-temp reverse CSV with columns reaction, FBAmodel, background_met, volume
				sub <- df[df$FBAmodel == mdl, ]
				if (!nrow(sub)) {
					return(data.frame(
						reaction       = character(),
						background_met = numeric(),
						volume         = numeric(),
						stringsAsFactors = FALSE
					))
				}
				# group by reaction
				agg_met <- tapply(sub$background_met, sub$reaction, sum)
				agg_vol <- tapply(sub$volume,         sub$reaction, sum)
				data.frame(
					reaction       = names(agg_met),
					background_met = as.numeric(agg_met),
					volume         = as.numeric(agg_vol),
					stringsAsFactors = FALSE
				)
			}
    
		# ------------------------------------------------------------------
		# helper: rebuild whole bounds file (projected, forward, or reverse)
		# ------------------------------------------------------------------
		rebuild_bounds_csv <- function(csv_path, slot_name) {
			all_models <- names(node_data())
			long_rows <- lapply(all_models, function(mdl) {
				wide <- node_data()[[mdl]][[slot_name]]
				if (!nrow(wide)) return(NULL)

				if (slot_name == "unproj_r") {
				  # reverse-only: background_met + volume
				  data.frame(
				    reaction       = wide$reaction,
				    FBAmodel       = mdl,
				    background_met = wide$background_met,
				    volume         = wide$volume,
				    stringsAsFactors = FALSE
				  )
				} else {
				  # projected or forward-unprojected: reaction + upper_bound
				  data.frame(
				    reaction    = c(paste0(wide$reaction, "_r"),
				                    paste0(wide$reaction, "_f")),
				    FBAmodel    = mdl,
				    upper_bound = c(wide$lower, wide$upper),
				    stringsAsFactors = FALSE
				  )
				}
			})

			long <- do.call(rbind, long_rows)
			if (!is.null(long)) {
				write.csv(long, csv_path, row.names = FALSE, quote = FALSE)
			}
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
			session_files$proj    <- file.path(out_dir, "ub_bounds_projected_gui.csv")
			session_files$nproj_f <- file.path(out_dir, "non_projected_forward_bounds_gui.csv")
			session_files$nproj_r <- file.path(out_dir, "non_projected_reverse_background_met_gui.csv")

			file.copy(file.path(out_dir, "ub_bounds_projected.csv"),
				        session_files$proj,  overwrite = TRUE)
			file.copy(file.path(out_dir, "non_projected_forward_bounds.csv"),
				        session_files$nproj_f, overwrite = TRUE)
			file.copy(file.path(out_dir, "non_projected_reverse_background_met.csv"),
				        session_files$nproj_r, overwrite = TRUE)

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
			proj_df    <- if (file.exists(session_files$proj))    read.csv(session_files$proj,  stringsAsFactors = FALSE) else data.frame()
			nproj_f_df <- if (file.exists(session_files$nproj_f)) read.csv(session_files$nproj_f, stringsAsFactors = FALSE) else data.frame()
			nproj_r_df <- if (file.exists(session_files$nproj_r)) read.csv(session_files$nproj_r, stringsAsFactors = FALSE) else data.frame()

			# ----- build node_data with separate projected, forward, reverse ----
			info_list <- lapply(models(), function(mdl) {
				row    <- yaml_tbl[yaml_tbl$model == mdl, , drop = FALSE]
				params <- if (nrow(row)) as.list(row[1, c("bioMax","bioMean","bioMin","starv","dup","death","mu_max")]) else 
						      as.list(setNames(rep(NA,7), c("bioMax","bioMean","bioMin","starv","dup","death","mu_max")))
				# set default initial biomass = bioMean from YAML (or NA)
				init_b <- if (nrow(row)) row$bioMean[1] else NA

				list(
					params           = params,
					population       = if (nrow(row)) row$population[1] else NA,
					projected        = make_bounds_proj(proj_df,    mdl),
					unproj_f         = make_bounds_f(nproj_f_df, mdl),
					unproj_r         = make_bounds_r(nproj_r_df, mdl),
					initial_biomass  = init_b                       # <<< new field
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
    shiny::observeEvent(input$btn_reset, {
      reset_flag(TRUE)
      
      unlink(c(session_files$proj, session_files$nproj_f, session_files$nproj_r), force = TRUE)
      session_files$proj <- session_files$nproj_f <- session_files$nproj_r <- NULL
      
      # clear in-memory caches
			node_data(list())             
			models(character())           
  
      dir_valid(FALSE)
      models(character())
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
			input       = input,
			id          = "load_proj",
			roots       = roots,
			session     = session,
			filetypes   = c("csv")
		)
		shinyFiles::shinyFileChoose(
			input       = input,
			id          = "load_nproj",
			roots       = roots,
			session     = session,
			filetypes   = c("csv")
		)

		## helper: refresh node_data() after either file is loaded -----------
		refresh_bounds <- function() {
			proj_df  <- if (file.exists(session_files$proj))
				            read.csv(session_files$proj,  stringsAsFactors = FALSE) else data.frame()
			nproj_df <- if (file.exists(session_files$nproj))
				            read.csv(session_files$nproj, stringsAsFactors = FALSE) else data.frame()

			make_wide <- function(df, mdl) {
				if (!nrow(df)) return(data.frame(reaction=character(),lower=numeric(),upper=numeric()))
				sub <- df[df$FBAmodel == mdl, , drop = FALSE]
				if (!nrow(sub)) return(data.frame(reaction=character(),lower=numeric(),upper=numeric()))
				sub$dir      <- ifelse(grepl("_r$", sub$reaction), "lower", "upper")
				sub$reaction <- sub("_[rf]$", "", sub$reaction)
				lower <- aggregate(upper_bound ~ reaction, sub[sub$dir=="lower",], `[`, 1)
				upper <- aggregate(upper_bound ~ reaction, sub[sub$dir=="upper",], `[`, 1)
				names(lower)[2] <- "lower"; names(upper)[2] <- "upper"
				merge(lower, upper, all = TRUE)
			}

			nd <- node_data()
			for (mdl in names(nd)) {
				nd[[mdl]]$projected   <- make_wide(proj_df , mdl)
				nd[[mdl]]$unprojected <- make_wide(nproj_df, mdl)
			}
			node_data(nd)
		}

		## load projected ----------------------------------------------------
		observeEvent(input$load_proj, {
			src <- shinyFiles::parseFilePaths(roots, input$load_proj)$datapath
			if (!length(src) || is.na(src)) return()

			ok <- file.copy(src, session_files$proj, overwrite = TRUE)
			if (ok) {
				refresh_bounds()
				shiny::showNotification("Projected bounds file loaded.", type = "message")
			} else {
				shiny::showNotification("Load failed – check permissions.", type = "error")
			}
		})

		## load not-projected -----------------------------------------------
		observeEvent(input$load_nproj, {
			src <- shinyFiles::parseFilePaths(roots, input$load_nproj)$datapath
			if (!length(src) || is.na(src)) return()

			ok <- file.copy(src, session_files$nproj, overwrite = TRUE)
			if (ok) {
				refresh_bounds()
				shiny::showNotification("Not-projected bounds file loaded.", type = "message")
			} else {
				shiny::showNotification("Load failed – check permissions.", type = "error")
			}
		})


		# 2) user confirms filename -----------------------------------------
		observeEvent(input$confirm_export, {
			fname <- trimws(input$export_fname)
			if (fname == "" || is.null(export_dest())) {
				shiny::showNotification("Please enter a file name.",
				                        type = "error", duration = 3)
				return()
			}

			## basic sanitising — keep letters, numbers, _ and -
			fname <- gsub("[^A-Za-z0-9_\\-]", "_", fname)

			dest <- export_dest()
			ok1  <- file.copy(session_files$proj,
				      file.path(dest, paste0(fname, "_projected.csv")),  overwrite = TRUE)
			ok2  <- file.copy(session_files$nproj,
				      file.path(dest, paste0(fname, "_not_projected.csv")), overwrite = TRUE)

			removeModal()
			shiny::showNotification(
				if (ok1 && ok2)
				  paste("Bounds saved as", fname, "_*.csv in", dest)
				else
				  "Copy failed – check permissions.",
				type = if (ok1 && ok2) "message" else "error",
				duration = 4
			)

			export_dest(NULL)   # reset for next export
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

		shiny::observeEvent(input$btn_run_sim, {

			# 0) Show blocking “Running…” modal with spinner
			showModal(
				modalDialog(
				  title    = NULL,
				  tagList(
				    div(style = "text-align:center;",
				        img(src = "running.png", height = "400px", alt = "Running…")
				    ),
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

				# ── 1) Set up paths ───────────────────────────────────────────────────────
				base       <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
				config_dir <- file.path(base, "config")
				hypernode  <- basename(base)

				# ── READ ORIGINAL YAML ────────────────────────────────────────────────────
				yml_file <- list.files(config_dir, "\\.ya?ml$", full.names = TRUE)[1]
				orig_yml <- if (length(yml_file)) yaml::read_yaml(yml_file) else list()
				gui_yml  <- orig_yml   # ci metto dentro solo le modifiche

				# ── UPDATE simulation section ────────────────────────────────────────────
				gui_yml$simulation <- list(
				  initial_time       = input$i_time,
				  final_time         = input$f_time,
				  step_size          = input$s_time,
				  absolute_tolerance = input$atol,
				  relative_tolerance = input$rtol
				)

				# ── UPDATE boundary concentrations ───────────────────────────────────────
				bm_defs <- orig_yml$boundary_metabolites %||% character()
				if (length(bm_defs)) {
				  met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
				  gui_yml$simulation$boundary_concentrations <-
				    setNames(
				      lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				      bm_defs
				    )
				}

			  # ── UPDATE system parameters ─────────────────────────────────────────────
				gui_yml$simulation$system_parameters <- list(
				  fba_upper_bound = input$fba_ub,
				  fba_lower_bound = input$fba_lb,
				  background_met  = input$background_met,
				  volume          = input$sys_volume,
				  cell_density    = input$cell_density
				)

				# ── UPDATE cellular_units to include initial_biomass ─────────────────────
				if (!is.null(orig_yml$cellular_units)) {
				  nd <- node_data()
				  for (i in seq_along(orig_yml$cellular_units)) {
				    unit <- orig_yml$cellular_units[[i]]
				    mdl  <- unit$model_name
				    if (mdl %in% names(nd)) {
				      info <- nd[[mdl]]
				      # overwrite existing fields…
				      orig_yml$cellular_units[[i]]$biomass$max   <- info$params$bioMax
				      orig_yml$cellular_units[[i]]$biomass$mean  <- info$params$bioMean
				      orig_yml$cellular_units[[i]]$biomass$min   <- info$params$bioMin
				      orig_yml$cellular_units[[i]]$population$starv <- info$params$starv
				      orig_yml$cellular_units[[i]]$population$dup   <- info$params$dup
				      orig_yml$cellular_units[[i]]$population$death <- info$params$death
				      orig_yml$cellular_units[[i]]$mu_max         <- info$params$mu_max
				      orig_yml$cellular_units[[i]]$initial_count  <- info$population
				      # ← new:
				      orig_yml$cellular_units[[i]]$initial_biomass <- info$initial_biomass %||% info$params$bioMean
				    }
				  }
				  gui_yml$cellular_units <- orig_yml$cellular_units
				}

				# ── WRITE OUT THE GUI SNAPSHOT YAML ──────────────────────────────────────
				gui_yaml_path <- file.path(config_dir, paste0(hypernode, "_gui.yaml"))
				yaml::write_yaml(gui_yml, gui_yaml_path)



				# ── 2) Model generation & 3) Model analysis ─────────────────────────────
				epimodFBAfunctions::model_analysis_GUI(
				  paths           = c(gen    = file.path(base, "gen"),
				                      config = config_dir,
				                      src    = file.path(base, "src"),
				                      output = file.path(base, "output")),
				  hypernode_name  = hypernode,
				  debug_solver    = FALSE,
				  i_time          = input$i_time,
				  f_time          = input$f_time,
				  s_time          = input$s_time,
				  atol            = input$atol,
				  rtol            = input$rtol,
				  fba_fname       = list.files(file.path(base, "biounits"), "\\.txt$", full.names = TRUE, recursive = TRUE),
				  user_files      = c(
				    file.path(config_dir, "population_parameters.csv"),
				    file.path(base, "gen",    paste0(hypernode, ".fbainfo")),
				    file.path(base, "output", "ub_bounds_projected_gui.csv"),
				    file.path(base, "output", "non_projected_forward_bounds_gui.csv"),
				    file.path(base, "output", "non_projected_reverse_background_met_gui.csv")
				  ),
				  volume = base
				)

				# ── Success: remove spinner and show completion modal ─────────────────────
				removeModal()
				showModal(
					modalDialog(
						title    = "✅ Simulation & Analysis Complete",
						tagList(
							div(style = "text-align:center;",
								  img(src = "success.png", height = "400px", alt = "Success")
							),
							br(),
							div("Your model has been generated and analyzed successfully.",
								  style = "text-align:center; font-weight:bold;"),
							br(),
							div("What would you like to do next?", style = "text-align:center;")
						),
						footer = tagList(
							actionButton(ns("btn_visualize"), "Visualize Results",
								           class = "btn-primary"),
							actionButton(ns("btn_new_sim"),   "New Simulation",
								           class = "btn-secondary")
						),
						easyClose = FALSE,  # disabilita chiusura cliccando fuori o con Esc
						size      = "l"
					)
				)

				# record last hypernode path
				session$userData$last_hypernode <- base

			}, error = function(err) {
				# ── Error: remove spinner and show error modal ───────────────────────────
				removeModal()
				showModal(
				  modalDialog(
				    title = "❌ Simulation Error",
				    tagList(
				      div(style = "text-align:center;",
				          img(src = "error.png", height = "400px", alt = "Error")
				      ),
				      br(),
				      div(paste("An error occurred during simulation:", err$message),
				          style = "text-align:center; color:red; font-weight:bold;")
				    ),
				    easyClose = TRUE,
				    footer = modalButton("Close"),
				    size   = "l"
				  )
				)
			}, finally = {
				# ── CLEANUP: remove all _gui files from output, config, and src ─────────
				try({
					# 1) remove GUI-CSV files
					unlink(c(session_files$proj,
						       session_files$nproj_f,
						       session_files$nproj_r),
						     force = TRUE)

					# 2) remove GUI-YAML snapshot
					unlink(gui_yaml_path, force = TRUE)

					# 2.1) remove the mu_max CSV
					unlink(file.path(config_dir, "mu_max_values_gui.csv"), force = TRUE)

					# 3) remove GUI-patched R stub(s) in src
					src_dir <- file.path(base, "src")
					r_stubs <- list.files(src_dir, "*_gui\\.R$", full.names = TRUE)
					if (length(r_stubs)) unlink(r_stubs, force = TRUE)

					# 4) remove GUI-patched C++ stub(s) in src
					cpp_stubs <- list.files(src_dir, "*_gui\\.cpp$", full.names = TRUE)
					if (length(cpp_stubs)) unlink(cpp_stubs, force = TRUE)
				}, silent = TRUE)
			})  # /tryCatch

		})  # /observeEvent input$btn_run_sim

 		  # ── modals for each bacteria ─────────────────────────────────────────
			observe({
				req(dir_valid())
				lapply(models(), function(mdl) {
				  local({
				    mdl2 <- mdl

				    # Quando si clicca sul nome del modello, apri il modal
				    observeEvent(input[[paste0("model_", mdl2)]], ignoreInit = TRUE, {
				      # Snapshot iniziale
				      info0    <- node_data()[[mdl2]]
				      params   <- info0$params
				      pop      <- info0$population
				      proj     <- info0$projected
				      unproj_f <- info0$unproj_f
				      unproj_r <- info0$unproj_r

				      # ID unici per output e proxy
				      param_id  <- paste0("param_", mdl2)
				      proj_id   <- paste0("proj_tbl_", mdl2)
				      unpr_f_id <- paste0("unpr_f_tbl_", mdl2)
				      unpr_r_id <- paste0("unpr_r_tbl_", mdl2)
				      save_id   <- paste0("save_", mdl2)

				      message(paste0("[DEBUG] Opening modal for model: ", mdl2))

				      # Mostra il modal
				      showModal(
				      	tags$div(
									id = "modelDetailModal",  # unique hook
									tags$head(
										tags$style(HTML("
											/* Wrapper specifico per questo modal */
											#modelDetailModal .modal-content {
												background-color: #f5f5f5;      /* grigio chiaro */
												border-radius: 8px;
												overflow: hidden;
											}

											/* Header e footer */
											#modelDetailModal .modal-header,
											#modelDetailModal .modal-footer {
												background-color: #27ae60;      /* verde scuro */
												color: #ffffff;
												border: none;
												padding: 1rem 1.5rem;
											}

											/* Titolo principale */
											#modelDetailModal .modal-title {
												font-size: 2rem;
												font-weight: 700;
												text-align: center;
												position: relative;
												margin-bottom: 0.5rem;
												color: #ffffff;
											}
											/* Bordo inferiore del titolo */
											#modelDetailModal .modal-title::after {
												content: \"\";
												display: block;
												width: 80px;
												height: 4px;
												background: #2ecc71;            /* verde più chiaro per contrasto */
												margin: 0.5rem auto 0;          /* centrato */
												border-radius: 2px;
											}

											/* Corpo del modal */
											#modelDetailModal .modal-body {
												padding: 1.5rem;
												color: #333333;
											}

											/* Bottone Close più evidente */
											#modelDetailModal .btn-default {
												background-color: transparent;
												color: #ffffff;
												border: 1px solid #ffffff;
											}
											#modelDetailModal .btn-default:hover {
												background-color: rgba(255,255,255,0.1);
											}

											/* Bottone Save */
											#modelDetailModal .modal-save-btn {
												background-color: #2ecc71;
												border-color: #27ae60;
												color: #ffffff;
											}
											#modelDetailModal .modal-save-btn:hover {
												background-color: #27ae60;
											}
										"))
									),
				        modalDialog(
				          title     = paste("Model:", mdl2),
				          size      = "l",
				          easyClose = FALSE,
				          class     = "modal-model modal-model-detail",
				          
				        ## ── Initial Biomass input ─────────────────────────────
				        {
						        # default: previously saved, or params$bioMean
						        init_val <- info0$initial_biomass %||% params$bioMean
						        div(class = "modelgen-subcard",
												h5("Biomass data", class = "subcard-title"),
										    fluidRow(
										      column(6,
										        numericInput(
										          ns(paste0("bio_init_", mdl2)),
										          label = "Initial Biomass",
										          value = init_val,
										          min   = 0,
										          step  = 0.01,
										          width = "30%"
										        )
										      )
										    )
										 )
				        },
				      	hr(class = "modal-divider"),
				       ## ── end Initial Biomass ───────────────────────────────
				          tagList(
				            # Parametri
				            div(class = "modelgen-subcard ",
						          fluidRow(
						            column(12,
						              h5("Parameters", class = "subcard-title"),
						              DT::dataTableOutput(ns(param_id))
						            )
						          )
						        ),
				            hr(class = "modal-divider"),
				            # Bounds
				           div(class = "modelgen-subcard",
				            	h5("Exchange reaction bounds", class = "subcard-title"),
						          div(class = "modal-tabs",
						            tabsetPanel(
						              type = "tabs",
						              tabPanel("Projected", DT::dataTableOutput(ns(proj_id))),
						              tabPanel("Unprojected Forward", DT::dataTableOutput(ns(unpr_f_id))),
						              tabPanel("Unprojected Reverse", DT::dataTableOutput(ns(unpr_r_id)))
						            )
						          )
						        )
						      ),
				          footer = tagList(
				            modalButton("Close"),
				            actionButton(ns(save_id), "Save changes", class = "btn-primary")
				          )
				        )
				      )
				     )

				      # Render parametri (server-side)
				      output[[param_id]] <- DT::renderDataTable({
				        df <- data.frame(
				          Parameter = c(names(params), "initial_count"),
				          Value     = c(unlist(params, use.names = FALSE), pop),
				          stringsAsFactors = FALSE,
				          check.names = FALSE
				        )
				        DT::datatable(
				          df,
				          rownames = FALSE,
				          editable = list(target = "cell", disable = list(columns = 0)),
				          options  = list(dom = "t", paging = FALSE, ordering = FALSE)
				        )
				      }, server = TRUE)
				      # Proxy
				      param_proxy <- DT::dataTableProxy(param_id, session)
				      
				   

				      # Edit parametri
				      observeEvent(input[[paste0(param_id, "_cell_edit")]], ignoreInit = TRUE, {
				        edit    <- input[[paste0(param_id, "_cell_edit")]]
				        row     <- edit$row
				        new_val <- suppressWarnings(as.numeric(edit$value))
				        message(paste0("[DEBUG] Param edit mdl=", mdl2, " row=", row, " val=", edit$value))
				        if (is.na(new_val)) {
				          showNotification("Valore non numerico!", type = "error")
				          return()
				        }
				        isolate({
				          nd     <- node_data()
				          info1  <- nd[[mdl2]]
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
				        # Refresh
				        upd <- node_data()[[mdl2]]
				        df2 <- data.frame(
				          Parameter = c(names(upd$params), "initial_count"),
				          Value     = c(unlist(upd$params, use.names = FALSE), upd$population),
				          stringsAsFactors = FALSE,
				          check.names = FALSE
				        )
				        DT::replaceData(param_proxy, df2, resetPaging = FALSE, rownames = FALSE)
				      })

				      # Bounds tables options
				      opts <- list(dom = "ft", paging = FALSE, ordering = FALSE, scrollY = 300, scrollCollapse = TRUE)
				      lock <- list(target = "cell", disable = list(columns = 0))
				      # Render bounds
				      output[[proj_id]]   <- DT::renderDataTable({ DT::datatable(proj,     rownames = FALSE, editable = lock, options = opts) })
				      output[[unpr_f_id]] <- DT::renderDataTable({ DT::datatable(unproj_f, rownames = FALSE, editable = lock, options = opts) })
				      output[[unpr_r_id]] <- DT::renderDataTable({ DT::datatable(unproj_r, rownames = FALSE, editable = lock, options = opts) })
				      # Proxies
				      proj_proxy   <- DT::dataTableProxy(proj_id, session)
				      unpr_f_proxy <- DT::dataTableProxy(unpr_f_id, session)
				      unpr_r_proxy <- DT::dataTableProxy(unpr_r_id, session)

				      # Edit projected
				      observeEvent(input[[paste0(proj_id, "_cell_edit")]], ignoreInit = TRUE, {
				        ed  <- input[[paste0(proj_id, "_cell_edit")]]
				        row <- ed$row; col <- ed$col + 1
				        message(paste0("[DEBUG] Bounds proj mdl=", mdl2, " row=", row, " col=", col, " val=", ed$value))
				        isolate({
				          nd      <- node_data()
				          info2   <- nd[[mdl2]]
				          info2$projected[row, col] <- as.numeric(ed$value)
				          nd[[mdl2]] <- info2
				          node_data(nd)
				        })
				        DT::replaceData(proj_proxy, node_data()[[mdl2]]$projected, resetPaging = FALSE, rownames = FALSE)
				        rebuild_bounds_csv(session_files$proj, "projected")
				      })

				      # Edit forward
				      observeEvent(input[[paste0(unpr_f_id, "_cell_edit")]], ignoreInit = TRUE, {
				        ed  <- input[[paste0(unpr_f_id, "_cell_edit")]]
				        row <- ed$row; col <- ed$col + 1
				        message(paste0("[DEBUG] Bounds fwd mdl=", mdl2, " row=", row, " col=", col, " val=", ed$value))
				        isolate({
				          nd      <- node_data()
				          info3   <- nd[[mdl2]]
				          info3$unproj_f[row, col] <- as.numeric(ed$value)
				          nd[[mdl2]] <- info3
				          node_data(nd)
				        })
				        DT::replaceData(unpr_f_proxy, node_data()[[mdl2]]$unproj_f, resetPaging = FALSE, rownames = FALSE)
				        rebuild_bounds_csv(session_files$nproj_f, "unproj_f")
				      })

				      # Edit reverse
				      observeEvent(input[[paste0(unpr_r_id, "_cell_edit")]], ignoreInit = TRUE, {
				        ed  <- input[[paste0(unpr_r_id, "_cell_edit")]]
				        row <- ed$row; col <- ed$col + 1
				        message(paste0("[DEBUG] Bounds rev mdl=", mdl2, " row=", row, " col=", col, " val=", ed$value))
				        isolate({
				          nd      <- node_data()
				          info4   <- nd[[mdl2]]
				          info4$unproj_r[row, col] <- as.numeric(ed$value)
				          nd[[mdl2]] <- info4
				          node_data(nd)
				        })
				        DT::replaceData(unpr_r_proxy, node_data()[[mdl2]]$unproj_r, resetPaging = FALSE, rownames = FALSE)
				        rebuild_bounds_csv(session_files$nproj_r, "unproj_r")
				      })

							# ── Save changes button ─────────────────────────────────
							observeEvent(input[[save_id]], ignoreInit = TRUE, {
								# 1) grab the new initial biomass
								ib_id   <- paste0("bio_init_", mdl2)
								new_bio <- input[[ib_id]]
								if (!is.null(new_bio)) {
									nd         <- node_data()
									info_sav   <- nd[[mdl2]]
									info_sav$initial_biomass <- new_bio
									nd[[mdl2]] <- info_sav
									node_data(nd)
									message("[DEBUG] Saved initial_biomass for ", mdl2, " = ", new_bio)
								}
								# 2) close modal
								removeModal()
							})

				    }) # end observeEvent(model_<mdl2>)
				  })   # end local
				})     # end lapply(models())
			})       # end observe()



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

		# 2) When they confirm, do the actual save
		observeEvent(input$confirm_save_config, {
			req(input$config_name)
			cfg_name <- trimws(input$config_name)
			if (cfg_name == "") {
				showNotification("Please enter a name.", type="error")
				return()
			}
			# sanitize to filesystem-safe
			cfg_name <- gsub("[^A-Za-z0-9_ -]", "_", cfg_name)
			
			# locate hypernode and settings folder
			base_dir      <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			settings_root <- file.path(base_dir, "settings")
			if (!dir.exists(settings_root)) dir.create(settings_root)
			
			save_dir <- file.path(settings_root, cfg_name)
			if (dir.exists(save_dir)) {
				showNotification("A configuration with that name already exists.", type="error")
				return()
			}
			dir.create(save_dir)
			
			# copy the GUI-temp CSVs
			file.copy(session_files$proj,    file.path(save_dir, basename(session_files$proj)))
			file.copy(session_files$nproj_f, file.path(save_dir, basename(session_files$nproj_f)))
			file.copy(session_files$nproj_r, file.path(save_dir, basename(session_files$nproj_r)))
			
			# 1) snapshot dei tempi e tolleranze
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
			
			# 2) snapshot concentrazioni boundary
			yml_f   <- list.files(file.path(base_dir,"config"), "\\.ya?ml$", full.names=TRUE)[1]
			yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
			bm_defs <- yml$boundary_metabolites %||% character()
			if (length(bm_defs)) {
				met_ids <- gsub("[^A-Za-z0-9_]","_", bm_defs)
				cfg$boundary_concentrations <- setNames(
				  lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				  bm_defs
				)
			}
			
			# 3) snapshot system parameters
			cfg$system_parameters <- list(
				fba_upper_bound = input$fba_ub,
				fba_lower_bound = input$fba_lb,
				background_met  = input$background_met,
				volume          = input$sys_volume,
				cell_density    = input$cell_density
			)
			
			# 4) snapshot dei parametri, popolazioni e initial_biomass per ogni modello
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

			
			# 5) scrivi il JSON di compatibilità (già esistente)
			jsonlite::write_json(cfg,
				                   file.path(save_dir, "config_values.json"),
				                   auto_unbox = TRUE, pretty = TRUE)
			
			# 6) scrivi anche un file YAML
			yaml::write_yaml(cfg, 
				               file.path(save_dir, paste0("config_", cfg_name, ".yaml")))
			
			removeModal()
			showNotification(paste("Configuration saved as", cfg_name), type="message")
		})


		# 1) When user clicks “Load Configuration…”, show modal with available configs
		observeEvent(input$load_configuration, {
			req(dir_valid())
			base_dir      <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			settings_root <- file.path(base_dir, "settings")
			configs       <- if (dir.exists(settings_root)) {
				                 basename(list.dirs(settings_root, full.names = TRUE, recursive = FALSE))
				               } else character()
			if (length(configs) == 0) {
				showNotification("No saved configurations found.", type = "warning")
				return()
			}

			showModal(
				modalDialog(
				  title = "Load Saved Configuration",
				  selectInput(ns("choose_config"),
				              "Select configuration:",
				              choices = configs),
				  footer = tagList(
				    modalButton("Cancel"),
				    actionButton(ns("confirm_load_config"),
				                 "Load Configuration",
				                 class = "btn-primary")
				  ),
				  easyClose = FALSE
				)
			)
		})

		# —————————————————————————————————————————————————————————————
		# 1) When user clicks “Load Configuration…”, ask which to load
		# —————————————————————————————————————————————————————————————
		observeEvent(input$load_configuration, {
			message("[LOAD] load_configuration button clicked")
			req(dir_valid())

			base_dir      <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			settings_root <- file.path(base_dir, "settings")
			message("[LOAD] settings_root = ", settings_root)

			if (!dir.exists(settings_root)) {
				message("[LOAD] No settings/ folder found")
				showNotification("No saved configurations found.", type = "warning")
				return()
			}
			configs <- basename(list.dirs(settings_root, full.names = TRUE, recursive = FALSE))
			message("[LOAD] Found configs: ", paste(configs, collapse = ", "))

			if (length(configs) == 0) {
				showNotification("No saved configurations found.", type = "warning")
				return()
			}

			showModal(modalDialog(
				title = "Load Saved Configuration",
				selectInput(ns("choose_config"), "Select configuration:", choices = configs),
				footer = tagList(
				  modalButton("Cancel"),
				  actionButton(ns("confirm_load_config"), "Load Configuration", class = "btn-primary")
				),
				easyClose = FALSE
			))
		})

		# —————————————————————————————————————————————————————————————
		# 1) Ask which config to load
		# —————————————————————————————————————————————————————————————
		observeEvent(input$load_configuration, {
			message("[LOAD] load_configuration clicked")
			req(dir_valid())

			base_dir      <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			settings_root <- file.path(base_dir, "settings")
			message("[LOAD] settings_root = ", settings_root)

			if (!dir.exists(settings_root)) {
				message("[LOAD] no settings folder")
				showNotification("No saved configurations found.", type="warning")
				return()
			}
			configs <- basename(list.dirs(settings_root, full.names=TRUE, recursive=FALSE))
			message("[LOAD] configs:", paste(configs, collapse=", "))
			if (!length(configs)) {
				showNotification("No saved configurations found.", type="warning")
				return()
			}

			showModal(modalDialog(
				title = "Load Saved Configuration",
				selectInput(ns("choose_config"), "Pick one:", configs),
				footer = tagList(
				  modalButton("Cancel"),
				  actionButton(ns("confirm_load_config"), "Load", class="btn-primary")
				),
				easyClose = FALSE
			))
		})

		# ──────────────────────────────────────────────────────────────────────────────
		# 2) Load & apply saved configuration
		# ──────────────────────────────────────────────────────────────────────────────
		observeEvent(input$confirm_load_config, {
			req(input$choose_config)
			cfg_name <- input$choose_config
			base_dir <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			load_dir <- file.path(base_dir, "settings", cfg_name)

			# Copy GUI-temp CSVs
			for (slot in c("proj","nproj_f","nproj_r")) {
				dest <- session_files[[slot]]
				src  <- file.path(load_dir, basename(dest))
				file.copy(from = src, to = dest, overwrite = TRUE)
			}

			# Rebuild node_data from the bounds
			proj_df    <- if (file.exists(session_files$proj))    read.csv(session_files$proj,  stringsAsFactors = FALSE) else data.frame()
			nproj_f_df <- if (file.exists(session_files$nproj_f)) read.csv(session_files$nproj_f, stringsAsFactors = FALSE) else data.frame()
			nproj_r_df <- if (file.exists(session_files$nproj_r)) read.csv(session_files$nproj_r, stringsAsFactors = FALSE) else data.frame()

			models_list <- models()
			info_list <- lapply(models_list, function(mdl) {
				list(
				  params     = node_data()[[mdl]]$params,
				  population = node_data()[[mdl]]$population,
				  projected  = make_bounds_proj(proj_df,    mdl),
				  unproj_f   = make_bounds_f(nproj_f_df, mdl),
				  unproj_r   = make_bounds_r(nproj_r_df, mdl)
				)
			})
			names(info_list) <- models_list
			node_data(info_list)

			# ——— LEGGO IL JSON SENZA simplifyVector ———————————
			json_path <- file.path(load_dir, "config_values.json")
			if (file.exists(json_path)) {
				vals <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

				# 1) Ripristino tempi & tolleranze
				updateNumericInput(session, "i_time", value = vals$times$initial_time)
				updateNumericInput(session, "f_time", value = vals$times$final_time)
				updateNumericInput(session, "s_time", value = vals$times$step_size)
				updateNumericInput(session, "atol",   value = vals$tolerances$atol)
				updateNumericInput(session, "rtol",   value = vals$tolerances$rtol)

				# 2) Boundary concentrations
				yml_f   <- list.files(file.path(base_dir, "config"), "\\.ya?ml$", full.names = TRUE)[1]
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

				# 3) System parameters
				sp <- vals$system_parameters
				updateNumericInput(session, "fba_ub",        value = sp$fba_upper_bound)
				updateNumericInput(session, "fba_lb",        value = sp$fba_lower_bound)
				updateNumericInput(session, "background_met", value = sp$background_met)
				updateNumericInput(session, "sys_volume",    value = sp$volume)
				updateNumericInput(session, "cell_density",  value = sp$cell_density)

				# 4) Ripristino params, population e initial_biomass nei modal
				if (!is.null(vals$models)) {
					nd <- node_data()
					for (m in vals$models) {
						mdl <- m$model_name
						if (mdl %in% names(nd)) {
							nd[[mdl]]$params           <- m$parameters
							nd[[mdl]]$population       <- m$population
							nd[[mdl]]$initial_biomass  <- m$initial_biomass  # ← restore starting biomass
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
