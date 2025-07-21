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
				          as.list(setNames(rep(NA,6), c("bioMax","bioMean","bioMin","starv","dup","death","mu_max")))
				list(
				  params     = params,
				  population = if (nrow(row)) row$population[1] else NA,
				  projected  = make_bounds_proj(proj_df,    mdl),
				  unproj_f   = make_bounds_f(nproj_f_df, mdl),
				  unproj_r   = make_bounds_r(nproj_r_df, mdl)
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
						   class = "btn btn-outline-secondary btn-sm"
						),
						actionButton(
						   ns("load_configuration"),
						   tagList(icon("upload"), "Load Configuration…"),
						   class = "btn btn-outline-secondary btn-sm"
						),
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
				  
					div(class = "mt-2 d-flex gap-2",
							shinyFiles::shinyDirButton(
								id    = ns("export_dir"),
								label = "Save bounds…",
								title = "Copy the *_gui.csv files to a folder you choose",
								icon  = icon("download"),
								class = "btn btn-outline-secondary btn-sm"
							),
							shinyFiles::shinyFilesButton(        # ←--- correct name
								id    = ns("load_proj"),
								label = "Load projected…",
								title = "Pick a CSV to replace ub_bounds_projected_gui.csv",
								multiple = FALSE,
								icon  = icon("file-upload"),
								class = "btn btn-outline-secondary btn-sm"
							),
							shinyFiles::shinyFilesButton(        # ←--- correct name
								id    = ns("load_nproj"),
								label = "Load not-projected…",
								title = "Pick a CSV to replace ub_bounds_not_projected_gui.csv",
								multiple = FALSE,
								icon  = icon("file-upload"),
								class = "btn btn-outline-secondary btn-sm"
							)
					)

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
				
				# ──────────────── System Parameters (from JSON) ────────────────
				hr(),
				# single tooltip on section title
				h5(
				  "System Parameters",
				  span(icon("question-circle"), style = "cursor:help; margin-left:8px;",
				       title = "Default system parameters."),
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

			# 1) Wrap the entire workflow in tryCatch with a finally for cleanup
			tryCatch({

				# ── 1) Set up paths ───────────────────────────────────────────────────────
				base       <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
				gen_dir    <- file.path(base, "gen")
				config_dir <- file.path(base, "config")
				src_dir    <- file.path(base, "src")
				output_dir <- file.path(base, "output")
				hypernode  <- basename(base)

				if (!dir.exists(gen_dir)) dir.create(gen_dir, recursive = TRUE)

				net_file   <- list.files(file.path(base, "petri_net"), "\\.PNPRO$", full.names = TRUE)
				trans_file <- list.files(src_dir, "\\.cpp$", full.names = TRUE)[1]
				fba_files  <- list.files(file.path(base, "biounits"), "\\.txt$", full.names = TRUE, recursive = TRUE)

				# ── SAVE GUI-SNAPSHOT YAML ─────────────────────────────────────────────────
				yml_file <- list.files(config_dir, "\\.ya?ml$", full.names = TRUE)[1]
				orig_yml <- if (length(yml_file)) yaml::read_yaml(yml_file) else list()
				gui_yml  <- orig_yml

				# 1) record simulation timing & tolerances
				gui_yml$simulation <- list(
				  initial_time       = input$i_time,
				  final_time         = input$f_time,
				  step_size          = input$s_time,
				  absolute_tolerance = input$atol,
				  relative_tolerance = input$rtol
				)

				# 2) boundary metabolite concentrations
				bm_defs <- orig_yml$boundary_metabolites %||% character()
				if (length(bm_defs)) {
				  met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
				  gui_yml$simulation$boundary_concentrations <-
				    setNames(
				      lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				      bm_defs
				    )
				}

				# 3) system parameters (including volume)
				gui_yml$simulation$system_parameters <- list(
				  fba_upper_bound = input$fba_ub,
				  fba_lower_bound = input$fba_lb,
				  background_met  = input$background_met,
				  volume          = input$sys_volume,
				  cell_density    = input$cell_density
				)

				# 4) write snapshot file
				gui_yaml_path <- file.path(config_dir, paste0(hypernode, "_gui.yaml"))
				yaml::write_yaml(gui_yml, gui_yaml_path)
				# ─────────────────────────────────────────────────────────────────────────

				# ── 2) Model generation ──────────────────────────────────────────────────
				# (omitted for brevity)

				# ── 3) Model analysis ────────────────────────────────────────────────────
				epimodFBAfunctions::model_analysis_GUI(
				  paths           = c(gen    = gen_dir,
				                      config = config_dir,
				                      src    = src_dir,
				                      output = output_dir),
				  hypernode_name  = hypernode,
				  debug_solver    = FALSE,
				  i_time          = input$i_time,
				  f_time          = input$f_time,
				  s_time          = input$s_time,
				  atol            = input$atol,
				  rtol            = input$rtol,
				  fba_fname       = fba_files,
				  user_files      = c(
				    file.path(config_dir, "population_parameters.csv"),
				    file.path(gen_dir,    paste0(hypernode, ".fbainfo")),
				    file.path(output_dir, "ub_bounds_projected_gui.csv"),
				    file.path(output_dir, "non_projected_forward_bounds_gui.csv"),
				    file.path(output_dir, "non_projected_reverse_background_met_gui.csv")
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
				  unlink(
				    c(session_files$proj,
				      session_files$nproj_f,
				      session_files$nproj_r),
				    force = TRUE
				  )
				  # 2) remove GUI-YAML snapshot
				  unlink(gui_yaml_path, force = TRUE)
				  # 3) remove GUI-patched R stub(s)
				  r_gui_files <- list.files(src_dir, "*_gui\\.R$", full.names = TRUE)
				  if (length(r_gui_files)) unlink(r_gui_files, force = TRUE)
				  # 4) remove GUI-patched C++ stub(s)
				  cpp_gui_files <- list.files(src_dir, "*_gui\\.cpp$", full.names = TRUE)
				  if (length(cpp_gui_files)) unlink(cpp_gui_files, force = TRUE)
				}, silent = TRUE)
			})  # /tryCatch

		})  # /observeEvent input$btn_run_sim



		
		 # ── modals for each bacteria ─────────────────────────────────────────
		observe({
			req(dir_valid())
			lapply(models(), function(mdl) {

				observeEvent(input[[paste0("model_", mdl)]], ignoreInit = TRUE, {
				  info     <- node_data()[[mdl]]
				  params   <- info$params
				  proj     <- info$projected
				  unproj_f <- info$unproj_f
				  unproj_r <- info$unproj_r
				  pop      <- info$population

				  # unique IDs
				  proj_id    <- paste0("proj_tbl_", mdl)
				  unpr_f_id  <- paste0("unpr_f_tbl_", mdl)
				  unpr_r_id  <- paste0("unpr_r_tbl_", mdl)
				  save_id    <- paste0("save_", mdl)
				  param_id   <- paste0("param_", mdl)
				  pop_id     <- paste0("pop_", mdl)

				  # show modal with three tabs
				  showModal(
				    modalDialog(
				      title     = paste("Model:", mdl),
				      size      = "l",
				      easyClose = FALSE,
				      class     = "modal-model",
				      tagList(
				        # params & pop
				        fluidRow(
				          column(6,
				            h4("Parameter recap", class = "modal-section-title"),
				            tableOutput(ns(param_id))
				          ),
				          column(6,
				            h4("Initial population", class = "modal-section-title"),
				            verbatimTextOutput(ns(pop_id))
				          )
				        ),
				        hr(class = "modal-divider"),
				        h4("Exchange reaction bounds", class = "modal-section-title"),
				        div(class = "modal-tabs",
				          tabsetPanel(
				            type = "tabs",
				            tabPanel(
				              "Projected",
				              div(
				                class = "modal-table-wrapper",
				                style = "max-height:350px; overflow-y:auto;",
				                DT::dataTableOutput(ns(proj_id))
				              )
				            ),
				            tabPanel(
				              "Unprojected Forward",
				              div(
				                class = "modal-table-wrapper",
				                style = "max-height:350px; overflow-y:auto;",
				                DT::dataTableOutput(ns(unpr_f_id))
				              )
				            ),
				            tabPanel(
				              "Unprojected Reverse",
				              div(
				                class = "modal-table-wrapper",
				                style = "max-height:350px; overflow-y:auto;",
				                DT::dataTableOutput(ns(unpr_r_id))
				              )
				            )
				          )
				        )
				      ),
				      footer = tagList(
				        modalButton("Close"),
				        actionButton(ns(save_id), "Save changes", class = "btn-primary modal-save-btn")
				      )
				    )
				  )

				  # static outputs
				  output[[param_id]] <- renderTable({
				    data.frame(
				      Parameter = names(params),
				      Value     = unlist(params, use.names = FALSE),
				      stringsAsFactors = FALSE
				    )
				  }, striped = TRUE, hover = TRUE, spacing = "s")
				  output[[pop_id]] <- renderText(pop)

				  # DataTable options
				  opts <- list(dom = "ft", paging = FALSE, ordering = FALSE,
				               scrollY = 300, scrollCollapse = TRUE)
				  lock <- list(target = "cell", disable = list(columns = c(0)))

				  # render each table
				  output[[proj_id]]   <- DT::renderDataTable({ DT::datatable(proj,     rownames = FALSE, editable = lock, options = opts) })
				  output[[unpr_f_id]] <- DT::renderDataTable({ DT::datatable(unproj_f, rownames = FALSE, editable = lock, options = opts) })
				  output[[unpr_r_id]] <- DT::renderDataTable({ DT::datatable(unproj_r, rownames = FALSE, editable = lock, options = opts) })

				  # proxies for editing
				  proj_proxy   <- DT::dataTableProxy(proj_id,   session = session)
				  unpr_f_proxy <- DT::dataTableProxy(unpr_f_id, session = session)
				  unpr_r_proxy <- DT::dataTableProxy(unpr_r_id, session = session)

				  # projected edits
				  observeEvent(input[[paste0(proj_id, "_cell_edit")]], ignoreInit = TRUE, {
				    edit <- input[[paste0(proj_id, "_cell_edit")]]
				    proj[edit$row, edit$col + 1] <<- as.numeric(edit$value)

				    info$projected <- proj
				    tmp <- node_data(); tmp[[mdl]] <- info; node_data(tmp)

				    DT::replaceData(proj_proxy, proj, resetPaging = FALSE, rownames = FALSE)
				    rebuild_bounds_csv(session_files$proj, "projected")
				  })

				  # forward unprojected edits
				  observeEvent(input[[paste0(unpr_f_id, "_cell_edit")]], ignoreInit = TRUE, {
				    edit <- input[[paste0(unpr_f_id, "_cell_edit")]]
				    unproj_f[edit$row, edit$col + 1] <<- as.numeric(edit$value)

				    info$unproj_f <- unproj_f
				    tmp <- node_data(); tmp[[mdl]] <- info; node_data(tmp)

				    DT::replaceData(unpr_f_proxy, unproj_f, resetPaging = FALSE, rownames = FALSE)
				    rebuild_bounds_csv(session_files$nproj_f, "unproj_f")
				  })

					# reverse unprojected edits
					observeEvent(input[[paste0(unpr_r_id, "_cell_edit")]], ignoreInit = TRUE, {
						edit <- input[[paste0(unpr_r_id, "_cell_edit")]]
						# update in-memory table
						unproj_r[edit$row, edit$col + 1] <<- as.numeric(edit$value)

						# write back to node_data
						info$unproj_r <- unproj_r
						tmp          <- node_data()
						tmp[[mdl]]   <- info
						node_data(tmp)

						# update the DT in the UI
						DT::replaceData(unpr_r_proxy, unproj_r, resetPaging = FALSE, rownames = FALSE)

						# rebuild the reverse CSV
						rebuild_bounds_csv(session_files$nproj_r, "unproj_r")
					})


				  # save button
				  observeEvent(input[[save_id]], ignoreInit = TRUE, {
				    removeModal()
				    showNotification("Bounds saved to GUI files.", id = "boundsToast", type = "message", duration = 3)
				  })

				})  # /observeEvent model_<mdl>
			})    # /lapply(models())
		})      # /observe


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
			
			# copy the GUI temps
			file.copy(session_files$proj,
				        file.path(save_dir, basename(session_files$proj)))
			file.copy(session_files$nproj_f,
				        file.path(save_dir, basename(session_files$nproj_f)))
			file.copy(session_files$nproj_r,
				        file.path(save_dir, basename(session_files$nproj_r)))
			
			# snapshot all inputs
			# Times & tolerances
			cfg_list <- list(
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
			# boundary metabolites (YAML order)
			yml_f   <- list.files(file.path(base_dir, "config"), "\\.ya?ml$", full.names=TRUE)[1]
			yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
			bm_defs <- yml$boundary_metabolites %||% character()
			met_ids <- gsub("[^A-Za-z0-9_]", "_", bm_defs)
			cfg_list$boundary_concentrations <- setNames(
				lapply(met_ids, function(id) input[[paste0("conc_", id)]]),
				bm_defs
			)
			# system params
			cfg_list$system_parameters <- list(
				fba_upper_bound = input$fba_ub,
				fba_lower_bound = input$fba_lb,
				background_met  = input$background_met,
				volume          = input$sys_volume,
				cell_density    = input$cell_density
			)
			# write JSON
			jsonlite::write_json(cfg_list,
				                   file.path(save_dir, "config_values.json"),
				                   auto_unbox = TRUE, pretty = TRUE)
			
			# finish
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

		# —————————————————————————————————————————————————————————————
		# 2) Load & apply
		# —————————————————————————————————————————————————————————————
		observeEvent(input$confirm_load_config, {
			message("[LOAD] confirm_load_config")
			req(input$choose_config)

			cfg_name <- input$choose_config
			base_dir <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
			load_dir <- file.path(base_dir, "settings", cfg_name)
			message("[LOAD] load_dir = ", load_dir)

			# Copy GUI CSVs
			for (slot in c("proj","nproj_f","nproj_r")) {
				dest <- session_files[[slot]]
				src  <- file.path(load_dir, basename(dest))
				message("[LOAD] copying ", src, " → ", dest)
				ok <- file.copy(from=src, to=dest, overwrite=TRUE)
				message("[LOAD] copy ", slot, " ok? ", ok)
			}

			# Rebuild node_data (assuming make_bounds_* already in scope)
			proj_df    <- if (file.exists(session_files$proj))    read.csv(session_files$proj,  stringsAsFactors=FALSE) else data.frame()
			nproj_f_df <- if (file.exists(session_files$nproj_f)) read.csv(session_files$nproj_f, stringsAsFactors=FALSE) else data.frame()
			nproj_r_df <- if (file.exists(session_files$nproj_r)) read.csv(session_files$nproj_r, stringsAsFactors=FALSE) else data.frame()

			models_list <- models()
			info_list <- lapply(models_list, function(mdl) {
				message("[LOAD] rebuild model ", mdl)
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
			message("[LOAD] node_data rebuilt")

			# Load JSON and update inputs (bare IDs!)
			json_path <- file.path(load_dir, "config_values.json")
			if (file.exists(json_path)) {
				vals <- jsonlite::fromJSON(json_path, simplifyVector=TRUE)
				message("[LOAD] JSON loaded")

				# times & tolerances
				updateNumericInput(session, "i_time", value=vals$times$initial_time)
				updateNumericInput(session, "f_time", value=vals$times$final_time)
				updateNumericInput(session, "s_time", value=vals$times$step_size)
				updateNumericInput(session, "atol",   value=vals$tolerances$atol)
				updateNumericInput(session, "rtol",   value=vals$tolerances$rtol)
				message("[LOAD] times/tolerances updated")

				# boundary concentrations
				yml_f   <- list.files(file.path(base_dir,"config"), "\\.ya?ml$", full.names=TRUE)[1]
				yml     <- if (length(yml_f)) yaml::read_yaml(yml_f) else list()
				bm_defs <- yml$boundary_metabolites %||% character()
				met_ids <- gsub("[^A-Za-z0-9_]","_", bm_defs)
				for (i in seq_along(bm_defs)) {
				  input_id <- paste0("conc_", met_ids[i])
				  val      <- vals$boundary_concentrations[[ bm_defs[i] ]]
				  updateNumericInput(session, input_id, value=val)
				  message("[LOAD] conc ", bm_defs[i], " → ", val)
				}

				# system parameters
				sp <- vals$system_parameters
				updateNumericInput(session, "fba_ub",       value=sp$fba_upper_bound)
				updateNumericInput(session, "fba_lb",       value=sp$fba_lower_bound)
				updateNumericInput(session, "background_met",value=sp$background_met)
				updateNumericInput(session, "sys_volume",   value=sp$volume)
				updateNumericInput(session, "cell_density", value=sp$cell_density)
				message("[LOAD] system parameters updated")
			} else {
				message("[LOAD] config_values.json missing")
			}

			removeModal()
			showNotification(paste("Loaded configuration:", cfg_name), type="message")
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
