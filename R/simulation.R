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
    
    # ------------------------------------------------------------------
    # helper: rebuild whole bounds file (projected or unprojected)
    # ------------------------------------------------------------------
    rebuild_bounds_csv <- function(csv_path, slot_name) {
      all_models <- names(node_data())
      long_rows <- lapply(all_models, function(mdl) {
        wide <- node_data()[[mdl]][[slot_name]]
        if (!nrow(wide)) return(NULL)
        data.frame(
          reaction     = c(paste0(wide$reaction, "_r"),
                           paste0(wide$reaction, "_f")),
          FBAmodel     = mdl,
          upper_bound  = c(wide$lower,  wide$upper),
          stringsAsFactors = FALSE
        )
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
      session_files$proj <- file.path(out_dir, "ub_bounds_projected_gui.csv")
      session_files$nproj<- file.path(out_dir, "ub_bounds_not_projected_gui.csv")

      file.copy(file.path(out_dir, "ub_bounds_projected.csv"),
                session_files$proj,  overwrite = TRUE)
      file.copy(file.path(out_dir, "ub_bounds_not_projected.csv"),
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
            population = u$initial_count,
            stringsAsFactors = FALSE
          )))
      } else data.frame()

      # ----- read the TEMP bounds files -------------------------------
      proj_df  <- if (file.exists(session_files$proj))
                    read.csv(session_files$proj,  stringsAsFactors = FALSE) else data.frame()
      nproj_df <- if (file.exists(session_files$nproj))
                    read.csv(session_files$nproj, stringsAsFactors = FALSE) else data.frame()

      make_bounds <- function(df, mdl) {
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

      info_list <- lapply(models(), function(mdl) {
        row    <- yaml_tbl[yaml_tbl$model == mdl, , drop = FALSE]
        params <- if (nrow(row)) as.list(row[1, c("bioMax","bioMean","bioMin","starv","dup","death")]) else
                  as.list(setNames(rep(NA,6), c("bioMax","bioMean","bioMin","starv","dup","death")))
        list(
          params      = params,
          population  = if (nrow(row)) row$population[1] else NA,
          projected   = make_bounds(proj_df , mdl),
          unprojected = make_bounds(nproj_df, mdl)
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
      
      unlink(c(session_files$proj, session_files$nproj), force = TRUE)
      session_files$proj <- session_files$nproj <- NULL
      
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
				  h5(icon("folder-open"), "Hypernode Directory", class = "sim-section-title"),
					div(class = "selected-dir d-flex align-items-center",
						strong("Current:", class = "me-2 text-secondary"),
						span(basename(path), class = "badge bg-primary text-white fs-6")
					),
				  div(class = "mt-3 text-right",
				    actionButton(ns("btn_reset"), NULL, icon = icon("redo"), class = "btn-reset-sim")
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
			if (dir_valid()) {
				div(class = "sim-section-card config",
				  # Section header
				  h5(icon("cogs"), " Simulation Configuration", class = "sim-section-title"),

				  # Times row
				  fluidRow(
				    column(4,
				      numericInput(
				        ns("i_time"), 
				        "Initial Time (h)", 
				        value = 0, min = 0, width = "100%"
				      )
				    ),
				    column(4,
				      numericInput(
				        ns("f_time"), 
				        "Final Time (h)", 
				        value = 10, min = 0, width = "100%"
				      )
				    ),
				    column(4,
				      numericInput(
				        ns("s_time"), 
				        "Step Size (h)", 
				        value = 1, min = 0.0001, width = "100%"
				      )
				    )
				  ),

				  # Tolerances row
				  fluidRow(
				    column(4,
				      numericInput(
				        ns("atol"), 
				        "Absolute Tolerance", 
				        value = 1e-6, min = 0, width = "100%"
				      )
				    ),
				    column(4,
				      numericInput(
				        ns("rtol"), 
				        "Relative Tolerance", 
				        value = 1e-6, min = 0, width = "100%"
				      )
				    )
				  ),

				  # Run button row
				  fluidRow(
				    column(12,
				      div(class = "text-center mt-3",
				        actionButton(
				          ns("btn_run_sim"),
				          "Run Simulation",
				          class = "btn-run-sim px-5"
				        )
				      )
				    )
				  )
				)
			}
		})

		shiny::observeEvent(input$btn_run_sim, {
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

			# ── 2) Model generation ──────────────────────────────────────────────────
			#shiny::withProgress(message = "Generating model...", value = 0, {
			#	epimodFBAfunctions::model_generation_GUI(
			#	  net_fname         = net_file,
			#	  transitions_fname = trans_file,
			#	  fba_fname         = fba_files,
			#	  output_dir        = gen_dir
			#	)
			#})
			#shiny::showNotification("Model generation completed.", type = "message")

			# ── 3) Model analysis ────────────────────────────────────────────────────
			shiny::withProgress(message = "Analyzing model...", value = 0, {
				epimodFBAfunctions::model_analysis_GUI(
				  paths           = c(gen    = gen_dir,
				                      config = config_dir,
				                      src    = src_dir,
				                      output = output_dir),
				  hypernode_name  = hypernode,
				  debug_solver    = FALSE,           # or input$debug_solver
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
				    file.path(output_dir, "ub_bounds_not_projected_gui.csv")
				  ),
				  volume = base
				)
			})
			shiny::showNotification("Model analysis completed.", type = "message")

			# ── 4) Show completion modal ─────────────────────────────────────────────
			showModal(
				shiny::modalDialog(
				  title = "✅ Simulation & Analysis Complete",
				  shiny::p("Your model has been generated and analyzed."),
				  shiny::p("What would you like to do next?"),
				  footer = tagList(
				    actionButton(ns("btn_visualize"),   "Visualize Results", class = "btn-primary"),
				    actionButton(ns("btn_new_sim"),     "New Simulation",    class = "btn-secondary"),
				    modalButton("Close")
				  ),
				  easyClose = FALSE
				)
			)
			  session$userData$last_hypernode <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
		})
		
		 # ── modals for each bacteria ─────────────────────────────────────────
# ── modals for each bacteria ─────────────────────────────────────────
observe({
  req(dir_valid())
  lapply(models(), function(mdl) {

    observeEvent(input[[paste0("model_", mdl)]], ignoreInit = TRUE, {

      info   <- node_data()[[mdl]]
      params <- info$params
      proj   <- info$projected
      unproj <- info$unprojected
      pop    <- info$population

      # unique IDs -----------------------------------------------------
      proj_id  <- paste0("proj_tbl_", mdl)
      unpr_id  <- paste0("unpr_tbl_", mdl)
      save_id  <- paste0("save_",     mdl)
      param_id <- paste0("param_",    mdl)
      pop_id   <- paste0("pop_",      mdl)

      # modal ----------------------------------------------------------
      showModal(
        modalDialog(
          title = paste("Model:", mdl), size = "l", easyClose = FALSE,
          tagList(
            fluidRow(
              column(6, h4("Parameter Recap"), tableOutput(ns(param_id))),
              column(6, h4("Initial Population"), verbatimTextOutput(ns(pop_id)))
            ),
            hr(),
            h4("Exchange Reaction Bounds"),
            tabsetPanel(
              tabPanel("Projected",
                div(style="max-height:350px; overflow-y:auto;",
                    DT::dataTableOutput(ns(proj_id)))
              ),
              tabPanel("Not projected",
                div(style="max-height:350px; overflow-y:auto;",
                    DT::dataTableOutput(ns(unpr_id)))
              )
            )
          ),
          footer = tagList(
            modalButton("Close"),
            actionButton(ns(save_id), "Save Changes", class = "btn-primary")
          ),
          class = "modal-model"
        )
      )

      # static outputs -------------------------------------------------
      output[[param_id]] <- renderTable({
        data.frame(Parameter = names(params),
                   Value     = unlist(params, use.names = FALSE),
                   stringsAsFactors = FALSE)
      }, striped = TRUE, hover = TRUE, spacing = "s")

      output[[pop_id]] <- renderText(pop)

      # datatables -----------------------------------------------------
      opts <- list(dom="ft", paging=FALSE, ordering=FALSE,
                   scrollY=300, scrollCollapse=TRUE)
      lock <- list(target="cell", disable=list(columns=c(0)))

      output[[proj_id]] <- DT::renderDataTable({
        DT::datatable(proj, rownames = FALSE, editable = lock, options = opts)
      })
      output[[unpr_id]] <- DT::renderDataTable({
        DT::datatable(unproj, rownames = FALSE, editable = lock, options = opts)
      })

      # *** IMPORTANT: proxy uses *un-namespaced* ID ***
      proj_proxy <- DT::dataTableProxy(proj_id , session = session)
      unpr_proxy <- DT::dataTableProxy(unpr_id , session = session)

			# projected edits ---------------------------------------------------
			observeEvent(input[[paste0(proj_id, "_cell_edit")]], ignoreInit = TRUE, {
				edit <- input[[paste0(proj_id, "_cell_edit")]]      # ← no ns()
				proj[edit$row, edit$col + 1] <<- as.numeric(edit$value)

				info$projected <- proj
				tmp <- node_data(); tmp[[mdl]] <- info; node_data(tmp)

				DT::replaceData(proj_proxy, proj, resetPaging = FALSE, rownames = FALSE)
				rebuild_bounds_csv(session_files$proj, "projected")
			})

			# unprojected edits -------------------------------------------------
			observeEvent(input[[paste0(unpr_id, "_cell_edit")]], ignoreInit = TRUE, {
				edit <- input[[paste0(unpr_id, "_cell_edit")]]      # ← no ns()
				unproj[edit$row, edit$col + 1] <<- as.numeric(edit$value)

				info$unprojected <- unproj
				tmp <- node_data(); tmp[[mdl]] <- info; node_data(tmp)

				DT::replaceData(unpr_proxy, unproj, resetPaging = FALSE, rownames = FALSE)
				rebuild_bounds_csv(session_files$nproj, "unprojected")
			})


      # save button ----------------------------------------------------
      observeEvent(input[[save_id]], ignoreInit = TRUE, {
        removeModal()
        shiny::showNotification(
          "Bounds saved to GUI files.",
          id = "boundsToast",          # single toast replaces previous
          type = "message", duration = 3
        )
      })
    })
  })
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
