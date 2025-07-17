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
      dir_valid(FALSE)
      models(character())
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
				    file.path(output_dir, "ub_bounds_projected.csv"),
				    file.path(output_dir, "ub_bounds_not_projected.csv")
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
		observe({
			req(dir_valid())
			lapply(models(), function(m) {
				observeEvent(input[[paste0("model_", m)]], ignoreInit = TRUE, {

				  ## ── pull cached data ───────────────────────────────────────────
				  info  <- node_data()[[m]]
				  params <- info$params
				  proj   <- info$projected
				  unproj <- info$unprojected
				  pop    <- info$population

				  ## ── build modal ────────────────────────────────────────────────
				  showModal(
				    modalDialog(
				      title     = paste("Model:", m),
				      size      = "l",
				      easyClose = FALSE,         # force use of footer buttons
				      tagList(
				        fluidRow(
				          column(6,
				                 h4("Parameter Recap"),
				                 tableOutput(ns("modal_param_table"))
				          ),
				          column(6,
				                 h4("Initial Population"),
				                 verbatimTextOutput(ns("modal_pop"))
				          )
				        ),
				        hr(),
				        h4("Exchange Reaction Bounds"),
				        tabsetPanel(
				          tabPanel("Projected",
				            div(style = "max-height:350px; overflow-y:auto;",
				                DT::dataTableOutput(ns("modal_proj_table"))
				            )
				          ),
				          tabPanel("Not projected",
				            div(style = "max-height:350px; overflow-y:auto;",
				                DT::dataTableOutput(ns("modal_unproj_table"))
				            )
				          )
				        )
				      ),
				      footer = tagList(
				        modalButton("Close"),
				        actionButton(ns("modal_save"), "Save Changes", class = "btn-primary")
				      ),
				      class = "modal-model"
				    )
				  )

				  ## ── renderers ──────────────────────────────────────────────────
				  output$modal_param_table <- renderTable({
				    data.frame(Parameter = c("bioMax","bioMean","bioMin","starv","dup","death"),
				               Value     = unlist(params, use.names = FALSE),
				               stringsAsFactors = FALSE)
				  }, striped = TRUE, hover = TRUE, spacing = "s")

				  output$modal_pop <- renderText(pop)

				  ## ----  DT helpers  --------------------------------------------
				  opts <- list(
				    dom            = "ft",
				    paging         = FALSE,
				    ordering       = FALSE,
				    scrollY        = 300,
				    scrollCollapse = TRUE
				  )
				  lock_first <- list(target = "cell", disable = list(columns = c(0)))

				  output$modal_proj_table <- DT::renderDataTable({
				    DT::datatable(proj,  rownames = FALSE,
				                  editable = lock_first,
				                  options  = opts)
				  })
				  output$modal_unproj_table <- DT::renderDataTable({
				    DT::datatable(unproj, rownames = FALSE,
				                  editable = lock_first,
				                  options  = opts)
				  })

				  ## ── listen for cell edits (keep node_data up-to-date) ──────────
				  proj_proxy   <- DT::dataTableProxy(ns("modal_proj_table"))
				  unproj_proxy <- DT::dataTableProxy(ns("modal_unproj_table"))
				  
				  # --- projected edits -------------------------------------------
				  observeEvent(input$modal_proj_table_cell_edit, ignoreInit = TRUE, {
				    edit <- input$modal_proj_table_cell_edit
				    proj[edit$row, edit$col + 1] <<- as.numeric(edit$value)
				    info$projected <- proj
				    tmp <- node_data(); tmp[[m]] <- info; node_data(tmp)

				    DT::replaceData(proj_proxy, proj, resetPaging = FALSE, rownames = FALSE)
				    write.csv(proj, session_files$proj, row.names = FALSE)   #  ⬅️  flush to temp
				  })

				  # --- unprojected edits -----------------------------------------
				  observeEvent(input$modal_unproj_table_cell_edit, ignoreInit = TRUE, {
				    edit <- input$modal_unproj_table_cell_edit
				    unproj[edit$row, edit$col + 1] <<- as.numeric(edit$value)
				    info$unprojected <- unproj
				    tmp <- node_data(); tmp[[m]] <- info; node_data(tmp)

				    DT::replaceData(unproj_proxy, unproj, resetPaging = FALSE, rownames = FALSE)
				    write.csv(unproj, session_files$nproj, row.names = FALSE) #  ⬅️  flush to temp
				  })

     			 # save button: commit temp to originals, close modal, toast
				  observeEvent(input$modal_save, {
				    base_dir <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
				    out_dir  <- file.path(base_dir, "output")

				    file.copy(session_files$proj,
				              file.path(out_dir, "ub_bounds_projected.csv"),
				              overwrite = TRUE)
				    file.copy(session_files$nproj,
				              file.path(out_dir, "ub_bounds_not_projected.csv"),
				              overwrite = TRUE)

				    removeModal()
				    shiny::showNotification(
				      paste("Bounds for", m, "saved to disk."),
				      type = "message", duration = 3
				    )
				  }, ignoreInit = TRUE)

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
