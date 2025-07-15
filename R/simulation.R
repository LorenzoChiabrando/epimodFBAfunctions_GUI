# R/simulation.R

#' Simulation UI Module
#'
#' @param id Module namespace id
#' @return A tabPanel for hypernode selection and simulation configuration
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
simulationServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Setup for directory chooser
    roots <- c(home = ".")
    shinyFiles::shinyDirChoose(
      input   = input,
      id      = "hypernode_dir",
      roots   = roots,
      session = session
    )

    # Reactive flags
    dir_valid  <- shiny::reactiveVal(FALSE)
    models     <- shiny::reactiveVal(character())
    reset_flag <- shiny::reactiveVal(FALSE)

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
				  h5(icon("folder-open"), "Choose Hypernode Directory", class = "sim-section-title"),
				  div(class = "sim-dir-selector mt-3",
				    shinyFiles::shinyDirButton(
				      id    = ns("hypernode_dir"),
				      label = "Browse…",
				      title = "Choose hypernode folder",
				      icon  = icon("folder-open"),
				      class = "btn-sim-dir"
				    )
				  )
				)
			}
		})

    # Render model list
		# Model list UI
		output$model_list <- renderUI({
			if (dir_valid()) {
				div(class = "sim-section-card models",
				  # Header styled like Simulation Configuration
				  h5(icon("cubes"), "Models", class = "sim-section-title"),

				  # Scrollable list
				  div(class = "sim-model-list",
				    tags$table(class = "table sim-table",
				      tags$thead(
				        tags$tr(tags$th("Model"))
				      ),
				      tags$tbody(
				        lapply(models(), function(m) {
				          tags$tr(
				            tags$td(
				              actionLink(ns(paste0("model_", m)), m)
				            )
				          )
				        })
				      )
				    )
				  )
				)
			}
		})

    # Model click modals
    shiny::observe({
      req(models())
      lapply(seq_along(models()), function(i) {
        id_link <- paste0("model_", i)
        shiny::observeEvent(input[[id_link]], {
          shiny::showModal(shiny::modalDialog(
            title = paste0("Model: ", models()[i]),
            shiny::p("Temporary message for ", models()[i]),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          ))
        }, ignoreInit = TRUE)
      })
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
