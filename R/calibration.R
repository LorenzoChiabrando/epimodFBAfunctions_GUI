# R/sensitivity.R

#' Sensitivity Analysis Section UI Module
#'
#' @export
calibrationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Calibration",
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

# Server for minMicrobiome Algorithm
#' @export
calibrationServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    ns <- session$ns

		project_root <- getOption("epimodFBAfunctionsGUI.user_proj",
				                      normalizePath("~"))
				                        
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
				  h5(icon("folder-open"), "Choose Hypernode Directory For Model Calibration Simulation", class = "sim-section-title"),
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
  })
}

