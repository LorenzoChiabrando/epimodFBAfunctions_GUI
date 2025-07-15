# UI for Data Visualization
dataVisUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Data Visualization",
    
		div(class = "card p-3",
			h4("Visualize Hypernode Results"),

			## ── directory picker card ────────────────────────
			div(class = "ddv-dir-card mt-3",
				shinyFiles::shinyDirButton(
				  id    = ns("hypernode_dir"),
				  label = "Browse Hypernode…",
				  title = "Select your hypernode folder",
				  icon  = icon("folder-open"),
				  class = "ddv-dir-btn"
				),
				span(textOutput(ns("dir_path")), class = "selected-ddv-dir")
			),

			actionButton(ns("btn_plot"), "Show Plots", class = "btn-primary mt-2")
		),

		
		
    # Plots panel
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("has_plots")),
      wellPanel(
        fluidRow(
          column(6,
            plotOutput(ns("species_plot"),    height = "250px")
          ),
          column(6,
            plotOutput(ns("metabolite_plot"), height = "250px")
          )
        ),
        fluidRow(
          column(6,
            plotOutput(ns("biomass_plot"),    height = "250px")
          ),
          column(6,
            # Reserved for future
            NULL
          )
        )
      )
    )
  )
}

# Server for Data Visualization
dataVisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set up shinyFiles
    roots <- c(home = ".")
    shinyDirChoose(input, "hypernode_dir", roots = roots, session = session)

    # Display chosen or fallback path
    output$dir_path <- renderText({
      p <- parseDirPath(roots, input$hypernode_dir)
      if (length(p)==1 && nzchar(p) && dir.exists(p)) {
        p
      } else if (!is.null(session$userData$last_hypernode)) {
        session$userData$last_hypernode
      } else {
        ""
      }
    })

    # Determine hypernode path: chooser first, then fallback from simulation
    hypernode_path <- reactive({
      p <- parseDirPath(roots, input$hypernode_dir)
      if (length(p)==1 && nzchar(p) && dir.exists(p)) {
        return(p)
      }
      # fallback to simulation-selected path
      p2 <- session$userData$last_hypernode
      req(!is.null(p2), dir.exists(p2))
      p2
    })

    # Flag if plots are ready
    has_plots <- reactiveVal(FALSE)
    output$has_plots <- reactive({ has_plots() })
    outputOptions(output, "has_plots", suspendWhenHidden = FALSE)

    # AUTO-TRIGGER: if coming from Simulation, click "Show Plots" once
    observeEvent(session$userData$last_hypernode, {
      # only if user hasn't browsed here manually
      p <- parseDirPath(roots, input$hypernode_dir)
      if (!(length(p)==1 && nzchar(p) && dir.exists(p))) {
        updateActionButton(session, "btn_plot", label = NULL)
      }
    }, once = TRUE)

    # On plot button (manual or auto)
    observeEvent(input$btn_plot, {
      # get path
      base <- hypernode_path()
      analysis_dir <- file.path(base, paste0(basename(base), "_analysis"))
      if (!dir.exists(analysis_dir)) {
        showModal(modalDialog(
          title = "Error",
          HTML(paste0("Analysis folder not found:<br><code>", analysis_dir, "</code>")),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        has_plots(FALSE)
        return()
      }

      # Generate plots (catch errors)
      p_species <- tryCatch(
        epimodFBAfunctions::plot_species_dynamics(base, final_time = Inf, output_dir = NULL),
        error = function(e) {
          showModal(modalDialog(title = "Species Plot Error", e$message, easyClose = TRUE, footer = modalButton("OK")))
          NULL
        }
      )
      p_metabolite <- tryCatch(
        epimodFBAfunctions::plot_metabolite_dynamics(base, output_dir = NULL),
        error = function(e) {
          showModal(modalDialog(title = "Metabolite Plot Error", e$message, easyClose = TRUE, footer = modalButton("OK")))
          NULL
        }
      )
      p_biomass <- tryCatch(
        epimodFBAfunctions::plot_biomass_dynamics(base, final_time = Inf, output_dir = NULL),
        error = function(e) {
          showModal(modalDialog(title = "Biomass Plot Error", e$message, easyClose = TRUE, footer = modalButton("OK")))
          NULL
        }
      )

      # Render if all succeeded
      if (!is.null(p_species) && !is.null(p_metabolite) && !is.null(p_biomass)) {
        output$species_plot    <- renderPlot({ p_species })
        output$metabolite_plot <- renderPlot({ p_metabolite })
        output$biomass_plot    <- renderPlot({ p_biomass })
        has_plots(TRUE)
      } else {
        has_plots(FALSE)
      }
    })
  })
}

