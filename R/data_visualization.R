#' @export
dataVisUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Data Visualization",

    ## ────────── WRAPPER sim-card ──────────
    div(class = "sim-card",

      ## Banner identico agli altri moduli
      div(class = "logo-hero-banner",
        tags$img(src = "Logo_QBio.png", alt = "Logo", class = "hero-logo"),
        tags$h1("Data Visualization", class = "hero-title")
      ),

      ## ────────── DIRECTORY (card blu) ──────────
      div(class = "sim-section-card directory",
        h5(icon("folder-open"), " Hypernode directory", class = "sim-section-title"),
        div(class = "selected-dir d-flex align-items-center gap-3 mb-3",
          shinyFiles::shinyDirButton(
            id    = ns("hypernode_dir"),
            label = "Browse…",
            title = "Select your hypernode folder",
            icon  = icon("folder-open"),
            class = "btn-sim-dir"          # ri-usa il bottone grande blu
          ),
          span(textOutput(ns("dir_path")), class = "badge bg-primary fs-6")
        ),
        hr(),
        actionButton(ns("btn_plot"), "Show Plots", class = "btn btn-primary px-4")
      ),

      ## ────────── PLOT GRID (card arancio) ──────────
      conditionalPanel(
        condition = sprintf("output['%s'] == true", ns("has_plots")),
        div(class = "sim-section-card config dv-plots-card",
          h5(icon("chart-bar"), " Hypernode dynamics", class = "sim-section-title"),
          div(class = "dv-plots-grid",
            plotOutput(ns("species_plot"),    height = 250),
            plotOutput(ns("metabolite_plot"), height = 250),
            plotOutput(ns("biomass_plot"),    height = 250)
            # quarto slot disponibile
          )
        )
      )
    )
  )
}


# Server for Data Visualization
#' @export
dataVisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set up shinyFiles
		project_root <- getOption("epimodFBAfunctionsGUI.user_proj",
				                      normalizePath("~"))
    roots <- c(
      Home    = "~",   # your real home folder
      Project = project_root    # your RStudio project directory
    )
    shinyDirChoose(input, "hypernode_dir", roots = roots, session = session, defaultRoot = "Project")

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

