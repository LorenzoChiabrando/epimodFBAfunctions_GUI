# R/sensitivity.R

#’ Sensitivity Analysis Section UI Module
#’
#’ @param id Module namespace id
#’ @return A tabPanel for hypernode selection and placeholder
#’ @export
sensitivityUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel(
    title = "Sensitivity",
    div(class = "sim-card",

      ## Directory selector
      uiOutput(ns("dir_selector")),

      ## Placeholder or future controls
      uiOutput(ns("body"))
    )
  )
}

#’ Sensitivity Analysis Section Server Module
#’
#’ @param id Module namespace id
#’ @export
sensitivityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Default project root (can be overridden via option)
    project_root <- getOption("epimodFBAfunctionsGUI.user_proj",
                              normalizePath("~"))

    # Allow browsing Home or Project
    roots <- c(
      Home    = "~",
      Project = project_root
    )
    shinyFiles::shinyDirChoose(
      input       = input,
      id          = "hypernode_dir",
      roots       = roots,
      session     = session,
      defaultRoot = "Project",
      defaultPath = ""
    )

    # Reactive flags
    dir_valid  <- shiny::reactiveVal(FALSE)
    reset_flag <- shiny::reactiveVal(FALSE)

    # When user picks a directory, validate it
    shiny::observeEvent(input$hypernode_dir, {
      path <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
      if (length(path) && dir.exists(path[[1]])) {
        dir_valid(TRUE)
      } else {
        dir_valid(FALSE)
      }
    })

    # Reset button handler
    shiny::observeEvent(input$btn_reset, {
      reset_flag(TRUE)
      dir_valid(FALSE)
    })

    # Directory selector UI
    output$dir_selector <- shiny::renderUI({
      if (dir_valid() && !reset_flag()) {
        path <- shinyFiles::parseDirPath(roots, input$hypernode_dir)
        shiny::div(class = "sim-section-card directory",
          shiny::h5(shiny::icon("folder-open"), "Hypernode Directory", class = "sim-section-title"),
          shiny::div(class = "selected-dir d-flex align-items-center",
            shiny::strong("Current:", class = "me-2 text-secondary"),
            shiny::span(basename(path), class = "badge bg-primary text-white fs-6")
          ),
          shiny::div(class = "mt-3 text-right",
            shiny::actionButton(
              inputId = ns("btn_reset"),
              label   = NULL,
              icon    = shiny::icon("redo"),
              class   = "btn-reset-sim"
            )
          )
        )
      } else {
        reset_flag(FALSE)
        shiny::div(class = "sim-section-card directory",
          shiny::h5(shiny::icon("folder-open"),
                    "Choose Hypernode Directory For Sensitivity Analysis Simulation",
                    class = "sim-section-title"),
          shiny::div(class = "sim-dir-selector mt-3",
            shinyFiles::shinyDirButton(
              id    = ns("hypernode_dir"),
              label = "Browse…",
              title = "Choose hypernode folder",
              icon  = shiny::icon("folder-open"),
              class = "btn-sim-dir"
            )
          )
        )
      }
    })

    # Body placeholder: show “work in progress” image once a valid dir is chosen
    output$body <- shiny::renderUI({
      if (dir_valid() && !reset_flag()) {
        shiny::div(
          style = "
            display: flex;
            justify-content: center;
            align-items:   center;
            height: calc(100vh - 200px);
            width:  100%;
            text-align: center;
          ",
          shiny::tags$img(
            src   = "work_in_progress.png",
            style = "max-width: 320px; height: auto;"
          )
        )
      } else {
        NULL
      }
    })

  })
}

