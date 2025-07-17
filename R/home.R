# R/home.R

# UI for Home tab, now including a Tutorial section
#' @export
homeUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Home",
    div(
      id = "home-page",
      div(
        class = "home-container",
        # logo + title card
        div(class = "home-main-card",
            tags$img(
              src   = "Logo_QBio.png",
              alt   = "QBIO Logo",
              class = "home-logo"
            ),
            h2("Welcome to epimodFBA", class = "home-title"),
            p(
              "Use this application to generate FBA models, run simulations, ",
              "execute the minMicrobiome algorithm, and visualize results.",
              class = "home-subtitle"
            )
        ),

        # feature sub-cards container
        div(
          class = "home-cards",
          actionLink(
            ns("go_mg"), 
            div(class = "home-card",
                h3("Model Generation", class = "home-card-title"),
                p("Easily build FBA models from your .mat files.", class = "home-card-text")
            )
          ),
          actionLink(
            ns("go_sim"), 
            div(class = "home-card",
                h3("Simulation", class = "home-card-title"),
                p("Run community flux–balance simulations in one click.", class = "home-card-text")
            )
          ),
          actionLink(
            ns("go_mm"), 
            div(class = "home-card",
                h3("minMicrobiome Algorithm", class = "home-card-title"),
                p("Identify minimal microbial consortia.", class = "home-card-text")
            )
          ),
          actionLink(
            ns("go_dv"), 
            div(class = "home-card",
                h3("Data Visualization", class = "home-card-title"),
                p("Explore results with interactive plots.", class = "home-card-text")
            )
          )
        ),

        # Tutorial section
        div(
          class = "home-tutorial",
          h3("Quick Tutorial", class = "home-tutorial-title"),
          p("Follow these steps to get started with epimodFBA:"),
          tags$ol(
            tags$li(
              strong("Step 1: Model Generation"),
              tags$br(),
              tags$img(src = "tutorial/step1_model_gen.png", alt = "Model Generation Tutorial", class = "tutorial-img"),
              p("Upload your .mat file and click 'Generate Model' to create your FBA model.")
            ),
            tags$li(
              strong("Step 2: Simulation"),
              tags$br(),
              tags$img(src = "tutorial/step2_simulation.png", alt = "Simulation Tutorial", class = "tutorial-img"),
              p("Select your community and parameters, then run the simulation with a single click.")
            ),
            tags$li(
              strong("Step 3: minMicrobiome Algorithm"),
              tags$br(),
              tags$img(src = "tutorial/step3_minmicrobiome.png", alt = "minMicrobiome Tutorial", class = "tutorial-img"),
              p("Identify the minimal set of microbes required to maintain function.")
            ),
            tags$li(
              strong("Step 4: Data Visualization"),
              tags$br(),
              tags$img(src = "tutorial/step4_datavis.png", alt = "Data Visualization Tutorial", class = "tutorial-img"),
              p("Explore flux distributions and community profiles with interactive plots.")
            )
          )
        ),

        # footer authorship
        div(class = "home-footer",
            p("© 2025 Authors: Aucello Riccardo, Pernice Simone, Chiabrando Lorenzo")
        )
      )
    )
  )
}


# Server for Home tab
#' @export
homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # climb to top‐level session
    rootSess <- session
    while (!is.null(rootSess$parent)) rootSess <- rootSess$parent

    observeEvent(input$go_mg, {
      updateNavlistPanel(rootSess, "main_nav", selected = "Model Generation")
    })
    observeEvent(input$go_sim, {
      updateNavlistPanel(rootSess, "main_nav", selected = "Simulation")
    })
    observeEvent(input$go_mm, {
      updateNavlistPanel(rootSess, "main_nav", selected = "minMicrobiome Algorithm")
    })
    observeEvent(input$go_dv, {
      updateNavlistPanel(rootSess, "main_nav", selected = "Data Visualization")
    })
  })
}

