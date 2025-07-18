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
        ## --------------------------------------------------------------------
        ##  QUICK TUTORIAL  (two macro-cards)
        ## --------------------------------------------------------------------
        div(
          class = "home-tutorial",
          h3("Quick Tutorial", class = "home-tutorial-title"),

          div(class = "tutorial-cards",

            # ── CARD ① : MODEL GENERATION ──────────────────────────────────
            div(class = "tutorial-card",
              tags$img(src = "home-css/tutorial-images/model-gen/step1.png",
                       alt = "Model Generation",
                       class = "tutorial-card-img"),
              h4("Model Generation"),

              ## --- Step-by-step list ------------------------------------
              tags$ol(class = "tutorial-steps",

                tags$li(
                  strong("Hypernode settings"),
                  br(),
                  "Open the ",
                  em("Model Generation"), " tab, type a unique ",
                  code("hypernode name"), "."
                ),

                tags$li(
                  strong("Choose directories"),
                  br(),
                  "• Pick a ", strong("working directory"), " (where all output ",
                  "folders will be created).",
                  br(),
                  "• Pick the ", strong(".mat directory"), " containing your models."
                ),

                tags$li(
                  tags$img(src = "home-css/tutorial-images/model-gen/step2.png",
                           alt = "Step 1 – directory selection",
                           class = "step-img")
                ),

                tags$li(
                  strong("Load / Reset"),
                  br(),
                  "Click ",
                  tags$span(class = "badge bg-success", "Load Models"),
                  " to import metadata.  ",
                  "Use the ",
                  tags$span(class = "badge bg-danger", "Reset"),
                  " button to clear everything."
                ),

                tags$li(
                  tags$img(src = "home-css/tutorial-images/model-gen/dir-models-list.png",
                           alt = "Models list",
                           class = "step-img")
                ),

                tags$li(
                  strong("Inspect & edit each model"),
                  br(),
                  "Click a model name to open its modal and change biomass, ",
                  "population or other parameters."
                ),

                tags$li(
                  tags$img(src = "home-css/tutorial-images/model-gen/modification_on_single_models.png",
                           alt = "Edit single model",
                           class = "step-img")
                ),

                tags$li(
                  strong("Global simulation settings"),
                  br(),
                  "Use the ",
                  em("Global Community Settings"), " card for volume, cell ",
                  "density and default bounds."
                ),

                tags$li(
                  tags$img(src = "home-css/tutorial-images/model-gen/global.png",
                           alt = "Global settings",
                           class = "step-img")
                ),

                tags$li(
                  strong("Select boundary metabolites (optional)"),
                  br(),
                  "Filter by species or search bar, then click a metabolite to ",
                  "include / exclude it."
                ),

                tags$li(
                  tags$img(src = "home-css/tutorial-images/model-gen/boundary.png",
                           alt = "Boundary metabolite filter",
                           class = "step-img")
                ),

                tags$li(
                  strong("Generate the hypernode"),
                  br(),
                  "Press ",
                  tags$span(class = "badge bg-success", "Generate Model"),
                  " to build YAML, Petri-net and FBA files. A success modal ",
                  "confirms completion."
                )
              ) # /ol
            ),  # /tutorial-card



            # ── CARD ② : MODEL ANALYSIS  (placeholder for now) ──────────────
            div(class = "tutorial-card",
              tags$img(src = "tutorial/model_analysis.png",
                       alt = "Model Analysis",
                       class = "tutorial-card-img"),
              h4("Model Analysis"),
              p("After generating a hypernode, switch to the ",
                em("Simulation → Analysis"), " tab to define time span, ",
                "tolerances and launch the solver.  ",
                "A dedicated tutorial will be added soon.")
            )
          ) # /tutorial-cards
        ),    # /home-tutorial


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

