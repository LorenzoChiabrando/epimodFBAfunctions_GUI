# R/home.R

# UI for Home tab, now including a Tutorial section
#' @export
homeUI <- function(id){
  ns <- NS(id)

  tabPanel(
    title = "Home",
    # dentro homeUI(), al posto di div(id="home-wrapper", …)
		div(id = "home-wrapper", class = "container-fluid py-5",

			## ---------- WRAPPER GRIGIO -------------------------------------
			div(class = "home-box mx-auto px-4 py-5",

				## ---------- HERO ---------------------------------------------
				div(class = "home-hero text-center text-white mb-5",
				  tags$img(src = "Logo_QBio.png", alt = "Logo", class = "hero-logo mb-3"),
				  h1("epimodFBA Workbench", class = "display-4 fw-bold mb-2"),
				  p("Flux-balance pipelines for microbial communities",
				    class = "lead mb-0")
				),

				## ---------- FEATURE CARDS ROW --------------------------------
				div(class = "feature-box",
		 			 h2(class = "feature-title", "The Functionality"),
					div(class = "home-cards d-flex flex-wrap justify-content-center gap-4 mb-5",

						featureCard(ns, "go_mg",  "prova.png",
						            "Model Generation",
						            "Build FBA models from your .mat files."),

						featureCard(ns, "go_sim", "prova2.png",
						            "Simulation",
						            "Run community flux–balance simulations."),

						featureCard(ns, "go_dv",  "data_vis.png",
						            "Data Visualization",
						            "Explore results with interactive plots."),

						featureCard(ns, "go_mm",  "min_microbiome_optimized.png",
						            "minMicrobiome Algorithm",
						            "Identify minimal microbial consortia.")
					)
				),

				## ---------- FOOTER VERDE -------------------------------------
				div(class = "home-footer text-center text-white py-3",
				    "© 2025 Aucello Riccardo · Pernice Simone · Chiabrando Lorenzo")
			)
		)

  )
}

featureCard <- function(ns, id, img, title, text){
  actionLink(
    inputId = ns(id),

    # ---- label ----------------------------------------------------
    label   = div(class = "card h-100 text-center p-3 border-0 shadow-sm",
      tags$img(src = img,
               class = "card-img-top mx-auto d-block",
               style = "max-height:160px; object-fit:contain;"),
      div(class = "card-body",
        h4(class = "card-title", title),
        p(class = "card-text", text)
      )
    ),

    # ---- HTML attributes (NOT part of signature) -----------------
    class   = "d-block",    # permette di cliccare l’intera card
    style   = "text-decoration:none;"   # rimuove underline default
  )
}


# Server for Home tab
#' @export
homeServer <- function(id){
  moduleServer(id, function(input, output, session){

    # riferimento al root per la navlist (resta)
    root <- session
    while(!is.null(root$parent)) root <- root$parent

    # ---- mapping card → contenuto tutorial -----------------------
    tutorial_html <- list(

      go_mg = tutorialModelGen(),
      go_sim = div(
        h3("Simulation – quick start"),
        p("Coming soon …")
      ),
      go_dv = div(
        h3("Data Visualization"),
        p("Coming soon …")
      ),
      go_mm = div(
        h3("minMicrobiome Algorithm"),
        p("Coming soon …")
      )
    )

    # osserva *tutti* i click con lapply ---------------------------
    lapply(names(tutorial_html), function(id_btn){

      observeEvent(input[[id_btn]], {

        # 1) opzionale: vai alla tab corrispondente
        updateNavlistPanel(root, "main_nav",
                           selected = btn2tab(id_btn))

        # 2) mostra il modal tutorial
        showModal(
          modalDialog(
            title = NULL, easyClose = TRUE, size = "l",
            tutorial_html[[id_btn]],
            footer = modalButton("Close")
          )
        )
      }, ignoreInit = TRUE)
    })

  })
}

# helper: card-id → nome tab navlist
btn2tab <- function(id){
  switch(id,
    go_mg  = "Model Generation",
    go_sim = "Simulation",
    go_dv  = "Data Visualization",
    go_mm  = "minMicrobiome Algorithm"
  )
}

# ---- tutorial HTML per Model Generation (riusa il tuo ol) --------
tutorialModelGen <- function(){
  div(
    h2("Model Generation – quick tutorial", class = "mb-3"),
    tags$ol(class = "list-group list-group-numbered",
      tags$li(class = "list-group-item",
        strong("Hypernode settings"), br(),
        "Open the ", em("Model Generation"), " tab and type a unique ",
        code("hypernode name"), "."
      ),
      tags$li(class = "list-group-item",
        strong("Choose directories"), br(),
        "Pick a working directory and the .mat folder."
      ),
      tags$li(class = "list-group-item",
        tags$img(src = "home-css/tutorial-images/model-gen/step2.png",
                 class = "img-fluid")
      ),
      # … continua i tuoi step …
      tags$li(class = "list-group-item",
        strong("Generate the hypernode"), br(),
        "Click ", span(class = "badge bg-success", "Generate Model"),
        " to build YAML, Petri-net and FBA files."
      )
    )
  )
}


