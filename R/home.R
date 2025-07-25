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
tutorialModelGen <- function(){
  div(
    h2("Model Generation – Quick Tutorial", class = "mb-4"),

    tags$ol(class = "list-group list-group-numbered",

      tags$li(class = "list-group-item",
        strong("Step 1: Define Hypernode Name"), br(),
        "Open the ", em("Model Generation"), " tab and insert a unique ",
        code("hypernode name"), " for your simulation entity."
      ),
     tags$li(class = "list-group-item",
        tags$img(
          src = "home-css/tutorial-images/model-gen/model_gen_step1.png",
          class = "tutorial-image"
        ),
        p("Interface showing hypernode name and directory configuration.")
      ),

			tags$li(class = "list-group-item",
				strong("Step 2: Configure Working and Model Directories"), br(),
				"Select the base working directory where simulation output files will be stored, and choose the folder containing the metabolic models (in .mat format) to be loaded into the application."
			),

			tags$li(class = "list-group-item",
				tags$img(
					src = "home-css/tutorial-images/model-gen/model_gen_step2.png",
					class = "tutorial-image"
				),
				p("Interface displaying a properly configured hypernode, ready for model generation. Clicking the green load button initializes the configuration environment, while the red reset button clears the selected name and directories to allow reconfiguration.")
			),

			tags$li(class = "list-group-item",
				strong("After Model Loading: Explore the Configuration Interface"), br(),
				"Once the metabolic models are successfully loaded, the configuration interface is organized into three main sections:", br(), br(),
				tags$ul(
					tags$li(
						strong("Model List:"), " allows you to individually select and inspect each loaded model."
					),
					tags$li(
						strong("Global Configuration:"), " defines general parameters for the ", code("hypernode"), ", such as growth settings and population dynamics."
					),
					tags$li(
						strong("Boundary Metabolite Selection:"), " enables the specification of exchange metabolites used to interface with the environment."
					)
				)
			),
			
			tags$li(class = "list-group-item",
				strong("Step 3: Configure Model Reactions"), br(),
				"In this section, all loaded metabolic models are listed. You can ",
				strong("select each model individually"), " to access and edit its ",
				strong("reaction-specific configuration"), " such as bounds, objective functions, or annotations."
			),

			tags$li(class = "list-group-item",
				tags$img(
					src = "home-css/tutorial-images/model-gen/model_gen_models.png",
					class = "tutorial-image"
				),
				p("Interface showing the model list. Clicking on a model opens a detailed editor for inspecting and customizing its reactions.")
			),

			tags$li(class = "list-group-item",
				p("Once a model is selected, additional configuration panels appear. These allow the user to specify ",
					strong("biomass constraints and biological dynamics"),
					" that are unique to each model, providing control over species-specific behavior.")
			),

			tags$li(class = "list-group-item",
				tags$img(
					src = "home-css/tutorial-images/model-gen/model_gen_modal1.png",
					class = "tutorial-image"
				),
				p("Panel for editing ", strong("biomass parameters"), ": maximum (", code("x_B,max"), "), average (", code("x_B,mean"), "), and minimum (", code("x_B,min"), ") biomass per cell.")
			),

			tags$li(class = "list-group-item",
				tags$img(
					src = "home-css/tutorial-images/model-gen/model_gen_modal2.png",
					class = "tutorial-image"
				),
				p("Panel for configuring ", strong("biological dynamics"), " including ",
					code("starvation rate"), ", ", code("duplication rate"), ", ", code("death rate"),
					", initial population at the start of the simulation, and ",
					code("μ_max"), ", the maximum biomass flux allowed for this species (default = 1).")
			),

			tags$li(class = "list-group-item",
				p("If any of these fields are left blank, the application will fall back to the ",
					strong("global configuration"), ". On the other hand, values entered here will ",
					strong("override the global defaults"), " and apply specifically to the selected model.")
			),

			tags$li(class = "list-group-item",
				tags$img(
					src = "home-css/tutorial-images/model-gen/model_gen_modal3.png",
					class = "tutorial-image"
				),
				p("Each model also includes a summary table that displays its main ",
					strong("reactions"), ", ", strong("metabolites"), ", and ", strong("boundary metabolites"), ". ",
					"You can use this panel to ", strong("filter specific elements"), " and quickly identify relevant parts of the metabolic network.")
			),

			tags$li(class = "list-group-item",
				strong("Step 4: Global Configuration for the Hypernode"), br(),
				"In this panel you define ", strong("global defaults"), " that apply to every model in the hypernode. ",
				"Any field left blank in an individual model’s settings will automatically inherit these values."
			),

			tags$li(class = "list-group-item",
				tags$img(
					src = "home-css/tutorial-images/model-gen/global_settings_1.png",
					class = "tutorial-image"
				),
				p("Here you set the per-cell parameters—initial population and biomass constraints (", 
					code("x_B,max"), ", ", code("x_B,mean"), ", ", code("x_B,min"), ")—as well as biological rates: ",
					code("starvation"), ", ", code("duplication"), ", and ", code("death"), 
					". These values serve as defaults for all models unless overridden individually.")
			),

			tags$li(class = "list-group-item",
				tags$img(
					src = "home-css/tutorial-images/model-gen/global_settings_2.png",
					class = "tutorial-image"
				),
				p("This section defines system-level constraints that are fixed at simulation start, including ",
					strong("total volume"), " and ", strong("cell density"), ", plus default concentrations for FBA and non-FBA reactions—exchange bounds for both ", 
					code("_f"), " and ", code("_r"), " types—and for non-projected reactions (applied equally to both ", 
					code("_f"), " and ", code("_r"), "). These settings cannot be modified during the run.")
			),


			 tags$li(class = "list-group-item",
			strong("Step 5: Boundary Metabolite Selection"), br(),
			"Define which exchange metabolites will serve as the hypernode’s interface with the environment. ",
			"Use the search bar to locate specific compounds or browse the complete list of available boundary metabolites."
		),

		tags$li(class = "list-group-item",
			tags$img(
				src = "home-css/tutorial-images/model-gen/boundary_filter.png",
				class = "tutorial-image"
			),
			p("Boundary metabolite panel: displays all exchange metabolites with a search box for quick lookup.")
		),

		tags$li(class = "list-group-item",
			tags$img(
				src = "home-css/tutorial-images/model-gen/boundary_filter_species.png",
				class = "tutorial-image"
			),
			p("Species filter view: when you select one model, only its boundary metabolites are shown; selecting two or more models will display only the metabolites common to all chosen species.")
		),


		tags$li(class = "list-group-item",
			strong("Final Step: Generate Model Files"), br(),
			"Once all parameters and selections are confirmed, click the ",
			span(class = "badge bg-success", "Generate Model"),
			" button to automatically assemble and export the hypernode artifacts. ",
			"This operation produces three core outputs in your working directory:",
			tags$ul(
				tags$li(code("hypernode.yaml"), " – comprehensive configuration manifest"),
				tags$li(code("hypernode.pnpro"), " – Petri Net representation of the hybrid model")
			)
		),

		tags$li(class = "list-group-item",
			tags$img(
				src = "home-css/tutorial-images/model-gen/generate.png",
				class = "tutorial-image"
			),
			p("The ‘Generate Model’ button finalizes and writes all required files for subsequent validation and simulation.")
		)

    )
  )
}


