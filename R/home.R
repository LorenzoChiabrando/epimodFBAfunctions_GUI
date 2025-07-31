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
      go_sim = tutorialSimAnalysis(),
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


tutorialSimAnalysis <- function(){
		div(
			h2("Model Analysis – Quick Tutorial", class = "mb-4"),

			tags$ol(class = "list-group list-group-numbered",

				tags$li(class = "list-group-item",
				  strong("Step 1: Hypernode Directory Selection"), br(),
				  "Open the ", em("Model Analysis"), " tab and select the directory associated with the previously generated ",
				  code("hypernode"), " to commence the simulation analysis."
				),

				tags$li(class = "list-group-item",
				  tags$img(
				    src = "home-css/tutorial-images/analysis/tutorial_analysis_1.png",
				    class = "tutorial-image"
				  ),
				),
				
				tags$li(class = "list-group-item",
				  tags$img(
				    src = "home-css/tutorial-images/analysis/tutorial_analysis_2.png",
				    class = "tutorial-image"
				  ),
				  p("Image showing the selection of a specific hypernode previously generated from the Model Generation tab.”")
				),

				# Step 2: Working Environment Selection
				tags$li(class = "list-group-item",
					strong("Step 2: Working Environment Selection"), br(),
					"Within the ", em("Model Analysis"), " tab you may choose from multiple working environments, each of which preserves its state across future modifications. You can either:",
					tags$ul(
						tags$li("Select an existing environment to continue analysis using its current configuration."),
						tags$li("Create a new environment by specifying a unique name; this will initialize a fresh workspace with the model’s base settings.")
					)
				),

				# Image: selecting an existing environment
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_3.png",
						class = "tutorial-image"
					),
					p("Interface displaying the selection of an existing working environment for the chosen hypernode.")
				),

				# Image: creating a new environment
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_4.png",
						class = "tutorial-image"
					),
					p("Dialog for creating a new working environment: enter a name to initialize with default model settings.")
				),
				# Step 3: Workspace Overview
				tags$li(class = "list-group-item",
					strong("Step 3: Workspace Overview"), br(),
					"Once the working environment is loaded, the recap section appears, displaying the environment name, a red reset button to return to environment selection, and two action buttons:",
					tags$ul(
						tags$li(code("Save Configuration"), 
								    "– export the current parameter set for reuse in other environments (e.g. saving a high-glucose diet profile)."),
						tags$li(code("Load Configuration"), 
								    "– import a previously saved parameter set into the current environment.")
					)
				),

				# Image: recap section with Save/Load controls
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_5.png",
						class = "tutorial-image"
					),
					p("Recap panel showing the working environment name, red Reset button, and Save/Load configuration buttons.")
				),

				# Image: Load Configuration dialog
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_13.png",
						class = "tutorial-image"
					),
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_14.png",
						class = "tutorial-image"
					),
					p("Dialog for selecting and loading a saved configuration into the active working environment.")
				),

				# Step 4: Model Configuration Section
				tags$li(class = "list-group-item",
					strong("Step 4: Model Configuration Section"), br(),
					"In this section, you may reconfigure the parameters originally defined during model generation. ",
					"Please note that modifying any of ", code("bioMin"), ", ", code("bioMean"), ", or ", code("bioMax"),
					" will necessitate model regeneration to apply these changes."
				),

				# Image 1: overview of the Model Configuration section
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_6.png",
						class = "tutorial-image"
					),
					p("Overview of the Model Configuration section, where all models loaded for this hypernode are listed.")
				),

				# Image 2: internal dialog – Parameters panel
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_7.png",
						class = "tutorial-image"
					),
					p("Internal dialog – Parameters panel: adjust ", code("bioMin"), ", ", code("bioMean"), " and ", code("bioMax"), ".")
				),

				# Image 3: reaction bounds modification panel
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_8.png",
						class = "tutorial-image"
					),
					p("Panel for configuring reaction bounds, applicable to both projected and non-projected reactions.")
				),

				# Image 4: warning on parameter modification
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_12.png",
						class = "tutorial-image"
					),
					p("Warning displayed when attempting to run the analysis after modifying biomass parameters, indicating that model regeneration is required.")
				),
				# Step 5: Simulation Parameters and Boundary Metabolite Configuration
				tags$li(class = "list-group-item",
					strong("Step 5: Simulation Parameters Configuration"), br(),
					"In this section, define global simulation parameters such as time steps, solver settings, and integration options. ",
					"Any changes made here will automatically propagate to the individual model panels, updating the ", code("_r projected"), " reaction bounds accordingly."
				),

				# Image 1: Simulation parameters panel
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_9.png",
						class = "tutorial-image"
					),
					p("Panel for configuring global simulation settings, including time resolution, solver tolerance, and integration method.")
				),

				tags$li(class = "list-group-item",
					strong("Boundary Metabolite Concentration Configuration"), br(),
					"Specify default concentrations for boundary metabolites used in the simulation. ",
					"Modifying these values will also update the corresponding ", code("_r projected"), " bounds in each individual model’s settings."
				),

				# Image 2: Boundary metabolite concentration panel
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_10.png",
						class = "tutorial-image"
					),
					p("Interface for setting default concentrations of boundary metabolites; changes here adjust the projected reaction bounds in all models.")
				),
				
				# Step 6: System-Level Parameter Adjustment
				tags$li(class = "list-group-item",
					strong("Step 6: System-Level Parameter Adjustment"), br(),
					"Here you may modify global bounds for non-specified reactions and, in particular, adjust the system ", code("volume"), " and ", code("cell density"), ". ",
					span(strong("Warning:"), 
							 " changing either volume or cell density will necessitate full model regeneration to incorporate these system-level updates.")
				),

				# Image 1: System parameters panel overview
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_15.png",
						class = "tutorial-image"
					),
					p("Overview of the System-Level Parameters section, showing adjustable bounds for undefined reactions alongside volume and cell density controls.")
				),

				# Image 2: Volume modification with regeneration warning
				tags$li(class = "list-group-item",
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_11.png",
						class = "tutorial-image"
					),
					tags$img(
						src = "home-css/tutorial-images/analysis/tutorial_analysis_12.png",
						class = "tutorial-image"
					),
					p("Dialog illustrating modification of the system volume and the prominent warning indicating that model regeneration is required.")
				),
				# Step 7: Running the Simulation
				tags$li(class = "list-group-item",
					strong("Step 7: Running the Simulation"), br(),
					"Click the ", span(class = "badge bg-primary", "Run Simulation"), " button to execute the analysis. ",
					"If you have modified any sensitive parameters (e.g. biomass constraints, boundary concentrations, volume, or cell density), ",
					"you will be prompted to confirm model regeneration before the simulation can proceed."
				),
				# Note on post-run configuration
				tags$li(class = "list-group-item",
					em("Note:"), " Reopening the configuration panels after the run will display the exact parameter set used in the most recent execution."
				)
			)
		)


}

