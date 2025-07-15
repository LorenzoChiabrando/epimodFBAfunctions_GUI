# ---------------------------------------------------------------------
# Model-Generation module:  UI  ---------------------------------------
# ---------------------------------------------------------------------
modelGenUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Model Generation",

    # (1) top card: Browse  ⇄  list of .mat files
    uiOutput(ns("top_card")),

    # (2) accordion + global cards in a scrollable box
    fluidRow(
      column(12,
        style = "max-height: calc(100vh - 200px); overflow-y: auto;",
        uiOutput(ns("unit_editor"))
      )
    )
  )
}

# ---------------------------------------------------------------------
# Model-Generation module  – SERVER
# ---------------------------------------------------------------------
modelGenServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    ns <- session$ns

    # ---- new reactive vals for step 1 inputs -----------------------
    hypernode_name <- shiny::reactiveVal(NULL)
    working_dir    <- shiny::reactiveVal(NULL)
    matfile_dir    <- shiny::reactiveVal(NULL)

    # ---- 1) helper: blank config skeleton ---------------------------
    empty_cfg <- function(fp) {
      base <- tools::file_path_sans_ext(basename(fp))
      list(
        file_path      = fp,
        model_name     = base,
        label          = base,
        biomass        = list(max = 1, mean = 1, min = 0),
        population     = list(starv = 0, dup = 1, death = 0),
        initial_count  = 1e6
      )
    }

    # ---- 2) reactive state for downstream modules -------------------
    unit_cfgs  <- shiny::reactiveVal(list())
    current    <- shiny::reactiveVal(1)
    meta_cache <- shiny::reactiveVal(list())

    global_cfg <- shiny::reactiveVal(list(
      biomass      = list(max = 1, mean = 1, min = 0),
      population   = list(starv = 0, dup = 1, death = 0),
      volume       = 0.001,
      cell_density = 1e10,
      initial_count= 1e6
    ))

    # ---- 3) folder choosers (shinyFiles) ----------------------------
    roots <- c(Home = "~", `R Project` = ".")
    shinyFiles::shinyDirChoose(
      input, id = "work_dir", roots = roots,
      defaultRoot = "R Project", defaultPath = "."
    )
    shinyFiles::shinyDirChoose(
      input, id = "mat_dir", roots = roots,
      defaultRoot = "R Project", defaultPath = "."
    )

		# ---- 4) top‐card UI: step 1 --------------------------------------
		output$top_card <- renderUI({
			# only show step1 when no models loaded yet
			if (length(unit_cfgs()) == 0) {
				# parse current paths
				wd_path <- shinyFiles::parseDirPath(roots, input$work_dir)
				md_path <- shinyFiles::parseDirPath(roots, input$mat_dir)

				div(class = "sim-card directory",

				  # Section title
				  h5(icon("folder-open"), "Step 1 – Initialize Hypernode", class = "sim-section-title"),

				  # Hypernode name input
				  div(class = "mt-3",
				    textInput(
				      ns("hypernode_name"),
				      "Hypernode Name",
				      placeholder = "Enter unique name for this run",
				      width = "100%"
				    )
				  ),

				  # Working Directory selector
				  if (length(wd_path) == 0 || !nzchar(wd_path)) {
				    div(class = "sim-dir-selector mt-4",
				      shinyFiles::shinyDirButton(
				        id    = ns("work_dir"),
				        label = "Browse Working Directory…",
				        title = "Choose folder where outputs will be saved",
				        icon  = icon("folder"),
				        class = "btn-sim-dir"
				      )
				    )
				  } else {
				    div(class = "selected-dir mt-4",
				      strong("Working Dir:"), 
				      span(basename(wd_path), class = "badge bg-primary text-white ms-2")
				    )
				  },

				  # MAT‐file Directory selector
				  if (length(md_path) == 0 || !nzchar(md_path)) {
				    div(class = "sim-dir-selector mt-4",
				      shinyFiles::shinyDirButton(
				        id    = ns("mat_dir"),
				        label = "Browse MAT-file Directory…",
				        title = "Choose folder containing your .mat models",
				        icon  = icon("folder-open"),
				        class = "btn-sim-dir"
				      )
				    )
				  } else {
				    div(class = "selected-dir mt-4",
				      strong("MAT‐file Dir:"), 
				      span(basename(md_path), class = "badge bg-primary text-white ms-2")
				    )
				  },

				  # Load Models button (primary color)
				  div(class = "text-center mt-4",
				    actionButton(
				      ns("btn_step1"),
				      "Load Models",
				      icon  = icon("play"),
				      class = "btn-primary px-5"
				    )
				  )
				)
			} else {
				NULL
			}
		})

    # ---- 5) show selected paths -------------------------------------
    output$work_dir_path <- renderText({
      p <- shinyFiles::parseDirPath(roots, input$work_dir)
      if (length(p)==1 && nzchar(p) && dir.exists(p)) p else ""
    })
    output$mat_dir_path <- renderText({
      p <- shinyFiles::parseDirPath(roots, input$mat_dir)
      if (length(p)==1 && nzchar(p) && dir.exists(p)) p else ""
    })

    # ---- 6) on clicking Load Models, run your old metadata logic ----
observeEvent(input$btn_step1, {
  # 1) store step1 inputs
  hypernode_name(input$hypernode_name)
  wd  <- shinyFiles::parseDirPath(roots, input$work_dir)
  matd <- shinyFiles::parseDirPath(roots, input$mat_dir)
  shiny::req(length(wd)==1, nzchar(wd), dir.exists(wd))
  shiny::req(length(matd)==1, nzchar(matd), dir.exists(matd))
  working_dir(wd)
  matfile_dir(matd)

  # 2) ensure config/ exists under working_dir
  cfg_dir <- file.path(working_dir(), "config")
  if (!dir.exists(cfg_dir)) dir.create(cfg_dir, recursive=TRUE)

  # 3) generate metadata CSVs under sel/<model_name>/
  withProgress(message="Generating metadata…", value=0, {
    epimodFBAfunctions::generate_metadata(
      matfile_dir(), overwrite=TRUE,
      progress=function(i, total) shiny::incProgress(1/total)
    )
  })

  # 4) find all .mat files in sel
  paths <- list.files(matfile_dir(), "\\.mat$", full.names=TRUE)
  if (length(paths)==0) {
    shiny::showNotification("Nessun file .mat trovato", type="error")
    return()
  }

  # 5) per ciascun modello copia cartella metadata → config/ e poi la rimuove
  model_names <- tools::file_path_sans_ext(basename(paths))
  for (mn in model_names) {
    src  <- file.path(matfile_dir(), mn)
    dest <- file.path(cfg_dir, mn)
    if (!dir.exists(src)) next
    if (!dir.exists(dest)) dir.create(dest, recursive=TRUE)

    # copia tutto tranne .mat (che sta in matfile_dir, non nella sottocartella)
    files_to_copy <- list.files(src, full.names=TRUE, all.files=TRUE)
    file.copy(files_to_copy, dest, recursive=TRUE, overwrite=TRUE)

    # elimina la cartella metadata originale
    unlink(src, recursive=TRUE, force=TRUE)
  }

  # 6) leggi metadata da config/
  meta <- list()
  for (mn in model_names) {
    md    <- file.path(cfg_dir, mn)
    files <- c(
      meta = file.path(md, "metabolites_metadata.csv"),
      rxn  = file.path(md, "reactions_metadata.csv"),
      bnd  = file.path(md, "boundary_metabolites.csv")
    )
    if (all(file.exists(files))) {
      meta[[mn]] <- list(
        meta = readr::read_csv(files["meta"], show_col_types=FALSE),
        rxn  = readr::read_csv(files["rxn"],  show_col_types=FALSE),
        bnd  = readr::read_csv(files["bnd"],  show_col_types=FALSE)
      )
    }
  }

  # 7) inizializza reactive values
  meta_cache(meta)
  unit_cfgs(lapply(paths, empty_cfg))
  current(1)
})




    # ---- 5) aggregate ALL boundary metabolites -----------------------
    all_boundaries <- shiny::reactive({
      mc <- meta_cache(); shiny::req(length(mc)>0)
      df_list <- lapply(names(mc), function(mn) {
        bnd <- mc[[mn]]$bnd
        if (nrow(bnd)==0) return(NULL)
        idcol <- colnames(bnd)[1]
        data.frame(
          metabolite_id = as.character(bnd[[idcol]]),
          species       = mn,
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, df_list); df <- unique(df)
      aggregate(species ~ metabolite_id, data = df,
                FUN=function(x) paste(unique(x), collapse=", "))
    })
		# ---- 6) UI: Models + Boundary + Export -------------------------------
		output$unit_editor <- renderUI({
			cfgs <- unit_cfgs()
			mc   <- meta_cache()

			if (length(cfgs) == 0) {
				return(h4(""))
			}

			# Models as plain table with actionLinks
			models_card <- div(class = "card", style = "margin-bottom:20px;",
				h4("Available Models"),
				div(style = "max-height:200px; overflow-y:auto;",
				  tags$table(class = "table table-hover mb-0",
				    tags$thead(tags$tr(tags$th("Model"))),
				    tags$tbody(
				      lapply(seq_along(cfgs), function(i) {
				        tags$tr(
				          tags$td(
				            actionLink(ns(paste0("model_", i)), cfgs[[i]]$label)
				          )
				        )
				      })
				    )
				  )
				)
			)

		# • GLOBAL Community Settings accordion (closed by default)
		 global_settings_card <- tags$details(class = "card mb-4 modelgen-global",
			tags$summary(
				class = "card-header d-flex align-items-center justify-content-between",
				style = "cursor: pointer;",
				span("Global Community Settings"),
				tags$span(
				  class = "fa fa-info-circle",
				  style = "cursor: help; font-size: 1.2em;",
				  title = "Any per-model adjustments made in the individual model modals will override these global settings when generating the YAML."
				)
			),
			tags$div(class = "card-body",
				# Biomass Flux Bounds
				div(class = "modelgen-global__section mb-3",
				  h5("Biomass Flux Bounds"),
				  fluidRow(
				    column(4,
				      numericInput(
				        ns("global_b_max"), "Max",
				        value = isolate(global_cfg()$biomass$max),
				        min = 0
				      )
				    ),
				    column(4,
				      numericInput(
				        ns("global_b_mean"), "Mean",
				        value = isolate(global_cfg()$biomass$mean),
				        min = 0
				      )
				    ),
				    column(4,
				      numericInput(
				        ns("global_b_min"), "Min",
				        value = isolate(global_cfg()$biomass$min),
				        min = 0
				      )
				    )
				  )
				),

				# Population Dynamics
				div(class = "modelgen-global__section mb-3",
				  h5("Population Dynamics"),
				  fluidRow(
				    column(4,
				      numericInput(
				        ns("global_p_starv"), "Starvation",
				        value = isolate(global_cfg()$population$starv),
				        min = 0
				      )
				    ),
				    column(4,
				      numericInput(
				        ns("global_p_dup"), "Duplication",
				        value = isolate(global_cfg()$population$dup),
				        min = 0
				      )
				    ),
				    column(4,
				      numericInput(
				        ns("global_p_death"), "Death",
				        value = isolate(global_cfg()$population$death),
				        min = 0
				      )
				    )
				  )
				),

				# Initial Population
				div(class = "modelgen-global__section mb-3",
				  h5("Initial Population"),
				  fluidRow(
				    column(6,
				      numericInput(
				        ns("global_init_count"), "Initial Count",
				        value = isolate(global_cfg()$initial_count),
				        min = 0
				      )
				    )
				  )
				),

				# Reactor Parameters
				div(class = "modelgen-global__section mb-3",
				  h5("Reactor Parameters"),
				  fluidRow(
				    column(6,
				      numericInput(
				        ns("global_volume"), "Volume [mL]",
				        value = isolate(global_cfg()$volume),
				        min = 0
				      )
				    ),
				    column(6,
				      numericInput(
				        ns("global_cell_density"), "Max Cell Density [cells/mL]",
				        value = isolate(global_cfg()$cell_density),
				        min = 0
				      )
				    )
				  )
				)
			)
		)




			# • Boundary‐metabolites accordion (closed by default)
			boundary_card <- tags$details(class = "card mb-4",
				tags$summary(class = "card-header", style = "cursor: pointer;", "Select Boundary Metabolites"),
				tags$div(class = "card-body",
					DT::dataTableOutput(ns("tbl_boundaries"))
				)
			)


			# Export YAML card
			export_card <- div(class = "card", style = "margin-bottom:20px;",
				h4("Export Configuration"),
				actionButton(ns("btn_generate"), "Generate Model", class = "btn-success"),
				verbatimTextOutput(ns("cfg_yaml"))
			)

			tagList(models_card, global_settings_card, boundary_card, export_card)
		})

		# ---- 7) Respond to clicks → inject modal content ----------------------
		observe({
			cfgs <- unit_cfgs()
			mc   <- meta_cache()

			lapply(seq_along(cfgs), function(i) {
				observeEvent(input[[paste0("model_", i)]], {
				  message("[DEBUG] model_", i, " clicked")

				  cfg   <- cfgs[[i]]
				  cache <- mc[[ cfg$model_name ]]
				  if (is.null(cache)) return()

				  # namespaced modal ID
				  id_modal <- ns(paste0("modal_", i))
				  message("[DEBUG] Preparing modal:", cfg$model_name, "→", id_modal)

				  # Build the body HTML for the Bootstrap modal
				  inner_rendered <- htmltools::renderTags(
				    shiny::tagList(
				      # • CONFIGURATION card
				      shiny::div(class = "card mb-3",
				        shiny::h4("Configuration"),
				        shiny::div(class = "modelgen-config",
				          # – Biomass data
				          shiny::div(class = "modelgen-config__group",
				            shiny::h5("Biomass data"),
				            tags$span(class = "tooltip-icon", "ℹ",
				              title = "Bounds & mean for biomass flux."
				            ),
				            shiny::numericInput(
				              ns(paste0("b_max_", i)), "Max",
				              value = cfg$biomass$max, min = 0
				            ),
				            shiny::numericInput(
				              ns(paste0("b_mean_", i)), "Mean",
				              value = cfg$biomass$mean, min = 0
				            ),
				            shiny::numericInput(
				              ns(paste0("b_min_", i)), "Min",
				              value = cfg$biomass$min, min = 0
				            )
				          ),
				          # – Population dynamics
				          shiny::div(class = "modelgen-config__group",
				            shiny::h5("Population dynamics"),
				            tags$span(class = "tooltip-icon", "ℹ",
				              title = "Starvation, duplication & death rates."
				            ),
				            shiny::numericInput(
				              ns(paste0("p_starv_", i)), "Starvation",
				              value = cfg$population$starv, min = 0
				            ),
				            shiny::numericInput(
				              ns(paste0("p_dup_", i)), "Duplication",
				              value = cfg$population$dup, min = 0
				            ),
				            shiny::numericInput(
				              ns(paste0("p_death_", i)), "Death",
				              value = cfg$population$death, min = 0
				            )
				          ),
				          # – Initial population
				          shiny::div(class = "modelgen-config__group",
				            shiny::h5("Initial population"),
				            tags$span(class = "tooltip-icon", "ℹ",
				              title = "Seed count for simulation."
				            ),
				            shiny::numericInput(
				              ns(paste0("init_", i)), "Count",
				              value = cfg$initial_count, min = 0
				            )
				          )
				        )
				      ),

				      # • DATA PREVIEW card
				      shiny::div(class = "card",
				        shiny::h4("Data Preview"),
				        DT::datatable(
				          cache$meta,
				          options = list(pageLength = 5, scrollY = "200px", scrollX = TRUE),
				          caption = "Metabolites"
				        ),
				        DT::datatable(
				          cache$rxn,
				          options = list(pageLength = 5, scrollY = "200px", scrollX = TRUE),
				          caption = "Reactions"
				        ),
				        DT::datatable(
				          cache$bnd,
				          options = list(pageLength = 5, scrollY = "200px", scrollX = TRUE),
				          caption = "Boundary Metabolites"
				        )
				      )
				    )
				  )
				  body_html <- inner_rendered$html

				  # Send to JS to inject and show the modal
				  message("[DEBUG] Sending initModal for ", id_modal)
				  session$sendCustomMessage(
				    type = "initModal",
				    message = list(
				      id        = id_modal,
				      title     = cfg$label,
				      body_html = body_html
				    )
				  )
				 
				}, ignoreInit = TRUE)
			})
		})

		# ---- 8) Boundary‐metabolites (unchanged) -----------------------------
		output$tbl_boundaries <- DT::renderDataTable({
			all_boundaries()
		}, selection='multiple', options=list(pageLength=10, scrollY="300px", scrollX=TRUE, dom='lrtip'))
		
		# Auto‐sync global_cfg whenever any global input changes
		observe({
			req(input$global_b_max, input$global_b_mean, input$global_b_min,
				  input$global_p_starv, input$global_p_dup,   input$global_p_death,
				  input$global_volume,  input$global_cell_density, input$global_init_count)

			global_cfg(list(
				biomass    = list(
				  max  = input$global_b_max,
				  mean = input$global_b_mean,
				  min  = input$global_b_min
				),
				population = list(
				  starv = input$global_p_starv,
				  dup   = input$global_p_dup,
				  death = input$global_p_death
				),
				volume       = input$global_volume,
				cell_density = input$global_cell_density,
				initial_count =  input$global_init_count
			))
		})

# ---- C‴) YAML gen: live-read per-model inputs with global fallbacks AND write to disk ----
# ---- C‴) YAML gen: live-read per-model inputs + write to disk + FBA gen ----
		observeEvent(input$btn_generate, {
			# 1) gather inputs for YAML
			bounds <- all_boundaries()[ input$tbl_boundaries_rows_selected, "metabolite_id" ]
			inputs <- reactiveValuesToList(input)

			# 2) build the YAML text
			yaml_txt <- tryCatch({
				build_hypernode_yaml(unit_cfgs(), global_cfg(), inputs, bounds)
			}, error = function(e) {
				showModal(modalDialog(title = "Error Generating YAML", e$message, easyClose = TRUE))
				return(NULL)
			})
			if (is.null(yaml_txt)) return()

			# 3) preview YAML in-app
			output$cfg_yaml <- renderText(paste(yaml_txt, collapse = "\n"))

			# 4) write YAML & build hypernode directory
			out_paths  <- write_hypernode_yaml(yaml_txt, working_dir(), hypernode_name())
			config_dir <- dirname(out_paths$yaml)
			bc_json    <- file.path(config_dir, "boundary_conditions.json")
			writeBoundaryConditionsStatic(output_json = bc_json)

			withProgress(message = "Building hypernode…", value = 0, {
				epimodFBAfunctions::build_hypernodeGUI(
				  hypernode_name           = hypernode_name(),
				  config_yaml              = out_paths$yaml,
				  boundary_conditions_file = bc_json,
				  initial_data             = file.path(config_dir, "initial_data.csv"),
				  mat_dir                  = config_dir,
				  base_dir                 = working_dir(),
				  overwrite                = TRUE
				)
				incProgress(1)
			})

			# 5) determine the true hypernode root
			cand1 <- file.path(working_dir(),          hypernode_name())
			cand2 <- file.path(working_dir(), "hypernodes", hypernode_name())
			hyper_base <- if (dir.exists(cand2)) cand2 else cand1

			# ── DEBUG: which one we use
			message("[DEBUG] trying cand1: ", cand1)
			message("[DEBUG] trying cand2: ", cand2)
			message("[DEBUG] using hyper_base: ", hyper_base)

			petri_net_dir <- file.path(hyper_base, "petri_net")
			src_dir       <- file.path(hyper_base, "src")
			biounits_dir  <- file.path(hyper_base, "biounits")
			gen_dir       <- file.path(hyper_base, "gen")

			# ── DEBUG: print subfolders
			message("[DEBUG] petri_net_dir:  ", petri_net_dir)
			message("[DEBUG] src_dir:        ", src_dir)
			message("[DEBUG] biounits_dir:   ", biounits_dir)
			message("[DEBUG] gen_dir:        ", gen_dir)

			# 6) locate files
			net_file   <- list.files(petri_net_dir, pattern = "\\.PNPRO$", full.names = TRUE)
			trans_file <- list.files(src_dir,       pattern = "\\.cpp$",   full.names = TRUE)[1]
			fba_files  <- list.files(biounits_dir,  pattern = "\\.txt$",   full.names = TRUE, recursive = TRUE)

			# ── DEBUG: list found files
			message("[DEBUG] net_file:   ", paste(net_file, collapse = ", "))
			message("[DEBUG] trans_file: ", trans_file)
			message("[DEBUG] fba_files:  ",
				      if (length(fba_files)) paste(head(fba_files, 10), collapse = ", ") else "none")

			# 7) run FBA model generation
			withProgress(message = "Generating FBA models…", value = 0, {
				epimodFBAfunctions::model_generation_GUI(
				  net_fname         = net_file,
				  transitions_fname = trans_file,
				  fba_fname         = fba_files,
				  output_dir        = gen_dir
				)
				incProgress(1)
			})
			showNotification("Model generation complete", type = "message")

			# 8) cleanup config folder if desired
			if (fs::dir_exists(config_dir)) fs::dir_delete(config_dir)

			# 9) final success modal
			showModal(modalDialog(
				title   = "🎉 Model Generated!",
				HTML(paste0(
				  "<p>The hypernode has been created at:<br><code>", hyper_base, "</code></p>",
				  "<p>FBA models are in:<br><code>", gen_dir, "</code></p>"
				)),
				easyClose = FALSE,
				footer    = modalButton("Close")
			))

			# 10) reset module back to step 1
			hypernode_name(NULL)
			working_dir(NULL)
			matfile_dir(NULL)
			unit_cfgs(list())
			meta_cache(list())
			current(1)
		}, ignoreInit = TRUE)



			
		observe({
			n <- length(unit_cfgs())
			lapply(seq_len(n), function(i) {
				# Biomass overrides
				lapply(c("b_max", "b_mean", "b_min"), function(pref) {
				  observeEvent(input[[paste0(pref, "_", i)]], {
				    tmp <- unit_cfgs()
				    val <- input[[paste0(pref, "_", i)]]
				    slot <- switch(pref,
				      b_max  = "max",
				      b_mean = "mean",
				      b_min  = "min"
				    )
				    tmp[[i]]$biomass[[slot]] <- val
				    unit_cfgs(tmp)
				  }, ignoreInit = TRUE)
				})
				# Population overrides
				lapply(c("p_starv", "p_dup", "p_death"), function(pref) {
				  observeEvent(input[[paste0(pref, "_", i)]], {
				    tmp <- unit_cfgs()
				    val <- input[[paste0(pref, "_", i)]]
				    slot <- sub("p_", "", pref)
				    tmp[[i]]$population[[slot]] <- val
				    unit_cfgs(tmp)
				  }, ignoreInit = TRUE)
				})
				# Initial count override
				observeEvent(input[[paste0("init_", i)]], {
				  tmp <- unit_cfgs()
				  tmp[[i]]$initial_count <- input[[paste0("init_", i)]]
				  unit_cfgs(tmp)
				}, ignoreInit = TRUE)
			})
		})

  }) # /moduleServer
}

