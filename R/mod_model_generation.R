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
    hypernode_name <- shiny::reactiveVal(NULL)
    working_dir    <- shiny::reactiveVal(NULL)


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

    # ---- 2) reactive state ------------------------------------------
    unit_cfgs  <- shiny::reactiveVal(list())
    current    <- shiny::reactiveVal(1)
    meta_cache <- shiny::reactiveVal(list())
				
		# ---- A) Global settings state ------------------------------------
		global_cfg <- shiny::reactiveVal(list(
			biomass      = list(max = 1,   mean = 1,   min = 0),
			population   = list(starv = 0, dup = 1, death = 0),
			volume       = 0.001,
			cell_density = 1e10,
			initial_count  = 1e6
		))


    # ---- 3) folder chooser (shinyFiles) ------------------------------
    roots <- c(home = "~", wd = ".")
    
    shinyFiles::shinyDirChoose(
      input, id = "mat_dir", roots = roots,
      defaultRoot = "wd", defaultPath = "."
    )
    selected_dir <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$mat_dir, {
      sel <- shinyFiles::parseDirPath(roots, input$mat_dir)
      if (length(sel)==0 || !dir.exists(sel)) return()

      selected_dir(sel)
      shiny::withProgress(message="Generating metadata…", value=0, {
        epimodFBAfunctions::generate_metadata(
          sel, overwrite=TRUE,
          progress=function(i,total) shiny::incProgress(1/total)
        )
      })

      paths <- list.files(sel, "\\.mat$", full.names=TRUE)
      if (length(paths)==0) {
        shiny::showNotification("No .mat files found in selected folder", type="error")
        return()
      }

      meta <- list()
      for (p in paths) {
        mn <- tools::file_path_sans_ext(basename(p))
        md <- file.path(sel, mn)
        files <- c(
          meta = file.path(md, "metabolites_metadata.csv"),
          rxn  = file.path(md, "reactions_metadata.csv"),
          bnd  = file.path(md, "boundary_metabolites.csv")
        )
        miss <- names(files)[!file.exists(files)]
        if (length(miss)>0) next

        meta[[mn]] <- list(
          meta = readr::read_csv(files["meta"], show_col_types=FALSE),
          rxn  = readr::read_csv(files["rxn"],  show_col_types=FALSE),
          bnd  = readr::read_csv(files["bnd"],  show_col_types=FALSE)
        )
      }
      meta_cache(meta)
      unit_cfgs(lapply(paths, empty_cfg))
      current(1)
    })

		# ---- 4) top card: folder chooser only ----------------------------
		output$top_card <- shiny::renderUI({
			cfgs <- unit_cfgs()
			if (length(cfgs) == 0) {
				shiny::div(class = "card",
				  shiny::h4("1) Select folder with MAT models"),
				  shinyFiles::shinyDirButton(
				    ns("mat_dir"), "Browse folder", "Choose a directory",
				    icon = shiny::icon("folder-open")
				  )
				)
			} else {
				NULL
			}
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
				return(h4("Choose a folder with .mat files to begin."))
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
		global_settings_card <- tags$details(class = "card mb-4",
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
				actionButton(ns("btn_generate"), "Generate YAML"),
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

		# ---- C‴) YAML gen: live-read per-model inputs with global fallbacks ----
		observeEvent(input$btn_generate, {
			# gather inputs
			bounds <- all_boundaries()[ input$tbl_boundaries_rows_selected, "metabolite_id" ]
			inputs <- reactiveValuesToList(input)

			# try to build; capacity‐overrun will throw
			yaml_txt <- tryCatch({
				build_hypernode_yaml(
				  unit_cfgs(), 
				  global_cfg(), 
				  inputs, 
				  bounds
				)
			}, error = function(e) {
				showModal(modalDialog(
				  title = "Error Generating YAML",
				  e$message, easyClose = TRUE
				))
				return(NULL)
			})
			if (!is.null(yaml_txt)) {
				output$cfg_yaml <- renderText(yaml_txt)
			}
		})


			
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

