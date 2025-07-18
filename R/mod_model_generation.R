# ---------------------------------------------------------------------
# Model-Generation module:  UI  ---------------------------------------
# ---------------------------------------------------------------------
#' @export
modelGenUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Model Generation",
    
    div(class = "sim-card",
      # (1) top card: Browse  ⇄  name  ⇄  reset
      uiOutput(ns("top_card")),

      # (2) accordion + global cards in a scrollable box
			uiOutput(ns("unit_editor"))
    )

  )
}


# ---------------------------------------------------------------------
# Model-Generation module  – SERVER
# ---------------------------------------------------------------------
#' @export
modelGenServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    ns <- session$ns

    # ---- new reactive vals for step 1 inputs -----------------------
    hypernode_name <- shiny::reactiveVal(NULL)

		observeEvent(input$hypernode_name, {
			hypernode_name(input$hypernode_name)
		})

    working_dir    <- shiny::reactiveVal(NULL)
    matfile_dir    <- shiny::reactiveVal(NULL)
		cfg_dir_path <- reactiveVal(NULL)   # ← NEW: store temp config/ path
    
		# ---- new reactive vals for folder pickers -----------------------------------
		work_valid  <- reactiveVal(FALSE)   # working-dir chosen & exists
		mat_valid   <- reactiveVal(FALSE)   # MAT-dir   chosen & exists
		reset_flag  <- reactiveVal(FALSE)   # forces UI to return to “choose” state

		selected_bmet <- reactiveVal(character())   # holds chosen metabolite IDs


    # ---- 1) helper: blank config skeleton ---------------------------
		empty_cfg <- function(fp) {
			base <- tools::file_path_sans_ext(basename(fp))
			list(
				file_path      = fp,
				model_name     = base,
				label          = base,
				biomass        = list(max = NA, mean = NA, min = NA),
				population     = list(starv = NA, dup = NA, death = NA),
				initial_count  = NA
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
    
	# ── 2-bis)  valore “effettivo” di ogni modello  ─────────────────────
	#           (override per-model  +  fallback ai global)
	effective_cfgs <- reactive({
		g   <- global_cfg()
		cfg <- unit_cfgs()

		lapply(cfg, function(c) {

		  # ─ Biomassa ────────────────────────────────────────────────────
		  c$biomass$max  <- ifelse(is.na(c$biomass$max),  g$biomass$max,  c$biomass$max)
		  c$biomass$mean <- ifelse(is.na(c$biomass$mean), g$biomass$mean, c$biomass$mean)
		  c$biomass$min  <- ifelse(is.na(c$biomass$min),  g$biomass$min,  c$biomass$min)

		  # ─ Dinamica della popolazione ─────────────────────────────────
		  c$population$starv <- ifelse(is.na(c$population$starv), g$population$starv, c$population$starv)
		  c$population$dup   <- ifelse(is.na(c$population$dup),   g$population$dup,   c$population$dup)
		  c$population$death <- ifelse(is.na(c$population$death), g$population$death, c$population$death)

		  # ─ Conteggio iniziale ─────────────────────────────────────────
		  c$initial_count <- ifelse(is.na(c$initial_count), g$initial_count, c$initial_count)

		  c
		})
	})
## ── DEBUG live: stampa ogni volta che cambiano -----------------------
observe({
  message("\n[DBG] global_cfg() ➜ ",
          jsonlite::toJSON(global_cfg(), pretty = TRUE, auto_unbox = TRUE))
})

observe({
  message("\n[DBG] unit_cfgs() (override) ➜ ",
          jsonlite::toJSON(unit_cfgs(), pretty = TRUE, auto_unbox = TRUE))
})

observe({
  message("\n[DBG] effective_cfgs() (merge) ➜ ",
          jsonlite::toJSON(effective_cfgs(), pretty = TRUE, auto_unbox = TRUE))
})

    
		project_root <- getOption("epimodFBAfunctionsGUI.user_proj",
				                      normalizePath("~"))
		                        
		                        
    # ---- 3) folder choosers (shinyFiles) ----------------------------
    roots <- c(Home = "~", Project = project_root)
    shinyFiles::shinyDirChoose(
      input, id = "work_dir", roots = roots,
      defaultRoot = "Project", defaultPath = "."
    )
    shinyFiles::shinyDirChoose(
      input, id = "mat_dir", roots = roots,
      defaultRoot = "Project", defaultPath = "."
    )

		# ── top card: choose / show directories ──────────────────────────────────────
		output$top_card <- renderUI({
			wd_ok  <- work_valid()
			md_ok  <- mat_valid()
			loaded <- length(unit_cfgs()) > 0
			reset  <- reset_flag(); if (reset) reset_flag(FALSE)

			#─────────────────────────  STATE ➊
			if (loaded && !is.null(working_dir()) && !is.null(matfile_dir())) {

				wd_lbl  <- basename(working_dir() %||% "")
				mat_lbl <- basename(matfile_dir() %||% "")
				hyper_nodename <- basename(hypernode_name() %||% "")

				div(class = "sim-section-card directory",
				  h4(icon("folder-open"), "Selected Directories", class = "sim-section-title"),

				  div(class = "selected-dir d-flex align-items-center mb-2",
				      strong("Working:", class = "me-2 text-secondary"),
				      span(wd_lbl, class = "badge bg-primary fs-6")
				  ),
				  div(class = "selected-dir d-flex align-items-center",
				      strong("MAT:", class = "me-2 text-secondary"),
				      span(mat_lbl, class = "badge bg-primary fs-6")
				  ),
				  div(class = "selected-dir d-flex align-items-center",
				      strong("Hypernode:", class = "me-2 text-secondary"),
				      span(hyper_nodename, class = "badge bg-primary fs-6")
				  ),
				  div(class = "mt-3 text-right",
				      actionButton(ns("btn_reset_dirs"), NULL,
				                   icon = icon("redo"),
				                   class = "btn-reset-sim")
				  )
				)

			} else {
				div(class = "sim-section-card directory",
				  h4(icon("folder-open"), "Initialize Hypernode", class = "sim-section-title"),

				  # Hypernode name
				  div(class = "mb-4",
						textInput(
							ns("hypernode_name"),
							"Hypernode Name",
							value       = hypernode_name(),
							placeholder = "Enter unique name for this run",
							width       = "40%"
						)

				  ),

				  # Pickers / badges row
				  div(class = "d-flex flex-wrap align-items-end gap-4 mt-4",

				    ## Working-dir control
				    if (wd_ok)
				      div(class = "selected-dir flex-grow-1",
				        strong("Working:", class = "me-2 text-secondary"),
				        span(basename(working_dir()), class = "badge bg-primary fs-6")
				      )
				    else
				      shinyFiles::shinyDirButton(
				        id    = ns("work_dir"),
				        label = "Working dir…",
				        title = "Choose working directory",
				        icon  = icon("folder-open"),
				        class = "btn btn-primary shinyDirButton flex-grow-1"
				      ),

				    ## MAT-dir control
				    if (md_ok)
				      div(class = "selected-dir flex-grow-1",
				        strong("MAT:", class = "me-2 text-secondary"),
				        span(basename(matfile_dir()), class = "badge bg-primary fs-6")
				      )
				    else
				      shinyFiles::shinyDirButton(
				        id    = ns("mat_dir"),
				        label = "MAT dir…",
				        title = "Choose MAT-file directory",
				        icon  = icon("folder-open"),
				        class = "btn btn-primary shinyDirButton flex-grow-1"
				      ),
						div(),
				    ## Load Models (left) – only when both dirs valid
				    if (wd_ok && md_ok)
				      div(class = "mt-3",
				        actionButton(
				          ns("btn_step1"), "Load Models",
				          icon  = icon("play"),
				          class = "btn btn-success px-4 align-self-center"
				        )
				      ),

				    ## Reset (right) – when any dir chosen
				    if (wd_ok || md_ok)
				      div(class = "mt-3 text-right",
				        actionButton(
				          ns("btn_reset_dirs"), NULL,
				          icon  = icon("redo"),
				          class = "btn-reset-sim align-self-center"
				        )
				      )
				  )
				)
			}
		})



		# ── 3a) validate Working directory ------------------------------------------
		observeEvent(input$work_dir, {
			p <- shinyFiles::parseDirPath(roots, input$work_dir)

			## nothing picked yet
			if (!length(p) || !nzchar(p)) return()

			if (dir.exists(p)) {
				working_dir(p)
				work_valid(TRUE)
			} else {
				work_valid(FALSE)
				shiny::showNotification(
				  "The selected working directory is not valid.",
				  type = "error", duration = 4
				)
			}
		})

		# ── 3b) validate MAT directory (must contain at least one *.mat) -------------
		observeEvent(input$mat_dir, {
			p <- shinyFiles::parseDirPath(roots, input$mat_dir)

			## nothing picked yet
			if (!length(p) || !nzchar(p)) return()

			if (!dir.exists(p)) {
				mat_valid(FALSE)
				shiny::showNotification(
				  "The selected MAT-file directory does not exist.",
				  type = "error", duration = 4
				)
				return()
			}

			has_mat <- length(list.files(p, pattern = "\\.mat$", ignore.case = TRUE)) > 0
			if (!has_mat) {
				mat_valid(FALSE)

				showModal(
					modalDialog(
						title     = "Error: No .mat Files Found",
						tagList(
						  div(style="text-align:center;",
						      img(src="error.png", height="400px", alt="Error")
						  ),
						  br(),
						  div("The selected folder contains no .mat files. Please choose a different directory.",
						      style="text-align:center; color:red;")
						),
						easyClose = TRUE,
						footer    = modalButton("OK"),
						size = "l"
					)
				)

				return()
			}


			## everything is fine
			matfile_dir(p)
			mat_valid(TRUE)
		})

		# ── 3c) “Reset” button ↺ -----------------------------------------------------
		observeEvent(input$btn_reset_dirs, {
			reset_state()
			reset_flag(TRUE)
		})


    # ---- 6) on clicking Load Models, run your old metadata logic ----
		observeEvent(input$btn_step1, {
		
			# ── NEW: ensure a hypernode name is provided ─────────────────────────────

			# 2) Hypernode name missing
			if (is.null(input$hypernode_name) ||
					trimws(input$hypernode_name) == "") {
				showModal(
					modalDialog(
						title = "Hypernode Name Required",
						tagList(
						  div(style="text-align:center;",
						      img(src="error.png", height="400px", alt="Error")
						  ),
						  br(),
						  div(
						    "Please enter a hypernode name before loading models.",
						    style="text-align:center; color:red; font-weight:bold;"
						  )
						),
						easyClose = TRUE,
						footer    = modalButton("OK"),
						size      = "l"
					)
				)
				return()   # abort handler
			}
				
			# --- 0) Mostra modal “Running…” e blocca tutto
			showModal(
				modalDialog(
					title    = NULL,
					tagList(
						# here’s the image, centered…
						div(
						  style = "text-align:center;",
						  img(src = "running.png", height = "400px", alt = "Loading…")
						),
						br(),
						div("Loading models, please wait…", style="text-align:center; font-weight:bold;")
					),
					footer   = NULL,
					easyClose= FALSE,
					size     = "l"
				)
			)

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
			if (!dir.exists(cfg_dir)) dir.create(cfg_dir, recursive = TRUE)

			cfg_dir_path(cfg_dir)          # ← add this


			# 3) generate metadata CSVs under sel/<model_name>/
			epimodFBAfunctions::generate_metadata(
				matfile_dir(), overwrite=TRUE,
				progress=function(i, total) shiny::incProgress(1/total)
			)
			

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
				message("[DBG] Contenuto di ", dest, ":")
 				message(paste(list.files(dest), collapse = ", "))
				if (!dir.exists(src)) next
				if (!dir.exists(dest)) dir.create(dest, recursive=TRUE)

				# copia tutto tranne .mat (che sta in matfile_dir, non nella sottocartella)
				files_to_copy <- list.files(src, full.names=TRUE, all.files=TRUE)
				file.copy(files_to_copy, dest, recursive=TRUE, overwrite=TRUE)

				# elimina la cartella metadata originale
				unlink(src, recursive=TRUE, force=TRUE)
			}


			# 6) read metadata from config/  ───────────────────────────────────────
			## 6) read metadata from config/ ----------------------------------
			meta <- lapply(model_names, function(mn) {
				md_dir <- file.path(cfg_dir, mn)

				meta_path <- list.files(md_dir,
						                    pattern = "^meta(bolite)?s?_?metadata\\.csv$",
						                    ignore.case = TRUE, full.names = TRUE)[1]
				rxn_path  <- list.files(md_dir,
						                    pattern = "^react.*_metadata\\.csv$",
						                    ignore.case = TRUE, full.names = TRUE)[1]
				bnd_path  <- list.files(md_dir,
						                    pattern = "^boundary.*\\.csv$",
						                    ignore.case = TRUE, full.names = TRUE)[1]

				if (all(file.exists(c(meta_path, rxn_path, bnd_path)))) {
					list(
						meta = readr::read_csv(meta_path, show_col_types = FALSE),
						rxn  = readr::read_csv(rxn_path,  show_col_types = FALSE),
						bnd  = readr::read_csv(bnd_path,  show_col_types = FALSE)
					)
				} else {
					message("[WARN] Metadati mancanti per ", mn)
					NULL
				}
			})

			names(meta) <- model_names               # ← **crucial** map by bare model name

			meta_cache(meta)                         # store for downstream use
			observeEvent(meta_cache(), {
				lapply(names(meta_cache()), function(mn) {
					mc <- meta_cache()[[mn]]
					if (is.null(mc)) return()
					message(sprintf("[DBG] %s: meta=%d rxn=%d bnd=%d",
						              mn, nrow(mc$meta), nrow(mc$rxn), nrow(mc$bnd)))
  				})
			})


			unit_cfgs(lapply(paths, empty_cfg))
    	removeModal()
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
				
		# render scrollable list of boundary metabolites
		output$bmeta_list <- renderUI({
			req(all_boundaries())

			search   <- tolower(trimws(input$bsearch %||% ""))
			sel_mod  <- input$model_filter %||% "All"

			df <- all_boundaries()

			## ── NEW FILTER LOGIC ───────────────────────────────────────────────
			if (!("All" %in% sel_mod)) {
				keep <- vapply(df$species, function(spstr){
				  mods <- trimws(strsplit(spstr, ",")[[1]])

				  if (length(sel_mod) == 1) {
				    ## unique metabolites of one model
				    length(mods) == 1 && identical(mods, sel_mod)
				  } else {
				    ## metabolites shared by exactly the selected models
				    length(mods) == length(sel_mod) && setequal(mods, sel_mod)
				  }
				}, logical(1))

				df <- df[keep, , drop = FALSE]
			}
			## -------------------------------------------------------------------

			## search filter (unchanged)
			if (nzchar(search))
				df <- df[grepl(search, tolower(df$metabolite_id), fixed = TRUE), , drop = FALSE]


			if (nrow(df) == 0) {
				return( div(class = "no-results text-muted ps-2",
				            icon("info-circle"), " No boundary metabolites found.") )
			}

			## build list items (unchanged except right-aligned species string)
			items <- lapply(seq_len(nrow(df)), function(i){
				id   <- df$metabolite_id[i]
				sp   <- gsub(",\\s*", " / ", df$species[i])
				sel  <- id %in% selected_bmet()

				cls  <- if (sel) "list-group-item list-group-item-action active"
				        else     "list-group-item list-group-item-action"
				icon <- if (sel) icon("check") else icon("circle")

				actionLink(
				  ns(paste0("bm_", digest::digest(id))), href = "#",
				  class = cls, `data-id` = id,
				  onclick = sprintf(
				    "Shiny.setInputValue('%s', this.getAttribute('data-id'), {priority:'event'}); return false;",
				    ns('bm_toggle')
				  ),
				  tagList(
				    icon,
				    span(id, class = "ms-2 fw-bold"),
				    span(sp,  class = "species-info ms-auto")   # pushed to right
				  )
				)
			})

			div(class = "bounds-list list-group", items)
		})


		
		observeEvent(input$bm_toggle, {
			id  <- input$bm_toggle
			sel <- selected_bmet()

			if (id %in% sel)
				sel <- setdiff(sel, id)  # unselect
			else
				sel <- c(sel, id)        # select

			selected_bmet(sel)
		})
		
		## ---------------------------------------------------------------
		## Filter logic for “All” vs. individual model check-boxes
		## ---------------------------------------------------------------
		prev_sel <- reactiveVal("All")    # remember previous state

		observeEvent(input$model_filter, ignoreInit = TRUE, {
			sel       <- input$model_filter %||% character(0)
			prev_now  <- prev_sel()         # what was selected last time?

			## 1)  Nothing selected  → re-enable All
			if (length(sel) == 0) {
				updateCheckboxGroupInput(session, "model_filter", selected = "All")
				prev_sel("All")
				return()
			}

			## 2)  Both “All” AND other model(s) are now checked
			if ("All" %in% sel && length(sel) > 1) {

				# 2-a  If we previously had ONLY “All”, the user just ticked a model
				if (identical(prev_now, "All")) {
				  new_sel <- setdiff(sel, "All")          # keep the model(s), drop All

				# 2-b  Otherwise the user just ticked “All” while some models were on
				} else {
				  new_sel <- "All"                        # keep All, drop the models
				}

				updateCheckboxGroupInput(session, "model_filter", selected = new_sel)
				prev_sel(new_sel)
				return()
			}

			## 3)  All good (only All, or only specific model(s))
			prev_sel(sel)       # update memory and do nothing else
		})






		# ---- 6) UI: Models + Boundary + Export -------------------------------
		output$unit_editor <- renderUI({
			cfgs <- unit_cfgs()

			## Show nothing until at least one model is loaded
			if (length(cfgs) == 0) return(NULL)

			# ── MODELS card (simulation style) ────────────────────────────────────────
			models_card <- div(
				id    = ns("mg_models_section"),
				class = "sim-section-card models",

				h5(icon("cubes"), "Models", class = "sim-section-title"),

				div(
				  id    = ns("mg_model_list"),
				  class = "sim-model-list list-group",
				  lapply(seq_along(cfgs), function(i) {
				    actionLink(
				      inputId = ns(paste0("model_", i)),
				      label   = tagList(icon("cube"),
				                        span(cfgs[[i]]$label, class = "model-label")),
				      class   = "list-group-item list-group-item-action model-link"
				    )
				  })
				)
			)

		# • GLOBAL Community Settings accordion (closed by default)
# • GLOBAL Community Settings  – ALWAYS VISIBLE -------------------------------
global_settings_card <- div(
  id    = ns("mg_global_section"),
  class = "sim-section-card config",   # stesso wrapper delle altre
  # Header
  h5(icon("globe"), " Global Community Settings", class = "sim-section-title"),

  # ---- Biomass Flux Bounds --------------------------------------------------
  div(class = "modelgen-global__section mb-3",
    h5("Biomass Flux Bounds"),
    fluidRow(
      column(4,
        numericInput(
          ns("global_b_max"), "Max",
          value = isolate(global_cfg()$biomass$max),
          min = 0, width = "100%"
        )
      ),
      column(4,
        numericInput(
          ns("global_b_mean"), "Mean",
          value = isolate(global_cfg()$biomass$mean),
          min = 0, width = "100%"
        )
      ),
      column(4,
        numericInput(
          ns("global_b_min"), "Min",
          value = isolate(global_cfg()$biomass$min),
          min = 0, width = "100%"
        )
      )
    )
  ),

  # ---- Population Dynamics --------------------------------------------------
  div(class = "modelgen-global__section mb-3",
    h5("Population Dynamics"),
    fluidRow(
      column(4,
        numericInput(
          ns("global_p_starv"), "Starvation",
          value = isolate(global_cfg()$population$starv),
          min = 0, width = "100%"
        )
      ),
      column(4,
        numericInput(
          ns("global_p_dup"), "Duplication",
          value = isolate(global_cfg()$population$dup),
          min = 0, width = "100%"
        )
      ),
      column(4,
        numericInput(
          ns("global_p_death"), "Death",
          value = isolate(global_cfg()$population$death),
          min = 0, width = "100%"
        )
      )
    )
  ),

  # ---- Initial Population ---------------------------------------------------
  div(class = "modelgen-global__section mb-3",
    h5("Initial Population"),
    fluidRow(
      column(6,
        numericInput(
          ns("global_init_count"), "Initial Count",
          value = isolate(global_cfg()$initial_count),
          min = 0, width = "100%"
        )
      )
    )
  ),

  # ---- Reactor Parameters ----------------------------------------------------
  div(class = "modelgen-global__section mb-3",
    h5("Reactor Parameters"),
    fluidRow(
      column(6,
        numericInput(
          ns("global_volume"), "Volume [mL]",
          value = isolate(global_cfg()$volume),
          min = 0, width = "100%"
        )
      ),
      column(6,
        numericInput(
          ns("global_cell_density"), "Max Cell Density [cells/mL]",
          value = isolate(global_cfg()$cell_density),
          min = 0, width = "100%"
        )
      )
    )
  )
)

		
		# ── BOUNDARY-METABOLITES card (purple) ──────────────────────────────
		boundary_card <- if (length(cfgs) > 0) div(
			id    = ns("mg_boundaries_section"),
			class = "sim-section-card boundaries",

			h5(icon("vial"), "Boundary Metabolites", class = "sim-section-title"),

			## search bar
			div(class = "search-holder mb-3",
				textInput(
				  ns("bsearch"), NULL,
				  placeholder = "Search metabolite…",
				  width = "40%"
				)
			),

			## model-filter checkboxes (rendered below)
			uiOutput(ns("model_filter_ui"), class = "mb-3"),

			## scroll list
			uiOutput(ns("bmeta_list"))
		)


				# Export YAML card
			export_card <- div(
				id    = ns("mg_export_section"),
				class = "sim-section-card directory",   # stesso wrapper / CSS
				# Titolo sezione
				h5(icon("file-export"), " Export Configuration", class = "sim-section-title"),

				# Corpo: un pulsante grande, centrato
				div(class = "text-center mt-3",
					actionButton(
						ns("btn_generate"),
						label  = tagList(icon("play"), "Generate Model"),
						class  = "btn-success px-5 btn-run-sim"   # riuso la tua classe “grande”
					)
				),

				# Preview YAML (mostrato solo dopo la generazione)
				div(class = "mt-4",
					verbatimTextOutput(ns("cfg_yaml"))
				)
			)


			tagList(
				div(style = "margin-bottom:60px;", models_card),
				div(style = "margin-bottom:60px;", global_settings_card),
				div(style = "margin-bottom:60px;", boundary_card),
				div(style = "margin-bottom:60px;", export_card)
			)

		})


		# dynamic checkbox group:  All + each model name
		output$model_filter_ui <- renderUI({
			models <- unit_cfgs();  req(length(models) > 0)
			choices <- c("All", vapply(models, `[[`, character(1), "label"))

			div(class = "bound-filter",
				checkboxGroupInput(
				  ns("model_filter"), NULL,
				  choices   = choices,
				  selected  = "All",
				  inline    = TRUE
				)
			)
		})


		# ---- 7) Respond to clicks → inject modal content ----------------------
		observe({
			models <- unit_cfgs()      # list() con le cfg dei singoli modelli
			meta   <- meta_cache()     # metadati letti da CSV

			## crea un modal per ogni modello caricato ---------------------------
			lapply(seq_along(models), function(i) {

				local({

				  my_i    <- i                               # congela l’indice
				  cfg_now <- models[[my_i]]                  # cfg corrente

				  base_id <- paste0("m", my_i, "_")          # prefisso unico
				  id_raw  <- function(suff) paste0(base_id, suff)

				  ## ID “plain” da usare con input$ / output$ / proxy
				  tbl_meta <- id_raw("tbl_meta")
				  tbl_rxn  <- id_raw("tbl_rxn")
				  tbl_bnd  <- id_raw("tbl_bnd")
				  save_raw <- id_raw("save")
				  
				  

				    ## ---------- SAVE button --------------------------------------
				    observeEvent(input[[save_raw]], ignoreInit = TRUE, {

				      cfgs <- unit_cfgs()          # copia dello stato globale

				      cfgs[[my_i]]$biomass$max  <- input[[id_raw("bmax")]]
				      cfgs[[my_i]]$biomass$mean <- input[[id_raw("bmean")]]
				      cfgs[[my_i]]$biomass$min  <- input[[id_raw("bmin")]]

				      cfgs[[my_i]]$population$starv <- input[[id_raw("pstarv")]]
				      cfgs[[my_i]]$population$dup   <- input[[id_raw("pdup")]]
				      cfgs[[my_i]]$population$death <- input[[id_raw("pdeath")]]

				      cfgs[[my_i]]$initial_count    <- input[[id_raw("init")]]

				      unit_cfgs(cfgs)              # salva nel reactiveVal

				      removeModal()
							showNotification(
								paste("Changes saved for", cfgs[[my_i]]$label),
								id       = "modelSaveToast",   # id unico e fisso
								type     = "message",
								duration = 2
							)
				    })      

				  ## ascolta il click sul link del modello -------------------------
				  observeEvent(input[[paste0("model_", my_i)]], ignoreInit = TRUE, {

				    cache <- meta[[cfg_now$model_name]]
				    if (is.null(cache)) return()

				    ## ---------- MODAL --------------------------------------------
				    showModal(
				      modalDialog(
				        title     = paste("Model:", cfg_now$label),
				        size      = "l",
				        easyClose = FALSE,
				        class     = "modal-model",

				        tagList(
				          div(class = "card mb-3",
				            h4("Configuration", class = "modal-section-title"),
				            div(class = "modelgen-config",
				              div(class = "modelgen-config__group",
				                h5("Biomass data"),
				                numericInput(ns(id_raw("bmax")),  "Max",
				                             value = cfg_now$biomass$max,  min = 0),
				                numericInput(ns(id_raw("bmean")), "Mean",
				                             value = cfg_now$biomass$mean, min = 0),
				                numericInput(ns(id_raw("bmin")),  "Min",
				                             value = cfg_now$biomass$min,  min = 0)
				              ),
				              div(class = "modelgen-config__group",
				                h5("Population dynamics"),
				                numericInput(ns(id_raw("pstarv")), "Starvation",
				                             value = cfg_now$population$starv, min = 0),
				                numericInput(ns(id_raw("pdup")),   "Duplication",
				                             value = cfg_now$population$dup,  min = 0),
				                numericInput(ns(id_raw("pdeath")), "Death",
				                             value = cfg_now$population$death, min = 0)
				              ),
				              div(class = "modelgen-config__group",
				                h5("Initial population"),
				                numericInput(ns(id_raw("init")),  "Count",
				                             value = cfg_now$initial_count, min = 0)
				              )
				            )
				          ),

				          div(class = "card",
				            h4("Data preview", class = "modal-section-title"),
				            tabsetPanel(type = "tabs",
				              tabPanel("Metabolites",
				                       DT::dataTableOutput(ns(tbl_meta))),
				              tabPanel("Reactions",
				                       DT::dataTableOutput(ns(tbl_rxn))),
				              tabPanel("Boundary metabolites",
				                       DT::dataTableOutput(ns(tbl_bnd)))
				            )
				          )
				        ),

				        footer = tagList(
				          modalButton("Close"),
				          actionButton(ns(save_raw), "Save changes",
				                       class = "btn-primary modal-save-btn")
				        )
				      )
				    )

				    ## ---------- rendering DT -------------------------------------
				    opts <- list(pageLength = 10, scrollY = 300, scrollX = TRUE,
				                 dom = "ftip", className = "stripe hover")

				    output[[tbl_meta]] <- DT::renderDataTable(
				      DT::datatable(cache$meta, options = opts, rownames = FALSE),
				      server = FALSE
				    )
				    output[[tbl_rxn]]  <- DT::renderDataTable(
				      DT::datatable(cache$rxn,  options = opts, rownames = FALSE),
				      server = FALSE
				    )
				    output[[tbl_bnd]]  <- DT::renderDataTable(
				      DT::datatable(cache$bnd,  options = opts, rownames = FALSE),
				      server = FALSE
				    )

				    lapply(c(tbl_meta, tbl_rxn, tbl_bnd), function(id)
				      outputOptions(output, id, suspendWhenHidden = FALSE))

				  })  # /observeEvent model click
				})    # /local
			})      # /lapply
		})        # /outer observe







		# ---- 8) Boundary‐metabolites (unchanged) -----------------------------
		output$tbl_boundaries <- DT::renderDataTable({
			all_boundaries()
		}, server = FALSE, escape = FALSE,
			 selection = list(mode = "multiple", target = "row"),
			 options = list(
				 dom        = "ftip",
				 pageLength = 10,
				 scrollY    = "300px",
				 scrollX    = TRUE,
				 className  = "stripe hover",
				 columnDefs = list(
				   list(targets = 0, searchable = FALSE, orderable = FALSE,
				        className = "select-checkbox", defaultContent = "")
				 ),
				 order = list(list(1, "asc"))
			 ),
			 callback = DT::JS("       // <- qualify
				 table.on('select.dt deselect.dt', function() {
				   table.rows().every(function() {
				     $(this.node()).toggleClass('row-selected', this.isSelected());
				   });
				 });
			 ")
		)


		
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
		# ---- C‴) YAML gen: live-read per-model inputs with global fallbacks AND write to disk ----
		observeEvent(input$btn_generate, {

			# 0) Show blocking “spinner” modal
			showModal(
				modalDialog(
				  title    = NULL,
				  tagList(
				    div(style = "text-align:center;",
				        img(src = "running.png", height = "400px", alt = "Loading…")
				    ),
				    br(),
				    div("Generating YAML and building hypernode…",
				        style="text-align:center; font-weight:bold;")
				  ),
				  footer    = NULL,    # no buttons
				  easyClose = FALSE,   # cannot close by ESC or click outside
				  size      = "l"
				)
			)

			# 1) Wrap everything in tryCatch to cleanly handle errors
			tryCatch({

				# • 1) gather inputs for YAML
				bounds   <- selected_bmet()
				eff_cfgs <- effective_cfgs()       # ← aplica subito i fallback

				# • 2) build the YAML text
				yaml_txt <- build_hypernode_yaml(eff_cfgs, global_cfg(), bounds)

				# • 3) preview YAML in-app
				output$cfg_yaml <- renderText(paste(yaml_txt, collapse = "\n"))

				# • 4) write YAML & build hypernode directory
				out_paths  <- write_hypernode_yaml(yaml_txt, working_dir(), hypernode_name())
				config_dir <- dirname(out_paths$yaml)
				bc_json    <- file.path(config_dir, "boundary_conditions.json")
				vol  <- global_cfg()$volume
				dens <- global_cfg()$cell_density

				writeBoundaryConditionsStatic(
					volume       = vol,
					cell_density = dens,
					output_json  = bc_json  
				)


				  epimodFBAfunctions::build_hypernodeGUI(
				    hypernode_name           = hypernode_name(),
				    config_yaml              = out_paths$yaml,
				    boundary_conditions_file = bc_json,
				    initial_data             = file.path(config_dir, "initial_data.csv"),
				    mat_dir                  = config_dir,
				    base_dir                 = working_dir(),
				    overwrite                = TRUE
				  )

				# • 5) determine the true hypernode root
				cand1 <- file.path(working_dir(),          hypernode_name())
				cand2 <- file.path(working_dir(), "hypernodes", hypernode_name())
				hyper_base <- if (dir.exists(cand2)) cand2 else cand1

				# • 6) locate files
				petri_net_dir <- file.path(hyper_base, "petri_net")
				src_dir       <- file.path(hyper_base, "src")
				biounits_dir  <- file.path(hyper_base, "biounits")
				gen_dir       <- file.path(hyper_base, "gen")

				# • 7) run FBA model generation
				net_file   <- list.files(petri_net_dir, pattern = "\\.PNPRO$", full.names = TRUE)
				trans_file <- list.files(src_dir,       pattern = "\\.cpp$",   full.names = TRUE)[1]
				fba_files  <- list.files(biounits_dir,  pattern = "\\.txt$",   full.names = TRUE, recursive = TRUE)


				  epimodFBAfunctions::model_generation_GUI(
				    net_fname         = net_file,
				    transitions_fname = trans_file,
				    fba_fname         = fba_files,
				    output_dir        = gen_dir
				  )


				# • 8) cleanup config folder if desired
				if (fs::dir_exists(config_dir)) fs::dir_delete(config_dir)

				# 9) SUCCESS: remove spinner and show final modal
				removeModal()
				showModal(
					modalDialog(
						title   = "🎉 Model Generated!",
						tagList(
							# Immagine di successo centrata
							div(style = "text-align:center;",
								  img(src    = "success.png",
								      height = "400px",
								      alt    = "Success")
							),
							br(),
							# Messaggio principale
							div("Hypernode generated successfully!", 
								  style="text-align:center; font-weight:bold; font-size:1.1em;"),
							br(),
							# Dettagli sul percorso
							HTML(paste0(
								"<div style='text-align:center; margin-top:10px;'>",
								  "<p>The hypernode has been created at:<br>",
								     "<code>", hyper_base, "</code></p>",
								  "<p>FBA models are in:<br>",
								     "<code>", gen_dir, "</code></p>",
								"</div>"
							))
						),
						easyClose = FALSE,
						footer    = modalButton("Close"),
						size      = "l"
					)
				)
				reset_state()

				# 10) reset module back to step 1
				hypernode_name(NULL)
				working_dir(NULL)
				matfile_dir(NULL)
				unit_cfgs(list())
				meta_cache(list())
				current(1)

			}, error = function(err) {
				# ERROR: remove spinner and show error modal with image
				removeModal()
				showModal(
				  modalDialog(
				    title = "❌ Error Generating Hypernode",
				    tagList(
				      div(style="text-align:center;",
				          img(src="error.png", height="400px", alt="Error")
				      ),
				      br(),
				      div(paste("An error occurred:", err$message),
				          style="text-align:center; color:red;")
				    ),
				    easyClose = TRUE,
				    footer = modalButton("Close"),
				    size      = "l"
				  )
				)
				reset_state()
			})

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
		
		reset_state <- function() {
			# 1) remove temp config dir
			cfg <- cfg_dir_path()
			if (!is.null(cfg) && dir.exists(cfg)) unlink(cfg, recursive = TRUE)
			cfg_dir_path(NULL)

			# 2) reset reactive values
			work_valid(FALSE);  mat_valid(FALSE)
			working_dir(NULL);  matfile_dir(NULL)
			hypernode_name(NULL)
			unit_cfgs(list());  meta_cache(list());  current(1)

			# 3) RESET GLOBAL CFG ← valori di default
			global_cfg(list(
				biomass      = list(max = 1, mean = 1, min = 0),
				population   = list(starv = 0, dup = 1, death = 0),
				volume       = 0.001,
				cell_density = 1e10,
				initial_count= 1e6
			))

			# 4) RESET BOUNDARY SELECTION
			selected_bmet(character())

			# 5) forza la UI: svuota search + riporta checkbox “All”
			updateTextInput(session, ns("bsearch"), value = "")
			updateCheckboxGroupInput(session, ns("model_filter"), selected = "All")

			# 6) flag di reset per ricostruire il top_card
			reset_flag(TRUE)
		}

		

  }) # /moduleServer
}
