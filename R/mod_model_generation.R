# ---------------------------------------------------------------------
# Model-Generation module:  UI  ---------------------------------------
# ---------------------------------------------------------------------
#' @export
modelGenUI <- function(id) {
  ns <- NS(id)
	tags$head(
		tags$script(HTML("
		  /* Chiudi TUTTI i modali di Bootstrap */
		  Shiny.addCustomMessageHandler('closeAllModals', function(msg){
		    $('.modal').modal('hide');          // chiude i modali
		    $('.modal-backdrop').remove();      // rimuove overlay rimasti
		  });
		"))
	)
  tabPanel(
    title = "Model Generation",
					
			div(class = "sim-card",
				div(class = "logo-hero-banner",
					tags$img(src = "Logo_QBio.png", alt = "Logo", class = "hero-logo"),
					tags$h1("Model Generation", class = "hero-title")
				),
				uiOutput(ns("top_card")),
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

		close_all_modals <- function() session$sendCustomMessage("closeAllModals", list())

		project_root <- getOption("epimodFBAfunctionsGUI.user_proj",
				                      normalizePath("~"))
		roots <- c(Home = "~", Project = project_root)

		shinyFiles::shinyDirChoose(
			input  = input,
			id     = "work_dir",
			roots  = roots,
			session = session,
			defaultRoot = "Project",
			defaultPath = ""
		)
		shinyFiles::shinyDirChoose(
			input  = input,
			id     = "mat_dir",
			roots  = roots,
			session = session,
			defaultRoot = "Project",
			defaultPath = ""
		)

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

		conf_name <- reactiveVal(NULL)

		empty_cfg <- function(fp) {
			base  <- tools::file_path_sans_ext(basename(fp))
			parts <- unlist(strsplit(base, "_"))
			# take the first character of each part, collapse and lower-case
			abbr  <- tolower(paste0(substr(parts, 1, 1), collapse = ""))
			list(
				file_path     = fp,
				model_name    = base,
				label         = abbr,            
				biomass       = list(max = NA, mean = NA, min = NA),
				population    = list(starv = NA, dup = NA, death = NA),
				initial_count = NA,
				mu_max        = NA
			)
		}

    # ---- 2) reactive state for downstream modules -------------------
    unit_cfgs  <- shiny::reactiveVal(list())
    current    <- shiny::reactiveVal(1)
    meta_cache <- shiny::reactiveVal(list())


		# ── lookup: label  → id modello  --------------------------------------------
		label2id <- reactive({
			cfgs <- unit_cfgs()
			if (!length(cfgs)) return(setNames(character(), character()))
			labs <- vapply(cfgs, `[[`, character(1), "label")
			ids  <- vapply(cfgs, `[[`, character(1), "model_name")
			# NB: assicurati che le label siano univoche
			if (any(duplicated(labs)))
				warning("Duplicated model labels detected! Adjust labels to be unique.")
			setNames(ids, labs)        # names = label, value = id
		})


		global_cfg <- shiny::reactiveVal(list(
			biomass                    = list(max = 1, mean = 1, min = 0),
			population                 = list(starv = 0, dup = 1, death = 0),
			volume                     = 0.001,
			cell_density               = 1e10,
			initial_count              = 1e6,
			projected_lower_bound      = 1000,
			projected_upper_bound      = 1000,
			not_projected_lower_bound  = 1000,
			not_projected_upper_bound  = 1000
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

		  # ── μ-max override: default to 1 if none set ─────────────────────────
		  c$mu_max <- ifelse(is.na(c$mu_max), 1, c$mu_max)

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

 ## ------------------------------------------------------------------
## Scrive i valori di biomass max / mean / min alle righe 5-6-7
## di ogni <label>_model.txt presente in biounits/
## ------------------------------------------------------------------
update_biounit_biomass <- function(biounits_dir, cfgs) {

  lapply(cfgs, function(cg) {

    ##  1) localizza il file dal label (es. "acd1_model.txt")
    lab   <- cg$label
    fpath <- list.files(
      biounits_dir,
      pattern   = paste0("^", lab, "_model\\.txt$"),
      recursive = TRUE, full.names = TRUE
    )

    if (length(fpath) != 1) {
      warning("⚠️  file .txt non trovato o duplicato per label: ", lab)
      return(invisible())
    }

    ##  2) rimpiazza le righe 5-6-7
    ln <- readLines(fpath, warn = FALSE)
    if (length(ln) < 7) {
      warning("⚠️  ", basename(fpath), " ha meno di 7 righe, salto.")
      return(invisible())
    }
    ln[5] <- as.character(cg$biomass$max  %||% "")
    ln[6] <- as.character(cg$biomass$mean %||% "")
    ln[7] <- as.character(cg$biomass$min  %||% "")

    writeLines(ln, fpath)
    message("✓ Aggiornato ", basename(fpath),
            "  →  (", cg$biomass$max, ", ",
                       cg$biomass$mean, ", ",
                       cg$biomass$min, ")")
  })
}
   

		#──────────────────────────────────────────────────────────────────────────────
		#  TOP CARD  (directory picker + hypernode name)  —  UI REATTIVA
		#──────────────────────────────────────────────────────────────────────────────
		output$top_card <- renderUI({

			wd_ok  <- work_valid()
			md_ok  <- mat_valid()
			loaded <- length(unit_cfgs()) > 0
			reset  <- reset_flag(); if (reset) reset_flag(FALSE)

			#──────────────   STATO ➊  (dopo il caricamento modelli)   ──────────────
			if (loaded && !is.null(working_dir()) && !is.null(matfile_dir())) {

				wd_lbl  <- basename(working_dir()  %||% "")
				mat_lbl <- basename(matfile_dir()  %||% "")
				hyper_nodename <- basename(isolate(hypernode_name()) %||% "")

				div(class = "sim-section-card directory",
				  h4(icon("folder-open"), "Selected Directories", class = "sim-section-title"),

				  div(class = "selected-dir d-flex align-items-center mb-2",
				    strong("Working:", class = "me-2 text-secondary"),
				    span(wd_lbl,  class = "badge bg-primary fs-6")
				  ),
				  div(class = "selected-dir d-flex align-items-center",
				    strong("MAT:", class = "me-2 text-secondary"),
				    span(mat_lbl, class = "badge bg-primary fs-6")
				  ),
				  div(class = "selected-dir d-flex align-items-center",
				    strong("Hypernode:", class = "me-2 text-secondary"),
				    span(hyper_nodename, class = "badge bg-primary fs-6")
				  ),
				  hr(),
				  div(class = "mt-3 text-right",
				    actionButton(ns("btn_reset_dirs"), NULL,
				      icon  = icon("redo"),
				      class = "btn-reset-sim")
				  )
				)
			#──────────────   STATO ➋  (inizializzazione)   ──────────────
			} else {

				div(class = "sim-section-card directory",
					h4(icon("folder-open"), "Initialize Hypernode", class = "sim-section-title"),

					## ——— Hypernode name ---------------------------------------------------
					div(class = "mb-3",
						textInput(
						  ns("hypernode_name"),
						  "Hypernode Name",
						  value       = isolate(hypernode_name()),
						  placeholder = "Enter hypernode name",
						  width       = "40%"
						)
					),

					## ①  Working / MAT picker & reset
					div(class = "d-flex flex-wrap align-items-end gap-4 mt-3",

						# Working dir
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

						# MAT dir
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

						# Reset ↺
						if (wd_ok || md_ok)
						  div(class = "mt-3 text-right",
						    actionButton(
						      ns("btn_reset_dirs"),
						      NULL,
						      icon  = icon("redo"),
						      class = "btn-reset-sim align-self-center"
						    )
						  ),

						# ②  Load-Models  (abilitato se nome + cartelle ok)
						if (wd_ok && md_ok)
						  div(class = "mt-3",
						    actionButton(
						      ns("btn_step1"), "Load Models",
						      icon  = icon("play"),
						      class = "btn btn-success px-4 align-self-center"
						    )
						  )
					)
				)
			}
		})
		

		# ── 3a) validate Working directory ----------------------------------------
		observeEvent(input$work_dir, ignoreInit = TRUE, {
			# NB: shinyFiles invia più eventi durante la navigazione;  
			#     ci interessano solo quelli in cui l’utente ha cliccato “Select”.
			req(is.list(input$work_dir))             # scarta NULL
			p <- shinyFiles::parseDirPath(roots, input$work_dir)
			req(length(p) == 1, nzchar(p))           # scarta eventi intermedi ""

			if (dir.exists(p)) {
				working_dir(p)
				work_valid(TRUE)
			} else {
				showNotification(
				  "La cartella di lavoro selezionata non esiste.",
				  type = "error", duration = 4
				)
				work_valid(FALSE)
			}
		})

		# ── 3b) validate MAT directory -------------------------------------------
		observeEvent(input$mat_dir, ignoreInit = TRUE, {
			req(is.list(input$mat_dir))
			p <- shinyFiles::parseDirPath(roots, input$mat_dir)
			req(length(p) == 1, nzchar(p))           # scarta eventi intermedi

			if (!dir.exists(p)) {
				showNotification(
				  "La cartella MAT selezionata non esiste.",
				  type = "error", duration = 4
				)
				mat_valid(FALSE)
				return()
			}

			if (length(list.files(p, pattern = "\\.mat$", ignore.case = TRUE)) == 0) {
				showModal(
				  modalDialog(
				    title     = "Nessun file .mat trovato",
				    "La cartella scelta non contiene file .mat validi.",
				    easyClose = TRUE, footer = modalButton("OK")
				  )
				)
				mat_valid(FALSE)
				return()
			}

			# tutto ok
			matfile_dir(p)
			mat_valid(TRUE)
		})


		# ── 3c) “Reset” button ↺ -----------------------------------------------------
		observeEvent(input$btn_reset_dirs, {
			reset_state()
			reset_flag(TRUE)
		})


		# ------------------------------------------------------------------
		# CLICK «Load Models»  – usa il nome già inserito nel textInput
		# ------------------------------------------------------------------
		observeEvent(input$btn_step1, {

			## nome mancante → errore immediato
			if (is.null(input$hypernode_name) || trimws(input$hypernode_name) == "") {
				showNotification("Please enter a hypernode name first.", type = "error")
				return()
			}
			hypernode_name(trimws(input$hypernode_name))

			## spinner “Loading…”
			showModal(modalDialog(
				title = NULL,
				div(style = "text-align:center;",
				    img(src = "running.png", height = "400px", alt = "Loading…")),
				br(),
				div("Loading models, please wait…",
				    style = "text-align:center; font-weight:bold;"),
				footer = NULL, easyClose = FALSE, size = "l"
			))

			## path preliminari
			wd   <- shinyFiles::parseDirPath(roots, input$work_dir)
			matd <- shinyFiles::parseDirPath(roots, input$mat_dir)
			shiny::req(dir.exists(wd), dir.exists(matd))
			working_dir(wd);  matfile_dir(matd)

			## cartella config/
			cfg_dir <- file.path(working_dir(), "config")
			if (!dir.exists(cfg_dir)) dir.create(cfg_dir, recursive = TRUE)
			cfg_dir_path(cfg_dir)

			## metadata dai .mat
			epimodFBAfunctions::generate_metadata(
				matfile_dir(), overwrite = TRUE,
				progress = function(i, total) shiny::incProgress(1 / total)
			)

			## copia metadata → config/
			paths <- list.files(matfile_dir(), "\\.mat$", full.names = TRUE)
			if (length(paths) == 0) {
				removeModal()
				showNotification("Nessun file .mat trovato", type = "error")
				return()
			}
			model_names <- tools::file_path_sans_ext(basename(paths))
			for (mn in model_names) {
				src  <- file.path(matfile_dir(), mn)
				dest <- file.path(cfg_dir, mn)
				if (!dir.exists(src)) next
				if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
				file.copy(list.files(src, full.names = TRUE, all.files = TRUE),
				          dest, recursive = TRUE, overwrite = TRUE)
				unlink(src, recursive = TRUE, force = TRUE)
			}

			## leggi i CSV
			meta <- lapply(model_names, function(mn) {
				md_dir <- file.path(cfg_dir, mn)
				meta_path <- list.files(md_dir, "^meta(bolite)?s?_?metadata\\.csv$",
				                        ignore.case = TRUE, full.names = TRUE)[1]
				rxn_path  <- list.files(md_dir, "^react.*_metadata\\.csv$",
				                        ignore.case = TRUE, full.names = TRUE)[1]
				bnd_path  <- list.files(md_dir, "^boundary.*\\.csv$",
				                        ignore.case = TRUE, full.names = TRUE)[1]
				if (all(file.exists(c(meta_path, rxn_path, bnd_path)))) {
				  list(
				    meta = readr::read_csv(meta_path, show_col_types = FALSE),
				    rxn  = readr::read_csv(rxn_path,  show_col_types = FALSE),
				    bnd  = readr::read_csv(bnd_path,  show_col_types = FALSE)
				  )
				} else NULL
			})
			names(meta) <- model_names
			meta_cache(meta)
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

			search  <- tolower(trimws(input$bsearch %||% ""))
			sel_lab <- input$model_filter %||% "All"     # ← sono label, non id

			df <- all_boundaries()

			## ----------- FILTRO PER MODELLO -----------------------------------------
			if (!("All" %in% sel_lab)) {

				# converto le label selezionate nei rispettivi id
				sel_id <- unname(label2id()[sel_lab])

				keep <- vapply(df$species, function(spstr) {
				  mods <- trimws(strsplit(spstr, ",")[[1]])   # ← id presenti nel metabolita

				  if (length(sel_id) == 1) {
				    ## metaboliti presenti SOLO in quel modello
				    length(mods) == 1 && identical(mods, sel_id)
				  } else {
				    ## metaboliti condivisi ESATTAMENTE dai modelli scelti
				    length(mods) == length(sel_id) && setequal(mods, sel_id)
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
				                        span(cfgs[[i]]$model_name, class = "model-label")),
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
	hr(),
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
	hr(),
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
	hr(),
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
  ),
  	hr(),
	# ── System Parameters ─────────────────────────────────────────────
	div(class = "modelgen-global__section mb-3",
		h5("System Parameters"),
		fluidRow(
		  column(3,
		    numericInput(
		      ns("global_proj_lb"),  "Projected Lower Bound (mmol/h)",
		      value = isolate(global_cfg()$projected_lower_bound),
		      min   = 0, width = "100%"
		    )
		  ),
		  column(3,
		    numericInput(
		      ns("global_proj_ub"),  "Projected Upper Bound (mmol/h)",
		      value = isolate(global_cfg()$projected_upper_bound),
		      min   = 0, width = "100%"
		    )
		  ),
		  column(3,
		    numericInput(
		      ns("global_nproj_lb"), "Non-projected Lower Bound (mmol/h)",
		      value = isolate(global_cfg()$not_projected_lower_bound),
		      min   = 0, width = "100%"
		    )
		  ),
		  column(3,
		    numericInput(
		      ns("global_nproj_ub"), "Non-projected Upper Bound (mmol/h)",
		      value = isolate(global_cfg()$not_projected_upper_bound),
		      min   = 0, width = "100%"
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
				#div(class = "mt-4",
				#	verbatimTextOutput(ns("cfg_yaml"))
				#)
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

## ---- 7) Respond to clicks → inject modal content ----------------------
observe({
  models <- unit_cfgs()      # per-model configs
  meta   <- meta_cache()     # metadata from CSV

  lapply(seq_along(models), function(i) {
    local({

      my_i    <- i
      cfg_now <- models[[my_i]]

      ## id helpers -------------------------------------------------------
      base_id <- paste0("m", my_i, "_")
      id_raw  <- function(suff) paste0(base_id, suff)

      tbl_meta <- id_raw("tbl_meta")
      tbl_rxn  <- id_raw("tbl_rxn")
      tbl_bnd  <- id_raw("tbl_bnd")
      save_raw <- id_raw("save")

      ## ---------------- SAVE button ------------------------------------
      observeEvent(input[[save_raw]], ignoreInit = TRUE, {
        cfgs <- unit_cfgs()

        ## Biomass
        cfgs[[my_i]]$biomass$max  <- input[[id_raw("bmax")]]
        cfgs[[my_i]]$biomass$mean <- input[[id_raw("bmean")]]
        cfgs[[my_i]]$biomass$min  <- input[[id_raw("bmin")]]

        ## Population
        cfgs[[my_i]]$population$starv <- input[[id_raw("pstarv")]]
        cfgs[[my_i]]$population$dup   <- input[[id_raw("pdup")]]
        cfgs[[my_i]]$population$death <- input[[id_raw("pdeath")]]

        ## Init pop & μ-max
        cfgs[[my_i]]$initial_count <- input[[id_raw("init")]]
        cfgs[[my_i]]$mu_max        <- input[[id_raw("mu_max")]]

        unit_cfgs(cfgs)
        removeModal()
        showNotification(
          paste("Changes saved for", cfgs[[my_i]]$model_name),
          id = "modelSaveToast", type = "message", duration = 2
        )
      })

      ## --------------- OPEN modal on model click -----------------------
      observeEvent(input[[paste0("model_", my_i)]], ignoreInit = TRUE, {
        cache <- meta[[cfg_now$model_name]]
        if (is.null(cache)) return()

        ## copie locali → evitiamo “cache non trovato” in renderDT
        meta_df <- cache$meta
        rxn_df  <- cache$rxn
        bnd_df  <- cache$bnd

        showModal(
          tags$div(
            id = "modelDetailModal",

            ## ---------- INLINE CSS ------------------------------------
            tags$head(
              tags$style(HTML("
                #modelDetailModal .modal-dialog  { max-width:900px; }
                #modelDetailModal .modal-content { background:#f5f5f5; border-radius:8px; overflow:hidden; }
                #modelDetailModal .modal-header,
                #modelDetailModal .modal-footer  { background:#27ae60; color:#fff; border:none; padding:1rem 1.5rem; }
                #modelDetailModal .modal-title   { font-size:2rem; font-weight:700; text-align:center; margin-bottom:.5rem; }
                #modelDetailModal .modal-title::after{ content:''; display:block; width:80px; height:4px; background:#2ecc71; margin:.5rem auto 0; border-radius:2px; }
                #modelDetailModal .modal-body    { padding:1.25rem 1.5rem; color:#333; }

                /* numericInput compatti */
                #modelDetailModal .num-wrap .form-control{ max-width:140px!important; padding:.35rem .5rem;
                                                            border-radius:.35rem; border:1px solid #27ae60; font-size:.9rem; }

                /* layout 2 colonne sopra 600 px */
                @media (min-width:600px){
                  #modelDetailModal .config-row { display:flex; flex-wrap:wrap; gap:1rem; }
                  #modelDetailModal .config-col { flex:1 1 calc(50% - 1rem); }
                }

                /* sub-card look */
                .modelgen-subcard{ background:#f7fff0; border-left:6px solid #27ae60;
                                   border-radius:6px; padding:1rem; }
                .modelgen-subcard+.modelgen-subcard{ margin-top:1.5rem; }
                .modelgen-subcard .subcard-title{ font-size:1.4rem; font-weight:700; text-transform:uppercase;
                                                  margin-bottom:1rem; color:#1A242F; }
                .modelgen-subcard .subcard-title::after{ content:''; display:block; width:60px; height:3px;
                                                         background:#27ae60; margin-top:.5rem; border-radius:2px; }

                /* DataTable header + righe */
                #modelDetailModal table.dataTable thead{ background:#27ae60; color:#fff; }
								/* ---------- Footer buttons --------------------------------------- */
								#modelDetailModal .modal-footer .btn{
									background:#2ecc71;           /* verde un po’ più chiaro per contrasto   */
									border:1px solid #1e8449;     /* tono compatibile con il footer scuro    */
									color:#fff;
									font-weight:600;
									padding:.45rem 1.2rem;
									border-radius:.4rem;
									transition:background .15s, box-shadow .15s;
								}

								#modelDetailModal .modal-footer .btn:hover{
									background:#1e8449;           /* scurisce on-hover */
									border-color:#196f3d;
									box-shadow:0 0 0 3px rgba(30,132,73,.25);
								}

								#modelDetailModal .modal-footer .btn:focus{
									box-shadow:0 0 0 3px rgba(46,204,113,.45);
								}

								#modelDetailModal .modal-footer .btn:active{
									background:#196f3d !important;
									border-color:#145a32 !important;
								}

              "))
            ),

            ## ---------- MODAL UI --------------------------------------
            modalDialog(
              title     = div(style="background:#27ae60;color:#fff;padding:1rem;",
                              paste("Model:", cfg_now$model_name)),
              size      = "l",
              easyClose = FALSE,
              class     = "modal-model modal-model-detail",

              tagList(
                ## ---- CONFIGURATION -----------------------------------
                div(class = "modelgen-config",

                  ## Biomass
                  div(class = "modelgen-subcard mb-3",
                    h5("Biomass data", class = "subcard-title"),
                    div(class = "config-row",
                      div(class="config-col num-wrap",
                          numericInput(ns(id_raw("bmax")),  "Max",  cfg_now$biomass$max,  min = 0)),
                      div(class="config-col num-wrap",
                          numericInput(ns(id_raw("bmean")), "Mean", cfg_now$biomass$mean, min = 0)),
                      div(class="config-col num-wrap",
                          numericInput(ns(id_raw("bmin")),  "Min",  cfg_now$biomass$min,  min = 0))
                    )
                  ),

                  ## Population
                  div(class = "modelgen-subcard mb-3",
                    h5("Population dynamics", class = "subcard-title"),
                    div(class = "config-row",
                      div(class="config-col num-wrap",
                          numericInput(ns(id_raw("pstarv")), "Starvation",  cfg_now$population$starv, min = 0)),
                      div(class="config-col num-wrap",
                          numericInput(ns(id_raw("pdup")),   "Duplication", cfg_now$population$dup,   min = 0)),
                      div(class="config-col num-wrap",
                          numericInput(ns(id_raw("pdeath")), "Death",       cfg_now$population$death, min = 0))
                    )
                  ),

                  ## Initial pop
                  div(class = "modelgen-subcard mb-3",
                    h5("Initial population", class = "subcard-title"),
                    div(class="num-wrap",
                        numericInput(ns(id_raw("init")), "Count", cfg_now$initial_count, min = 0))
                  ),

                  ## μ max
                  div(class = "modelgen-subcard mb-3",
                    h5("μ max", class = "subcard-title"),
                    div(class="num-wrap",
                        numericInput(ns(id_raw("mu_max")), "μ max",
                                     ifelse(is.null(cfg_now$mu_max), 1, cfg_now$mu_max), min = 0))
                  )
                ),

                ## ---- DATA PREVIEW ------------------------------------
								div(class = "modelgen-subcard mb-3 data-preview-card",   #  ← aggiunta classe
									h5("Data Preview", class = "subcard-title"),
									tabsetPanel(type = "tabs",
										tabPanel("Metabolites",          DT::dataTableOutput(ns(tbl_meta))),
										tabPanel("Reactions",            DT::dataTableOutput(ns(tbl_rxn))),
										tabPanel("Boundary metabolites", DT::dataTableOutput(ns(tbl_bnd)))
									)
								)
              ),

              footer = tagList(
                modalButton("Close"),
                actionButton(ns(save_raw), "Save changes",
                             class = "btn-primary modal-save-btn")
              )
            )  # /modalDialog
          )   # /tags$div
        )     # /showModal

        ## ---------- DataTable rendering -------------------------------
        dt_opts <- list(
          pageLength = 10, scrollY = 500, scrollX = TRUE,
          dom = "ftip", className = "stripe hover"
        )

        output[[tbl_meta]] <- DT::renderDataTable(
          DT::datatable(meta_df, options = dt_opts, rownames = FALSE), server = FALSE)

        output[[tbl_rxn]]  <- DT::renderDataTable(
          DT::datatable(rxn_df,  options = dt_opts, rownames = FALSE), server = FALSE)

        output[[tbl_bnd]]  <- DT::renderDataTable(
          DT::datatable(bnd_df,  options = dt_opts, rownames = FALSE), server = FALSE)

        lapply(c(tbl_meta, tbl_rxn, tbl_bnd), function(id)
          outputOptions(output, id, suspendWhenHidden = FALSE))
      })   # /observeEvent click
    })     # /local
  })       # /lapply
})         # /outer observe



		# ---- 8) Boundary‐metabolites (unchanged) -----------------------------
		output$tbl_boundaries <- DT::renderDataTable({
			all_boundaries()
		}, server = FALSE, escape = FALSE,
			 selection = list(mode = "multiple", target = "row"),
			 options = list(
				 dom        = "ftip",
				 pageLength = 10,
				 scrollY    = "500px",
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
			req(
				input$global_b_max,  input$global_b_mean,  input$global_b_min,
				input$global_p_starv, input$global_p_dup,  input$global_p_death,
				input$global_init_count,
				input$global_volume, input$global_cell_density,
				input$global_proj_lb, input$global_proj_ub,
				input$global_nproj_lb, input$global_nproj_ub
			)

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
				initial_count             = input$global_init_count,
				volume                    = input$global_volume,
				cell_density              = input$global_cell_density,
				projected_lower_bound     = input$global_proj_lb,
				projected_upper_bound     = input$global_proj_ub,
				not_projected_lower_bound = input$global_nproj_lb,
				not_projected_upper_bound = input$global_nproj_ub
			))
		})

		# 1) CLICK «Generate Model»  →  chiedi il nome della configurazione
		observeEvent(input$btn_generate, {
			showModal(
				modalDialog(
				  title = "Configuration Name",
				  textInput(
				    ns("modal_conf_name"),
				    "Choose a name for this configuration",
				    placeholder = "e.g., cfg_01"
				  ),
				  footer = tagList(
				    modalButton("Cancel"),
				    actionButton(ns("confirm_confname"), "Generate", class = "btn-primary")
				  ),
				  easyClose = FALSE
				)
			)
		})

		# 2) CLICK «Generate» sul modal → esegui tutto il flusso con debug
		observeEvent(input$confirm_confname, {
			message("🔔 [0] Enter confirm_confname")

			# 2.1 Leggi e salva il nome della configurazione
			cname <- trimws(input$modal_conf_name)
			message(sprintf("🔔 [1] modal_conf_name = '%s'", cname))
			if (cname == "") {
				showNotification("Please enter a valid name.", type = "error")
				message("🔔 [1a] Invalid config name, aborting")
				return()
			}
			conf_name(cname)
			removeModal()
			message("🔔 [2] conf_name set to: ", conf_name())

			# 2.2 Mostra spinner
			showModal(modalDialog(
				title = NULL,
				tagList(
				  div(style="text-align:center;", img(src="running.png", height="400px")),
				  br(),
				  div("Generating YAML and building hypernode…",
				      style="text-align:center; font-weight:bold;")
				),
				footer    = NULL, easyClose = FALSE, size = "l"
			))

			on.exit({
				message("🔔 [X] on.exit → reset_state()")
				reset_state()
			}, add = TRUE)

			tryCatch({
				# 3) Gather inputs + build YAML
				message("🔔 [3] Gather inputs and build YAML")
				bounds   <- selected_bmet()
				eff_cfgs <- effective_cfgs()
				yaml_txt <- build_hypernode_yaml(eff_cfgs, global_cfg(), bounds)

				# 4) Prepare names
				hn         <- hypernode_name()             # "katlin"
				cfg        <- conf_name()                  # "omega"
				tmp_name   <- paste(hn, cfg, sep = "_")    # "katlin_omega"
				nested_dir <- file.path(hn, cfg)           # "katlin/omega"
				message(sprintf("   • hn='%s', cfg='%s', tmp_name='%s'", hn, cfg, tmp_name))

				# 5) Staging: YAML + initial_data in working_dir()/config
				wd <- working_dir()
				message("🔔 [4] write_hypernode_yaml to staging")
				out_paths <- write_hypernode_yaml(
				  yaml_txt  = yaml_txt,
				  out_dir   = wd,
				  hypernode = tmp_name
				)
				config_dir <- fs::path_expand(dirname(out_paths$yaml))
				fs::dir_create(config_dir, recurse = TRUE)
				message("   • staging config_dir =", config_dir)

				# Copy tmp_cfg if different
				tmp_cfg <- cfg_dir_path()
				if (!is.null(tmp_cfg) && dir.exists(tmp_cfg) && fs::path_expand(tmp_cfg) != config_dir) {
				  message("🔔 [5] Copy tmp_cfg → staging")
				  fs::dir_copy(fs::path_expand(tmp_cfg), config_dir, overwrite = TRUE)
				}
				cfg_dir_path(NULL)

				# Write JSON
				message("🔔 [6] writeBoundaryConditionsStatic()")
				g       <- global_cfg()
				bc_json <- fs::path_expand(file.path(config_dir, "boundary_conditions.json"))
				fs::dir_create(dirname(bc_json), recurse = TRUE)
				writeBoundaryConditionsStatic(
				  volume                     = g$volume,
				  cell_density               = g$cell_density,
				  output_json                = bc_json,
				  projected_lower_bound      = g$projected_lower_bound,
				  projected_upper_bound      = g$projected_upper_bound,
				  not_projected_lower_bound  = g$not_projected_lower_bound,
				  not_projected_upper_bound  = g$not_projected_upper_bound
				)

				# 6) build_hypernodeGUI su tmp_name
				message("🔔 [7] Calling build_hypernodeGUI(tmp_name)")
				epimodFBAfunctions::build_hypernodeGUI(
				  hypernode_name           = tmp_name,
				  config_yaml              = out_paths$yaml,
				  boundary_conditions_file = bc_json,
				  initial_data             = out_paths$initial_data,
				  mat_dir                  = config_dir,
				  base_dir                 = wd,
				  overwrite                = TRUE
				)
				message("   • build_hypernodeGUI(tmp_name) done")

				# 7) Sposta la cartella generata in nested_dir
				hyper_tmp            <- file.path(wd, "hypernodes", tmp_name)
				hyper_target_parent  <- file.path(wd, "hypernodes", hn)
				hyper_target         <- file.path(hyper_target_parent, cfg)
				message(sprintf("🔔 [8] Moving '%s' → '%s'", hyper_tmp, hyper_target))

				fs::dir_create(hyper_target_parent, recurse = TRUE)
				fs::file_move(hyper_tmp, hyper_target)  
				


				# 8) Ora hyper_base è la directory annidata
				hyper_base <- hyper_target
				message("   • hyper_base =", hyper_base)

				# 9) Run FBA model generation
				message("🔔 [9] Running model_generation_GUI()")
				petri_net_dir <- file.path(hyper_base, "petri_net")
				src_dir       <- file.path(hyper_base, "src")
				biounits_dir  <- file.path(hyper_base, "biounits")
				gen_dir       <- file.path(hyper_base, "gen")

				net_file   <- list.files(petri_net_dir, "\\.PNPRO$", full.names = TRUE)
				trans_file <- list.files(src_dir,       "\\.cpp$",   full.names = TRUE)[1]
				fba_files  <- list.files(biounits_dir,  "\\.txt$",   full.names = TRUE, recursive = TRUE)
				message(sprintf("   • net_file: %s", net_file))
				message(sprintf("   • trans_file: %s", trans_file))
				message(sprintf("   • fba_files count: %d", length(fba_files)))

				epimodFBAfunctions::model_generation_GUI(
				  net_fname         = net_file,
				  transitions_fname = trans_file,
				  fba_fname         = fba_files,
				  output_dir        = gen_dir
				)
				message("   • model_generation_GUI() completed")

				message("🔔 [10] Snapshot completo in .base alla radice del hypernode")

				# hyper_base = .../hypernodes/<hn>/<cfg>
				hyper_root <- dirname(hyper_base)              # .../hypernodes/<hn>
				hidden_base <- file.path(hyper_root, ".base")  # .../hypernodes/<hn>/.base

				# 1) se esiste già, cancellalo e ricrealo
				if (dir.exists(hidden_base)) fs::dir_delete(hidden_base)
				fs::dir_create(hidden_base)

				# 2) copia QUI la cartella cfg_01 dentro .base
				fs::dir_copy(hyper_base, hidden_base, overwrite = TRUE)
				message("  • copiato ", hyper_base, " → ", hidden_base)

				# 3) rinomina in .base tutti i file che contengono _<conf_name>  in _base
				cfg <- conf_name()  # "cfg_01"
				all_files <- list.files(hidden_base, recursive = TRUE, full.names = TRUE)
				for (f in all_files) {
					dn       <- dirname(f)
					old_name <- basename(f)
					new_name <- gsub(paste0("_", cfg), "_base", old_name, fixed = TRUE)
					if (new_name != old_name) {
						file.rename(f, file.path(dn, new_name))
						message("    • rinominato: ", old_name, " → ", new_name)
					}
				}

				# 4) adesso puoi continuare col cleanup staging …
				if (dir.exists(config_dir)) {
					fs::dir_delete(config_dir)
					message("🔔 [11] staging config_dir rimosso")
				}

				# 11) SUCCESS
				message("🔔 [11] SUCCESS, showing final modal")
				removeModal()
				showModal(modalDialog(
				  title = "🎉 Model Generated!",
				  tagList(
				    div(style="text-align:center;", img(src="success.png", height="400px")),
				    br(),
				    div("Hypernode generated successfully!",
				        style="text-align:center; font-weight:bold;"),
				    br(),
				    HTML(sprintf(
				      "<div style='text-align:center;'>
				         <p>Hypernode at:<br><code>%s</code></p>
				         <p>FBA models in:<br><code>%s</code></p>
				       </div>",
				      hyper_base, gen_dir
				    ))
				  ),
				  easyClose = FALSE,
				  footer    = modalButton("Close"),
				  size      = "l"
				))

			}, error = function(err) {
				message("🔔 [ERROR] in confirm_confname: ", err$message)
				removeModal()
				showModal(modalDialog(
				  title = "❌ Error Generating Hypernode",
				  div(style="text-align:center; color:red;", err$message),
				  easyClose = TRUE,
				  footer    = modalButton("Close"),
				  size      = "l"
				))
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

			## 0) chiudo qualunque modal residuo
		#	close_all_modals(); removeModal()      # fallback per l’ultimo aperto

			## 1) pulizia cartella config/
			cfg <- cfg_dir_path()
			if (!is.null(cfg) && dir.exists(cfg)) unlink(cfg, recursive = TRUE)
			cfg_dir_path(NULL)

			## 2) azzera i reactive
			work_valid(FALSE);  mat_valid(FALSE)
			working_dir(NULL);  matfile_dir(NULL)
			hypernode_name(NULL)
			unit_cfgs(list());  meta_cache(list());  current(1)
			selected_bmet(character())

			## 3) default globali
			defaults <- list(
				biomass                    = list(max = 1, mean = 1, min = 0),
				population                 = list(starv = 0, dup = 1, death = 0),
				volume                     = 0.001,
				cell_density               = 1e10,
				initial_count              = 1e6,
				projected_lower_bound      = 1000,
				projected_upper_bound      = 1000,
				not_projected_lower_bound  = 1000,
				not_projected_upper_bound  = 1000
			)
			global_cfg(defaults)

			## 3-bis) aggiorna anche i campi numerici mostrati a video
			num_ids <- c(
				global_b_max      = 1,  global_b_mean     = 1,  global_b_min  = 0,
				global_p_starv    = 0,  global_p_dup      = 1,  global_p_death= 0,
				global_init_count = 1e6,
				global_volume     = 0.001,  global_cell_density = 1e10,
				global_proj_lb    = 1000, global_proj_ub  = 1000,
				global_nproj_lb   = 1000, global_nproj_ub = 1000
			)
			for (id in names(num_ids))
				updateNumericInput(session, id, value = num_ids[[id]])

			## 4) reset UI filtri / search
			updateTextInput(session,  ns("bsearch"), value = "")
			updateCheckboxGroupInput(session, ns("model_filter"), selected = "All")

			## 5) forza il rebuild del top-card
			reset_flag(TRUE)
		}



		

  }) # /moduleServer
}
