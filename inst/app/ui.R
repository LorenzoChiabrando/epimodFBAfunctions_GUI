# inst/app/ui.R
library(shiny)
library(shinyjs)

fluidPage(
  useShinyjs(),

  ## ──  Top header  ───────────────────────────────────────────────────
  tags$div(
    id    = "topbar",
    tags$img(src = "logoqbio.png",
             height = 50,
             style  = "margin-right:10px;"),
    tags$span("epimodFBA")
  ),

  ## ──  Sidebar + pages  ──────────────────────────────────────────────
  navlistPanel(
    id      = "main_nav",
    widths  = c(2, 10),   # 2 cols for nav, 10 for content
    well    = FALSE,      # we’ll style it ourselves
    fluid   = TRUE,

    ## ──  TAB: Model Generation  ────────────────────────────────────────────
    modelGenUI("mg")
  ),

    tags$head(
			tags$link(rel="stylesheet", type = "text/css", href="reset.css"),
			tags$link(rel="stylesheet", type = "text/css", href="layout.css"),
			tags$link(rel="stylesheet", type = "text/css", href="components/topbar.css"),
			tags$link(rel="stylesheet", type = "text/css", href="components/sidebar.css"),
			tags$link(rel="stylesheet", type = "text/css", href="components/card.css"),
			tags$link(rel="stylesheet", type = "text/css", href="components/modelgen.css"),
			tags$script(src = "modal-init.js")

  	)
)

