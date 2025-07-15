# inst/app/ui.R
library(shiny)
library(shinyjs)

fluidPage(
  useShinyjs(),

  ## ──  Top header  ───────────────────────────────────────────────────
  ## ──  Top header  ───────────────────────────────────────────────────
  tags$div(
    id = "topbar",
    # left: logo, title, and hamburger
    tags$div(class = "topbar-left",
      tags$img(src = "Logo_QBio.png", height = 50, style = "margin-right:10px;"),
      tags$span("epimodFBA", style = "margin-right:15px;"),
      actionButton(
        inputId = "btn_toggle_sidebar",
        label   = NULL,
        icon    = icon("bars"),
        class   = "btn-toggle-sidebar",
        style   = "background: none; border: none; color: inherit;"
      )
    ),
    # right: GitHub
    tags$div(class = "topbar-right",
      tags$a(
        href    = "https://github.com/LorenzoChiabrando/epimodFBAfunctions_GUI.git",
        target  = "_blank",
        icon("github"),
        title   = "GitHub"
      )
    )
  ),


  ## ──  Sidebar + pages  ──────────────────────────────────────────────
	navlistPanel(
		id     = "main_nav",
		widths = c(2,10),
		well   = FALSE,
		fluid  = TRUE,

		tabPanel(
		  title = tagList(icon("home"), "Home"),
		  homeUI("home")
		),

		tabPanel(
		  title = tagList(icon("cogs"), "Model Generation"),
		  modelGenUI("mg")
		),

		tabPanel(
		  title = tagList(icon("play-circle"), "Simulation"),
		  simulationUI("sim")
		),

		tabPanel(
		  title = tagList(icon("chart-line"), "Data Visualization"),
		  dataVisUI("dv")
		),

		tabPanel(
		  title = tagList(icon("flask"), "minMicrobiome"),
		  minMicrobiomeUI("mm")
		)
	),

    tags$head(
			tags$link(rel="stylesheet", type = "text/css", href="reset.css"),
			tags$link(rel="stylesheet", type = "text/css", href="layout.css"),
			tags$link(rel="stylesheet", type = "text/css", href="model-gen//topbar.css"),
			tags$link(rel="stylesheet", type = "text/css", href="model-gen//sidebar.css"),
			
			# model gen CSS
			tags$link(rel="stylesheet", type = "text/css", href="model-gen/card.css"),
			tags$link(rel="stylesheet", type = "text/css", href="model-gen//modelgen.css"),
			tags$link(rel="stylesheet", type = "text/css", href="model-gen//model-list.css"),
			
			# home-only CSS
			tags$link(rel="stylesheet", type="text/css", href="home-css/home.css"),

			# simulation CSS
			tags$link(rel="stylesheet", type="text/css", href="simulation-css/simulation.css"),
			tags$link(rel="stylesheet", type="text/css", href="simulation-css/models.css"),

			# data vis CSS
			tags$link(rel="stylesheet", type="text/css", href="data-vis/data-vis.css"),

			tags$script(src = "modal-init.js"),
			# inline JS: toggle a class on <body> when the hamburger is clicked
			tags$script(HTML("
				$(document).on('click', '#btn_toggle_sidebar', function(){
				  $('body').toggleClass('sidebar-collapsed');
				});
			"))

  	)
)

