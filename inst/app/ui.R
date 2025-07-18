# inst/app/ui.R
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)

dashboardPage(
  ## ── HEADER ───────────────────────────────────────────
  dashboardHeader(
    titleWidth = 300,
    title = tagList(
      tags$img(src = "Logo_QBio.png", height = "40px"),
      span(
        "epimodFBA",
        style = "margin-left:8px; font-weight:bold; font-size:22px; line-height:40px;"
      )
    ),
    # GitHub icon
    tags$li(
      class = "dropdown",
      tags$a(
        href   = "https://github.com/LorenzoChiabrando/epimodFBAfunctions_GUI.git",
        icon("github"), target = "_blank", title = "GitHub",
        style = "color: #fff; padding: 15px;"
      )
    )
  ),

  ## ── SIDEBAR ──────────────────────────────────────────
  dashboardSidebar(
    width = 500,
    sidebarMenu(id = "main_nav",
      menuItem("Home",             tabName = "home",           icon = icon("home")),
      menuItem("Model Generation", tabName = "mg",             icon = icon("cogs")),
      menuItem("Simulation",       icon    = icon("play-circle"),
        menuSubItem("Analysis",    tabName = "sim_analysis"),
        menuSubItem("Sensitivity", tabName = "sim_sensitivity"),
        menuSubItem("Calibration", tabName = "sim_calibration")
      ),
      menuItem("Data Visualization", tabName = "dv", icon = icon("chart-line"),
        menuSubItem("Analysis",    tabName = "dv"),
        menuSubItem("Sensitivity", tabName = "dv_sensitivity"),
        menuSubItem("Calibration", tabName = "dv_calibration")
      ),
      menuItem("minMicrobiome",       tabName = "mm", icon = icon("flask"))
    )
  ),

  ## ── BODY ─────────────────────────────────────────────
  dashboardBody(
    useShinyjs(),

    ## inject favicon + all your CSS/JS + sidebar tweaks
    tags$head(
      # favicon (32×32 .ico)
			tags$head(
				# standard favicon for most browsers
				tags$link(rel    = "icon",
						      type   = "image/png",
						      sizes  = "32x32",
						      href   = "/favicon-32x32.png"),
				# legacy support
				tags$link(rel    = "shortcut icon",
						      type   = "image/x-icon",
						      href   = "/favicon.ico"),

				# optional: Apple touch icon
				tags$link(rel = "apple-touch-icon",
						      sizes = "180x180",
						      href  = "/apple-touch-icon.png")
			),

      # your existing stylesheets
      tags$link(rel = "stylesheet", type = "text/css", href = "reset.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "layout.css"),
      
      tags$link(rel = "stylesheet", type = "text/css", href = "model-gen/boundary-met.css"),

      tags$link(rel = "stylesheet", type = "text/css", href = "home-css/home.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "home-css/home-main.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "home-css/home-cards.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "home-css/tutorial.css"),
      
      
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/base.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/models-card.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/sim-card.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/models-links.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/config-cards.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/dir-selector.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/modal-model.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/load-save-buttons.css"),       
      tags$link(rel = "stylesheet", type = "text/css", href = "simulation-css/link-modals.css"),       
      
      tags$link(rel = "stylesheet", type = "text/css", href = "data-vis/data-vis.css"),

      # your JS
      tags$script(src = "modal-init.js"),

      # enlarge sidebar & text
      tags$style(HTML("
        .main-sidebar { width: 300px; }
        .content-wrapper, .main-footer { margin-left: 300px; }
        .sidebar-menu > li > a {
          font-size: 16px !important;
          line-height: 1.4 !important;
        }
        .sidebar-menu .treeview-menu li a {
          font-size: 14px !important;
          padding-left: 30px !important;
        }
      "))
    ),

    ## your tab content
    tabItems(
      tabItem(tabName = "home",           homeUI("home")          ),
      tabItem(tabName = "mg",             modelGenUI("mg")        ),
      tabItem(tabName = "sim_analysis",   simulationUI("sim")     ),
      tabItem(tabName = "sim_sensitivity",sensitivityUI("sen")     ),
      tabItem(tabName = "sim_calibration",calibrationUI("cal")     ),
      tabItem(tabName = "dv",             dataVisUI("dv")         ),
      tabItem(tabName = "dv_sensitivity",             dataVisUI_sen("dv_sen")         ),
      tabItem(tabName = "dv_calibration",             dataVisUI_cal("dv_cal")         ),
      tabItem(tabName = "mm",             minMicrobiomeUI("mm")   )
    )
  ),

  title = "epimodFBA"
)

