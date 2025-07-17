# inst/app/server.R
library(shiny)
library(shinyFiles)
library(shinyjs)
library(yaml)


project_root <- getOption(
  "epimodFBAfunctionsGUI.user_proj",
  normalizePath("~")
)

function(input, output, session) {
  roots <- c(wd = ".")

  # modules
  homeServer("home")
  modelGenServer("mg")
  simulationServer("sim")
  sensitivityServer("sen")
  calibrationServer("cal")
  dataVisServer("dv")
  minMicrobiomeServer("mm")

  # —————————————————————————————————————————————
  # Jump from Home cards into the sidebar panels
  # —————————————————————————————————————————————
  observeEvent(input$`home-go_mg`, {
    updateNavlistPanel(session, "main_nav", selected = "Model Generation")
  })
  observeEvent(input$`home-go_sim`, {
    updateNavlistPanel(session, "main_nav", selected = "Simulation")
  })
  observeEvent(input$`home-go_mm`, {
    updateNavlistPanel(session, "main_nav", selected = "minMicrobiome Algorithm")
  })
  observeEvent(input$`home-go_dv`, {
    updateNavlistPanel(session, "main_nav", selected = "Data Visualization")
  })
  

	# Simulation → Data Visualization
  observeEvent(input$`sim-btn_visualize`, {
    updateNavlistPanel(session, "main_nav", selected = "Data Visualization")
    # Click the dv plot button after a short delay
    runjs('setTimeout(function(){ $("#dv-btn_plot").click(); }, 200);')
  })


  observeEvent(input$`sim-btn_new_sim`, {
    # simply reset back to Simulation
    updateNavlistPanel(session, "main_nav", selected = "Simulation")
  })
}

