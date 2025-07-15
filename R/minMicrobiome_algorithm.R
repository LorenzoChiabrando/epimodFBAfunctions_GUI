# R/minMicrobiome.R

# UI for minMicrobiome Algorithm
minMicrobiomeUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "minMicrobiome Algorithm",
    div(class="card p-3",
      h4("Configure minMicrobiome"),
      # your inputs here, e.g.
      numericInput(ns("mm_threshold"), "Abundance threshold:", value = 0.01, min = 0),
      actionButton(ns("btn_run_mm"), "Run Algorithm", class="btn-primary")
    ),
    div(class="mt-4",
      h4("Output"),
      tableOutput(ns("mm_results"))
    )
  )
}

# Server for minMicrobiome Algorithm
minMicrobiomeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    mm_data <- eventReactive(input$btn_run_mm, {
      # placeholder: run your algorithm, return a data.frame
      data.frame(species = letters[1:5], abundance = runif(5))
    })

    output$mm_results <- renderTable({
      req(mm_data())
      mm_data()
    })
  })
}

