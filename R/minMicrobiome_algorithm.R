# R/minMicrobiome.R

# R/minMicrobiome.R
#' @export
minMicrobiomeUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "minMicrobiome Algorithm",

    # full‑height flexbox centres its child both ways
    div(
      style = "
        display: flex;
        justify-content: center;
        align-items:   center;
        height: calc(100vh - 100px);  /* subtract header + tabs height */
        width:  100%;
        text-align: center;
      ",
      tags$img(
        src   = "work_in_progress2.png",
        style = "max-width: 320px; height: auto;"
      )
    )
  )
}

# Server for minMicrobiome Algorithm
#' @export
minMicrobiomeServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

