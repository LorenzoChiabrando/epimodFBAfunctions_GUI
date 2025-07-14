# inst/app/server.R
library(shiny)
library(shinyFiles)
library(shinyjs)
library(yaml)

function(input, output, session) {

  roots <- c(wd = ".")

	modelGenServer("mg")

}

