#' acompanhamento_geral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_acompanhamento_geral_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('index'))
  )
}

#' acompanhamento_geral Server Functions
#'
#' @noRd 
mod_acompanhamento_geral_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$index <- renderUI({
      fluidPage(
        loader_pagina_em_construcao(),
      )
    })
    
  })
}

## To be copied in the UI
# mod_acompanhamento_geral_ui("acompanhamento_geral_1")

## To be copied in the server
# mod_acompanhamento_geral_server("acompanhamento_geral_1")
