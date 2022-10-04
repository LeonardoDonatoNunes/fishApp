#' cadastro_registro_manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_registro_manual_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' cadastro_registro_manual Server Functions
#'
#' @noRd 
mod_registro_manual_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}