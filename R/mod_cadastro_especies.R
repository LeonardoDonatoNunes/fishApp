#' cadastro_especies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadastro_especies_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' cadastro_especies Server Functions
#'
#' @noRd 
mod_cadastro_especies_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cadastro_especies_ui("cadastro_especies_1")
    
## To be copied in the server
# mod_cadastro_especies_server("cadastro_especies_1")
