#' captura_recaptura UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_recaptura_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' captura_recaptura Server Functions
#'
#' @noRd 
mod_recaptura_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_captura_recaptura_ui("captura_recaptura_1")
    
## To be copied in the server
# mod_captura_recaptura_server("captura_recaptura_1")
