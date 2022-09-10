#' cadastro_equipamentos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadastro_equipamentos_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' cadastro_equipamentos Server Functions
#'
#' @noRd 
mod_cadastro_equipamentos_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cadastro_equipamentos_ui("cadastro_equipamentos")
    
## To be copied in the server
# mod_cadastro_equipamentos_server("cadastro_equipamentos")
