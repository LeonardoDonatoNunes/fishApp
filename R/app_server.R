#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#'     
#' @import shiny
#' @import dplyr
#' @import purrr
#' @import leaflet
#' @import shinyTime
#' @import shinyWidgets
#' @import shinyjs
#' @import shinyalert
#' @import reactable
#' @import pool
#' @import dbx
#' @import DBI
#' @import shinyFeedback
#' @import RPostgreSQL
#' 
#' @noRd
app_server <- function( input, output, session ) {
  
  user <- reactiveValues()
  
  output$dropdown_upmenu_ui <- renderUI({
    
    
    if (teste_existencia_bd()) {
      
      user$pool <- pool_fishApp_bd()
      
    } else {
      
      conn <- pool_generic_bd()
      create_fishApp_bd(conn)
      user$pool <- pool_fishApp_bd()
      create_fishApp_tables(user$pool)
      
    }
    
    user$info_projeto <- get_projetos(user$pool)
    
    
    vct_proj <- c("Nenhum projeto cadastrado" = 0)
    
    if (nrow(user$info_projeto) != 0) {
      vct_proj <- user$info_projeto$id %>% setNames(user$info_projeto$nome) 
    }
    
    tagList(
      tags$li(
        class = 'dropdown notifications -menu',
        style = 'height:51px;',
        selectInput('projeto', label = NULL, choices = vct_proj)
      )
    )
    
  })
  
  observeEvent(input$projeto, {
    user$projeto <- input$projeto
    
    if (input$projeto != 0) {
      user$info_projeto_sel <- user$info_projeto %>% filter(id == input$projeto)
    }
    
  })
  
  mod_cadastro_projetos_server('cadastro_projetos', user = user)
  mod_cadastro_pessoas_server('cadastro_pessoas', user = user)
  mod_cadastro_locais_server('cadastro_locais', user = user)
  mod_cadastro_equipamentos_server('cadastro_equipamentos', user = user)
  mod_cadastro_bases_server("cadastro_bases", user = user)
  mod_cadastro_especies_server('cadastro_especies', user = user)
  mod_cadastro_peixes_server("cadastro_peixes", user = user)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
}


#' Buscar projetos
#'
#' @param con 
#'
#' @return data_frame
#' @export
#'
#' @examples get_projetos(con)
get_projetos <- function(con) {
  
  conn <- poolCheckout(con)
  
  query_return <- DBI::dbGetQuery(con, "select * from projetos")
  
  poolReturn(conn)
  
  return(query_return)
  
}

#' Buscar pessoas
#'
#' @param con 
#'
#' @return data_frame
#' @export
#'
#' @examples get_projetos(con)
get_pessoas_projetos <- function(con) {
  
  conn <- poolCheckout(con)
  
  query <- "
   select p.id, p.nome, p.cargo, pr.id as projeto_id, pr.nome as projeto
    from pessoas p 
    left join pessoa_projeto pp on p.id = pp.pessoa_id
    left join projetos pr on pp.projeto_id = pr.id;
  "
  
  query_return <- DBI::dbGetQuery(conn, query)
  
  poolReturn(conn)
  
  return(query_return)
  
}

#' Buscar tipo_local
#'
#' @param con 
#'
#' @return data_frame
#' @export
#'
#' @examples get_tipo_local(con)
get_tipo_local <- function(con) {
  
  conn <- poolCheckout(con)
  
  query <- "select * from tipo_local"
  
  query_return <- DBI::dbGetQuery(conn, query)
  
  poolReturn(conn)
  return(query_return)
  
}

#' Buscar locais
#'
#' @param con 
#'
#' @return data_frame
#' @export
#'
#' @examples get_locais(con)
get_locais <- function(con, projeto_id) {
  
  conn <- poolCheckout(con)
  
  query <- glue::glue("
  select l.*, tl.nome as tipo_nome
  from locais l
  left join tipo_local tl on l.tipo_id = tl.id
  where projeto_id = {projeto_id}
  ")
  
  query_return <- DBI::dbGetQuery(conn, query)
  
  poolReturn(conn)
  return(query_return)
  
}

#' Buscar equipamentos
#'
#' @param con 
#'
#' @return data_frame
#' @export
#'
#' @examples get_equipamentos(con)
get_equipamentos <- function(con, projeto_id) {
  
  conn <- poolCheckout(con)
  
  query <- glue::glue("
  select *
  from equipamentos 
  where projeto_id = {projeto_id}
  ")
  
  query_return <- DBI::dbGetQuery(conn, query)
  
  poolReturn(conn)
  return(query_return)
  
}

#' Buscar peixes
#'
#' @param con 
#'
#' @return data_frame
#' @export
#'
#' @examples get_peixes(con)
get_peixes <- function(con) {
  
  conn <- poolCheckout(con)
  
  query <- glue::glue("
  select *
  from peixes
  ")
  
  query_return <- DBI::dbGetQuery(conn, query)
  
  poolReturn(conn)
  return(query_return)
  
}


#' Teste de campo obrigatorio
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
campo_obrigatorio_feedback <- function(inputId, show) {
  
  shinyFeedback::feedbackDanger(
    inputId = inputId, 
    show = show,
    text = "Campo obrigatório!",
    color = "red", 
    icon = icon('triangle-exclamation'))
  
}

#' Icone para o mapa
#'
#' @param con 
#'
#' @return icone
#' @export
#'
#' @examples icons('green')
icons <- function(color) {
  awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = color)
} 
