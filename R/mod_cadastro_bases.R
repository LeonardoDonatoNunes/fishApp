#' cadastro_bases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadastro_bases_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("index"))
  )
}

#' cadastro_bases Server Functions
#'
#' @noRd 
mod_cadastro_bases_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dados <- reactiveValues()
    
    # Render | index -------------------------------------------------------
    
    output$index <- renderUI({
      
      dados$bases_fixas <- get_bases_fixas(user$pool, user$info_projeto_sel)
      
      vct_locais <- c("Nenhum local cadastrado" = 0)
      dados$locais <- get_locais(user$pool, user$info_projeto_sel$id)
      
      if (nrow(dados$locais) != 0) {
        
        vct_locais <- dados$locais$id %>% setNames(dados$locais$nome)
        
      }
      
      vct_equipamentos <- c("Nenhum equipamento cadastrado" = 0)
      dados$equipamentos <- get_equipamentos(user$pool, user$info_projeto_sel$id)
      
      if (nrow(dados$locais) != 0) {
        
        vct_equipamentos <- dados$equipamentos$id %>% setNames(dados$equipamentos$numero_serie)
        
      }
      
      fluidPage(
        tags$h3("Cadastro de bases fixas"),
        column(
          width = 3,
          tags$div(
            class = 'cadastro_base',
            
            tags$fieldset(
              class = 'input_box',
              tags$legend("Identificação"),
              textInput(ns('nome'), "", placeholder = "Nome da base"),
              selectInput(ns('local_id'), "Local", choices = vct_locais)
            ),
            
            tags$fieldset(
              class = 'input_box',
              tags$legend("Equipamento"),
              selectInput(ns('equipamento_id'), "S/N", choices = vct_equipamentos)
            ),
            
            tags$fieldset(
              class = 'input_box',
              tags$legend("Datas"),
              dateInput(ns('data_inicio'), "Início operação"),
              dateInput(ns('data_fim'), "Fim operação"),
              checkboxInput(ns('base_ativa'), "Base ativa", value = TRUE)
            )
          )
        ),
        column(
          width = 9,
          tags$fieldset(
            class = 'input_box',
            tags$legend("Bases cadastradas"),
            reactableOutput(ns('tbl'))
          )
        )
      )
      
    })
    
    # Render | tbl ---------------------------------------------------------------
    output$tbl <- renderReactable({
      
      dados$bases_fixas %>% 
        select(nome, nome_local, numero_serie, data_hora_ini, data_hora_fim, id, projeto_id, local_id, equipamento_id) %>% 
        reactable::reactable(
          onClick = "select", 
          selection = "single", 
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
          highlight = TRUE,
          outlined  =  TRUE,
          columns = list(
            nome = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Nome da base", align = "left"),
            nome_local = colDef(minWidth = 100, maxWidth = 250, name = "Local", align = "center"),
            numero_serie =  colDef(minWidth = 100, maxWidth = 250, name = "Equipamento S/N", align = "center"),
            data_hora_ini =  colDef(minWidth = 100, maxWidth = 250, name = "Inicio operação", align = "center"),
            data_hora_fim =  colDef(minWidth = 100, maxWidth = 250, name = "Fim operação", align = "center"),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE),
            projeto_id = colDef(show = FALSE),
            local_id = colDef(show = FALSE),
            equipamento_id = colDef(show = FALSE)
          )
        )
      
    })
    
    
    # Observe | tbl__reactable__selected -----------------------------------
    observeEvent(input$tbl__reactable__selected, {
      
      if (!is.null(input$tbl__reactable__selected)) {
        
      } else {
        
      }
      
    })
    
    # Observe | base_ativa ---------------------------------------------------
    observeEvent(input$base_ativa, {
      
      if (input$base_ativa) {
        shinyjs::disable('data_fim')
      } else {
        shinyjs::enable('data_fim')
      }
      
      
    })
    
    # Observe | salvar -------------------------------------------------------
    observeEvent(input$salvar, {
      
      
    })
    
    
  })
  
}

#' Upserte de base
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
upsert_base <- function(con, id, projeto_id, nome_base, local_id, equipamento_id, data_hora_ini, data_hora_fim) {
  
  df <- data.frame(
    id = id, 
    projeto_id = projeto_id, 
    nome_base = nome_base, 
    local_id = local_id, 
    equipamento_id = equipamento_id, 
    data_hora_ini = data_hora_ini, 
    data_hora_fim = data_hora_fim
  )
  
  dbx::dbxUpsert(con,"bases", df, "id")
  
}

#' Upsert de equipamento
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
upsert_equipamenoto <- function(con, id,numero_serie,tipo,marca,modelo) {
  df <- data.frame(
    id = id,
    numero_serie = numero_serie,
    tipo = tipo, 
    marca = marca,
    modelo = modelo
  )
  
  dbx::dbxUpsert(con,"equipamentos", df, "id")
  
}

#' Upsert de local
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
upsert_local <- function(con, id, projeto_id, nome, tipo, lat, lon) {
  df <- data.frame(
    id = id,
    projeto_id = projeto_id,
    nome = nome, 
    tipo = tipo,
    lat = lat,
    lon = lon
  )
  
  dbx::dbxUpsert(con,"locais", df, "id")
  
}

#' Select de local
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
select_equipamento <- function(con, tipo = NULL) {
  
  query <- "select * from equipamentos"
  
  if (!is.null(tipo)) {
    query <- glue::glue("{query} where tipo = '{tipo}'")
  }
  
  dbx::dbxSelect(con, query) 
  
}

#' Select de local
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
select_locais <- function(con, tipo = NULL) {
  
  query <- "select * from locais"
  
  if (!is.null(tipo)) {
    query <- glue::glue("{query} where tipo = '{tipo}'")
  }
  
  dbx::dbxSelect(con, query) 
  
}
