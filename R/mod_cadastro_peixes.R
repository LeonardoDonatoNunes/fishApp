#' cadastro_peixes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadastro_peixes_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("index"))
  )
}

#' cadastro_peixes Server Functions
#'
#' @noRd 
mod_cadastro_peixes_server <- function(id, user){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dados <- reactiveValues()
  
    # renderUI | index ----------------------------------------------------  
    output$index <- renderUI({
      
      dados$peixes <- get_peixes(user$pool)
      dados$pessoas <- get_pessoas(user$pool)
      
      vct_peixes <- NULL
      if (!is.null(dados$peixes)) {
        if (nrow(dados$peixes > 0)) {
          vct_peixes <- dados$peixes$id %>% setNames(dados$peixes$especie)
        }
      }
      
      vct_pessoas <- NULL
      if (!is.null(dados$pessoas)) {
        if (nrow(dados$pessoas > 0)) {
          vct_pessoas <- dados$pessoas$id %>% setNames(dados$pessoas$nome)
        }
      }

      tagList(
        tags$div(
          class = 'cadastro_peixe',
          
          box(title = "Captura",
              tagList(
                tags$div(
                  class = "display_flex_row select_plus_button",
                  selectInput(ns('especie'), "Especie", choices = vct_peixes),
                  actionButton(ns('btn_nova_especie'), "", icon = icon("plus-circle"))
                ),
                tags$div(
                  class = 'display_flex_row',
                dateInput(ns('data_captura'), "Data da captura"),
                timeInput(ns('hora_captura'), "Hora da captura", seconds = FALSE)
                ),
                tags$div(
                  class = 'display_flex_row select_plus_button',
                selectInput(ns('pescador'), "Pescador", choices = vct_pessoas),
                actionButton(ns('btn_nova_pessoa'), "", icon = icon("plus-circle"))
                ),
                
                radioGroupButtons(
                  inputId = 'somevalue', 
                  label = "Condição geral do peixe", 
                  choices = c("Muito bem" = 0 , "Bem" = 1, "Ruim" = 2, "Muito ruim" = 3),
                  selected = 0,
                  justified = TRUE, 
                  status = "primary",
                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                )
               
              )
          ),
          
          box(title = "Transmissor/T-bar",
              tagList(
                textInput(ns('numero_serie'), "Número de série"),
                textInput(ns('frequencia'), "Frquência"),
                textInput(ns('radio_id'), "Radio ID"),
                textInput(ns('acustica_id'), "Acustica ID"),
                textInput(ns('tbar'), "T-bar")
              )
              )
          
        )
      )
    })
    
    
    # Observe | btn_nova_especie -------------------------------------------
    observeEvent(input$btn_nova_especie, {
      
      showModal(
        modalDialog(
          title = NULL,
          size = "s", 
          footer = list(
            modalButton("Cancelar"),
            actionButton(ns('salvar_especie'), "Salvar")
          ),
          tagList(
            tags$h3("Nova espécie"),
            textInput(ns('especie_nova'),"", placeholder = "Espécie"),
            textInput(ns('nome_comum'),"", placeholder = "Nome comum"),
            textInput(ns('genero'),"", placeholder = "Gênero"),
            textInput(ns('familia'),"", placeholder = "Família")
          )
        )
      )
      
    })
    
    # Observe | salvar_especie ----------------------------------------------
    observeEvent(input$salvar_especie, {
      insert_peixe(user$pool, input$especie_nova, input$nome_comum, input$genero, input$familia)
      dados$peixes <- get_peixes(user$pool)
      shinyalert::shinyalert("Nova espécie adicionada")
      removeModal()
    })
    
    
    # Observe | btn_nova_pessoa -------------------------------------------
    observeEvent(input$btn_nova_pessoa, {
      
      showModal(
        modalDialog(
          title = NULL,
          size = "s", 
          footer = list(
            modalButton("Cancelar"),
            actionButton(ns('salvar_pessoa'), "Salvar")
          ),
          tagList(
            tags$h3("Nova pessoa"),
            textInput(ns('pessoa_nova'),"", placeholder = "Nome"),
            textInput(ns('funcao'),"", placeholder = "Função")
          )
        )
      )
      
    })
    
    # Observe | salvar_pessoa ----------------------------------------------
    observeEvent(input$salvar_pessoa, {
      insert_pessoa(con, input$pessoa_nova, input$funcao)
      dados$pessoas <- get_pessoas(user$pool)
      shinyalert::shinyalert("Nova pessoa adicionada")
      removeModal()
    })
    
    
    
  })
}


#' utils 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
insert_peixe <- function(con, especie, nome_comum, genero, familia) {
  
  df <- data.frame(
    especie = especie, 
    nome_comum = nome_comum, 
    genero = genero, 
    familia = familia
  )
  
  dbx::dbxInsert(con, "peixes", df)
  
}

#' utils 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
insert_pessoa <- function(con, nome, cargo) {
  
  df <- data.frame(
    nome = nome, 
    cargo = cargo
  )
  
  dbx::dbxInsert(con, "pessoas", df)
  
}

#' utils 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#' @noRd
get_peixes <- function(con) {
  
  query <- "select * from peixes"
  
  return(DBI::dbGetQuery(con, query))
  
}

#' utils 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#' @noRd
get_pessoas <- function(con) {
  
  query <- "select * from pessoas"
  
  return(DBI::dbGetQuery(con, query))
  
}
