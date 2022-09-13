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
    dados <- reactiveValues()
    
    # Index -----------------------------------------------------------------
    output$index <- renderUI({
      
      dados$equipamentos <- get_equipamentos(user$pool, user$info_projeto_sel$id)
      
      vct_tipos <- c("Rádio fixo" = 'radio_fixo', "Rádio móvel" = 'radio_movel', "Acústica fixo" = 'acustica_fixo', "Acústica móvel" = 'acustica_movel')
      
      fluidPage(
        tags$h3("Cadastro de equipamentos"),
        column(
          width = 3,
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Identificação"),
            textInput(ns('nome'), "", placeholder = "Número de série"),
            selectInput(ns('tipo'), "Tipo do equipamento", choices = vct_tipos),
            textInput(ns('marca'), "", placeholder = "Marca"),
            textInput(ns('modelo'), "", placeholder = "Modelo")
          ),
          
          tags$div(
            class = 'display_flex_row',
            hidden(actionButton(ns('deletar'), "Deletar", icon = icon('trash-can'))),
            actionButton(ns('salvar'), "Salvar", icon = icon('floppy-disk'))
          ),
        ),
        
        column(
          width = 9,
          tags$fieldset(
            class = 'input_box',
            tags$legend("Equipamentos cadastrados"),
            reactableOutput(ns('tbl'))
          )
        )
      )
    })
    
    
    # Render | tbl ---------------------------------------------------------------
    output$tbl <- renderReactable({
     
      dados$equipamentos %>% 
        reactable::reactable(
          onClick = "select", 
          selection = "single", 
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
          highlight = TRUE,
          outlined  =  TRUE,
          pagination = TRUE,
          height = 300,
          resizable = TRUE,
          showPageSizeOptions = TRUE,
          columns = list(
            numero_serie = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Nome", align = "left"),
            tipo =  colDef(minWidth = 100, maxWidth = 250, name = "Tipo do equipamento", align = "center"),
            marca =  colDef(minWidth = 100, maxWidth = 250, name = "Marca", align = "center"),
            modelo =  colDef(minWidth = 100, maxWidth = 250, name = "Modelo", align = "center"),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE),
            projeto_id = colDef(show = FALSE)
          )
        )
    })
    
    # Observe | tbl__reactable__selected -------------------------------------
    observeEvent(input$tbl__reactable__selected, {
      
      if (!is.null(input$tbl__reactable__selected)) {
        
        aux <- 
          dados$equipamentos %>% slice(input$tbl__reactable__selected)
        
        if (nrow(aux) != 0) {
          
          dados$id_clicado <- aux$id
          updateTextInput(inputId = "nome", value = aux$numero_serie)
          updateSelectInput(inputId = "tipo", selected = aux$tipo)
          updateTextInput(inputId = "marca", value = aux$marca)
          updateTextInput(inputId = "modelo", value = aux$modelo)
          updateActionButton(inputId = 'salvar', label = "Salvar alterações")
          show('deletar')
          
        }
        
      } else {
        
        dados$id_clicado <- NULL
        updateTextInput(inputId = "nome", value = "")
        updateTextInput(inputId = "marca", value = "")
        updateTextInput(inputId = "modelo", value = "")
        updateActionButton(inputId = 'salvar', label = "Salvar")
        hide('deletar')
      }
      
    }, ignoreNULL = FALSE)
    
    
    # Controle | feedback ----------------------------------------------------
    observeEvent(input$nome, {if (input$nome != "") {campo_obrigatorio_feedback("nome", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$marca, {if (input$marca != "") {campo_obrigatorio_feedback("marca", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$modelo, {if (input$modelo != "") {campo_obrigatorio_feedback("modelo", FALSE)}}, ignoreNULL = TRUE)

    
    
    # Observe | salvar -------------------------------------------------------
    observeEvent(input$salvar, {
      
      nome <- TRUE
      marca <- TRUE
      modelo <- TRUE
      
      
      if (input$nome == "") {nome <- FALSE; campo_obrigatorio_feedback("nome", TRUE)}
      if (input$marca == "") {marca <- FALSE; campo_obrigatorio_feedback("marca", TRUE)}
      if (input$modelo == "") {modelo <- FALSE; campo_obrigatorio_feedback("modelo", TRUE)}

      
      if (all(c(nome, marca, modelo))) {
        
        tabela <- data.frame(
          id = NA,
          projeto_id = user$info_projeto_sel$id,
          numero_serie = input$nome, 
          tipo = input$tipo,
          marca = input$marca,
          modelo = input$modelo
        ) %>%  mutate(id = dados$id_clicado)
        
        tryCatch({
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$id_clicado)) {
            
            dbx::dbxInsert(conn, 'equipamentos', tabela)
            
          } else {
            
            dbx::dbxUpdate(conn, 'equipamentos', tabela, where_cols = 'id')
            
          }
          
          shinyjs::reset("nome")
          shinyjs::reset("marca")
          shinyjs::reset("modelo")
          
          dados$equipamentos <- get_equipamentos(user$pool,  user$info_projeto_sel$id)
          
          shinyalert::shinyalert("Equipamento salvo!", type = "success")
          poolReturn(conn)
          
        }, error = function(e) {shinyalert::shinyalert(type = 'error', title = "Erro ao salvar equipamento", text = paste0(e))})
        
      }
      
    })
    
    # Observe | deletar ------------------------------------------------------
    observeEvent(input$deletar, {
      
      tryCatch({
        
        conn <- poolCheckout(user$pool)
        
        dbx::dbxDelete(conn, 'equipamentos', where = data.frame(id = dados$id_clicado))
        dados$equipamentos <- get_equipamentos(user$pool,  user$info_projeto_sel$id)
        
        poolReturn(conn)
        
        shinyalert::shinyalert(type = 'success', title = "Equipamento deletado")
        
        
      }, error = function(e) {
        shinyalert::shinyalert(type = 'error', title = "Erro ao deletar equipamento", text = paste0(e))
      })
      
      
    })
    
    
    
    
    
    
  })
  
}
    
## To be copied in the UI
# mod_cadastro_equipamentos_ui("cadastro_equipamentos")
    
## To be copied in the server
# mod_cadastro_equipamentos_server("cadastro_equipamentos")
