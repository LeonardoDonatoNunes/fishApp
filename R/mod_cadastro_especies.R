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
    dados <- reactiveValues()
    
    # Index -----------------------------------------------------------------
    output$index <- renderUI({

      dados$peixes <- get_peixes(user$pool)
      
      fluidPage(
        tags$h3("Cadastro de espécies"),
        column(
          width = 3,
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Identificação"),
            textInput(ns('nome'), "", placeholder = "Nome popular"),
            textInput(ns('especie'), "", placeholder = "Espécie"),
            textInput(ns('genero'), "", placeholder = "Gênero"),
            textInput(ns('familia'), "", placeholder = "Família")
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
            tags$legend("Peixes cadastrados"),
            reactableOutput(ns('tbl'))
          )
        )
      )
    })
    
    
    # Render | tbl ---------------------------------------------------------------
    output$tbl <- renderReactable({
      
      dados$peixes %>% 
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
            nome_comum = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Nome popular", align = "left"),
            especie =  colDef(minWidth = 100, maxWidth = 250, name = "Espécie", align = "center"),
            genero =  colDef(minWidth = 100, maxWidth = 250, name = "Gênero", align = "center"),
            familia =  colDef(minWidth = 100, maxWidth = 250, name = "Família", align = "center"),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE)
          )
        )
    })
    
    # Observe | tbl__reactable__selected -------------------------------------
    observeEvent(input$tbl__reactable__selected, {
      
      if (!is.null(input$tbl__reactable__selected)) {
        
        aux <- 
          dados$peixes %>% slice(input$tbl__reactable__selected)
        
        if (nrow(aux) != 0) {
          
          dados$id_clicado <- aux$id
          updateTextInput(inputId = "nome", value = aux$nome_comum)
          updateTextInput(inputId = "especie", value = aux$especie)
          updateTextInput(inputId = "genero", value = aux$genero)
          updateTextInput(inputId = "familia", value = aux$familia)
          updateActionButton(inputId = 'salvar', label = "Salvar alterações")
          show('deletar')
          
        }
        
      } else {
        
        dados$id_clicado <- NULL
        updateTextInput(inputId = "nome", value = "")
        updateTextInput(inputId = "especie", value = "")
        updateTextInput(inputId = "genero", value = "")
        updateTextInput(inputId = "familia", value = "")
        updateActionButton(inputId = 'salvar', label = "Salvar alterações")
        updateActionButton(inputId = 'salvar', label = "Salvar")
        hide('deletar')
      }
      
    }, ignoreNULL = FALSE)
    
    
    # Controle | feedback ----------------------------------------------------
    observeEvent(input$nome, {if (input$nome != "") {campo_obrigatorio_feedback("nome", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$especie, {if (input$especie != "") {campo_obrigatorio_feedback("especie", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$genero, {if (input$genero != "") {campo_obrigatorio_feedback("genero", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$familia, {if (input$familia != "") {campo_obrigatorio_feedback("familia", FALSE)}}, ignoreNULL = TRUE)
    
    
    
    # Observe | salvar -------------------------------------------------------
    observeEvent(input$salvar, {
      
      nome <- TRUE
      especie <- TRUE
      genero <- TRUE
      familia <- TRUE
      
      if (input$nome == "") {nome <- FALSE; campo_obrigatorio_feedback("nome", TRUE)}
      if (input$especie == "") {especie <- FALSE; campo_obrigatorio_feedback("especie", TRUE)}
      if (input$genero == "") {genero <- FALSE; campo_obrigatorio_feedback("genero", TRUE)}
      if (input$familia == "") {familia <- FALSE; campo_obrigatorio_feedback("familia", TRUE)}
      
      
      if (all(c(nome, especie, genero, familia))) {
        
        tabela <- data.frame(
          id = NA,
          nome_comum = input$nome, 
          especie = input$especie,
          genero = input$genero,
          familia = input$familia
        ) %>%  mutate(id = dados$id_clicado)
        
        tryCatch({
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$id_clicado)) {
            
            dbx::dbxInsert(conn, 'peixes', tabela)
            
          } else {
            
            dbx::dbxUpdate(conn, 'peixes', tabela, where_cols = 'id')
            
          }
          
          shinyjs::reset("nome")
          shinyjs::reset("especie")
          shinyjs::reset("genero")
          shinyjs::reset("familia")
                    
          dados$peixes <- get_peixes(user$pool)
          
          shinyalert::shinyalert("Espécie salva!", type = "success")
          poolReturn(conn)
          
        }, error = function(e) {shinyalert::shinyalert(type = 'error', title = "Erro ao salvar peixe", text = paste0(e))})
        
      }
      
    })
    
    # Observe | deletar ------------------------------------------------------
    observeEvent(input$deletar, {
      
      tryCatch({
        
        conn <- poolCheckout(user$pool)
        
        dbx::dbxDelete(conn, 'peixes', where = data.frame(id = dados$id_clicado))
        dados$peixes <- get_peixes(user$pool)
        
        poolReturn(conn)
        
        shinyalert::shinyalert(type = 'success', title = "Peixe deletado")
        
        
      }, error = function(e) {
        shinyalert::shinyalert(type = 'error', title = "Erro ao deletar peixe", text = paste0(e))
      })
      
      
    })
    
    
    
    
    
    
  })
  
  
}
    
## To be copied in the UI
# mod_cadastro_especies_ui("cadastro_especies_1")
    
## To be copied in the server
# mod_cadastro_especies_server("cadastro_especies_1")
