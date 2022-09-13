#' cadastro_pessoas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadastro_pessoas_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('index'))
  )
}

#' cadastro_pessoas Server Functions
#'
#' @noRd 
mod_cadastro_pessoas_server <- function(id, user){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    dados <- reactiveValues()
    
    # Index ------------------------------------------------------------------ 
    output$index <- renderUI({
      
      vct_projetos <- c("Nenhum projeto disponível" = 0)
      if (nrow(user$info_projeto) > 0) {
        vct_projetos <- user$info_projeto$id %>% setNames(user$info_projeto$nome)
      }
      
      fluidPage(
        tags$h3("Cadastro de pessoas"),
        column(
          width = 3,
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Identificação"),
            textInput(ns('nome'), "", placeholder = "Nome"),
            textInput(ns('cargo'), "", placeholder = "Função")
          ),
          tags$fieldset(
            class = 'input_box',
            tags$legend("Associar projeto(s)"),
            selectInput(ns('projeto'), "Selecione um ou mais projetos", multiple = TRUE,  choices = vct_projetos),
            checkboxInput(ns('todos'), "Todos os projetos")
          ),
          tags$div(
            class = 'display_flex_row',
            hidden(actionButton(ns('deletar'), "Deletar", icon = icon('trash-can'))),
            actionButton(ns('salvar'), "Salvar", icon = icon('floppy-disk'))
          )
          
        ),
        column(
          width = 9,
          tags$fieldset(
            class = 'input_box',
            tags$legend("Pessoas cadastrados"),
            reactableOutput(ns('tbl'))
          )
        )
      )
    })
    
    
    output$tbl <- renderReactable({
      
      input$deletar
      input$salvar
      dados$pessoa_sel <- NULL
      
      dados$pessoas <- get_pessoas_projetos(user$pool)
      
      dados$tbl_reactable <-
        dados$pessoas %>% 
        select(id, nome, cargo, projeto) %>% 
        mutate(projeto = paste0("<span class = 'nome_projeto'>",projeto,"</span>")) %>% 
        group_by(id, nome, cargo) %>% 
        summarise(projeto = paste0(projeto, collapse = " "))
      
      dados$tbl_reactable %>% 
        reactable(
          onClick = 'select', 
          selection = 'single', 
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = '#eee', boxShadow = 'inset 2px 0 0 0 #ffa62d')),
          highlight = TRUE,
          outlined  =  TRUE,
          pagination = TRUE,
          height = 300,
          resizable = TRUE,
          showPageSizeOptions = TRUE,
          columns = list(
            nome = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Nome", align = "left"),
            cargo =  colDef(minWidth = 100, maxWidth = 250, name = "Função", align = 'center'),
            projeto =  colDef(minWidth = 100, maxWidth = 250, name = "Projetos", align = 'center', html = TRUE),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE)
          )
        )
      
    })
    
    # Observe | -------------------------------------------------
    observeEvent(input$tbl__reactable__selected, {
      
      if (!is.null(input$tbl__reactable__selected)) {
        
        dados$pessoa_sel <- dados$tbl_reactable[input$tbl__reactable__selected,] %>% pull(id)
        
        aux <- 
          dados$pessoas %>% 
          filter(id == dados$pessoa_sel) 
        
        updateTextInput(inputId = 'nome', value = unique(aux$nome))  
        updateTextInput(inputId = 'cargo', value = unique(aux$cargo)) 
        updateSelectInput(inputId = 'projeto', selected = aux$projeto_id)
        updateActionButton(inputId = 'salvar', label = "Salvar alterações")
        show('deletar')
        
      } else {
        
        dados$pessoa_sel <- NULL
        reset('nome')
        reset('cargo')
        reset('projeto')
        updateActionButton(inputId = 'salvar', label = "Salvar")
        hide('deletar')
        
      }
      
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$todos, {
      
      if (input$todos) {
        updateSelectInput(inputId = 'projeto', selected = user$info_projeto$id)
      } else {
        reset('projeto')
      }
      
      
    })
    
    # Observe | salvar ------------------------------------------------------
    observeEvent(input$salvar, {
      
      tabela <- data.frame(
        id = NA,
        nome = input$nome,
        cargo = input$cargo
      ) %>% 
        mutate(id = dados$pessoa_sel)
      
      tryCatch({
        
        conn <- poolCheckout(user$pool)
        
        if (is.null(dados$pessoa_sel)) { 
          
          pessoa_id <- dbx::dbxInsert(conn, 'pessoas', tabela, returning = 'id')
          pessoa_projeto <- data.frame(pessoa_id = as.numeric(pessoa_id), projeto_id = input$projeto)
          
          dbx::dbxUpsert(
            con = conn, 
            table = 'pessoa_projeto', 
            records = pessoa_projeto,
            where_cols = c('pessoa_id', 'projeto_id'))
          
        } else {
          
          dbx::dbxUpdate(conn, 'pessoas', tabela, where_cols = 'id')
          pessoa_projeto <-  data.frame(pessoa_id = dados$pessoa_sel, projeto_id = input$projeto)
            
          dbx::dbxUpsert(
            con = conn, 
            table = 'pessoa_projeto', 
            records = pessoa_projeto,
            where_cols = c('pessoa_id', 'projeto_id'))
          
        }
        
        shinyalert::shinyalert(type = 'success', title = paste0("Sucesso ao adicionar!"), text = paste0(input$nome, " foi adicionado(a)"))
        
        poolReturn(conn)
        
      }, error = function(e) {shinyalert::shinyalert(type = 'error', title = "Erro ao salvar pessoa", text = paste0(e))})
      
      
    })
    
    
    # Observe | deletar ------------------------------------------------------
    observeEvent(input$deletar, {
      
      tryCatch({
        
        conn <- poolCheckout(user$pool)
        
        dbx::dbxDelete(conn = user$pool, 'pessoas', where = data.frame(id = dados$pessoa_sel))
        shinyalert::shinyalert(title = "Pessoa excluída", type = 'success')
        poolReturn(conn)
        
      }, error = function(e) {shinyalert::shinyalert(type = 'error', title = "Erro ao deletar pessoa", text = paste0(e))})
  
      
    })
    
  })
}
