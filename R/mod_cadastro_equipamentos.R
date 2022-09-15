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
      
      input$salvar_tipo
      input$deletar_tipo
      
      dados$equipamentos <- get_equipamentos(user$pool, user$info_projeto_sel$id)
      dados$tipo_equipamento <- get_tipo_equipamento(user$pool)
      
      vct_tipos <- c("Nenhum tipo encontrado" = 0)
      
      if (nrow(dados$tipo_equipamento) > 0) {
        vct_tipos  <- dados$tipo_equipamento$id %>% setNames(dados$tipo_equipamento$nome)
      } 
      
      
      fluidPage(
        tags$h3("Cadastro de equipamentos"),
        column(
          width = 3,
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Identificação"),
            textInput(ns('nome'), "", placeholder = "Número de série"),
            selectInput(ns('tipo'), "Tipo do equipamento", choices = vct_tipos),
            actionLink(ns('novo_tipo'), "Adicionar tipo de equipamento", icon = icon("circle-plus")),
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
            tipo_nome =  colDef(minWidth = 100, maxWidth = 250, name = "Tipo do equipamento", align = "center"),
            marca =  colDef(minWidth = 100, maxWidth = 250, name = "Marca", align = "center"),
            modelo =  colDef(minWidth = 100, maxWidth = 250, name = "Modelo", align = "center"),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE),
            tipo_id = colDef(show = FALSE),
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
          
          dados$equipamento_id_sel <- aux$id
          updateTextInput(inputId = "nome", value = aux$numero_serie)
          updateSelectInput(inputId = "tipo", selected = aux$tipo)
          updateTextInput(inputId = "marca", value = aux$marca)
          updateTextInput(inputId = "modelo", value = aux$modelo)
          updateActionButton(inputId = 'salvar', label = "Salvar alterações")
          show('deletar')
          
        }
        
      } else {
        
        dados$equipamento_id_sel <- NULL
        updateTextInput(inputId = "nome", value = "")
        updateTextInput(inputId = "marca", value = "")
        updateTextInput(inputId = "modelo", value = "")
        updateActionButton(inputId = 'salvar', label = "Salvar")
        hide('deletar')
      }
      
    }, ignoreNULL = FALSE)
    
    
    # Controle | feedback ----------------------------------------------------
    observeEvent(input$nome, {
      
      if (input$nome != '') {campo_obrigatorio_feedback('nome', FALSE)}

      dados$teste_nome <- TRUE
      
      if (is.null(dados$equipamento_id_sel)) {
        
        if (input$nome %in% dados$equipamentos$numero_serie) {
          
          shinyFeedback::feedbackDanger('nome', text = "Número de série existente, não será possível salvar", show = TRUE)
          dados$teste_nome <- FALSE
          
        } else {
          
          shinyFeedback::feedbackDanger('nome', show = FALSE)
          
        }
        
      }
      
    })
    
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
      
      
      if (all(c(nome, marca, modelo, dados$teste_nome))) {
        
        tabela <- data.frame(
          id = NA,
          projeto_id = user$info_projeto_sel$id,
          numero_serie = input$nome, 
          tipo_id = input$tipo,
          marca = input$marca,
          modelo = input$modelo
        ) %>%  mutate(id = dados$equipamento_id_sel)
        
        tryCatch({
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$equipamento_id_sel)) {
            
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
        
        dbx::dbxDelete(conn, 'equipamentos', where = data.frame(id = dados$equipamento_id_sel))
        dados$equipamentos <- get_equipamentos(user$pool,  user$info_projeto_sel$id)
        
        poolReturn(conn)
        
        shinyalert::shinyalert(type = 'success', title = "Equipamento deletado")
        
        
      }, error = function(e) {
        shinyalert::shinyalert(type = 'error', title = "Erro ao deletar equipamento", text = paste0(e))
      })
      
      
    })
    
    
    
    # Observe | novo_tipo ----------------------------------------------
    observeEvent(input$novo_tipo, {
      showModal(
        modalDialog(
          title = "Novo tipo de equipamento",
          easyClose = TRUE,
          size = 'l',
          footer = list(
            actionButton(ns('cancelar_modal_tipo'), "Fechar", icon = icon('arrow-right-from-bracket')),
            hidden(actionButton(ns('deletar_tipo'), "Deletar", icon = icon('trash-can'))),
            actionButton(ns('salvar_tipo'), "Salvar", icon = icon('floppy-disk'))
          ),
          fluidPage(
            column(4,
                   textInput(ns('nome_tipo'), "", placeholder = "Nome do tipo"),
                   textInput(ns('descricao_tipo'), "", placeholder = "Descrição")
            ),
            column(8, reactableOutput(ns('tbl_tipos'))
            )
          )
        )
      )
    })
    
    # Render | tbl_tipos ------------------------------------------------
    output$tbl_tipos <-  renderReactable({
      
      altura <- 'auto'
      if (nrow(dados$tipo_equipamento) > 3) {
        altura <- 150
      }
      
      dados$tipo_equipamento %>% 
        reactable(
          onClick = "select", 
          selection = 'single',
          highlight = TRUE,
          height = altura,
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
          columns = list(
            nome = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Tipo de equipamento", align = "left"),
            descricao =  colDef(minWidth = 100, maxWidth = 250, name = "Descrição", align = "center"),
            id = colDef(show = FALSE),
            .selection = colDef(show = FALSE)
          )
        )
    })
    
    # Observe |  tbl_tipos__reactable__selected --------------------------
    observeEvent(input$tbl_tipos__reactable__selected, {
      
      if (!is.null(input$tbl_tipos__reactable__selected)) {
        
        aux <-
          dados$tipo_equipamento %>% 
          slice(input$tbl_tipos__reactable__selected)
        
        dados$tipo_equipamento_id_sel <- aux$id
        
        updateTextInput(inputId = 'nome_tipo', value = aux$nome)
        updateTextInput(inputId = 'descricao_tipo', value = aux$descricao)
        updateActionButton(inputId = 'salvar_tipo', label = "Salvar alteração")
        shinyjs::show('deletar_tipo')
        
      } else {
        
        dados$tipo_equipamento_id_sel <- NULL
        shinyjs::reset('nome_tipo')
        shinyjs::reset('descricao_tipo')
        updateActionButton(inputId = 'salvar_tipo', label = "Salvar")
        shinyjs::hide('deletar_tipo')
        
      }
    }, ignoreNULL = FALSE)
    
    # Observe | nome_tipo -----------------------------------------------
    observeEvent(input$nome_tipo, {
      
      dados$teste_nome_tipo <- TRUE
      
      if (is.null(dados$tipo_equipamento_id_sel)) {
        
        if (input$nome_tipo %in% dados$tipo_equipamento$nome) {
          
          shinyFeedback::feedbackDanger('nome_tipo', text = "Tipo de local existente, não será possível salvar", show = TRUE)
          dados$teste_nome_tipo <- FALSE
          
        } else {
          
          shinyFeedback::feedbackDanger('nome_tipo', show = FALSE)
          
        }
        
      }
      
    })
    
    # Observe | salvar_tipo --------------------------------------
    observeEvent(input$salvar_tipo, {
      
      if (dados$teste_nome_tipo) {
        
        tabela <- data.frame(
          id = NA,
          nome = input$nome_tipo, 
          descricao = input$descricao_tipo
        ) %>% 
          mutate(id = dados$tipo_equipamento_id_sel)
        
        tryCatch({
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$tipo_equipamento_id_sel)) {
            
            dbx::dbxInsert(conn, 'tipo_equipamento', tabela)
            
          } else {
            
            dbx::dbxUpdate(conn, 'tipo_equipamento', tabela, where_cols = 'id')
            
          }
          
          dados$tipo_equipamento <- get_tipo_equipamento(user$pool)
          
          shinyjs::reset("nome_tipo")
          shinyjs::reset("descricao_tipo")
          poolReturn(conn)
          removeModal()
          
          shinyalert::shinyalert(
            type = 'success', 
            title = "Tipo de equipamento salvo")
          
          
        }, error = function(e) {
          shinyalert::shinyalert(
            type = 'error', 
            title = "Erro ao salvar tipo de equipamento",
            text = paste0(e))
        })
        
      }
      
    })
    
    observeEvent(input$cancelar_modal_tipo, {
      
      removeModal()
      
    })
    
    # Observe | deletar_tipo ----------------------
    observeEvent(input$deletar_tipo, {
      
      tryCatch({
        
        conn <- poolCheckout(user$pool)
        
        dbx::dbxDelete(conn, 'tipo_equipamento', where = data.frame(id = dados$tipo_equipamento_id_sel))
        poolReturn(conn)
        
        shinyalert::shinyalert(
          type = 'success',
          title = "Tipo de equipamento deletado"
        )
        
        
      }, error = function(e) {
        
        shinyalert::shinyalert(
          type = 'error', 
          title = "Erro ao deletar tipo de equipamento", 
          text = paste0(e)
        )
        
      })
      
    })
    
    
    
    
    
  })
  
}

## To be copied in the UI
# mod_cadastro_equipamentos_ui("cadastro_equipamentos")

## To be copied in the server
# mod_cadastro_equipamentos_server("cadastro_equipamentos")
