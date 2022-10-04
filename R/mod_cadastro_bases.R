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
    
    # Controle de alterações -----------------------------------------------
    atualizar_pagina <- reactiveValues(status=0)
    observeEvent(user$sidebar, {
      
      if (user$sidebar == id) {
        
        return_controle_alt <-  confirmar_alteracao(user$controle_alteracoes, id)
        
        if (return_controle_alt$atualizar_pagina) {
          atualizar_pagina$status <- isolate(atualizar_pagina$status + 1)
        }
        
        user$controle_alteracoes <- return_controle_alt$historico_alteracoes
        
      }
      
    })
    
    
    # Render | index -------------------------------------------------------
    
    output$index <- renderUI({
      
      atualizar_pagina$status
      input$salvar
      
      dados$bases_fixas <- get_bases_fixas(user$pool, user$info_projeto_sel)
      equipamentos_indisponiveis <- NULL
      
      vct_locais <- c("Nenhum local cadastrado" = 0)
      dados$locais <- get_locais(user$pool, user$info_projeto_sel$id)
      
      if (nrow(dados$locais) != 0) {
        
        vct_locais <- dados$locais$id %>% setNames(dados$locais$nome)
        
        equipamentos_indisponiveis <- 
          dados$bases_fixas %>% 
          filter(is.na(data_hora_fim)) %>% 
          pull(equipamento_id)
        
      }
      
      
      vct_equipamentos <- c("Nenhum equipamento disponível" = 0)
      dados$equipamentos <- get_equipamentos(user$pool, user$info_projeto_sel$id)
      
      if (!is.null(equipamentos_indisponiveis)) {
        
        dados$equipamentos <-
          isolate(dados$equipamentos %>% 
                    filter(!id %in% equipamentos_indisponiveis))
        
      }
      
      if (nrow(dados$equipamentos) != 0) {
        
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
              selectInput(ns('equipamento_id'), "Número de série", choices = vct_equipamentos)
            ),
            
            tags$fieldset(
              class = 'input_box',
              tags$legend("Datas"),
              tags$span("Início da operação"),
              div(class = 'display_flex_row',
                  dateInput(ns('data_inicio'), "Data", width = '50%'),
                  timeInput(ns('hora_inicio'), "Hora", seconds = FALSE)
              ),
              
              tags$span("Fim da operação"),
              div(class = 'display_flex_row',
                  disabled(dateInput(ns('data_fim'), "Data", width = '50%')),
                  disabled(timeInput(ns('hora_fim'), "Hora", seconds = FALSE, value = strptime("23:59:59", "%T")))
              ),
              checkboxInput(ns('base_ativa'), "Base ativa", value = TRUE)
            ),
            tags$div(
              class = 'display_flex_row',
              hidden(actionButton(ns('deletar'), "Deletar", icon = icon('trash-can'))),
              actionButton(ns('salvar'), "Salvar", icon = icon('floppy-disk'))
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
        shinyjs::disable('hora_fim')
      } else {
        shinyjs::enable('data_fim')
        shinyjs::enable('hora_fim')
      }
      
      
    })
    
    
    # Controle | feedback ----------------------------------------------------
    observeEvent(input$nome, {
      
      if (input$nome != '') {campo_obrigatorio_feedback('nome', FALSE)}
      
      dados$teste_nome <- TRUE
      
      if (is.null(dados$base_id_sel)) {
        
        if (input$nome %in% dados$bases_fixas$nome) {
          
          shinyFeedback::feedbackDanger('nome', text = "Base existente, não será possível salvar", show = TRUE)
          dados$teste_nome <- FALSE
          
        } else {
          
          shinyFeedback::feedbackDanger('nome', show = FALSE)
          
        }
        
      }
      
    })
    
    
    
    # Observe | salvar -------------------------------------------------------
    observeEvent(input$salvar, {
      
      nome <- TRUE
      
      if (input$nome == "") {nome <- FALSE; campo_obrigatorio_feedback("nome", TRUE)}
      
      if (nome) {
        
        hora_inicio_fmt <- strftime(input$hora_inicio, format = "%H:%M:%S")
        hora_fim_fmt <- strftime(input$hora_fim, format = "%H:%M:%S")
        
        data_hora_ini <- paste(input$data_inicio, hora_inicio_fmt) %>% as.POSIXct(tz = 'GMT')
        data_hora_fim <- paste(input$data_fim, hora_fim_fmt) %>% as.POSIXct(tz = 'GMT')
        
        
        if (input$base_ativa) {
          data_hora_fim <- NA_POSIXct_
        }
        
        
        tabela <- data.frame(
          id = NA,
          projeto_id = user$info_projeto_sel$id,
          nome = input$nome,
          local_id = input$local_id,
          equipamento_id = input$equipamento_id,
          data_hora_ini = data_hora_ini,
          data_hora_fim = data_hora_fim
        ) %>% 
          mutate(
            id = dados$base_id_sel
          )
        
        tryCatch({        
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$base_id_sel)) {
            
            dbx::dbxInsert(conn, 'bases_fixas', tabela)
            
          } else {
            
            dbx::dbxUpdate(conn, 'baes_fixas', tabela)
            
          }
          
          shinyjs::reset("nome")
          shinyjs::reset("data_inicio")
          shinyjs::reset("hora_inicio")
          shinyjs::reset("data_fim")
          shinyjs::reset("hora_fim")
          shinyjs::reset("base_ativa")
          
          
          shinyalert::shinyalert("Base fixa salva!", type = "success")
          poolReturn(conn)
          
        }, error = function(e) {shinyalert::shinyalert(type = 'error', title = "Erro ao salvar base fixa", text = paste0(e))})
        
      }
      
    })
    
    
  })
  
}
