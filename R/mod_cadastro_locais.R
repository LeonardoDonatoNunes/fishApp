#' cadastro_locais UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadastro_locais_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('index'))
  )
}

#' cadastro_locais Server Functions
#'
#' @noRd 
mod_cadastro_locais_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dados <- reactiveValues()
    
    # Index -----------------------------------------------------------------
    output$index <- renderUI({
      
      input$salvar_tipo
      input$deletar_tipo
      
      dados$locais <- get_locais(user$pool, user$info_projeto_sel$id)
      dados$tipo_local <- get_tipo_local(user$pool)
      
      vct_tipos <- c("Nenhum tipo cadastrado" = 0)
      
      if (nrow(dados$tipo_local) != 0) {
        
        vct_tipos <- dados$tipo_local$id %>% setNames(dados$tipo_local$nome)
        
      }
      
      
      fluidPage(
        tags$h3("Cadastro de locais"),
        column(
          width = 3,
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Identificação"),
            textInput(ns('nome'), "", placeholder = "Nome do local"),
            textInput(ns('descricao'), "", placeholder = "Descrição do local"),
            selectInput(ns('tipo'), "Tipo do local", choices = vct_tipos),
            actionLink(ns('novo_tipo_local'), "Adicionar tipo de local", icon = icon("circle-plus"))
            
          ),
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Localização"),
            textInput(ns('latitude'), "", placeholder = "Latitude"),
            textInput(ns('longitude'), "", placeholder = "Longitude"),
            actionLink(ns('limpar_coordenadas'), "Limpar coordenadas", icon = icon('broom'))
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
            tags$legend("Locais cadastrados"),
            reactableOutput(ns('tbl'))
          ),
          tags$fieldset(
            class = 'input_box',
            tags$legend("Localização dos locais cadastrados"),
            leafletOutput(ns('mapa_principal'), height = 500)
          )
        )
      )
    })
    
    
    # Render | tbl ---------------------------------------------------------------
    output$tbl <- renderReactable({
      
      dados$locais %>% 
        reactable::reactable(
          onClick = "select", 
          selection = "single", 
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
          highlight = TRUE,
          outlined  =  TRUE,
          pagination = TRUE,
          height = 200,
          resizable = TRUE,
          showPageSizeOptions = TRUE,
          columns = list(
            nome = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Nome", align = "left"),
            tipo_nome =  colDef(minWidth = 100, maxWidth = 250, name = "Tipo do local", align = "center"),
            lat =  colDef(minWidth = 100, maxWidth = 250, name = "Latitude", align = "center"),
            lon =  colDef(minWidth = 100, maxWidth = 250, name = "Longitude", align = "center"),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE),
            projeto_id = colDef(show = FALSE),
            tipo_id = colDef(show = FALSE)
          )
        )
    })
    
    # Observe | tbl__reactable__selected -------------------------------------
    observeEvent(input$tbl__reactable__selected, {
      
      if (!is.null(input$tbl__reactable__selected)) {
        
        aux <- 
          dados$locais %>% slice(input$tbl__reactable__selected)
        
        if (nrow(aux) != 0) {
          
          dados$local_id_sel <- aux$id
          updateTextInput(inputId = "nome", value = aux$nome)
          updateTextInput(inputId = "descricao", value = aux$descricao)
          updateSelectInput(inputId = "tipo", selected = aux$tipo_id)
          updateTextInput(inputId = "latitude", value = aux$lat)
          updateTextInput(inputId = "longitude", value = aux$lon)
          updateActionButton(inputId = 'salvar', label = "Salvar alterações")
          show('deletar')
          
          leafletProxy('mapa_principal') %>% 
            addAwesomeMarkers(
              layerId = 'projeto_clicado', 
              lng = as.numeric(aux$lon), 
              lat = as.numeric(aux$lat), 
              icon = icons("red"), 
              group = 'projetos'
            )
          
        }
        
      } else {
        
        dados$local_id_sel <- NULL
        updateTextInput(inputId = "nome", value = "")
        updateTextInput(inputId = "latitude", value = "")
        updateTextInput(inputId = "longitude", value = "")
        updateActionButton(inputId = 'salvar', label = "Salvar")
        hide('deletar')
        leafletProxy('mapa_principal') %>% 
          removeMarker('projeto_clicado')
      }
      
    }, ignoreNULL = FALSE)
    
    
    # Render | mapa_principal ----------------------------------------------
    output$mapa_principal <- renderLeaflet({
      
      input$cancelar_modal_tipo
      input$salvar_tipo
      
      lng_tbl <- -52
      lat_tbl <- -15
      label <- "geral"
      
      lat <- user$info_projeto_sel$lat
      lng <- user$info_projeto_sel$lon
      
      
      if (nrow(dados$locais) > 0) {
        
        lng_tbl <- as.numeric(dados$locais$lon)
        lat_tbl <- as.numeric(dados$locais$lat)
        label <- dados$locais$nome
        
      }
      
      
      leaflet::leaflet() %>%
        leaflet::setView(lng = lng, lat = lat, zoom = 12) %>% 
        addTiles(group = "Open Streat Map") %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Escuro") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>% 
        addAwesomeMarkers(
          lng = lng_tbl, 
          lat = lat_tbl,
          icon = icons("green"), 
          group = 'projetos'
        ) %>% 
        addLayersControl(
          baseGroups = c("Open Streat Map", "Escuro", "Satelite"),
          overlayGroups = 'projetos',
          options = layersControlOptions(collapsed = TRUE)
        )
      
    })
    
    
    # Observe | mapa_principal_click ----------------------------------------
    observeEvent(input$mapa_principal_click, {
      
      leafletProxy('mapa_principal') %>% 
        removeMarker(layerId = 'captura_coordenada') %>% 
        addAwesomeMarkers(
          layerId = 'captura_coordenada', 
          lng = input$mapa_principal_click$lng,
          lat = input$mapa_principal_click$lat, 
          icon = icons('orange')
        )
      
      updateTextInput(inputId = 'latitude', value = input$mapa_principal_click$lat)
      updateTextInput(inputId = 'longitude', value = input$mapa_principal_click$lng)
      
      
    })
    
    # Observe | limpar_coordenadas -----------------------------------------
    observeEvent(input$limpar_coordenadas, {
      
      leafletProxy('mapa_principal') %>% 
        removeMarker(layerId = 'captura_coordenada')
      
      shinyjs::reset('latitude')
      shinyjs::reset('longitude')
      
    })
    
    
    
    # Controle | feedback ----------------------------------------------------
    observeEvent(input$nome, {
      
      if (input$nome != "") {campo_obrigatorio_feedback("nome", FALSE)}
      
      dados$teste_nome <- TRUE
      
      if (is.null(dados$local_id_sel)) {
        
        if (input$nome %in% dados$locais$nome) {
          
          shinyFeedback::feedbackDanger('nome', text = "Local existente", show = TRUE)
          dados$teste_nome <- FALSE
          
        } else {
          
          shinyFeedback::feedbackDanger('nome', show = FALSE)
          
        }
        
      }
      
    })
    
    observeEvent(input$latitude, {if (input$latitude != "") {campo_obrigatorio_feedback("latitude", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$longitude, {if (input$longitude != "") {campo_obrigatorio_feedback("longitude", FALSE)}}, ignoreNULL = TRUE)
    
    
    # Observe | salvar -------------------------------------------------------
    observeEvent(input$salvar, {
      
      nome <- TRUE
      latitude <- TRUE 
      longitude <- TRUE
      
      
      if (input$nome == "") {nome <- FALSE; campo_obrigatorio_feedback("nome", TRUE)}
      if (input$latitude == "") {latitude <- FALSE; campo_obrigatorio_feedback("latitude", TRUE)}
      if (input$longitude == "") {longitude <- FALSE; campo_obrigatorio_feedback("longitude", TRUE)}
      
      
      if (all(c(nome, latitude, longitude, dados$teste_nome))) {
        
        tabela <- data.frame(
          id = NA,
          projeto_id = user$info_projeto_sel$id,
          nome = input$nome, 
          descricao = input$descricao,
          tipo_id = input$tipo,
          lat = input$latitude, 
          lon = input$longitude
        ) %>%  mutate(id = dados$local_id_sel)
        
        tryCatch({
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$local_id_sel)) {
            
            dbx::dbxInsert(conn, 'locais', tabela)
            
          } else {
            
            dbx::dbxUpdate(conn, 'locais', tabela, where_cols = 'id')
            
          }
          
          shinyjs::reset("nome")
          shinyjs::reset("latitude")
          shinyjs::reset("longitude")
          
          dados$locais <- get_locais(user$pool,  user$info_projeto_sel$id)
          
          shinyalert::shinyalert("Local salvo!", type = "success")
          poolReturn(conn)
          
        }, error = function(e) {shinyalert::shinyalert(type = 'error', title = "Erro ao salvar local", text = paste0(e))})
        
      }
      
    })
    
    # Observe | deletar ------------------------------------------------------
    observeEvent(input$deletar, {
      
      tryCatch({
        
        conn <- poolCheckout(user$pool)
        
        dbx::dbxDelete(conn, 'locais', where = data.frame(id = dados$local_id_sel))
        dados$locais <- get_locais(user$pool, user$info_projeto_sel$id) 
        
        poolReturn(conn)
        
        shinyalert::shinyalert(type = 'success', title = "Local deletado")
        
        
      }, error = function(e) {
        shinyalert::shinyalert(type = 'error', title = "Erro ao deletar locais", text = paste0(e))
      })
      
      
    })
    
    
    
    
    
    # Observe | novo_tipo_local ----------------------------------------------
    observeEvent(input$novo_tipo_local, {
      showModal(
        modalDialog(
          title = "Novo tipo de local",
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
            column(8, reactableOutput(ns('tbl_locais'))
            )
          )
        )
      )
    })
    
    # Render | tbl_locais ------------------------------------------------
    output$tbl_locais <-  renderReactable({
      
      altura <- 'auto'
      if (nrow(dados$tipo_local) > 3) {
        altura <- 150
      }
      
      dados$tipo_local %>% 
        reactable(
          onClick = "select", 
          selection = 'single',
          highlight = TRUE,
          height = altura,
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
          columns = list(
            nome = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Tipo de local", align = "left"),
            descricao =  colDef(minWidth = 100, maxWidth = 250, name = "Descrição", align = "center"),
            id = colDef(show = FALSE),
            .selection = colDef(show = FALSE)
          )
        )
    })
    
    # Observe |  tbl_locais__reactable__selected --------------------------
    observeEvent(input$tbl_locais__reactable__selected, {
      
      if (!is.null(input$tbl_locais__reactable__selected)) {
        
        aux <-
          dados$tipo_local %>% 
          slice(input$tbl_locais__reactable__selected)
        
        dados$tipo_local_id_sel <- aux$id
        
        updateTextInput(inputId = 'nome_tipo', value = aux$nome)
        updateTextInput(inputId = 'descricao_tipo', value = aux$descricao)
        updateActionButton(inputId = 'salvar_tipo', label = "Salvar alteração")
        shinyjs::show('deletar_tipo')
        
      } else {
        
        dados$tipo_local_id_sel <- NULL
        shinyjs::reset('nome_tipo')
        shinyjs::reset('descricao_tipo')
        updateActionButton(inputId = 'salvar_tipo', label = "Salvar")
        shinyjs::hide('deletar_tipo')
        
      }
    }, ignoreNULL = FALSE)
    
    # Observe | nome_tipo -----------------------------------------------
    observeEvent(input$nome_tipo, {
      
      dados$teste_nome_tipo <- TRUE
      
      if (is.null(dados$tipo_local_id_sel)) {
        
        if (input$nome_tipo %in% dados$tipo_local$nome) {
          
          shinyFeedback::feedbackDanger('nome_tipo', text = "Tipo de local existente", show = TRUE)
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
          mutate(id = dados$tipo_local_id_sel)
        
        tryCatch({
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$tipo_local_id_sel)) {
            
            dbx::dbxInsert(conn, 'tipo_local', tabela)
            
          } else {
            
            dbx::dbxUpdate(conn, 'tipo_local', tabela, where_cols = 'id')
            
          }
          
          shinyjs::reset("nome_tipo")
          shinyjs::reset("descricao_tipo")
          poolReturn(conn)
          
          removeModal()
          shinyjs::reset('mapa_principal')
          
        }, error = function(e) {
          shinyalert::shinyalert(
            type = 'error', 
            title = "Erro ao salvar tipo de local",
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
        
        dbx::dbxDelete(conn, 'tipo_local', where = data.frame(id = dados$tipo_local_id_sel))
        poolReturn(conn)
        
        shinyalert::shinyalert(
          type = 'success',
          title = "Tipo de local deletado"
        )
        
        
      }, error = function(e) {
        
        shinyalert::shinyalert(
          type = 'error', 
          title = "Erro ao deletar tipo de local", 
          text = paste0(e)
        )
        
      })
      
    })
    
  })
}

## To be copied in the UI
# mod_cadastro_locais_ui("cadastro_locais_1")

## To be copied in the server
# mod_cadastro_locais_server("cadastro_locais_1")
