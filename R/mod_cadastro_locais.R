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
      
      dados$locais <- get_locais(user$pool, user$info_projeto_sel$id)
      
      vct_tipos <- c("Marcação" = 'marcacao', "Base fixa" = 'base_fixa', "Soltura" = 'soltura', "Outro" = 'outro')
      
      fluidPage(
        tags$h3("Cadastro de locais"),
        column(
          width = 3,
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Identificação"),
            textInput(ns('nome'), "", placeholder = "Nome do local"),
            selectInput(ns('tipo'), "Tipo do local", choices = vct_tipos)
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
            tipo =  colDef(minWidth = 100, maxWidth = 250, name = "Tipo do local", align = "center"),
            lat =  colDef(minWidth = 100, maxWidth = 250, name = "Latitude", align = "center"),
            lon =  colDef(minWidth = 100, maxWidth = 250, name = "Longitude", align = "center"),
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
          dados$locais %>% slice(input$tbl__reactable__selected)
        
        if (nrow(aux) != 0) {
          
          dados$id_clicado <- aux$id
          updateTextInput(inputId = "nome", value = aux$nome)
          updateSelectInput(inputId = "tipo", selected = aux$tipo)
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
        
        dados$id_clicado <- NULL
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
    observeEvent(input$nome, {if (input$nome != "") {campo_obrigatorio_feedback("nome", FALSE)}}, ignoreNULL = TRUE)
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
      
      
      if (all(c(nome, latitude, longitude))) {

        tabela <- data.frame(
          id = NA,
          projeto_id = user$info_projeto_sel$id,
          nome = input$nome, 
          tipo = input$tipo,
          lat = input$latitude, 
          lon = input$longitude
        ) %>%  mutate(id = dados$id_clicado)
        
        tryCatch({
          
          conn <- poolCheckout(user$pool)
          
          if (is.null(dados$id_clicado)) {
            
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
        
        dbx::dbxDelete(conn, 'locais', where = data.frame(id = dados$id_clicado))
        dados$locais <- get_locais(user$pool, user$info_projeto_sel$id) 
        
        poolReturn(conn)
        
        shinyalert::shinyalert(type = 'success', title = "Local deletado")
        
        
      }, error = function(e) {
        shinyalert::shinyalert(type = 'error', title = "Erro ao deletar locais", text = paste0(e))
      })
      
      
    })
    
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_cadastro_locais_ui("cadastro_locais_1")

## To be copied in the server
# mod_cadastro_locais_server("cadastro_locais_1")
