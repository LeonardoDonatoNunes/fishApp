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
      
      vct_projetos <- c("Nenhum projeto disponível" = 0)
      if (nrow(user$info_projeto) > 0) {
        vct_projetos <- user$info_projeto$id %>% setNames(user$info_projeto$nome)
      }
      
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
            tags$legend("Associar projeto"),
            selectInput(ns('projeto'), "Projeto", multiple = TRUE,  choices = vct_projetos)          ),
          
          tags$fieldset(
            class = 'input_box',
            tags$legend("Localização"),
            textInput(ns('latitude'), "", placeholder = "Latitude"),
            textInput(ns('longitude'), "", placeholder = "Longitude"),
            actionLink(ns('limpar_coordenadas'), "Limpar coordenadas", icon = icon('broom'))
          ),
          
          tags$div(
            class = 'display_flex_row',
            hidden(actionButton(ns('deletar'), "Deletar", icon = icon('trash-alt'))),
            actionButton(ns('salvar'), "Salvar", icon = icon('save'))
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
            leafletOutput(ns('mapa_principal'))
          )
        )
      )
    })
    
    
    # Render | tbl ---------------------------------------------------------------
    output$tbl <- renderReactable({
      
      user$info_projeto %>% 
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
            nome = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Nome", align = "left"),
            sigla =  colDef(minWidth = 100, maxWidth = 250, name = "Sigla", align = "center"),
            cidade =  colDef(minWidth = 100, maxWidth = 250, name = "Cidade", align = "center"),
            estado =  colDef(minWidth = 100, maxWidth = 250, name = "UF", align = "center"),
            lat =  colDef(minWidth = 100, maxWidth = 250, name = "Latitude", align = "center"),
            lon =  colDef(minWidth = 100, maxWidth = 250, name = "Longitude", align = "center"),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE)
          )
        )
    })
    
    # Observe | tbl__reactable__selected -------------------------------------
    observeEvent(input$tbl__reactable__selected, {
      
    }, ignoreNULL = FALSE)
    
    
    # Render | mapa_principal ----------------------------------------------
    output$mapa_principal <- renderLeaflet({
      
      lng_tbl <- -52
      lat_tbl <- -15
      
      lat <- user$info_projeto_sel$lat
      lng <- user$info_projeto_sel$lon
      
      if (nrow(user$info_projeto) > 0) {
        
        lng_tbl <- as.numeric(user$info_projeto$lon)
        lat_tbl <- as.numeric(user$info_projeto$lat)
        label <- user$info_projeto$sigla
        
      }
      
      
      leaflet::leaflet() %>%
        leaflet::setView(lng = lng, lat = lat, zoom = 8) %>% 
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
    
    
    
    
  })
}

## To be copied in the UI
# mod_cadastro_locais_ui("cadastro_locais_1")

## To be copied in the server
# mod_cadastro_locais_server("cadastro_locais_1")
