#' cadastro_projetos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadastro_projetos_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    uiOutput(ns('index')) 
  )
}

#' cadastro_projetos Server Functions
#'
#' @noRd 
mod_cadastro_projetos_server <- function(id, user){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    dados <- reactiveValues()
    
    # Index -----------------------------------------------------------------
    output$index <- renderUI({
      
      fluidPage(
        tags$h3("Cadastro de projetos"),
        column(
          width = 3,
          tags$fieldset(
            class = 'input_box',
            tags$legend("Identificação"),
            textInput(ns('nome'), "", placeholder = "Nome do projeto"),
            textInput(ns('sigla'), "", placeholder = "Sigla")
          ),
          tags$fieldset(
            class = 'input_box',
            tags$legend("Localização"),
            textInput(ns('cidade'), "", placeholder = "Cidade"),
            textInput(ns('estado'), "", placeholder = "Estado"),
            textInput(ns('pais'), "", placeholder = "País"),
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
            tags$legend("Projetos cadastrados"),
            reactableOutput(ns('tbl'))
          ),
          tags$fieldset(
            class = 'input_box',
            tags$legend("Localização dos projetos cadastrados"),
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
      
      if (!is.null(input$tbl__reactable__selected)) {
        
        aux <- 
          user$info_projeto %>% slice(input$tbl__reactable__selected)
        
        if (nrow(aux) != 0) {
          
          dados$id_clicado <- aux$id
          updateTextInput(inputId = "nome", value = aux$nome)
          updateTextInput(inputId = "sigla", value = aux$sigla)
          updateTextInput(inputId = "cidade", value = aux$cidade)
          updateTextInput(inputId = "estado", value = aux$estado)
          updateTextInput(inputId = "pais", value = aux$pais)
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
        updateTextInput(inputId = "sigla", value = "")
        updateTextInput(inputId = "cidade", value = "")
        updateTextInput(inputId = "estado", value = "")
        updateTextInput(inputId = "pais", value = "")
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
      
      lng <- -52
      lat <- -15
      
      lng_tbl <- -52
      lat_tbl <- -15
      
      if (nrow(user$info_projeto) > 0) {
        
        lng_tbl <- as.numeric(user$info_projeto$lon)
        lat_tbl <- as.numeric(user$info_projeto$lat)
        label <- user$info_projeto$sigla
        
      }
      
      
      leaflet::leaflet() %>%
        leaflet::setView(lng = lng, lat = lat, zoom = 3) %>% 
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
    observeEvent(input$sigla, {if (input$sigla != "") {campo_obrigatorio_feedback("sigla", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$cidade, {if (input$cidade != "") {campo_obrigatorio_feedback("cidade", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$estado, {if (input$estado != "") {campo_obrigatorio_feedback("estado", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$pais, {if (input$pais != "") {campo_obrigatorio_feedback("pais", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$latitude, {if (input$latitude != "") {campo_obrigatorio_feedback("latitude", FALSE)}}, ignoreNULL = TRUE)
    observeEvent(input$longitude, {if (input$longitude != "") {campo_obrigatorio_feedback("longitude", FALSE)}}, ignoreNULL = TRUE)
    
    
    # Observe | salvar -------------------------------------------------------
    observeEvent(input$salvar, {
      
      nome <- TRUE
      sigla <- TRUE
      cidade <- TRUE
      estado <- TRUE
      pais <- TRUE
      latitude <- TRUE 
      longitude <- TRUE
      
      
      if (input$nome == "") {nome <- FALSE; campo_obrigatorio_feedback("nome", TRUE)}
      if (input$sigla == "") {sigla <- FALSE; campo_obrigatorio_feedback("sigla", TRUE)}
      if (input$cidade == "") {cidade <- FALSE; campo_obrigatorio_feedback("cidade", TRUE)}
      if (input$estado == "") {estado <- FALSE; campo_obrigatorio_feedback("estado", TRUE)}
      if (input$pais == "") {pais <- FALSE; campo_obrigatorio_feedback("pais", TRUE)}
      if (input$latitude == "") {latitude <- FALSE; campo_obrigatorio_feedback("latitude", TRUE)}
      if (input$longitude == "") {longitude <- FALSE; campo_obrigatorio_feedback("longitude", TRUE)}
      
      
      if (all(c(nome, sigla, cidade, estado, pais, latitude, longitude))) {
        
        
        tabela <- data.frame(
          id = NA,
          nome = input$nome, 
          sigla = input$sigla,
          cidade = input$cidade, 
          estado = input$estado,
          pais = input$pais,
          lat = input$latitude, 
          lon = input$longitude
        ) %>%  mutate(id = dados$id_clicado)
        
        tryCatch({
          
          if (is.null(dados$id_clicado)) {
            
            dbx::dbxInsert(user$con, 'projetos', tabela)
            
          } else {
            
            dbx::dbxUpdate(user$con, 'projetos', tabela, where_cols = 'id')
            
          }
          
          dbxSelect(user$con, 'select * from projetos')
          
          user$info_projeto <- get_projetos(user$con) 
          
          shinyjs::reset("nome")
          shinyjs::reset("sigla")
          shinyjs::reset("cidade")
          shinyjs::reset("estado")
          shinyjs::reset("pais")
          shinyjs::reset("latitude")
          shinyjs::reset("longitude")
          
          shinyalert::shinyalert("Projeto salvo!", type = "success")
          
        }, error = function(e) {shinyalert::shinyalert(type = 'error', title = "Erro ao salvar projetos", text = paste0(e))})
        
      }
      
    })
    
    # Observe | deletar ------------------------------------------------------
    observeEvent(input$deletar, {
      
      tryCatch({
        
        dbx::dbxDelete(user$con, 'projetos', where = data.frame(id = dados$id_clicado))
        user$info_projeto <- get_projetos(user$con) 
        shinyalert::shinyalert(type = 'success', title = "Projeto deletado")
        
        
      }, error = function(e) {
        shinyalert::shinyalert(type = 'error', title = "Erro ao deletar projetos", text = paste0(e))
      })
      
      
    })
    
    
    
    
  })
  
}

