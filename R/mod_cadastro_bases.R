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
    
    # Render | index -------------------------------------------------------
    
    output$index <- renderUI({
      
      vct_tipo <- c("Radio" = "radio", "Acustica" = "acustica")
      vct_equipamentos <- c("Nenhum equipamento cadastrado" = 0)
      vct_locais <- c("Nenhum local cadastrado" = 0)
      
      dados$locais <- select_locais(user$con)
      
      if (nrow(dados$locais) != 0) {
        vct_locais <- dados$locais$id %>% setNames(dados$locais$nome)
      }
      
      fluidPage(
        tags$h3("Cadastro de bases fixas"),
        column(
          width = 3,
          tags$div(
            class = 'cadastro_base',
            tags$fieldset(
              class = 'input_box',
              tags$legend("Equipamento"),
              selectInput(ns('tipo'), "Tipo da base", choices = vct_tipo),
              textInput(ns('nome'), "", placeholder = "Nome da base"),
              
              tags$div(
                class = "display_flex_row select_plus_button",
                selectInput(ns('local_id'), "Local", choices = vct_locais),
                actionButton(ns('btn_novo_local'), "", icon = icon("plus-circle"))
              ),
              
              tags$div(
                class = "display_flex_row select_plus_button",
                selectInput(ns('equipamento_id'), "Número de série", choices = vct_equipamentos),
                actionButton(ns('btn_novo_equipamento'), "", icon = icon("plus-circle"))
              )
            ),
            tags$fieldset(
              class = 'input_box',
              tags$legend("Datas"),
              dateInput(ns('data_inicio'), "Início do funcionamento"),
              dateInput(ns('data_fim'), "Fim do funcionamento"),
              checkboxInput(ns('base_ativa'), "Base ativa", value = TRUE)
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
    
    observeEvent(input$tipo, {
      
      dados$equipamentos <- select_equipamento(user$con, input$tipo)
      
      if (nrow(dados$equipamentos) != 0) {
        
        vct_equipamentos <- dados$equipamentos$id %>% setNames(dados$equipamentos$numero_serie)
        updateSelectInput(inputId = 'equipamento_id', choices = vct_equipamentos)      
        
      }
      
    })
    
    # Observe | btn_novo_local -----------------------------------------------
    observeEvent(input$btn_novo_local, {
      
      vct_locais <- c('base_fixa', 'soltura', 'captura', 'outro') %>% setNames(c('Base fixa', 'Soltura', 'Captura', 'Outro'))
      
      showModal(
        modalDialog(
          title = NULL,
          footer = NULL,
          easyClose = TRUE,
          size = "l",
          fluidPage(
            tags$h3("Cadastro de local"),
            column(
              width = 4,
              tags$fieldset(
                class = 'input_box',
                tags$legend("Identificação"),
                textInput(ns('nome_local'), "", placeholder = "Nome do local"),
                selectInput(ns('tipo_local'), "Tipo do local", choices = vct_locais)
              ),
              tags$fieldset(
                class = 'input_box',
                tags$legend("Localização"),
                textInput(ns('latitude'), "", placeholder = "Latitude"),
                textInput(ns('longitude'), "", placeholder = "Longitude")
              ),
              actionButton(ns('salvar_local'), "Salvar")
            ),
            column(
              width = 8,
              leafletOutput(ns('mapa'), height = '400px'),
              reactableOutput(ns('tbl_locais'), height = '200px'),
              textOutput(ns('click_data'))
            )
            
          )
        )
      )
      
      
      # Render | mapa ----------------------------------------------------
      
      output$mapa <- renderLeaflet({
        
        lng <- -52
        lat <- -15
        zoom <- 4
        
        if (!is.null(user$info_projeto_sel)) {
          if (nrow(user$info_projeto_sel) > 0) {
            lng <- user$info_projeto_sel$lon
            lat <- user$info_projeto_sel$lat
            zoom <- 8
          }
        }
        
        leaflet::leaflet() %>%
          setView(lng = lng, lat = lat, zoom = zoom) %>% 
          addTiles(group = "Open Streat Map") %>%
          addProviderTiles(providers$CartoDB.DarkMatter, group = "Escuro") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>% 
          addLayersControl(
            baseGroups = c("Open Streat Map", "Escuro", "Satelite"),
            #overlayGroups = c("Quakes", "Outline"),
            options = layersControlOptions(collapsed = FALSE)
          )
      })
      
      
      
    })
    
    
    #Observe | btn_novo_equipamento ------------------------------------------
    observeEvent(input$btn_novo_equipamento, {
      
      vct_tipo <- c("Radio" = "radio", "Acustica" = "acustica")
      
      showModal(
        modalDialog(
          title = NULL,
          footer = NULL,
          easyClose = TRUE,
          size = "s",
          tagList(
            tags$h3("Novo equipamento"),
            textInput(ns('numero_serie'), "", placeholder = "Número de série"),
            selectInput(ns('tipo_equipamento'), "", choices = vct_tipo),
            textInput(ns('marca_equipamento') , "", placeholder = "Marca"),
            textInput(ns('modelo_equipamento') , "", placeholder = "Modelo"),
            actionButton(ns('salvar_equipamento'), "Salvar")
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
    
    
    # Observe | tbl__reactable__selected -----------------------------------
    observeEvent(input$tbl__reactable__selected, {
      
      if (!is.null(input$tbl__reactable__selected)) {
        
      } else {
        
      }
      
    })
    
    # Render | tbl_locais ----------------------------------------------------
    output$tbl_locais <- renderReactable({
      dados$locais %>% 
        reactable::reactable(
          onClick = "select", 
          selection = "single", 
          theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")),
          highlight = TRUE,
          outlined  =  TRUE,
          columns = list(
            nome = colDef(html = TRUE, minWidth = 100, maxWidth = 250, name = "Nome", align = "left"),
            tipo =  colDef(minWidth = 100, maxWidth = 250, name = "Tipo", align = "center"),
            lat =  colDef(minWidth = 100, maxWidth = 250, name = "Latitude", align = "center"),
            lon =  colDef(minWidth = 100, maxWidth = 250, name = "Longitude", align = "center"),
            .selection = colDef(show = FALSE),
            id = colDef(show = FALSE),
            projeto_id = colDef(show = FALSE)
          )
        )
    })
    
    
    # Observe | base_ativa ---------------------------------------------------
    observeEvent(input$base_ativa, {
      
      if (input$base_ativa) {
        shinyjs::disable('data_fim')
      } else {
        shinyjs::enable('data_fim')
      }
      
      
    })
    
    
    output$click_data <- renderText({
      
      if (!is.null(input$mapa_click)) {
        
        lat <- input$mapa_click$lat %>% round(7)
        lng <- input$mapa_click$lng %>% round(7)
        
        glue::glue("Latitude: {lat}   Longitude: {lng}")
        
      }
      
    })
    
    
    observeEvent(input$mapa_click, {
      
      leafletProxy('mapa') %>% 
        clearMarkers() %>% 
        addMarkers(lng = input$mapa_click$lng, lat = input$mapa_click$lat)
      
      updateTextInput(inputId = 'latitude', value = input$mapa_click$lat)
      updateTextInput(inputId = 'longitude', value = input$mapa_click$lng)
      
      
    })
    
    # Observe | salvar_local -------------------------------------------------------
    observeEvent(input$salvar_local, {
      
      id_local <- NA
      upsert_local(user$con, id_local, user$info_projeto_sel$id, input$nome_local, input$tipo_local, input$latitude, input$longitude)
      shinyalert::shinyalert("Local salvo!", type = "success")
      dados$locais <- select_locais(user$con)
      vct_locais <- dados$locais$id %>% setNames(dados$locais$nome)
      updateSelectInput(inputId = 'local_id', choices = vct_locais)      
      removeModal()
      
    })
    
    
    # Observe | salvar_equipamento -------------------------------------------------------
    observeEvent(input$salvar_equipamento, {
      
      id_equipamento <- NA
      upsert_equipamenoto(user$con, id_equipamento, input$numero_serie, input$tipo_equipamento, input$marca_equipamento, input$modelo_equipamento)
      shinyalert::shinyalert("Equipamento salvo!", type = "success")
      dados$equipamentos <- select_equipamento(user$con, input$tipo)
      vct_equipamentos <- dados$equipamentos$id %>% setNames(dados$equipamentos$numero_serie)
      updateSelectInput(inputId = 'equipamento_id', choices = vct_equipamentos)      
      removeModal()
      
    })
    
    # Observe | salvar -------------------------------------------------------
    observeEvent(input$salvar, {
      
      id <- NA
      upsert_base(user$con, id, user$info_projeto_sel$id, input$nome, input$local_id, input$equipamento_id, input$data_hora_ini, input$data_hora_fim)
      shinyalert::shinyalert("Base salva!", type = "success")
      removeModal()
      
      
    })
    
    
  })
  
}

#' Upserte de base
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
upsert_base <- function(con, id, projeto_id, nome_base, local_id, equipamento_id, data_hora_ini, data_hora_fim) {
  
  df <- data.frame(
    id = id, 
    projeto_id = projeto_id, 
    nome_base = nome_base, 
    local_id = local_id, 
    equipamento_id = equipamento_id, 
    data_hora_ini = data_hora_ini, 
    data_hora_fim = data_hora_fim
  )
  
  dbx::dbxUpsert(con,"bases", df, "id")
  
}

#' Upsert de equipamento
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
upsert_equipamenoto <- function(con, id,numero_serie,tipo,marca,modelo) {
  df <- data.frame(
    id = id,
    numero_serie = numero_serie,
    tipo = tipo, 
    marca = marca,
    modelo = modelo
  )
  
  dbx::dbxUpsert(con,"equipamentos", df, "id")
  
}

#' Upsert de local
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
upsert_local <- function(con, id, projeto_id, nome, tipo, lat, lon) {
  df <- data.frame(
    id = id,
    projeto_id = projeto_id,
    nome = nome, 
    tipo = tipo,
    lat = lat,
    lon = lon
  )
  
  dbx::dbxUpsert(con,"locais", df, "id")
  
}

#' Select de local
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
select_equipamento <- function(con, tipo = NULL) {
  
  query <- "select * from equipamentos"
  
  if (!is.null(tipo)) {
    query <- glue::glue("{query} where tipo = '{tipo}'")
  }
  
  dbx::dbxSelect(con, query) 
  
}

#' Select de local
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#'
#' @noRd
select_locais <- function(con, tipo = NULL) {
  
  query <- "select * from locais"
  
  if (!is.null(tipo)) {
    query <- glue::glue("{query} where tipo = '{tipo}'")
  }
  
  dbx::dbxSelect(con, query) 
  
}
