#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(
        title = tags$img(src = "www/logo_header.png"),
        dropdownMenuOutput('dropdown_upmenu_ui')
        ),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Cadastro", icon = icon('list-check'), startExpanded = TRUE,
                   menuSubItem("Projetos", tabName = 'cadastro_projetos', icon = icon('angles-right')),
                   menuSubItem("Pessoas", tabName = 'cadastro_pessoas', icon = icon('angles-right')),
                   menuSubItem("Locais", tabName = 'cadastro_locais', icon = icon('angles-right')),
                   menuSubItem("Equipamentos", tabName = 'cadastro_equipamentos', icon = icon('angles-right')),
                   menuSubItem("Bases", tabName = 'cadastro_bases', icon = icon('angles-right')),
                   menuSubItem("Espécies", tabName = 'cadastro_especies', icon = icon('angles-right')),
                   menuSubItem("Peixes", tabName = 'cadastro_peixes', icon = icon('angles-right'))
                   
          )
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = 'cadastro_projetos', mod_cadastro_projetos_ui('cadastro_projetos')),
          tabItem(tabName = 'cadastro_pessoas',  mod_cadastro_pessoas_ui('cadastro_pessoas')),
          tabItem(tabName = 'cadastro_locais',  mod_cadastro_pessoas_ui('cadastro_locais')),
          tabItem(tabName = 'cadastro_equipamentos',  mod_cadastro_pessoas_ui('cadastro_equipamentos')),
          tabItem(tabName = 'cadastro_bases', mod_cadastro_bases_ui('cadastro_bases')),
          tabItem(tabName = 'cadastro_especies',  mod_cadastro_pessoas_ui('cadastro_especies')),
          tabItem(tabName = 'cadastro_peixes', mod_cadastro_peixes_ui('cadastro_peixes'))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'www/geral.css'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'fishApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

