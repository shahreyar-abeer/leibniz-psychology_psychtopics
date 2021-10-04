#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shiny.fluent
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shiny.fluent::fluentPage(
      layout(router$ui),
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny shiny.fluent
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  add_resource_path("shiny.router", system.file("www", package = "shiny.router"))
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'psychtopics'
    ),
    tags$script(type = "text/javascript", src = file.path("shiny.router", "shiny.router.js"))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

