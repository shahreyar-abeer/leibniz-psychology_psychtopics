#' methods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_methods_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shiny.fluent::Stack(
      horizontal = TRUE,
      div(
        class = "ms-Grid-col ms-sm6 ms-xl6",
        makeCard(
          title = "Topic Identification",
          size = 12,
          content = tagList(
            
          )
        )
      ),
      
      div(
        class = "ms-Grid-col ms-sm6 ms-xl6",
        shiny.fluent::Stack(
          div(
            class = "ms-Grid-col ms-sm12 ms-xl12",
            makeCard(
              title = "Topic Identification",
              size = 12,
              content = tagList(
                
              )
            )
          ),
          div(
            class = "ms-Grid-col ms-sm12 ms-xl12",
            makeCard(
              title = "Topic Identification",
              size = 12,
              content = tagList(
                
              )
            )
          )
        )
      )
    )
  )
}
    
#' methods Server Functions
#'
#' @noRd 
mod_methods_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_methods_ui("methods_ui_1")
    
## To be copied in the server
# mod_methods_server("methods_ui_1")
