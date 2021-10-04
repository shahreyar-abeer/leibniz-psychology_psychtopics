#' popular_by_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_popular_by_year_ui <- function(id){
  ns <- NS(id)
  tagList(
    makeCard(
      title = "Browse PSYNDEX Topics",
      content = tagList(
        shiny.fluent::Slider.shinyInput(inputId = ns("slider"), value = 10),
        uiOutput(ns("text"))
      ),
      size = 4
    ),
    
    makeCard(
      title = "Browse PSYNDEX Topics",
      content = tagList(
        Text("This is another card")
      ),
      size = 4
    )
  )
}

    
#' popular_by_year Server Functions
#'
#' @noRd 
mod_popular_by_year_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$text = renderUI({
      shiny.fluent::Text(glue::glue("The value of slider is {input$slider}"))
    })
  })
}
    
## To be copied in the UI
# mod_popular_by_year_ui("popular_by_year_ui_1")
    
## To be copied in the server
# mod_popular_by_year_server("popular_by_year_ui_1")
