#' browse-topics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_browse_topics_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 17),
      makeCard(
        title = "Browse PSYNDEX Topics",
        content = Text("Here you can browse all topics included in the model."),
        size = 6
      ),
      makeCard(
        title = "Topic Trends",
        content = Text("Here you can browse all topics included in the model."),
        size = 4
      ),
      makeCard(
        title = "Share of Empirical Researc",
        content = Text("Here you can browse all topics included in the model."),
        size = 4
      )
    )
    
  )
}
    
#' browse-topics Server Functions
#'
#' @noRd 
mod_browse_topics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_browse-topics_ui("browse-topics_ui_1")
    
## To be copied in the server
# mod_browse-topics_server("browse-topics_ui_1")
