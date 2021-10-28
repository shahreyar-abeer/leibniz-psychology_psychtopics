#' topic_eval UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_topic_eval_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' topic_eval Server Functions
#'
#' @noRd 
mod_topic_eval_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_topic_eval_ui("topic_eval_ui_1")
    
## To be copied in the server
# mod_topic_eval_server("topic_eval_ui_1")
