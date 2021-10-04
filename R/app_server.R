#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  r = reactiveValues()
  
  # Your application server logic 
  # router <- shiny.router::make_router(
  #   route("/", home_page),
  #   route("browse-topics", mod_browse_topics_ui("browse"))
  # )
  # 
  # observeEvent(router, {
  #   r$router = router
  # })
  mod_start_server("start")
  mod_browse_topics_server("browse")
  mod_popular_by_year_server("popular")
  
  router$server(input, output, session)
    
}
