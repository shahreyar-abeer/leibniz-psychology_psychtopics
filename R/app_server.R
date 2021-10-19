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
  
  # data ----
  # result of topic modeling (LDA.R)
  r$theta_year <- isolate(readRDS("inst/data/theta_year.rds")) # theta_mean_by_year with labels instead of topic numbers
  r$theta_mean_by_year <- isolate(readRDS("inst/data/theta_mean_by_year.rds")) # mean theta of topic by year
  r$theta_mean_by_year_time <- isolate(readRDS("inst/data/theta_mean_by_year_time.rds")) # for trend analysis
  r$theta_mean_by_year_ts <- isolate(readRDS("inst/data/theta_mean_by_year_ts.rds")) # for trend analysis
  r$years <- isolate(readRDS("inst/data/years.rds")) # a list of publication years
  r$topic <- isolate(readRDS("inst/data/topic.rds")) # a list of topics and top terms
  r$booster <- isolate(readRDS("inst/data/booster.rds")) # a table with factors for term boosting in PubPsych.eu
  r$k <- 325 # set number of topics in the model (all topics, not only the reliable ones)
  
  
  
  
  mod_start_server("start", r)
  mod_browse_topics_server("browse", r)
  mod_popular_by_year_server("popular")
  
  router$server(input, output, session)
    
}
