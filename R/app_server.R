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
  r$theta_year <- isolate(readRDS("inst/data/theta_year.RDS")) # theta_mean_by_year with labels instead of topic numbers
  r$theta_mean_by_year <- isolate(readRDS("inst/data/theta_mean_by_year.RDS")) # mean theta of topic by year
  r$theta_mean_by_year_time <- isolate(readRDS("inst/data/theta_mean_by_year_time.RDS")) # for trend analysis
  r$theta_mean_by_year_ts <- isolate(readRDS("inst/data/theta_mean_by_year_ts.RDS")) # for trend analysis
  r$years <- isolate(readRDS("inst/data/years.RDS")) # a list of publication years
  r$topic <- isolate(readRDS("inst/data/topic.RDS")) # a list of topics and top terms
  r$booster <- isolate(readRDS("inst/data/booster.RDS")) # a table with factors for term boosting in PubPsych.eu
  r$n_doc_year = isolate(readRDS("inst/data/n_docs_year.RDS")) %>% 
    as.table() %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      year = as.numeric(as.character(Var1)),
      label = Var2,
      id = rep(1:199, each = 40),
      Freq = round(Freq, 2)
    )
  r$empirical = isolate(readRDS("inst/data/empirical_year.RDS")) %>% 
    as.table() %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      year = as.numeric(as.character(Var1)),
      label = Var2,
      id = rep(1:199, each = 40),
      Freq = round(Freq, 2)
    )
  r$topic_evo = readRDS("./inst/data/topic_evo.RDS")
  r$k <- 325 # set number of topics in the model (all topics, not only the reliable ones)
  
  
  
  ## update these two lines by hand
  r$last_updated = "18th Oct, 2021"
  r$current_year = 2019
  
  
  mod_start_server("start", r)
  mod_browse_topics_server("browse", r)
  mod_popular_by_year_server("popular", r)
  mod_hot_cold_server("hot_cold", r)
  mod_topic_evol_server("topic_evol", r)
  mod_methods_server("methods")
  
  router$server(input, output, session)
    
}
