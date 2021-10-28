#' hot_cold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hot_cold_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    div(
      class = "browse-cards",
      
      makeCard(
        title = "Browse PSYNDEX Topics",
        style = "background-color: #c6cf78ff",
        content = tagList(
          
          bodyText("Here you can browse all topics included in the model."),
          br(),
          bodyText("Select topics in the table below for adding them to the plots."),
          br(),
          bodyText("For Trends, only records from 1980 to [current_year - 1] are included, since publications of the current year are not yet fully covered. (learn more)")
        ),
        
        
        
        size = 11
      ),
      makeCard(
        title = "Topic Trends",
        size = 11,
        content = tagList(
          echarts4r::echarts4rOutput(ns("plot_box2"))
        )
      ),
      makeCard(
        title = "Share of Empirical Research",
        content = tagList(
          tagList(
            verbatimTextOutput(ns("p1"))
          )
        ),
        size = 11
      )
    ),
    
    br(),
    
    div(
      class = "start-cards",
      makeCard(
        title = "Share of Empirical Research",
        content = tagList(
          tagList(
            verbatimTextOutput(ns("p3"))
          )
        ),
        size = 11
      ),
      
      makeCard(
        title = "Share of Empirical Research",
        content = tagList(
          tagList(
            verbatimTextOutput(ns("p2"))
          )
        ),
        size = 11
      )
    )
    
    
  )
  
}
    
#' hot_cold Server Functions
#'
#' @noRd 
mod_hot_cold_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # trends function
    trends <- reactive({
      trends.ab(2015-1979, 2019-1979, 
                r$theta_year, r$theta_mean_by_year, r$theta_mean_by_year_time,
                r$theta_mean_by_year_ts, r$years, r$topic)
      
    })
    
    
    print("in hot/cold")
    #print(str(trends()))
    output$p1 = renderPrint({
      str(trends())
    })
    
  })
}
    
## To be copied in the UI
# mod_hot_cold_ui("hot_cold_ui_1")
    
## To be copied in the server
# mod_hot_cold_server("hot_cold_ui_1")
