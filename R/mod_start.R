#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_start_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 33),
      makeCard(
        title = "About PsychTopics",
        style = "background-color: #c6cf78ff",
        content = tagList(
          bodyText(tagList("With PsychTopics, you can explore current and past research trends in psychology from the ",  tags$b("German-speaking countries."))),
          br(),
          br(),
          bodyText(tagList("Conclusions should be drawn carefully, as topics are derived from the ",
                       tags$a("PSYNDEX database", href = "http://www.psyndex.de", target = "_blank"),
                       tags$b(" automatically using machine learning algorithms."), 
                       " PsychTopics is in an experimental state, makes no claim to completeness and cannot replace specific search strategies.")),
          br(),
          br(),
          bodyText("PsychTopics is updated quarterly. Last update: [date_update")
        ),
        size = 6
      ),
      makeCard(
        title = "Tutorial video",
        content = Text("A tutorial video will be added here"),
        size = 6
      )
    ),
    
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 33),
      makeCard(
        title = uiOutput(ns("title_box3")),
        content = tagList(
          bodyText("Please note that these topics are preliminary!"),
          br(),
          plotOutput(ns("plot_box3"), height = 300)
        ),
        size = 6
      ),
      makeCard(
        title = "Overall Most Popular Topics in PSYNDEX",
        content = tagList(
          shiny.fluent::Dropdown.shinyInput(
            inputId = ns("dropdown_box4"),
            options = list(
              list(key = "A", text = "Option A"),
              list(key = "B", text = "Option B"),
              list(key = "C", text = "Option C")
            )
          ),
          br(),
          plotOutput(ns("plot_box4"), height = 300)
        ),
        size = 6
      )
    )
  )
}
    
#' start Server Functions
#'
#' @noRd 
mod_start_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x = 2021
    
    output$title_box3 = renderUI({
      glue::glue("Popular PSYNDEX Topics in {x}")
    })
    
    output$plot_box3 = renderPlot({
      shinipsum::random_ggplot(type = "bar")
    })
    
    output$plot_box4 = renderPlot({
      shinipsum::random_ggplot(type = "bar")
    })
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")
