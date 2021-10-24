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
        content = Text("Here you can browse all topics included in the model."),
        size = 11
      )
    ),
    br(),
    div(
      class = "browse-box4",
      makeCard(
        title = "Topic Details",
        size = 12,
        content = tagList(
          reactable::reactableOutput(ns("topics_table"))
        )
      )
    )
      
      
    
  )
}
    
#' browse-topics Server Functions
#'
#' @noRd 
mod_browse_topics_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    topic = reactive({
      r$topic %>% 
        dplyr::mutate(
          id = Nr..,
          topic = Thema,
          freq = round(Prävalenz * 100, 2),
          search = createLink(topic, r$booster, id)
        ) %>% 
        dplyr::arrange(-freq) %>% 
        dplyr::select(-c(Nr.., Prävalenz, Thema))
    })
    
    output$plot_box2 = echarts4r::renderEcharts4r({
      req(r$theta_mean_by_year, r$topic, id_selected())
      
      #print(str(d1))
      
      print(id_selected())
      
      color <- "#241b3e"
      
      top = 3
      
      d1 = as.data.frame(as.table(r$theta_mean_by_year)) %>% 
        dplyr::mutate(
          year = as.numeric(as.character(Var1)),
          id = as.numeric(as.character(Var2)),
          Freq = round(Freq * 100, 2)
        ) %>%
        dplyr::filter(id %in% id_selected()) %>% 
        dplyr::group_by(id) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(Freq) %>% 
        echarts4r::e_x_axis(min = 1980, max = 2019) %>% 
        echarts4r::e_tooltip()
      
    })
    
    output$topics_table = reactable::renderReactable({
      topic() %>% 
        reactable::reactable(
          rownames = FALSE,
          searchable = TRUE,
          sortable = FALSE,
          selection = "multiple",
          defaultSelected = 1:3,
          onClick = "select",
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#aaa", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          columns = list(
            id = reactable::colDef(
              name = "ID"
            ),
            search = reactable::colDef(
              name = "Search",
              html = TRUE
            ),
            freq = reactable::colDef(
              name = "Prevalence"
            ),
            .selection = reactable::colDef(
              show = FALSE
            )
          )
          
        )
    })
    
    selected <- reactive(reactable::getReactableState("topics_table", "selected"))
    
    id_selected = reactive({
      topic()[selected(), ] %>%
        dplyr::select(id) %>%  
        dplyr::pull()
    })
    
    
    output$topiclist <- DT::renderDataTable({
      topic <- topic[(grepl(search_lower(), topic$Thema)),]
      topicnum <- topic[,1]
      topic$Recherche <- createLink(topic$Thema, booster, topicnum)
      topic[,3] <- round(topic[,3], 4)*100
      names(topic) <- c("ID", "Topic", "Prevalence", "Search")
      return(topic)
    }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe', extensions = 'Responsive',
    options = list(lengthChange = TRUE, info = TRUE, paging = TRUE, searching = FALSE))
    
    
    
  })
}
    
## To be copied in the UI
# mod_browse-topics_ui("browse-topics_ui_1")
    
## To be copied in the server
# mod_browse-topics_server("browse-topics_ui_1")
