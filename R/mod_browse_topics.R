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
        content = bodyText("Here you can browse all topics included in the model."),
        size = 4
      ),
      makeCard(
        title = "Topic Trends",
        size = 4,
        content = tagList(
          echarts4r::echarts4rOutput(ns("plot_box2"))
        )
      ),
      makeCard(
        title = "Share of Empirical Researc",
        content = Text("Here you can browse all topics included in the model."),
        size = 4
      )
    ),
    
    shiny.fluent::Stack(
      
      makeCard(
        title = "Topic Details",
        size = 12,
        content = tagList(
          #plotOutput(ns("plot1"))
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
    
    output$plot_box2 = echarts4r::renderEcharts4r({
      req(r$theta_mean_by_year, r$topic, r$browse_top_3)
      
      #print(str(d1))
      
      color <- "#241b3e"
      
      top = 3
      
      d1 = as.data.frame(as.table(r$theta_mean_by_year)) %>% 
        dplyr::mutate(
          year = as.numeric(as.character(Var1)),
          id = as.numeric(as.character(Var2)),
          Freq = round(Freq * 100, 2)
        ) %>%
        dplyr::filter(id %in% r$browse_top_3) %>% 
        dplyr::group_by(id) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(Freq) %>% 
        echarts4r::e_x_axis(min = 1980, max = 2019) %>% 
        echarts4r::e_tooltip()
      
      
      
      
      
      # df = d1 %>%
      #   dplyr::filter(id %in% 1:3) %>% 
      #   #dplyr::arrange(-Freq) %>% 
      #   #dplyr::slice_head(n = top) %>% 
      #   dplyr::mutate(Freq = round(Freq * 100, 2))
      #   #dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>% 
      #   #dplyr::mutate(topic_split = stringr::str_split(Thema, ","))
      # 
      # 
      # hch1 = df %>%
      #   highcharter::hchart(
      #     "line",
      #     highcharter::hcaes(x = "year", y = "Freq", group = "Var2"),
      #     #name = "Prevalence",
      #     #colorByPoint = TRUE,
      #     borderColor = "black"
      #     # dataLabels = list(
      #     #   enabled = TRUE,
      #     #   align = "right",
      #     #   x = -33,
      #     #   color = "#fff",
      #     #   style = list(fontSize = 13),
      #     #   formatter = JS('
      #     #   function() {
      #     #     return this.point.topicSplit.slice(0, 2);
      #     #   }'
      #     #   )
      #     # )
      #   ) %>% 
      #   highcharter::hc_chart(
      #     plotBorderColor = "#aaa",
      #     plotBorderWidth = 2
      #   ) %>% 
      #   #highcharter::hc_colors(color) %>%
      #   #highcharter::hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = "17px")), gridLineColor = 'transparent') %>% 
      #   #highcharter::hc_yAxis(title = list(text = "Prevalence"), gridLineColor = 'transparent') %>% 
      #   #highcharter::hc_add_theme(highcharter::hc_theme_google()) %>% 
      #   highcharter::hc_title(text = glue::glue("Popular topics in 2019"), style = list(fontSize = "21px")) %>% 
      #   # highcharter::hc_tooltip(
      #   #   pointFormat = "ID: {point.id} <br/> Prevalence: {point.y} <br/> Topic: {point.topic}",
      #   #   headerFormat = "",
      #   #   style = list(fontSize = "15px", opacity = 1),
      #   #   borderWidth = 2,
      #   #   backgroundColor = "#fff",
      #   #   hideDelay = 333
      #   # ) %>% 
      #   highcharter::hc_size(height = 450)
      # 
      
    })
    
    
  })
}
    
## To be copied in the UI
# mod_browse-topics_ui("browse-topics_ui_1")
    
## To be copied in the server
# mod_browse-topics_server("browse-topics_ui_1")
