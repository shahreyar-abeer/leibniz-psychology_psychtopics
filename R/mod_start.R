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
    div(
      class = "start-cards",
      makeCard(
        size = 11,
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
        )
      ),
      makeCard(
        size = 11,
        title = "Tutorial video",
        content = Text("A tutorial video will be added here")
      ),
      
      makeCard(
        size = 11,
        title = uiOutput(ns("title_box3")),
        content = tagList(
          bodyText("Please note that these topics are preliminary!"),
          br(),
          highcharter::highchartOutput(ns("plot_box3"))
        )
      ),
      
      makeCard(
        size = 11,
        title = "Overall Most Popular Topics in PSYNDEX",
        content = tagList(
          shiny.fluent::Dropdown.shinyInput(
            inputId = ns("dropdown_most_popular"),
            options = list(
              list(key = 5, text = "5"),
              list(key = 10, text = "10"),
              list(key = 15, text = "15"),
              list(key = 20, text = "20")
            ),
            value = 5
          ),
          br(),
          highcharter::highchartOutput(ns("plot_box4"))
        )
      )
      
    )
  )
}
    
#' start Server Functions
#'
#' @noRd 
mod_start_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
    output$title_box3 = renderUI({
      req(r$theta_mean_by_year, r$topic)
      x = 2019
      glue::glue("Popular PSYNDEX Topics in {x}")
    })
    
    output$plot_box3 = highcharter::renderHighchart({
      req(r$theta_mean_by_year, r$topic, input$dropdown_most_popular)
      
      d1 = as.data.frame(as.table(r$theta_mean_by_year)) %>% 
        dplyr::mutate(year = as.numeric(as.character(Var1)), id = as.numeric(as.character(Var2)))
      #print(str(d1))
      
      color <- "#241b3e"
      
      
      df = d1 %>%
        dplyr::filter(year == 2019) %>% 
        dplyr::arrange(-Freq) %>% 
        dplyr::slice_head(n = input$dropdown_most_popular) %>% 
        dplyr::mutate(Freq = round(Freq * 100, 2)) %>% 
        dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>% 
        dplyr::mutate(topic_split = stringr::str_split(Thema, ","))
      
      
      hch1 = df %>%
        highcharter::hchart(
          "bar",
          highcharter::hcaes(x = "Var2", y = "Freq", topic = "Thema", topicSplit = "topic_split", id = "id"),
          name = "Prevalence",
          #colorByPoint = TRUE,
          borderColor = "black",
          dataLabels = list(
            enabled = TRUE,
            align = "right",
            x = -100,
            color = "#fff",
            formatter = JS('
            function() {
              return this.point.topicSplit.slice(0, 2);
            }'
            )
          )
        ) %>% 
        highcharter::hc_chart(
          plotBorderColor = "#aaa",
          plotBorderWidth = 2
        ) %>% 
        highcharter::hc_colors(color) %>%
        highcharter::hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = "17px")), gridLineColor = 'transparent') %>% 
        highcharter::hc_yAxis(title = list(text = "Prevalence"), gridLineColor = 'transparent') %>% 
        highcharter::hc_add_theme(highcharter::hc_theme_google()) %>% 
        highcharter::hc_title(text = glue::glue("Popular topics in 2019"), style = list(fontSize = "21px")) %>% 
        highcharter::hc_tooltip(
          pointFormat = "ID: {point.id} <br/> Topic: {point.topic} <br/> Prevalence: {point.y}",
          headerFormat = "",
          style = list(fontSize = "15px", opacity = 1),
          borderWidth = 2,
          backgroundColor = "#fff",
          hideDelay = 11
        ) 
        #highcharter::hc_size(height = 600)
      
      hch1

    })
    
    output$plot_box4 = highcharter::renderHighchart({
      req(r$theta_mean_by_year, r$topic, input$dropdown_most_popular)
      
      d1 = as.data.frame(as.table(r$theta_mean_by_year)) %>% 
        dplyr::mutate(year = as.numeric(as.character(Var1)), id = as.numeric(as.character(Var2)))
      #print(str(d1))
      
      color <- "#241b3e"
      
      
      df = d1 %>%
        #dplyr::filter(year == 2019) %>% 
        dplyr::arrange(-Freq) %>% 
        dplyr::slice_head(n = input$dropdown_most_popular) %>% 
        dplyr::mutate(Freq = round(Freq * 100, 2)) %>% 
        dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>% 
        dplyr::mutate(topic_split = stringr::str_split(Thema, ","))
      
      print(str(df))
      
      hch2 = df %>%
        highcharter::hchart(
          "bar",
          highcharter::hcaes(x = "Var2", y = "Freq", topic = "Thema", topicSplit = "topic_split", id = "id"),
          name = "Prevalence",
          #colorByPoint = TRUE,
          borderColor = "black",
          dataLabels = list(
            enabled = TRUE,
            align = "right",
            x = -100,
            color = "#fff",
            formatter = JS('
            function() {
              return this.point.topicSplit.slice(0, 2);
            }'
            )
          )
        ) %>% 
        highcharter::hc_chart(
          plotBorderColor = "#aaa",
          plotBorderWidth = 2
        ) %>% 
        highcharter::hc_colors(color) %>%
        highcharter::hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = "17px")), gridLineColor = 'transparent') %>% 
        highcharter::hc_yAxis(title = list(text = "Prevalence"), gridLineColor = 'transparent') %>% 
        highcharter::hc_add_theme(highcharter::hc_theme_google()) %>% 
        highcharter::hc_title(text = glue::glue("Popular topics overall"), style = list(fontSize = "21px")) %>% 
        highcharter::hc_tooltip(
          pointFormat = "ID: {point.id} <br/> Topic: {point.topic} <br/> Prevalence: {point.y}",
          headerFormat = "",
          style = list(fontSize = "15px", opacity = 1),
          borderWidth = 2,
          backgroundColor = "#fff"
        ) 
      #highcharter::hc_size(height = 600)
      #print(hch2)
      
      hch2
      
    })
    
    
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")
