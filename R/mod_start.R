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
          bodyText(uiOutput(ns("last_update")))
        )
      ),
      makeCard(
        size = 11,
        title = NULL,
        content = tagList(
          # tags$iframe(width = "100%", height = "400px", src = "https://zpid.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=b4408d9e-68ee-4a17-933b-ac1a0094197d",
          #             frameborder = "0", allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen = TRUE)
        )
      ),
      
      makeCard(
        size = 11,
        title = uiOutput(ns("title_box3")),
        content = tagList(
          
          div(
            class = "grid-p1-b3-b4",
            div(
              class = "text",
              style = "margin-top: 11px",
              bodyText(text = "Please note that these topics are preliminary!")
            ),
            div(
              class = "dropdown",
              
              shiny.fluent::Dropdown.shinyInput(
                inputId = ns("dropdown_most_popular1"),
                style = list(textAlign = "center"),
                label = "Show top",
                options = list(
                  list(key = 5, text = "5"),
                  list(key = 10, text = "10"),
                  list(key = 15, text = "15"),
                  list(key = 20, text = "20")
                ),
                value = 5
              ),
            )
          ),
          
          br(),
          echarts4r::echarts4rOutput(ns("plot_box3"), height = 550)
          #highcharter::highchartOutput(ns("plot_box3"), height = 650)
        )
#          br(),
      ),
      
      makeCard(
        size = 11,
        title = "Overall Most Popular Topics in PSYNDEX",
        content = tagList(
          
          
          div(
            class = "grid-p1-b3-b4",
            div(
              class = "text"
              #style = "margin-top: 11px",
              #bodyText(text = "Please note that these topics are preliminary!")
            ),
            div(
              class = "dropdown",
              
              shiny.fluent::Dropdown.shinyInput(
                inputId = ns("dropdown_most_popular2"),
                style = list(textAlign = "center"),
                label = "Show top",
                options = list(
                  list(key = 5, text = "5"),
                  list(key = 10, text = "10"),
                  list(key = 15, text = "15"),
                  list(key = 20, text = "20")
                ),
                value = 5
              )
            )
          ),
          br(),
          echarts4r::echarts4rOutput(ns("plot_box4"), height = 550)
          #highcharter::highchartOutput(ns("plot_box4"), height = 650)
          #plotOutput(ns("plot_box4"))
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
    
    output$last_update = renderUI({
      
      ## update these two line by hand
      date_updated = "18th Oct, 2021"
      latest_year = 2019
      
      ##---
      
      r$latest_year = 2019
      glue::glue("PsychTopics is updated quarterly. Last update: {date_updated}")
    })
    # 
    # observeEvent(input$dropdown_most_popular1, {
    #   shiny.fluent::updateDropdown.shinyInput(inputId = "dropdown_most_popular2", value = 5)
    # }, once = TRUE)
    
    output$title_box3 = renderUI({
      req(r$theta_mean_by_year, r$topic, r$latest_year)
      #x = 2019
      glue::glue("Popular PSYNDEX Topics in {r$latest_year}")
    })
    
    

    output$plot_box3 = echarts4r::renderEcharts4r({
      req(r$theta_mean_by_year, r$topic, r$latest_year, input$dropdown_most_popular1)

      d1 = as.data.frame(as.table(r$theta_mean_by_year)) %>%
        dplyr::mutate(year = as.numeric(as.character(Var1)), id = as.numeric(as.character(Var2)))
      #print(str(d1))

      color <- "#241b3e"

      top = input$dropdown_most_popular1

      height = dplyr::case_when(
        top == 5 ~ 500,
        top == 10 ~ 550,
        top == 15 ~ 600,
        top == 20 ~ 650
      )

      df = d1 %>%
        dplyr::filter(year == r$latest_year) %>%
        dplyr::arrange(-Freq) %>%
        dplyr::slice_head(n = top) %>%
        dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>% 
        dplyr::mutate(
          id2 = as.factor(id)
        )
      
      
      #print(str(df))
      
      df %>% 
        echarts4r::e_charts(Var2) %>% 
        echarts4r::e_bar(Freq, name = "Prevalence", bind = Thema) %>% 
        echarts4r::e_title(text = glue::glue("Popular topics in {r$latest_year}")) %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_y_axis(inverse = TRUE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              return('ID: ' + params.value[1] + 
                      '<br/> Prevalence: ' + params.value[0]) + 
                      '<br/> Topic: ' + params.name
                      }
          ")
        ) %>% 
        echarts4r::e_labels(
          position = "insideLeft",
          fontSize = 15,
          color = "#fff",
          formatter = htmlwidgets::JS("
            function(params){
              return(params.name.split(',').slice(0,1))
              }
          ")
        ) %>% 
        echarts4r::e_color(color = color)
      
      
      
      
    #   
    #   
    #   hch1 = df %>%
    #     highcharter::hchart(
    #       "bar",
    #       highcharter::hcaes(x = "Var2", y = "Freq", topic = "Thema", topicSplit = "topic_split"),
    #       name = "Prevalence",
    #       #colorByPoint = TRUE,
    #       borderColor = "black",
    #       dataLabels = list(
    #         enabled = TRUE,
    #         align = "right",
    #         x = -33,
    #         color = "#fff",
    #         style = list(fontSize = 13),
    #         formatter = JS('
    #         function() {
    #           return this.point.topicSplit.slice(0, 2);
    #         }'
    #         )
    #       )
    #     ) %>% 
    #     highcharter::hc_chart(
    #       plotBorderColor = "#aaa",
    #       plotBorderWidth = 2
    #     ) %>% 
    #     highcharter::hc_colors(color) %>%
    #     highcharter::hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = "17px")), gridLineColor = 'transparent') %>% 
    #     highcharter::hc_yAxis(title = list(text = "Prevalence"), gridLineColor = 'transparent') %>% 
    #     highcharter::hc_add_theme(highcharter::hc_theme_google()) %>% 
    #     highcharter::hc_title(text = glue::glue("Popular topics in 2019"), style = list(fontSize = "21px")) %>% 
    #     highcharter::hc_tooltip(
    #       pointFormat = "ID: {point.id} <br/> Prevalence: {point.y} <br/> Topic: {point.topic}",
    #       headerFormat = "",
    #       style = list(fontSize = "15px", opacity = 1),
    #       borderWidth = 2,
    #       backgroundColor = "#fff",
    #       hideDelay = 333
    #     ) %>% 
    #     highcharter::hc_size(height = height)
    #   
    #   hch1
    # 
      
      
    })
    
    output$plot_box4 = echarts4r::renderEcharts4r({
      req(r$theta_mean_by_year, r$topic, input$dropdown_most_popular2)

      d1 = as.data.frame(as.table(r$theta_mean_by_year)) %>%
        dplyr::mutate(year = as.numeric(as.character(Var1)), id = as.numeric(as.character(Var2)))
      #print(str(d1))

      color <- "#241b3e"

      top = input$dropdown_most_popular2

      # height = dplyr::case_when(
      #   top == 5 ~ 400,
      #   top == 10 ~ 450,
      #   top == 15 ~ 500,
      #   top == 20 ~ 550
      # )


      df = d1 %>%
        #dplyr::filter(year == 2019) %>%
        dplyr::arrange(-Freq) %>%
        dplyr::slice_head(n = top) %>%
        dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>%
        dplyr::mutate(
          id2 = as.factor(glue::glue("{id} ({year})"))
        )
      
      r$browse_top_3 = unique(df$id)[1:3]

      print(str(df))
      
      df %>% 
        echarts4r::e_charts(id2, reorder = FALSE) %>% 
        echarts4r::e_bar(Freq, name = "Prevalence", bind = Thema) %>% 
        echarts4r::e_title(text = "Popular topics overall") %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_y_axis(inverse = TRUE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              return('ID: ' + params.value[1] + 
                      '<br/> Prevalence: ' + params.value[0]) + 
                      '<br/> Topic: ' + params.name
                      }
          ")
        ) %>% 
        echarts4r::e_labels(
          position = "insideLeft",
          fontSize = 15,
          color = "#fff",
          formatter = htmlwidgets::JS("
            function(params){
              return(params.name.split(',').slice(0,1))
              }
          ")
        ) %>% 
        echarts4r::e_color(color = color)
      

      # hch2 = df %>%
      #   highcharter::hchart(
      #     "bar",
      #     highcharter::hcaes(x = "id2", y = "Freq", topic = "Thema", topicSplit = "topic_split", id = "id", year = "year"),
      #     name = "Prevalence",
      #     #colorByPoint = TRUE,
      #     borderColor = "black",
      #     dataLabels = list(
      #       enabled = TRUE,
      #       align = "right",
      #       x = -33,
      #       color = "#fff",
      #       style = list(fontSize = 13),
      #       formatter = JS('
      #       function() {
      #         return this.point.topicSplit.slice(0, 2);
      #       }'
      #       )
      #     )
      #   ) %>%
      #   highcharter::hc_chart(
      #     plotBorderColor = "#aaa",
      #     plotBorderWidth = 2
      #   ) %>%
      #   highcharter::hc_colors(color) %>%
      #   highcharter::hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = "17px")), gridLineColor = 'transparent') %>%
      #   highcharter::hc_yAxis(title = list(text = "Prevalence"), gridLineColor = 'transparent') %>%
      #   #highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
      #   highcharter::hc_title(text = glue::glue("Popular topics overall"), style = list(fontSize = "21px")) %>%
      #   highcharter::hc_tooltip(
      #     pointFormat = "ID: {point.id} <br/> Year: {point.year} <br/> Prevalence: {point.y} <br/> Topic: {point.topic}",
      #     headerFormat = "",
      #     style = list(fontSize = "15px", opacity = 1),
      #     borderWidth = 2,
      #     backgroundColor = "#fff",
      #     hideDelay = 333
      #   ) %>%
      #   highcharter::hc_size(height = height)



    })
    
    # output$plot_box4 = renderPlot({
    #   shinipsum::random_ggplot()
    # })
    # 
    
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")
