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
      class = "two-cards",
      makeCard(
        size = 12,
        title = "About PsychTopics",
        style = "background-color: #c6cf78ff",
        content = tagList(
          bodyText(tagList("With PsychTopics, you can explore current and past research trends in psychology from the ",  tags$b("German-speaking countries."))),
          bodyText(
            tagList(
              "Topics are identified in ", tags$a("PSYNDEX", href = "http://www.psyndex.de", target = "_blank"),
              ", the comprehensive database produced by the Leibniz Institute for Psychology (ZPID)."
            )
          ),
          
          # bodyText(tagList("Conclusions should be drawn carefully, as topics are derived from the ",
          #                  tags$a("PSYNDEX database", href = "http://www.psyndex.de", target = "_blank"),
          #                  tags$b(" automatically using machine learning algorithms."), 
          #                  " PsychTopics is in an experimental state, makes no claim to completeness and cannot replace specific search strategies.")),
          br(),
          bodyText(uiOutput(ns("last_update"))),
          
          br(),
          bodyText(
            tagList(
              "How to cite: ",
              shiny.fluent::TooltipHost(
                content = tagList(
                  shiny.fluent::Text("Bittermann, A. (2019). Development of a user-friendly app for exploring and analyzing research topics in psychology.",
                                     br(),
                                     "In G. Catalano, C. Daraio, M. Gregori, H. F. Moed & G. Ruocco (Eds.), Proceedings of the 17th Conference of the International Society for Scientometrics and Informetrics (2634–2635).",
                                     br(),
                                     "Rome: Edizioni Efesto."),
                  br(),
                  tags$a("http://dx.doi.org/10.23668/psycharchives.2521", href = "http://dx.doi.org/10.23668/psycharchives.2521", target = "_blank")
                ),
                delay = 0,
                tags$a("Bittermann (2019)")

              )
            )
          ),
          
          bodyText(
            tagList(
              "Shiny App Developed by: ",
              shiny.fluent::TooltipHost(
                content = tagList(
                  shiny.fluent::Text("R Shiny Developer"),
                  br(),
                  tags$a("the way we R", href = "http://dx.doi.org/10.23668/psycharchives.2521", target = "_blank")
                ),
                delay = 0,
                tags$a("Zauad Shahreer Abeer")
                
              )
            )
          ),
          bodyText(
            tagList(
              "PsychTopics is open-source software ",
              tags$a("(explore the code)", href = "https://www.google.com", target = "_blank")
            )
          )
          
        )
      ),
      div(
        
      ),
      makeCard(
        size = 12,
        title = "How To Use PsychTopics",
        style = "background-color: #c6cf78ff",
        content = tagList(
          bodyText(
            "Use the menu on the left for different topic views.
            Look out for information icons in the top right corner of the boxes"
          ),
          br(),
          
          bodyText(
            tagList(
              tags$b("Conclusions should be drawn carefully, "),
              "as topics are derived automatically using machine learning algorithms.
              PsychTopics makes no claim to completeness and cannot replace specific search strategies."
            )
          ),
          br(),
          bodyText("For more information, click “Methods” on the left.")
          
          
          # shiny.fluent::Stack(
          #   horizontal = TRUE,
          #   div(class = "ms-Grid-col ms-sm2 ms-xl2"),
          #   div(
          #     class = "ms-Grid-col ms-sm4 ms-xl4",
          #     shiny.fluent::PrimaryButton.shinyInput(
          #       inputId = ns("begin_tutorial"),
          #       text = "Begin Tutorial",
          #       className = "buttons-tab2",
          #       iconProps = list("iconName" = "Education", className = "icon-tab2")
          #       #onRenderIcon = shiny.fluent::JS("() => {return <i className='fas fa-plus'></i>}")
          #     ),
          #   ),
          #   div(
          #     class = "ms-Grid-col ms-sm4 ms-xl4",
          #     shiny.fluent::PrimaryButton.shinyInput(
          #       inputId = ns("github"),
          #       text = "Github",
          #       className = "buttons-tab2",
          #       iconProps = list("iconName" = "Link", className = "icon-tab2")
          #       #onRenderIcon = shiny.fluent::JS("() => {return <i className='fas fa-plus'></i>}")
          #     ),
          #   )
          # ),
          


          # tags$iframe(width = "100%", height = "400px", src = "https://zpid.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=b4408d9e-68ee-4a17-933b-ac1a0094197d",
          #             frameborder = "0", allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen = TRUE)
        )
      )
    ),
    
    #br(),
    
    div(
      class = "two-cards",
      style = "margin-bottom: 0",
      makeCard(
        size = 12,
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
      ),
      
      div(
        
      ),
      
      makeCard(
        size = 12,
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
#' @import echarts4r
#'
#' @noRd 
mod_start_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$last_update = renderUI({
      req(r$last_updated)

      glue::glue("PsychTopics is updated quarterly. Last update: {r$last_updated}")
    })
    # 
    # observeEvent(input$dropdown_most_popular1, {
    #   shiny.fluent::updateDropdown.shinyInput(inputId = "dropdown_most_popular2", value = 5)
    # }, once = TRUE)
    
    output$title_box3 = renderUI({
      req(r$current_year)
      #x = 2019
      glue::glue("Popular PSYNDEX Topics in {r$current_year}")
    })
    
    

    output$plot_box3 = echarts4r::renderEcharts4r({
      req(r$n_doc_year, r$topic, r$current_year, input$dropdown_most_popular1)

      # d1 = as.data.frame(as.table(r$n_doc_year)) %>%
      #   dplyr::mutate(year = as.numeric(as.character(Var1)), label = Var2)
      #print(str(d1))
      
      
      d1 = r$n_doc_year
      
      color <- "#953386"
      
      

      top = input$dropdown_most_popular1

      df = d1 %>%

        #dplyr::arrange(-Freq) %>%
        #dplyr::slice_head(n = top) %>%
        #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>% 
        dplyr::filter(year == r$current_year) %>%
        dplyr::arrange(-Freq) %>% 
        dplyr::slice_head(n = top) %>% 
        dplyr::mutate(
          id2 = as.factor(id)
        )
      
      
      #print(str(df))
      
      df %>% 
        echarts4r::e_charts(id2) %>% 
        echarts4r::e_bar(Freq, name = "n-docs", bind = TopTerms) %>% 
        echarts4r::e_title(text = glue::glue("Popular topics in {r$latest_year}")) %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_x_axis(name = "number of documents", nameLocation = "center", nameGap = 27) %>% 
        echarts4r::e_y_axis(name = "ID", nameLocation = "center", nameRotate = 0, nameGap = 35, inverse = TRUE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              return('ID: ' + params.value[1] + 
                      '<br/> N docs: ' + params.value[0]) + 
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
        echarts4r::e_color(color = color) %>% 
        echarts4r::e_legend(show = FALSE)
      
      
    })
    
    output$plot_box4 = echarts4r::renderEcharts4r({
      req(r$n_doc_year, r$topic, input$dropdown_most_popular2)

      # d1 = as.data.frame(as.table(r$n_doc_year)) %>%
      #   dplyr::mutate(year = as.numeric(as.character(Var1)), label = Var2)
      #print(str(d1))
      
      d1 = r$n_doc_year
      
      color <- "#953386"

      top = input$dropdown_most_popular2
      


      df = d1 %>%
        #dplyr::filter(year == 2019) %>%
        #dplyr::arrange(-Freq) %>%
        #dplyr::slice_head(n = top) %>%
        #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>%
        #tibble::glimpse(.) %>% 
        dplyr::arrange(-Freq) %>% 
        dplyr::slice_head(n = top) %>% 
        #tibble::glimpse(.) %>% 
        dplyr::mutate(
          id2 = as.factor(id)
        )
      
      #print(tail(df))
      
      #r$browse_top_3 = unique(df$id)[1:3]

      #print(str(df))
      
      df %>% 
        echarts4r::e_charts(id2, reorder = FALSE) %>% 
        echarts4r::e_bar(Freq, name = "n-docs", bind = TopTerms) %>% 
        echarts4r::e_title(text = "Popular topics overall") %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_x_axis(name = "number of documents", nameLocation = "center", nameGap = 27) %>% 
        echarts4r::e_y_axis(inverse = TRUE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              return('ID: ' + params.value[1] + 
                      '<br/> N docs: ' + params.value[0]) + 
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
        echarts4r::e_color(color = color) %>% 
        echarts4r::e_legend(show = FALSE)
      

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
