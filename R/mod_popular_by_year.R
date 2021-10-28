#' popular_by_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_popular_by_year_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    div(
      class = "start-cards",
      
      makeCard(
        title = "Popular PSYNDEX Topics by Year",
        style = "background-color: #c6cf78ff",
        content = tagList(
          
          bodyText("Explore the most popular topics in a specific year."),
          br(),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(
              class = glue("ms-Grid-col ms-sm{4} ms-xl{4}"),
            ),
            div(
              class = glue("ms-Grid-col ms-sm{4} ms-xl{4}"),
              style = "text-align: center",
              shiny.fluent::Dropdown.shinyInput(
                inputId = ns("selected_year"),
                style = list(textAlign = "center", width = "100%"),
                calloutProps = list(directionalHintFixed = TRUE, calloutMaxHeight = 300),
                #dropdown = list(width = "40%"),
                # styles = list(
                #   dropdown = list(width = "50%"),
                #   dropdownItem = list(height = "20px"),
                #   dropdownOptions = list(height = "40px")
                # ),
                
                label = "Select year",
                options = lapply(1980:2019, function(x) list(key = x, text = glue::glue("{x}"))),
                value = 2019
              )
            )
            
          ),
          
          br(),
          uiOutput(ns("box1_text")),
          br(),
          uiOutput(ns("last_updated"))
        ),
        
        
        
        size = 11
      ),
      makeCard(
        title = "Topic Trends",
        size = 11,
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
                inputId = ns("dropdown_most_popular"),
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
          
          echarts4r::echarts4rOutput(ns("plot_box2"), height = 550)
        )
      )
    ),
    br(),
    div(
      class = "tab3-box3",
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

    
#' popular_by_year Server Functions
#'
#' @noRd 
mod_popular_by_year_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$box1_text = renderUI({
      req(r$current_year)
      print(class(input$selected_year))
      req(input$selected_year == r$current_year)
      
      #bodyText(shiny.fluent::Icon(iconName = "WarningSolid", style = list(fontSize = 33)), glue::glue("  Topics of {r$current_year} are preliminary, as journals, books, and reports on specific topics are published in waves throughout the year."))
      bodyText(tags$b("NOTE:"), glue::glue(" Topics of {r$current_year} are preliminary, as journals, books, and reports on specific topics are published in waves throughout the year."))
      
    })
    
    output$last_updated = renderUI({
      req(r$last_updated)
      bodyText(glue::glue("Last Updated: {r$last_updated}"))
    })
    
    output$plot_box2 = echarts4r::renderEcharts4r({
      req(r$n_doc_year, r$topic, input$dropdown_most_popular)
      
      # d1 = as.data.frame(as.table(r$n_doc_year)) %>%
      #   dplyr::mutate(year = as.numeric(as.character(Var1)), label = Var2)
      # print(str(d1))
      
      d1 = r$n_doc_year
      
      color <- "#241b3e"
      
      top = input$dropdown_most_popular
      
      
      # df = d1 %>%
      #   
      #   #dplyr::arrange(-Freq) %>%
      #   #dplyr::slice_head(n = top) %>%
      #   #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
      #   dplyr::left_join(r$topic, by = c("label" = "Label")) %>% 
      #   dplyr::filter(year == r$current_year) %>%
      #   dplyr::arrange(-Freq) %>% 
      #   dplyr::slice_head(n = top) %>% 
      #   dplyr::mutate(
      #     id2 = as.factor(ID)
      #   )
      
      
      df = d1 %>%
        dplyr::filter(year == input$selected_year) %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>% 
        dplyr::arrange(-Freq) %>%
        #tibble::glimpse(.) %>% 
        dplyr::slice_head(n = top) %>%
        #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        #dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>% 
        dplyr::mutate(
          id2 = as.factor(id),
          top_terms_year = glue::glue("{TopTerms}; {input$selected_year}")
        )
      
      
      #print(str(df))
      
      df %>% 
        echarts4r::e_charts(id2) %>% 
        echarts4r::e_bar(Freq, name = "N docs", bind = top_terms_year) %>% 
        echarts4r::e_title(text = glue::glue("Popular topics in {input$selected_year}")) %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_y_axis(inverse = TRUE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              return('ID: ' + params.value[1] + 
                      '<br/> N docs: ' + params.value[0]) +
                      '<br/> Year: ' + vals[1] + 
                      '<br/> Topic: ' + vals[0]
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
      
    })
    
  })
}
    
## To be copied in the UI
# mod_popular_by_year_ui("popular_by_year_ui_1")
    
## To be copied in the server
# mod_popular_by_year_server("popular_by_year_ui_1")
