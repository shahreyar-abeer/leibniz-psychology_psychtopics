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
      class = "two-cards",
      
      makeCard(
        title = "Popular PSYNDEX Topics by Year",
        size = 12,
        style = "background-color: #c6cf78ff",
        content = tagList(
          
          bodyText("Explore the most popular topics in a specific year."),
          br(),
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
                calloutProps = list(directionalHintFixed = TRUE, calloutMaxHeight = 350),
                #dropdown = list(width = "40%"),
                # styles = list(
                #   dropdown = list(width = "50%"),
                #   dropdownItem = list(height = "20px"),
                #   dropdownOptions = list(height = "40px")
                # ),
                
                label = "Select year",
                options = lapply(sort(1980:2019, decreasing = TRUE), function(x) list(key = x, text = glue::glue("{x}"))),
                value = 2019
              )
            )
            
          ),
          
          br(),
          br(),
          uiOutput(ns("box1_text")),
          br(),
          uiOutput(ns("last_updated"))
        )
        
      ),
      
      div(),
      
      makeCard(
        title = "Topic Trends",
        size = 12,
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
    
    div(
      class = "one-card",
      style = "margin-bottom: 0",
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
    
    ## reactiveValues for this mod
    r_mod_pby = reactiveValues()
    
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
      
      color <- "#953386"
      top = input$dropdown_most_popular
      
      df = r$n_doc_year %>%
        dplyr::filter(year == input$selected_year) %>%
        dplyr::arrange(-Freq) %>%
        #tibble::glimpse(.) %>% 
        dplyr::slice_head(n = top) %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>% 
        #dplyr::mutate(Freq = round(Freq * 100, 2)) %>%
        #dplyr::left_join(r$topic, by = c("id" = "Nr..")) %>% 
        dplyr::mutate(
          id2 = as.factor(id),
          tooltip = glue::glue("{TopTerms}; {input$selected_year};{Label}")
        )
      
      r_mod_pby$df = df
      
      
      print(str(df))
      
      df %>%
        #dplyr::mutate(colors = c(color, rep("red", 4))) %>% 
        echarts4r::e_charts(id2) %>% 
        echarts4r::e_bar(Freq, name = "N docs", bind = tooltip, selectedMode = TRUE, select = list(itemStyle = list(color = "#241b3e"))) %>% 
        echarts4r::e_title(text = glue::glue("Popular topics in {input$selected_year}")) %>% 
        echarts4r::e_flip_coords() %>% 
        echarts4r::e_x_axis(name = "number of documents", nameLocation = "center", nameGap = 27) %>% 
        echarts4r::e_y_axis(name = "ID", nameLocation = "center", nameRotate = 0, nameGap = 35, inverse = TRUE) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              return('ID: ' + params.value[1] + 
                      '<br/> Label: ' + vals[2] +
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
              return(params.name.split(';')[2])
              }
          ")
        ) %>% 
        echarts4r::e_color(color = color) %>% 
        echarts4r::e_show_loading() %>% 
        echarts4r::e_legend(show = FALSE)
        #echarts4r::e_highlight(series_index = 0, dataIndex = 2)
        #echarts4r::e_add("itemStyle", colors)
        #echarts4r::e_add("dataIndex", 1:5)
      
    })  ## end plot_box2
    
    observeEvent(selected(), {
      proxy = echarts4r::echarts4rProxy(ns("plot_box2"))
      
      if (is.null(selected())) {
        proxy %>% 
          echarts4r::e_dispatch_action_p("select", dataIndex = NULL)
      } else {
        proxy %>% 
          echarts4r::e_dispatch_action_p("select", dataIndex = (selected() - 1))
      }

    }, ignoreNULL = FALSE)
    
    # observeEvent(input$plot_box2_clicked_data, {
    #   print(input$plot_box2_clicked_data)
    # })
    
    output$topics_table = reactable::renderReactable({
      req(r_mod_pby$df)
      
      r_mod_pby$df %>% 
        dplyr::select(ID = id2, Label, year, TopTerms, n_docs = Freq, Empirical, Journals) %>% 
        reactable::reactable(
          rownames = FALSE,
          searchable = TRUE,
          sortable = FALSE,
          resizable = TRUE,
          selection = "single",
          defaultSelected = 1,
          defaultPageSize = 5,
          onClick = "select",
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#c6cf78ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          columns = list(
            # id = reactable::colDef(
            #   name = "ID"
            # ),
            # search = reactable::colDef(
            #   name = "Search",
            #   html = TRUE
            # ),
            # freq = reactable::colDef(
            #   name = "Prevalence"
            # ),
            Empirical = reactable::colDef(
              format = reactable::colFormat(digits = 2)
            ),
            .selection = reactable::colDef(
              show = TRUE,
              headerClass = "hide-checkbox"
            )
          )
          
        )
    })  ## end topics_table
    
    selected <- reactive(reactable::getReactableState("topics_table", "selected"))
    
    
    
  })
}
    
## To be copied in the UI
# mod_popular_by_year_ui("popular_by_year_ui_1")
    
## To be copied in the server
# mod_popular_by_year_server("popular_by_year_ui_1")
