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
      class = "three-cards",

      makeCard(
        title = "Browse PSYNDEX Topics",
        style = "background-color: #c6cf78ff",
        size = 12,
        content = tagList(
          
          bodyText("Here you can browse all topics included in the model."),
          br(),
          bodyText("Select topics in the table below for adding them to the plots."),
          br(),
          bodyText("For Trends, only records from 1980 to [current_year - 1] are included, since publications of the current year are not yet fully covered. (learn more)")
        )
      ),
      
      div(),
      
      makeCard(
        title = "Topic Trends",
        size = 12,
        content = tagList(
          echarts4r::echarts4rOutput(ns("plot_box2")),
          
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4"),
            div(
              class = "ms-Grid-col ms-sm4 ms-xl4",
              shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot"), text = "Clear Plot")
            )
          )

        )
      ),
      
      div(),
      
      makeCard(
        title = "Share of Empirical Research",
        size = 12,
        content = tagList(
          
          echarts4r::echarts4rOutput(ns("plot_box3")),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4"),
            div(
              class = "ms-Grid-col ms-sm4 ms-xl4",
              shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot2"), text = "Clear Plot")
            )
          )
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
    
#' browse-topics Server Functions
#'
#' @noRd 
mod_browse_topics_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    topic = reactive({
      r$topic %>% 
        dplyr::mutate(
          search = createLink(TopTerms, r$booster, ID)
        ) %>% 
        dplyr::arrange(-n_docs)
    })
    
    observeEvent(input$clear_plot, {
      reactable::updateReactable("topics_table", selected = NA)
    })
    
    observeEvent(input$clear_plot2, {
      reactable::updateReactable("topics_table", selected = NA)
    })
    
    
    output$plot_box2 = echarts4r::renderEcharts4r({
      req(r$n_doc_year, r$topic, id_selected())
      
      label1 <- list(
        formatter = htmlwidgets::JS(
          'function(value, index){
            return value;
          }'
        )
      )
      
      r$n_doc_year %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>%
        dplyr::filter(id %in% id_selected()) %>% 

        #tibble::glimpse(.) %>% 
        dplyr::group_by(id) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{TopTerms};{id}"),
          year = as.character(year)
        ) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(Freq, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "n_docs", nameLocation = "center", nameGap = 35) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          appendToBody = TRUE,
          textStyle = list(width = 50, overflow = "break"),
          axisPointer = list(type = "cross"),
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              return('ID: ' + vals[1] + 
                      '<br/> N docs: ' + params.value[1]) +
                      '<br/> Year: ' + params.value[0] + 
                      '<br/> Topic: ' + vals[0]
                      }
          ")
        )
      
    })  ## plot_box2
    
    
    output$plot_box3 = echarts4r::renderEcharts4r({
      req(r$empirical, r$topic, id_selected())

      
      r$empirical %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>%
        dplyr::filter(id %in% id_selected()) %>% 
        
        #tibble::glimpse(.) %>% 
        dplyr::group_by(id) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{TopTerms};{id}"),
          year = as.character(year)
        ) %>% 
        echarts4r::e_charts(year) %>% 
        echarts4r::e_line(Freq, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "%", nameLocation = "center", nameGap = 27, nameRotate = 0) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          axisPointer = list(type = "cross"),
          
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              return('ID: ' + vals[1] + 
                      '<br/> Empirical research: ' + params.value[1]) + '%' +
                      '<br/> Year: ' + params.value[0] + 
                      '<br/> Topic: ' + vals[0]
                      }
          ")
        )
    })  ## plot_box3
    
    
    output$topics_table = reactable::renderReactable({
      topic() %>% 
        reactable::reactable(
          rownames = FALSE,
          compact = TRUE,
          searchable = TRUE,
          sortable = FALSE,
          resizable = TRUE,
          fullWidth = FALSE,
          defaultPageSize = 5,
          selection = "multiple",
          defaultSelected = 1:3,
          onClick = "select",
          style = list(
            width = "100%"
          ),
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#c6cf78ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          columns = list(
            # id = reactable::colDef(
            #   name = "ID"
            # ),
            search = reactable::colDef(
              name = "Search",
              html = TRUE
            ),
            Empirical = reactable::colDef(
              format = reactable::colFormat(digits = 2)
            ),
            # freq = reactable::colDef(
            #   name = "Prevalence"
            # ),
            .selection = reactable::colDef(
              show = TRUE,
              headerClass = "hide-checkbox"
            )
          )
          
        )
    })
    
    selected <- reactive(reactable::getReactableState("topics_table", "selected"))
    
    id_selected = reactive({
      topic()[selected(), ] %>%
        dplyr::select(ID) %>%  
        dplyr::pull()
    })
    
    
  })
}
    
## To be copied in the UI
# mod_browse-topics_ui("browse-topics_ui_1")
    
## To be copied in the server
# mod_browse-topics_server("browse-topics_ui_1")
