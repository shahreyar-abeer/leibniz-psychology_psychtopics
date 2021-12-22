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
          uiOutput(ns("cur_year_text"))
        )
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help2"),
          title = "Topic Trends",
          content = tagList(
            shiny.fluent::Text(
              "A topic's ", tags$b("number of documents"), " is determined by counting all publications that mainly address
              the topic (i.e., at least 50% of a publications' content is related to the topic)."
            )
          )
        ),
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
        title = title_with_help(
          id = ns("help3"),
          title = "Share of Empirical Research",
          content = tagList(
            shiny.fluent::Text(
              "The share of empirical research is the relative frequency of topic-related publications
              with a empirical study methodology.",
              br(),
              br(),
              "Some topics may address theoretical issues or conceptual work.
              Some topics might be characterized by a large share of empirical research.
              And some topics might shift from mostly theoretical publications in its early years
              to an increasing investigation of empirical evidence."
            )
          )
        ),
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
        title = title_with_help(
          id = ns("help2"),
          title = "Topic Details",
          content = tagList(
            shiny.fluent::Text(
              "The topics are sorted in decreasing order according to the number of associated papers.",
              br(),
              br(),
              "Basically, a topic is a group of words that are frequently used together in publications ",
              tags$b("(= top terms)"), ". These terms are found automatically by the algorithm.
              For better interpretation, the PsychTopics team formulated topic ", tags$b("labels."),
              br(),
              br(),
              "The ", tags$b("number of documents"), " across all years is determined by counting all publications
              that mainly address the topic (i.e., at least 50% of a publications’ content is related to the topic).",
              br(),
              br(),
              "The share of ", tags$b("empirical research"), " is the relative frequency of these publications with a empirical study methodology.",
              br(),
              br(),
              "The ", tags$b("journals"), " column shows the three most frequent journals that publish articles related to the topic.",
              br(),
              br(),
              "With ", tags$b("Search PSYNDEX"), ", you can explore topic-related articles in PubPsych.eu.
              The search query is generated from the top terms."
            )
          )
        ),
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
    
    output$cur_year_text = renderUI({
      req(r$current_year)
      bodyText(glue::glue("For Trends, only records from 1980 to {r$current_year - 1} are included,
               since publications of the current year may not be recorded yet 
               (journals, books, and reports on specific topics are published in waves throughout the year).")
      )
    })
    
    ## the data for the table
    topic = reactive({
      
      # make_tooltip = function(topic_evo_string) {
      #   
      #   text_to_display = stringr::str_split(topic_evo_string, " \n ")[[1]][1]
      #   
      #   shiny.fluent::TooltipHost(
      #     content = topic_evo_string,
      #     delay = 0,
      #     HTML(text_to_display)
      #   ) %>% as.character()
      # }
      
      req(r$topic_evo)
      
      make_topic_evo_string = function(each) {
        #years = attributes(each)$dimnames[[2]]
        
        each = as.data.frame(each)
        
        years = names(each)
        
        get_all_strings = function(year) {
          strings = glue::glue_collapse(each[[year]], sep = ", ")
          glue::glue("{year}: {strings}")
        }
        
        all_strings = sapply(years, get_all_strings)
        glue::glue_collapse(all_strings, sep = " \n ")
        
      }
      
      topics = sapply(r$topic_evo, make_topic_evo_string)
      
      r$topic %>% 
        dplyr::mutate(
          search = createLink(TopTerms, r$booster, ID),
          topic_evo = topics
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
        dplyr::mutate(
          tooltip = glue::glue("{TopTerms};{id};{Label}"),
          year = as.character(year),
          Label = factor(Label)
        ) %>% 
        dplyr::group_by(Label) %>% 
        
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(Freq, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "no of documents", nameLocation = "center", nameGap = 31) %>% 
        echarts4r::e_tooltip(
          confine = TRUE,
          appendToBody = TRUE,
          textStyle = list(width = 50, overflow = "break"),
          axisPointer = list(type = "cross"),
          formatter = htmlwidgets::JS("
            function(params){
              var vals = params.name.split(';');
              return('ID: ' + vals[1] + 
                      '<br/> Label: ' + vals[2] + 
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
        dplyr::group_by(Label) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{TopTerms};{id};{Label}"),
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
                      '<br/> Label: ' + vals[2] + 
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
            topic_evo = reactable::colDef(
              show = TRUE,
              html = TRUE,
              
              cell = htmlwidgets::JS('
                function(cellInfo, state) {
                  // input:
                  //  - cellInfo, an object containing cell info
                  //  - state, an object containing the table state (optional, new in v0.2.3.9000)
                  //
                  // output:
                  //  - content to render (e.g. an HTML string)
                  
                  all_years = cellInfo.value.split(" \\n ")
                  n = all_years.length - 1
                  
                  if (!state.searchValue) {
                    show = all_years[n]
                  } else {
                    x = all_years.filter(s => s.includes(state.searchValue))
                    if (x.length == 0) {
                      show = "No match"
                    } else if (x.length == 1) {
                      show = x
                    } else if (x.length > 1) {
                      show = x[(x.length - 1)]
                    }
                  }
                

                  console.log(show)
                  
                
                  // return "<div id = " + cellInfo.index + ">" + show + "</div>"
                  return show
                  
                }
              '),
              
              
              # cell = reactable::JS(
              #   'function(value) {
              #     console.log(value);
              #     if (value) {
              #       valueArray = value.split(" \n ");
              #       console.log(valueArray);
              #       searched = $("#browse-topics_table .rt-search").val();
              #       console.log(searched);
              #       x = valueArray.findIndex(s => s.includes(searched));
              #       return x;
              #     } else {
              #       return "this is a cell";
              #     }
              #   
              # 
              #   }'
              # )
              #style = "overflow: hidden"
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
