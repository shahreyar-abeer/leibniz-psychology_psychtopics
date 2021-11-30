#' topic_evol UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_topic_evol_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "two-cards",
      
      makeCard(
        title = "Evolution of PSYNDEX Topics",
        size = 12,
        style = "background-color: #c6cf78ff",
        content = tagList(
          bodyText("Here, you can explore how topic contents change over time."),
          br(),
          br(),
          
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(
              class = "ms-Grid-col ms-sm2 ms-xl2 mod-evol-search1"
            ),
            div(
              class = "ms-Grid-col ms-sm5 ms-xl5 mod-evol-search2",
              shiny.fluent::Label("Select a Topic"),
              shiny.fluent::NormalPeoplePicker.shinyInput(
                inputId = ns("search"),
                options = 1:10,
                itemLimit = 1
              )
            )
          ),
          
          br(),
          br(),
          uiOutput(ns("cur_year_text"))
        )
      ),
      
      div(),
      
      makeCard(
        title = title_with_help(
          id = ns("help2"),
          title = uiOutput(ns("title_box2")),
          content = tagList(
            shiny.fluent::Text(
              "A topic's ", tags$b("number of documents"),
              " is determined by counting all publications that mainly address the topic 
              (i.e., at least 50% of a publications’ content is related to the topic)."
            )
          )
        ),
        size = 12,
        content = tagList(
          echarts4r::echarts4rOutput(ns("plot"))
        )
      )
    ),
    
    div(
      class = "one-card",
      makeCard(
        title = title_with_help(
          id = ns("help3"),
          title = uiOutput(ns("title_box3")),
          content = tagList(
            shiny.fluent::Text(
              "Basically, a topic is a group of words that are frequently used together in publications.
              These word groups are found automatically by the algorithm.",
              br(), br(),
              "This table shows the ", tags$b("ten most characterizing words of the topic"),
              " (sorted from top to bottom), and how they change over years.",
              br(), br(),
              "The underlying topic identification method adds new documents every year, letting the topics evolve over time."
            )
          )
        ),
        size = 12,
        content = tagList(
          
          br(),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4 mod-evol-slider-col1"),
            div(
              class = "ms-Grid-col ms-sm3 ms-xl3 mod-evol-slider-col2",
              
              ## may need to be changed when https://github.com/Appsilon/shiny.fluent/issues/63 is solved
              # shiny.fluent::Slider(
              #   onChange = shiny.fluent::setInput(ns("slider"), 2),
              #   ranged = TRUE,
              #   label = "Select the range of years",
              #   min = 1999,
              #   max = 2019,
              #   defaultValue = 2019,
              #   defaultLowerValue = 2015,
              #   snapToStep = TRUE
              # )
              uiOutput(ns("slider_input"))
            ),
            div(
              class = "ms-Grid-col ms-sm1 ms-xl1",
              br(),
              shiny.fluent::IconButton.shinyInput(
                inputId = ns("go"),
                iconProps = list(iconName = "Forward"),
                className = "buttons-tab2",
                disabled = TRUE
              )
            )
          ),
          
          br(),
          reactable::reactableOutput(ns("table"))
        )
      )
    )
    
  )
}
    
#' topic_eval Server Functions
#'
#' @noRd 
mod_topic_evol_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    r_mod_topic_eval = reactiveValues(
      lower = NULL,
      upper = NULL
    )
    
    output$slider_input = renderUI({
      
      req(r$current_year, r$start_year)
      
      shiny.fluent::Slider(
        onChange = shiny.fluent::setInput(ns("slider"), 2),
        ranged = TRUE,
        label = "Select the range of years",
        min = r$start_year,
        max = r$current_year,
        defaultValue = r$current_year,
        defaultLowerValue = (r$current_year - 5),
        snapToStep = TRUE
      )
    })
    
    output$cur_year_text = renderUI({
      req(r$current_year)
      bodyText(glue::glue("For Trends, only records from 1980 to {r$current_year - 1} are included,
               since publications of the current year may not be fully covered yet."))
    })
    
    observeEvent(r$topic, {
      req(r$topic, r$current_year)
      
      options_data = data.frame(
        key = r$topic$ID,
        imageUrl = NA,
        imageInitials = as.character(r$topic$ID),
        text = as.character(r$topic$Label),
        secondaryText = r$topic$ID,
        presence = 0,
        initialsColor = 22
      )
      
      
      shiny.fluent::updateNormalPeoplePicker.shinyInput(
        inputId = "search",
        options = options_data,
        #value = options_data[1, ],
        defaultSelectedItems = options_data[1, ]
      )
      
      golem::invoke_js("pickOne", list = list())
      golem::invoke_js("setSlider", list = list(id = ns("slider"), vals = c((r$current_year - 5), r$current_year)))
    })
    
    observeEvent(input$slider, {
      #req(r_mod_topic_eval$lower)

      
      if (!is.null(r_mod_topic_eval$lower)) {
        #print("slider is null")
        
        if (r_mod_topic_eval$lower != input$slider[1] | r_mod_topic_eval$upper != input$slider[2]) {
          shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
        } else {
          shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
        }
        
        
      } else {
        shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
        golem::invoke_js("clickGo", list = list(button = ns("go")))
      }
    })
    
    
    output$title_box2 = renderUI({
      
      if (is.null(input$search)) {
        HTML("Trend Plot")
      } else {
        HTML("Trend of Topic: ", r$topic$Label[r$topic$ID == input$search])
      }
      
    })
    
    
    output$title_box3 = renderUI({
      
      if (is.null(input$search)) {
        HTML("Change of Terms Table")
      } else {
        HTML("Change of Top Terms for Topic: ", r$topic$Label[r$topic$ID == input$search])
      }
      
    })
    
    observeEvent(input$go, {
      req(input$slider)
      
      shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
      if (input$slider[1] == input$slider[2]) {
        r_mod_topic_eval$lower = NULL
        r_mod_topic_eval$upper = NULL
      } else {
        r_mod_topic_eval$lower = input$slider[1] %>% as.character()
        r_mod_topic_eval$upper = input$slider[2] %>% as.character()
      }
      
    })
    
    
    # observeEvent(input$search, {
    #   print(input$search)
    #   
    #   r$topic %>% 
    #     dplyr::filter(ID == input$search) %>% 
    #     print()
    #   
    #   r$topic_evo[[input$search]] %>% print()
    #   
    # })
    
    output$table = reactable::renderReactable({
      req(r$topic_evo, input$search, r_mod_topic_eval$lower)
      
      r$topic_evo[[input$search]] %>% 
        as.data.frame() %>% 
        dplyr::select(r_mod_topic_eval$lower:r_mod_topic_eval$upper) %>% 
        reactable::reactable(
          rownames = FALSE,
          compact = TRUE,
          striped = TRUE,
          searchable = FALSE,
          sortable = FALSE,
          resizable = TRUE,
          fullWidth = TRUE,
          defaultPageSize = 10,
          # selection = "multiple",
          # defaultSelected = 1:3,
          # onClick = "select",
          # style = list(
          #   width = "100%"
          # ),
          theme = reactable::reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#c6cf78ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
          )
          # columns = list(
          #   search = reactable::colDef(
          #     name = "Search",
          #     html = TRUE
          #   ),
          #   .selection = reactable::colDef(
          #     show = TRUE,
          #     headerClass = "hide-checkbox"
          #   ),
          #   TopTerms = reactable::colDef(
          #     show = FALSE
          #   )
          # )
          
        )
      
    })
    
    output$plot = echarts4r::renderEcharts4r({
      req(r$topic, input$search)
      
      
      r$n_doc_year %>%
        dplyr::filter(id == input$search) %>% 
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>%
        dplyr::group_by(Label) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{TopTerms};{id};{Label}"),
          year = as.character(year)
        ) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(Freq, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "number of documents", nameLocation = "center", nameGap = 38) %>% 
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
                      '<br/> Overall top terms: ' + vals[0]
                      }
          ")
        )
    })
    
    
  })
}
    
## To be copied in the UI
# mod_topic_eval_ui("topic_eval_ui_1")
    
## To be copied in the server
# mod_topic_eval_server("topic_eval_ui_1")
