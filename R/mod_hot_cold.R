#' hot_cold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hot_cold_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    div(
      class = "three-cards",
      
      makeCard(
        title = "Hot and Cold Topics in PSYNDEX",
        style = "background-color: #c6cf78ff",
        content = tagList(
          
          bodyText("Hot topics show the highest increase in topic prevalence over the years,
                   while cold topics are characterized by a decrease."),
          br(),
          #bodyText("You can set the range of years here"),
          
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(
              class = "ms-Grid-col ms-sm10 ms-xl10",
              
              ## may need to be changed when https://github.com/Appsilon/shiny.fluent/issues/63 is solved
              shiny.fluent::Slider(
                onChange = shiny.fluent::setInput(ns("slider"), 2),
                ranged = TRUE,
                label = "Select the range of years",
                min = 1980,
                max = 2019,
                defaultValue = 2019,
                defaultLowerValue = 2015,
                snapToStep = TRUE
              )
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
          bodyText("For Trends, only records from 1980 to [current_year - 1] are included, since publications of the current year are not yet fully covered.")
        ),
        
        
        
        size = 12
      ),
      
      div(),
      
      makeCard(
        title = uiOutput(ns("title_box2")),
        size = 12,
        content = tagList(
          echarts4r::echarts4rOutput(ns("hot_plot")),
          shiny.fluent::Stack(
            horizontal = TRUE,
            div(class = "ms-Grid-col ms-sm4 ms-xl4"),
            div(
              class = "ms-Grid-col ms-sm4 ms-xl4",
              shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot_hot"), text = "Clear Plot")
            )
          )
        )
      ),
      
      div(),
      
      makeCard(
        title = uiOutput(ns("title_box3")),
        size = 12,
        content = tagList(
          tagList(
            echarts4r::echarts4rOutput(ns("cold_plot")),
            shiny.fluent::Stack(
              horizontal = TRUE,
              div(class = "ms-Grid-col ms-sm4 ms-xl4"),
              div(
                class = "ms-Grid-col ms-sm4 ms-xl4",
                shiny.fluent::DefaultButton.shinyInput(inputId = ns("clear_plot_cold"), text = "Clear Plot")
              )
            )
          )
        )
      )
    ),
    
    
    div(
      class = "two-cards",
      style = "margin-bottom: 0",
      makeCard(
        title = uiOutput(ns("title_box4")),
        size = 12,
        content = tagList(
          tagList(
            reactable::reactableOutput(ns("hot_table"))
          )
        )
      ),
      
      div(),
      
      makeCard(
        title = uiOutput(ns("title_box5")),
        size = 12,
        content = tagList(
          tagList(
            reactable::reactableOutput(ns("cold_table"))
          )
        )
      )
    )
    
    
  )
  
}
    
#' hot_cold Server Functions
#'
#' @noRd 
mod_hot_cold_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    r_mod_hot_cold = reactiveValues(
      lower = NULL,
      upper = NULL
    )
    
    observeEvent(r$current_year, {
      print("r$current_year")
      shiny.react::updateReactInput(
        session = session,
        inputId = ns("slider"),
        max = r$current_year
      )
    })
    
    observeEvent(input$slider, {
      #req(r_mod_hot_cold$lower)
      
      if (!is.null(r_mod_hot_cold$lower)) {
        #print("slider is null")
        
        if (r_mod_hot_cold$lower != input$slider[1]) {
          shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
        } else {
          shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
        }
        

      } else {
        shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = FALSE)
      }
    })
    
    output$title_box2 = renderUI({
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Hot Topics Plot")
      } else {
        HTML("Hot Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }

    })
    
    output$title_box3 = renderUI({
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Cold Topics Plot")
      } else {
        HTML("Cold Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }
      
    })
    
    output$title_box4 = renderUI({
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Hot Topics Table")
      } else {
        HTML("Hot Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }
      
    })
    
    output$title_box5 = renderUI({
      
      if (is.null(r_mod_hot_cold$lower)) {
        HTML("Cold Topics Table")
      } else {
        HTML("Cold Topics from", r_mod_hot_cold$lower, "to", r_mod_hot_cold$upper)
      }
      
    })
    
    # trends function
    trends <- reactive({
      req(r_mod_hot_cold$lower, r_mod_hot_cold$upper)
      
      trends.ab(r_mod_hot_cold$lower-1979, r_mod_hot_cold$upper-1979, 
                r$theta_year, r$theta_mean_by_year, r$theta_mean_by_year_time,
                r$theta_mean_by_year_ts, r$years, r$topic)
      
    })
    
    observeEvent(input$go, {
      req(input$slider)
      
      shiny.fluent::updateIconButton.shinyInput(inputId = "go", disabled = TRUE)
      if (input$slider[1] == input$slider[2]) {
        r_mod_hot_cold$lower = NULL
        r_mod_hot_cold$upper = NULL
      } else {
        r_mod_hot_cold$lower = input$slider[1]
        r_mod_hot_cold$upper = input$slider[2]
      }
      
    })
    
    
    output$hot_plot = echarts4r::renderEcharts4r({
      req(input$go, r_mod_hot_cold$lower)
      
      trends()[[3]] %>% tsbox::ts_data.frame() %>% 
        dplyr::mutate(
          year = format(time, "%Y") %>% as.character(),
          id = as.numeric(id)
        ) %>% 
        dplyr::select(-time) %>% 
        dplyr::filter(id %in% id_selected_hot()) %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>%
        dplyr::group_by(id) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{TopTerms};{id}"),
          value = round(value * 100, 2)
        ) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(value, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "n_docs", nameLocation = "center", nameGap = 30) %>% 
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
      
      
    })  ## hot_plot
    
    output$cold_plot = echarts4r::renderEcharts4r({
      req(input$go, r_mod_hot_cold$lower)
      
      trends()[[4]] %>% tsbox::ts_data.frame() %>% 
        dplyr::mutate(
          year = format(time, "%Y") %>% as.character(),
          id = as.numeric(id)
        ) %>% 
        dplyr::select(-time) %>% 
        dplyr::filter(id %in% id_selected_cold()) %>%
        dplyr::left_join(r$topic, by = c("id" = "ID")) %>%
        dplyr::group_by(id) %>% 
        dplyr::mutate(
          tooltip = glue::glue("{TopTerms};{id}"),
          value = round(value * 100, 2)
        ) %>% 
        echarts4r::e_charts(year, reorder = FALSE) %>% 
        echarts4r::e_line(value, bind = tooltip) %>% 
        echarts4r::e_x_axis(name = "Year", nameLocation = "center", nameGap = 27, axisPointer = list(snap = TRUE)) %>% 
        echarts4r::e_y_axis(name = "n_docs", nameLocation = "center", nameGap = 30) %>% 
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
      
      
    })
    
    
    
    output$hot_table = reactable::renderReactable({
      req(input$go, r_mod_hot_cold$lower)
      
      trends()[[1]] %>%
        dplyr::select(ID = NR) %>% 
        dplyr::left_join(r$topic, by = "ID") %>% 
        dplyr::select(ID, TopTerms) %>% 
        dplyr::mutate(
          search = createLink(TopTerms, r$booster, ID)
        ) %>% 
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
            search = reactable::colDef(
              name = "Search",
              html = TRUE
            ),
            .selection = reactable::colDef(
              show = TRUE,
              headerClass = "hide-checkbox"
            )
          )
          
        )
    })  ## hot_table
    
    
    output$cold_table = reactable::renderReactable({
      req(input$go)
      
      trends()[[2]] %>%
        dplyr::select(ID = NR) %>% 
        dplyr::left_join(r$topic, by = "ID") %>% 
        dplyr::select(ID, TopTerms) %>% 
        dplyr::mutate(
          search = createLink(TopTerms, r$booster, ID)
        ) %>% 
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
            search = reactable::colDef(
              name = "Search",
              html = TRUE
            ),
            .selection = reactable::colDef(
              show = TRUE,
              headerClass = "hide-checkbox"
            )
          )
          
        )
    })  ## cold_table
    
    ## get selections from table
    selected_hot <- reactive(reactable::getReactableState("hot_table", "selected"))
    selected_cold <- reactive(reactable::getReactableState("cold_table", "selected"))
    
    id_selected_hot = reactive({
      trends()[[1]][selected_hot(), ] %>%
        dplyr::select(NR) %>%  
        dplyr::pull()
    })
    
    id_selected_cold = reactive({
      trends()[[2]][selected_cold(), ] %>%
        dplyr::select(NR) %>%  
        dplyr::pull()
    })
    
    observeEvent(input$clear_plot_hot, {
      reactable::updateReactable("hot_table", selected = NA)
    })
    
    observeEvent(input$clear_plot_cold, {
      reactable::updateReactable("cold_table", selected = NA)
    })
    
    
  })  ## module_server  
}
    
