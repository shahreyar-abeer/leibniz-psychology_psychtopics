#' helper_functions 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd



router <- shiny.router::make_router(
  route("/", mod_start_ui("start")),
  route("browse-topics", mod_browse_topics_ui("browse")),
  route("popular", mod_popular_by_year_ui("popular"))
)



# colors ----

col_bars <- "#0094c5"
col_highlight <- "gold"
# colors in tags$style have to be set manually in respective lines
