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
  route("popular", mod_popular_by_year_ui("popular")),
  route("hot-cold", mod_hot_cold_ui("hot_cold")),
  route("topic-evaluation", mod_topic_eval_ui("topic_eval")),
  route("methods", mod_methods_ui("methods"))
)



# colors ----

col_bars <- "#0094c5"
col_highlight <- "gold"
# colors in tags$style have to be set manually in respective lines

# PubPsych.eu search terms are boosted according to the relations of beta probabilites
# Factors were computed by dividing the beta probabilites of Terms 1-4 by beta of Term 5


createLink <- function(val, boost, topicnum) {
  list <- list()
  for (i in 1:length(val)){
    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
    for (j in 1:4){
      list[[i]][j] <- paste0('"', list[[i]][j], '"%5E', boost[j, topicnum[i]]) # add boost factors for first 4 terms
      # list[[i]][j] <- paste0('"', list[[i]][j], '"%5E', boost[[i]][j]) STM version
    }
    list[[i]][5] <- paste0('"', list[[i]][5], '"') # Term 5 is reference, so no boosting
    list[[i]] <- paste0(list[[i]], collapse="+OR+")
    list[[i]] <- gsub("'", "%27", list[[i]])
  }
  val <- unlist(list)
  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?q=%28CT%3D%28", 
         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Search PSYNDEX</a>")
}
