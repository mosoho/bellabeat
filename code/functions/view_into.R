#' Viewing into data
#'
#' Preform skim, head, colnames and glimpse
#'
#' @param df Data frame
#'
#' @return Variable
#'
#' @examples
#' view_into(daily_activity)
#'
#' @export

# Function to look into data
view_into <- function(df){
  view_into_skim <- skim_without_charts(df)
  view_into_head <- head(df)
  view_into_colnames <- colnames(df)
  view_into_glimpse <- glimpse(df)
  view_into_list <- list("skim_without_charts" = view_into_skim,
                         "head" = view_into_head,
                         "colnames" = view_into_colnames,
                         "glimpse" = view_into_glimpse)
  return(view_into_list)
}
