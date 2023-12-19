#' @rdname totemData
#' @import tibble
#' @export

setClass(
  Class = "totemData",
  representation = representation(
    data = "tbl_df",
    iProjections = "list"
  )
)

setValidity(
  Class = "totemData",
  method = function(object){
    errors <- character()
    if (! is(data(object), "tbl_df")){
      msg <- "The 'data' slot must be a 'tibble'."
      errors <- c(errors, msg)
    }
    if (! all(c("count", "freq") %in% colnames(data(object)))){
      msg <- "The 'data' slot must contain the columns 'counts' and 'freq'."
      errors <- c(errors, msg)
    }

    if (!is(iProjections(object), "list")){
      msg <- "The 'iProjections' should be a list"
      errors <- c(errors, msg)
    }

    if(! length(iProjections(object)) == 0){
      isIProjection <- sapply(
        iProjections(object),
        function(iProjection){
          is(iProjection, "iProjection")
        }
      )

      if (! all(isIProjection)){
        msg <- paste0("All iProjections should be iProjection objects, but list entries ", which(!isIProjection), " are not.")
      }
    }

    if (length(errors) == 0) TRUE else return(errors)

  }
)


#' totemData object and constructors
#'
#' \code{totemData} is used to store the TOTEM representation
#'
#' @slot data a 'tibble' representing entities, their counts (column 'count') and relative frequencies (column 'freq').
#' @slot iProjections a 'list' of iProjections
#'
#' @param df a 'data.frame' object with the subjects in the rows and their attributes in the columns.
#' @param dropUnseen logical, drop unseen attribute combinations (default: TRUE).
#' @return A totemData object
#'
#' @author Orestis Loukas & Ho Ryun Chung
#' @import dplyr
#' @rdname totemData
#' @export
totemDataFromDataFrame <- function(df, dropUnseen = TRUE){
  if (! is(df, "data.frame")){
    stop("'df' has to be a 'data.frame'")
  }
  df <- df %>%
    group_by(!!!syms(colnames(df))) %>%
    count(name = "count", .drop = dropUnseen)

  ## mutate does not work, I do not known why.
  df$freq <- df$count / sum(df$count, na.rm = TRUE)

  object <- new("totemData", data = df, iProjections = list())

  return(object)
}
