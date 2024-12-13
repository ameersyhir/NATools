#' Detect Missing Values
#'
#' This function detects missing values in each column of a dataset.
#' It returns a named vector with counts of missing values for each column that has missing data.
#'
#' @param data A data frame or tibble
#' @return A named vector with counts of missing values for each column.
#' @export
#' @examples
#' detectNA(airquality)
#'

detectNA <- function(data){
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)

  for(i in 1:ncol(data)){
    missing_counts[i] <- sum(is.na(data[[i]]))
  }

  # Filter out columns with no missing values
  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}

