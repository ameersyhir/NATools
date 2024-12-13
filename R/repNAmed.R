#' Replace Missing Values with Medians
#'
#' This function replaces missing values in numeric columns with the median of each column.
#'
#' @param dataset A data frame or tibble
#' @return A data frame with missing values replaced by column medians.
#' @export
#' @examples
#' repNAmed(airquality)
#'


repNAmed <- function(dataset) {
  # Loop through each column
  for (i in 1:ncol(dataset)) {
    # Check if the column is numeric and contains missing values
    if (is.numeric(dataset[[i]]) && any(is.na(dataset[[i]]))) {
      # Calculate the median for the column, excluding NA values
      median_value <- median(dataset[[i]], na.rm = TRUE)
      # Replace missing values with the median
      dataset[[i]][is.na(dataset[[i]])] <- median_value
    }
  }
  return(dataset)
}

