#' @title Column_means
#' 
#' @description
#' This function calculates the mean of each column in a data frame.
#' 
#' @param data_frame 
#'
#' @return A numeric vector containing the column means.
#' @export
#' @examples
#' sample_df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' col_means(sample_df)
col_means <- function(data_frame) {
  means <- numeric(ncol(data_frame))
    for (i in 1:ncol(data_frame)) {
    means[i] <- mean(data_frame[[i]])
  }
    return(means)
}

#' @title Count NA
#' 
#' @description
#' This function counts the number of NA values in a vector using a for-loop.
#' 
#' @param vec 
#'
#' @return An integer indicating the number of NA values.
#' @export
#'
#' @examples
#' sample_vector <- c(1, 2, NA, 4, NA, 6, NA)
#' count_na(sample_vector)
count_na <- function(vec) {
  na_count <- 0
    for (val in vec) {
    if (is.na(val)) {
      na_count <- na_count + 1
    }
  }
    return(na_count)
}

