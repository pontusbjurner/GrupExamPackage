#' @title Image filter
#'
#' @param
#' @param
#'
#' @return
#'
#' @examples
#'
#'
install.packages("magick")
library(magick)
# load image
my_object <- image_read("Test image-1.jpg")
# convert image
my_bitmap <- my_object[[1]]

grayscale_filter <- function(pixel) {
  if (is.numeric(pixel[1]) && is.numeric(pixel[2]) && is.numeric(pixel[3])) {
    r <- pixel[1]
    g <- pixel[2]
    b <- pixel[3]

    gray_value <- round(0.299 * r + 0.587 * g + 0.114 * b)
    return(c(gray_value, gray_value, gray_value))
  } else {
    warning("Non-numeric pixel value encountered.")
    return(pixel)
  }
}

cutoff_filter <- function(pixel, cutoff = 127) {
  if (is.numeric(pixel[1]) && is.numeric(pixel[2]) && is.numeric(pixel[3])) {
    gray_value <- round(0.299 * pixel[1] + 0.587 * pixel[2] + 0.114 * pixel[3])

    if (gray_value <= cutoff) {
      return(c(0, 0, 0))  # Black
    } else {
      return(c(255, 255, 255))  # White
    }
  } else {
    warning("Non-numeric pixel value encountered.")
    return(pixel)
  }
}

filtered_array <- apply(my_bitmap, c(1, 2), grayscale_filter)  # For grayscale
filtered_array <- apply(my_bitmap, c(1, 2), function(x) cutoff_filter(x, cutoff = 150))
