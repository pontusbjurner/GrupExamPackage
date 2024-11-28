#' @title Image filter
#'
#' @param bitmapimage
#' @param pixelvector
#'
#' @return grey or color filtered image
#'
#' @examples
#'
#'
install.packages("magick")
library(magick)
# load image
my_object <- image_read("Test image-1.jpg")
# convert image
my_bitmap <-my_object[[1]]


# view image
image_read(my_bitmap)

## gray and color filter functions ##
# Grayscale filter function
gray_filter <- function(bitmap) {
  # Extract RGB values
  r <- as.numeric(bitmap[1])
  g <- as.numeric(bitmap[2])
  b <- as.numeric(bitmap[3])

  # Grayscale value using the weighted average formula
  grayscale <- round(0.299 * r + 0.587 * g + 0.114 * b)

  # Return grayscale value repeated for R, G, and B as raw
  as.raw(rep(grayscale, 3))
}

# Color filter function (unchanged since it already uses raw)
color_filter <- function(bitmap, threshold = 127) {
  # Extract RGB values
  r <- as.numeric(bitmap[1])
  g <- as.numeric(bitmap[2])
  b <- as.numeric(bitmap[3])

  # Calculate grayscale
  grayscale <- round(0.299 * r + 0.587 * g + 0.114 * b)

  # Apply threshold
  if (grayscale > threshold) {
    as.raw(c(255, 0, 0))  # Red if above threshold
  } else {
    as.raw(c(0, 0, 255))  # Blue if below threshold
  }
}


#image filter function
apply_filter <- function(bitmap, filter_function, threshold = NULL) {
  dims <- dim(bitmap)  # dims[1]: color channels, dims[2]: width, dims[3]: height

  # Create a copy to hold the filtered image
  filtered_bitmap <- bitmap

  # Iterate through width and height
  for (y in 1:dims[2]) {  # Columns
    for (z in 1:dims[3]) {  # Rows
      # Extract the RGB vector for the current pixel
      pixel <- bitmap[, y, z]

      # Apply the filter function, with or without a threshold
      if (!is.null(threshold)) {
        filtered_pixel <- filter_function(pixel, threshold)
      } else {
        filtered_pixel <- filter_function(pixel)
      }

      # Assign the result back to the filtered bitmap
      filtered_bitmap[, y, z] <- filtered_pixel
    }
  }

  return(filtered_bitmap)
}


# Apply the grayscale filter
grayscale_bitmap <- apply_filter(my_bitmap, gray_filter)

# Convert back to an image and display
grayscale_image <- image_read(grayscale_bitmap)
print(grayscale_image)


# Apply color filter with a threshold of 127
color_filtered_bitmap <- apply_filter(my_bitmap, color_filter, threshold = 127)

# Convert back to an image and display
color_image <- image_read(color_filtered_bitmap)
print(color_image)

