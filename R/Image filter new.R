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

#' @title Image to bitmap
#' @param file_path File path to the image to be converted
#' @return image converted to bitmap format
#' @export
#'
# Return image as a bitmap
bitmap <- function(file_path){
  # read in image
  my_object <- image_read("Test image-1.jpg")
  # convert image
  my_bitmap <-my_object[[1]]
  return(my_bitmap)
}

#' @title RGV vector to greyscale value
#' @param rgb_vector a vector of three elements represent the r,g and b values
#' @return The greyscale value or cutoff value
#' @export
#'
# Return image as a bitmap

## gray and color filter functions ##
# Grayscale filter function
gray_filter <- function(rgb_vector) { # argument name changed to rbg_vector to make it clear what the input should be
  # Extract RGB values
  r <- as.numeric(rgb_vector[1])
  g <- as.numeric(rgb_vector[2])
  b <- as.numeric(rgb_vector[3])

  # Grayscale value using the weighted average formula
  grayscale <- round(0.299 * r + 0.587 * g + 0.114 * b)

  # Return grayscale value repeated for R, G, and B as raw
  rgb_grayscale <- as.raw(rep(grayscale, 3))

  # return output of function
  return(rgb_grayscale) # ensure you return the output for the function to work
}


#' @title Colour cut off
#' @param rgb_vector a vector of three elements represent the r,g and b values
#' @return Return specific colour depending on whether above or below a threshold
#' @export
#'
# Return image as a bitmap

# Color filter function (unchanged since it already uses raw)
color_filter <- function(rgb_vector, threshold = 127) { #changed argument bitmap to rgb filter for clarity
  grayscale <- gray_filter(rgb_vector)
  # Apply threshold
  if (grayscale[[1]] > threshold) { #taking the first of the three greyscale values
    out_colour <- as.raw(c(255, 0, 0))  # Red if above threshold
  } else {
    out_colour <- as.raw(c(0, 0, 255))  # Blue if below threshold
  }
  return(out_colour) #added a return function
}

#' @title Iterative colour filter
#' @param rgb_vector a vector of three elements represent the r,g and b values
#' @return Return specific colour depending on whether above or below a threshold
#' @export
#'
# Return image as a bitmap

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
