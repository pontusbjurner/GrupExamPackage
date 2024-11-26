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
image_read(my_bitmap)
View(my_bitmap)

# Grayscale conversion function
rgb_to_gray <- function(r, g, b) {
  gray_value <- round(r * 0.299 + g * 0.587 + b * 0.114)
  return(rep(gray_value, 3))  # Return a vector with same grayscale value for all channels
}
# Color cut-off function
color_cutoff <- function(r, g, b, threshold = 127) {
  # Convert the pixel to grayscale first
  gray_value <- round(r * 0.299 + g * 0.587 + b * 0.114)

  # Apply a color cut-off based on the threshold
  if (gray_value < threshold) {
    # If below threshold, color it red
    return(c(255, 0, 0))  # RGB for red
  } else {
    # If above threshold, color it green
    return(c(0, 255, 0))  # RGB for green
  }
}

# Function to process image with a given filter (grayscale or color cutoff)
apply_filter <- function(my_bitmap, filter_function, threshold = 127) {
  # Get image dimensions
  height <- dim(my_bitmap)[1]
  width <- dim(my_bitmap)[2]

  # Iterate over each pixel and apply the filter
  for (y in 1:height) {
    for (x in 1:width) {
      # Get the RGB values of the pixel (ensure they are numeric)
      r <- as.numeric(my_bitmap[y, x, 1])
      g <- as.numeric(my_bitmap[y, x, 2])
      b <- as.numeric(my_bitmap[y, x, 3])

      # Check if the filter function expects a threshold argument
      if (length(formals(filter_function)) == 3) {
        # If the function expects 3 arguments (no threshold), apply it directly
        new_color <- filter_function(r, g, b)
      } else {
        # If the function expects 4 arguments (with threshold), pass the threshold
        new_color <- filter_function(r, g, b, threshold)
      }

      # Set the new color back to the pixel
      my_bitmap[y, x, 1] <- new_color[1]  # Update red channel
      my_bitmap[y, x, 2] <- new_color[2]  # Update green channel
      my_bitmap[y, x, 3] <- new_color[3]  # Update blue channel
    }
  }

  return(my_bitmap)
}


# Apply grayscale filter (no threshold needed for grayscale)
grayscale_bitmap <- apply_filter(my_bitmap, rgb_to_gray)

# Create a new image from the modified bitmap
grayscale_image <- image_read(grayscale_bitmap)

# Display the grayscale image
print(grayscale_image)

# Apply color cut-off filter with a threshold (127)
cutoff_bitmap <- apply_filter(my_bitmap, color_cutoff, threshold = 127)

# Create a new image from the modified bitmap
cutoff_image <- image_read(cutoff_bitmap)

# Display the cut-off image
print(cutoff_image)

# Save the processed image
image_write(grayscale_image, "grayscale_image.jpeg")
image_write(cutoff_image, "cutoff_image.jpeg")
