# How to Draw a Christmas Tree Using Crayon in R

# Check if the 'crayon' package is installed; install it if not
if (!requireNamespace("crayon", quietly = TRUE)) {
  install.packages("crayon")
}

# Load the 'crayon' package for colored text output
library(crayon)

# Set the height of the Christmas tree
height <- 50

# Define the symbols used for decorating the tree and their corresponding colors
# Each decoration symbol will be colored differently
decorations <- c("o", "+", "^")  # Decoration symbols
deco_colors <- list(red, blue, magenta)  # Corresponding color functions from 'crayon'

# Define a custom color function for the trunk using RGB values
# 'make_style' creates a new style for the specified color
brown <- function(text) {
  crayon::make_style(rgb(165, 42, 42, maxColorValue = 255))(text)
}

# Loop to create each row of the tree
for (i in 1:height) {
  # Calculate the number of stars (and decorations) for the current row
  # Using a factor of 2.5 to make the base of the tree wider
  num_stars <- round(2.5 * i - 1)
  
  # Create an empty character vector to store the row's content
  tree_row <- character(num_stars)
  
  # Fill the row with stars and decorations
  for (j in 1:num_stars) {
    if (j %% 2 == 0) {
      # For even positions, use decorations
      deco_index <- (j / 2) %% length(decorations) + 1
      tree_row[j] <- deco_colors[[deco_index]](decorations[deco_index])
    } else {
      # For odd positions, use stars
      tree_row[j] <- yellow("*")
    }
  }
  
  # Calculate the number of leading spaces for centering the row
  spaces <- max(0, (height * 2.5 - num_stars) / 2)
  
  # Print the row with leading spaces for centering
  cat(rep(" ", spaces), paste(tree_row, collapse = ""), "\n", sep = "")
}

# Print the trunk of the tree
trunk_row <- brown("#")  # Trunk symbol
trunk_width <- round(height / 5)  # Calculate trunk width relative to tree height
trunk_spaces <- (height * 2.5 - trunk_width) / 2  # Leading spaces for trunk centering
for (j in 1:2) {
  # Print two rows for the trunk
  cat(rep(" ", trunk_spaces), rep(trunk_row, trunk_width), "\n", sep = "")
}

# Print the "Merry Christmas" message
message <- "Merry Christmas from Linus Chirchir"  # Define the message
message_colored <- blue(message)  # Color the message blue
tree_width <- round(height * 2.5)  # Calculate the full width of the tree
message_padding <- max(0, (tree_width - nchar(message)) / 2)  # Calculate padding for centering the message
cat(rep(" ", message_padding), message_colored, "\n", sep = "")  # Print the centered message

