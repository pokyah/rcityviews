# Copyright (C) 2022-2024 Thomas Goossens

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Export Centered Square ggplot Using ragg with Consistent Scaling
#'
#' This function exports a square ggplot object to various standard paper sizes 
#' using the ragg package. It ensures consistent scaling of fonts and plot elements 
#' relative to the DPI.
#'
#' @param plot A ggplot object to be exported.
#' @param filename A character string specifying the name of the output file (without extension).
#' @param paper_size A character string specifying the paper size. 
#'        Supported sizes include "A0" to "A10", "B0" to "B10", "C4", "C5", "C6", 
#'        "Letter", "Legal", "Tabloid", "Postcard", "Double Postcard", and "Square Postcard".
#'        Default is "A4".
#' @param orientation A character string specifying the orientation of the paper. 
#'        Options are "portrait" (default) or "landscape".
#' @param format A character string specifying the output format. 
#'        Options are "png", "jpeg", or "tiff" (ragg does not support PDF). Default is "png".
#' @param dpi An integer specifying the resolution in dots per inch (DPI). Default is 300.
#' @param keep_square A logical indicating whether to maintain the plot as a square. 
#'        If TRUE, the shorter side of the paper size will be used for both width and height.
#'        Default is TRUE.
#' @param scale_factor A numeric value specifying how much to scale plot elements relative to the plot size.
#'        Default is 1, which means no scaling.
#'
#' @return No return value, called for side effects. The function saves the plot as a file in the specified format.
#' @export
#'
export_scaled_plot_ragg <- function(plot, filename, paper_size = "A4", orientation = "portrait", format = "png", dpi = 300, keep_square = TRUE, scale_factor = 1) {
  library(ggplot2)
  library(gridExtra)
  library(cowplot)
  
  # Define dimensions for paper sizes in mm
  paper_dimensions_mm <- list(
    "A0" = c(841, 1189),
    "A1" = c(594, 841),
    "A2" = c(420, 594),
    "A3" = c(297, 420),
    "A4" = c(210, 297),
    "A5" = c(148, 210),
    "A6" = c(105, 148),
    "A7" = c(74, 105),
    "A8" = c(52, 74),
    "A9" = c(37, 52),
    "A10" = c(26, 37),
    "B0" = c(1000, 1414),
    "B1" = c(707, 1000),
    "B2" = c(500, 707),
    "B3" = c(353, 500),
    "B4" = c(250, 353),
    "B5" = c(176, 250),
    "B6" = c(125, 176),
    "B7" = c(88, 125),
    "B8" = c(62, 88),
    "B9" = c(44, 62),
    "B10" = c(31, 44),
    "C4" = c(229, 324),
    "C5" = c(162, 229),
    "C6" = c(114, 162),
    "Letter" = c(216, 279),
    "Legal" = c(216, 356),
    "Tabloid" = c(279, 432),
    "Postcard" = c(100, 148),
    "Double Postcard" = c(148, 200),
    "Square Postcard" = c(120, 120)
  )
  
  # Check if the provided paper size is supported
  if (!paper_size %in% names(paper_dimensions_mm)) {
    stop("Unsupported paper size. Available sizes: ", paste(names(paper_dimensions_mm), collapse = ", "))
  }
  
  # Get the dimensions for the selected paper size in mm
  dimensions_mm <- paper_dimensions_mm[[paper_size]]
  
  # Adjust dimensions for orientation
  if (orientation == "landscape") {
    dimensions_mm <- rev(dimensions_mm)
  }
  
  # Adjust dimensions to maintain square aspect ratio if keep_square is TRUE
  if (keep_square) {
    square_side_mm <- min(dimensions_mm)
    dimensions_mm <- c(square_side_mm, square_side_mm)
  }
  
  # Convert dimensions directly from mm to pixels
  width_px <- dimensions_mm[1] * dpi / 25.4
  height_px <- dimensions_mm[2] * dpi / 25.4
  
  # Scale text elements, titles, and legend relative to scale_factor
  plot <- plot +
    theme(
      text = element_text(size = rel(scale_factor)),
      plot.title = element_text(size = rel(1.2 * scale_factor)),
      legend.text = element_text(size = rel(scale_factor)),
      legend.title = element_text(size = rel(1.1 * scale_factor)),
      axis.title = element_text(size = rel(scale_factor)),
      axis.text = element_text(size = rel(scale_factor))
    )
  
  # Use ragg functions to export based on the selected format
  if (format == "png") {
    ragg::agg_png(filename = paste0(filename, ".png"), width = width_px, height = height_px, units = "px", res = dpi)
  } else if (format == "jpeg") {
    ragg::agg_jpeg(filename = paste0(filename, ".jpeg"), width = width_px, height = height_px, units = "px", res = dpi)
  } else if (format == "tiff") {
    ragg::agg_tiff(filename = paste0(filename, ".tiff"), width = width_px, height = height_px, units = "px", res = dpi)
  } else {
    stop("Unsupported format for ragg. Available formats: png, jpeg, tiff.")
  }
  
  # Print the plot and save
  print(plot)
  dev.off()
  
  message("Plot exported successfully with consistent scaling using ragg!")
}

