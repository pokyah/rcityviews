# Copyright (C) 2022-2024 Koen Derks

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

.getCity <- function(name) {
  if (is.null(name)) {
    city <- .randomCity(NULL)
  } else {
    if (inherits(name, "data.frame")) {
      stopifnot("input data frame is missing 'name' column" = "name" %in% colnames(name))
      stopifnot("input data frame is missing 'country' column" = "country" %in% colnames(name))
      stopifnot("input data frame is missing 'lat' column" = "lat" %in% colnames(name))
      stopifnot("input data frame is missing 'long' column" = "long" %in% colnames(name))
      city <- name
    } else {
      dataset <- rcityviews::cities
      indexes <- which(dataset[["name"]] == name)
      index <- .resolveConflicts(name, indexes, dataset)
      if (is.null(index)) {
        return(NULL)
      }
      city <- dataset[index, ]
    }
  }
  return(city)
}

.randomCity <- function(seed) {
  set.seed(seed)
  dataset <- rcityviews::cities
  dataset <- subset(dataset, dataset[["population"]] > 200000)
  index <- sample.int(nrow(dataset), size = 1)
  selected <- dataset[index, ]
  return(selected)
}

.resolveConflicts <- function(name, indexes, dataset) {
  index <- indexes
  if (length(indexes) == 0) {
    stop(paste0("There is no city called '", name, "' in the available data.\nUse 'new_city()' or create an issue including lat/long coordinates at https://github.com/koenderks/rcityviews/issues."))
  } else if (length(indexes) > 1) {
    selection <- utils::menu(
      choices = paste0(dataset[indexes, 1], ", ", dataset[indexes, 2], " | Lat: ", round(dataset[indexes, 3], 3), " | Long: ", round(dataset[indexes, 4], 3)),
      title = "More than one city matched to this name, which one to pick?"
    )
    if (selection == 0) {
      return(NULL)
    }
    index <- indexes[selection]
  }
  return(index)
}

.isColor <- function(x) {
  res <- try(grDevices::col2rgb(x), silent = TRUE)
  return(!inherits(res, "try-error"))
}

.tick <- function(verbose, progBar, ticks, shiny) {
  if (shiny) {
    shiny::incProgress(amount = 1 / ticks)
  } else {
    if (verbose) {
      progBar$tick()
    }
  }
}


.themeOptions <- function(theme) {
  colors <- switch(theme,
                   "vintage" = list(
                     "background" = "#fff7d8",
                     "water" = "#9ebfaa",
                     "landuse" = "#fff7d8",
                     "contours" = "#32130f",
                     "streets" = "#32130f",
                     "rails" = c("#32130f", "#fff7d8"),
                     "buildings" = c("#facc87", "#f39848", "#f8c98c", "#f58762"),
                     "text" = "#32130f",
                     "waterlines" = "#9ebfaa"
                   ),
                   "modern" = list(
                     "background" = "#e6ddd6",
                     "water" = "#656c7c",
                     "landuse" = "#7c9c6b",
                     "contours" = "#e6ddd6",
                     "streets" = "#fafafa",
                     "rails" = c("#fafafa", "#e6ddd6"),
                     "buildings" = "#eb3e20",
                     "text" = "#000000",
                     "waterlines" = "#656c7c"
                   ),
                   "bright" = list(
                     "background" = "#eeefc9",
                     "water" = "#9ddffb",
                     "landuse" = c("#f2f4cb", "#d0f1bf", "#64b96a"),
                     "contours" = "#eeefc9",
                     "streets" = "#2f3737",
                     "rails" = c("#2f3737", "#eeefc9"),
                     "buildings" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
                     "text" = "#2f3737",
                     "waterlines" = "#9ddffb"
                   ),
                   "delftware" = list(
                     "background" = "#fafafa",
                     "water" = "#fafafa",
                     "landuse" = c("#7ebaee", "#8da8d7", "#3259a6", "#0c133f", "#080e1c"),
                     "contours" = "#fafafa",
                     "streets" = "#1F305E",
                     "rails" = c("#1F305E", "#fafafa"),
                     "buildings" = c("#7ebaee", "#8da8d7", "#3259a6", "#0c133f", "#080e1c"),
                     "text" = "#000000",
                     "waterlines" = "#fafafa"
                   ),
                   "comic" = list(
                     "background" = "#ffffff",
                     "water" = "#607ba4",
                     "landuse" = "#4b9475",
                     "contours" = "#222222",
                     "streets" = "#222222",
                     "rails" = c("#222222", "#ffffff"),
                     "buildings" = c("#f4d749", "#daa520", "#a63c44"),
                     "text" = "#222222",
                     "waterlines" = "#607ba4"
                   ),
                   "rouge" = list(
                     "background" = "#a25543",
                     "water" = "#f2deb8",
                     "landuse" = "#a25543",
                     "contours" = "#f2deb8",
                     "streets" = "#f2deb8",
                     "rails" = c("#f2deb8", "#a25543"),
                     "buildings" = "#f2deb8",
                     "text" = "#f2deb8",
                     "waterlines" = "#f2deb8"
                   ),
                   "original" = list(
                     "background" = "#fdf9f5",
                     "water" = "#fdf9f5",
                     "landuse" = "#fdf9f5",
                     "contours" = "#32130f",
                     "streets" = "#32130f",
                     "rails" = "#32130f",
                     "buildings" = "#fdf9f5",
                     "text" = "#32130f",
                     "waterlines" = "#32130f"
                   ),
                   "midearth" = list(
                     "background" = "#b8a580",
                     "water" = "#c3c9b6",
                     "landuse" = "#b8a580",
                     "contours" = "#53402a",
                     "streets" = "#221c18",
                     "rails" = "#221c18",
                     "buildings" = "#53402a",
                     "text" = "#221c18",
                     "waterlines" = "#c3c9b6",
                     "textshadow" = "#f7f3ea"
                   ),
                   "batik" = list(
                     "background" = "#161417",
                     "water" = "#214040",
                     "landuse" = c("#ece3d9", "#9e5426", "#5d473c", "#C0b28a"),
                     "contours" = "#1d1d23",
                     "streets" = "#d7c5b8",
                     "rails" = "#d7c5b8",
                     "buildings" = c("#ece3d9", "#9e5426", "#5d473c", "#c0b28a"),
                     "text" = "#d7c5b8",
                     "waterlines" = "#214040"
                   ),
                   "vice" = list(
                     "background" = "#ffffff",
                     "water" = "#a3bff4",
                     "landuse" = "#6ece92",
                     "contours" = "#000000",
                     "streets" = "#e282af",
                     "rails" = "#e282af",
                     "buildings" = "#fff01f",
                     "text" = "#ffffff",
                     "waterlines" = "#a3bff4",
                     "textshadow" = "#e282af"
                   ),
                    # Prettymaps themes
                   "default" = list(
                     "background" = "#F4F4F4",
                     "water" = "#9ED0E6",
                     "landuse" = c("#A7C4A0", "#DDECDC", "#B5E3C6"),
                     "contours" = "#CCCCCC",
                     "streets" = "#FFFFFF",
                     "rails" = c("#B5B5B5", "#E0E0E0"),
                     "buildings" = c("#CCCCCC", "#E5E5E5"),
                     "text" = "#333333",
                     "waterlines" = "#99C4DE"
                   ),
                   "macao" = list(
                     "background" = "#ECE2D0",
                     "water" = "#B8D7E6",
                     "landuse" = c("#B4D4C3", "#E6E6A1", "#F8C8DC"),
                     "contours" = "#F3B3A5",
                     "streets" = "#E8D4C3",
                     "rails" = c("#C2C1C1", "#F5D3B1"),
                     "buildings" = c("#F0E3CA", "#EDD7B1", "#D9BC8C"),
                     "text" = "#4B4B4B",
                     "waterlines" = "#9FC0D4"
                   ),
                   "minimal" = list(
                     "background" = "#F8F8F8",
                     "water" = "#E0E0E0",
                     "landuse" = "#F0F0F0",
                     "contours" = "#C0C0C0",
                     "streets" = "#A0A0A0",
                     "rails" = c("#B0B0B0", "#D0D0D0"),
                     "buildings" = c("#D0D0D0", "#B0B0B0", "#808080"),
                     "text" = "#2F2F2F",
                     "waterlines" = "#A9A9A9"
                   ),
                   "tijuca" = list(
                     "background" = "#1E1F26",
                     "water" = "#2E5C77",
                     "landuse" = c("#517875", "#A3BF80", "#D4D4A8"),
                     "contours" = "#3C3C3C",
                     "streets" = "#F5E9DA",
                     "rails" = c("#F1F1F1", "#333333"),
                     "buildings" = c("#3A3A3A", "#6C6C6C", "#9E9E9E"),
                     "text" = "#FFFFFF",
                     "waterlines" = "#336E87"
                   ),
                   "oslo" = list(
                     "background" = "#EBF4FA",
                     "water" = "#C0D6DF",
                     "landuse" = c("#9BC2B3", "#B4E197", "#F9F871"),
                     "contours" = "#A3A3A3",
                     "streets" = "#D9D9D9",
                     "rails" = c("#BFBFBF", "#DADADA"),
                     "buildings" = c("#898989", "#B3B3B3", "#D6D6D6"),
                     "text" = "#505050",
                     "waterlines" = "#AACCE1"
                   ),
                   "tokyo" = list(
                     "background" = "#FAF3F2",
                     "water" = "#A8D3E6",
                     "landuse" = c("#BCE2E8", "#FFE4CC", "#F7CACD"),
                     "contours" = "#E7E7E7",
                     "streets" = "#FFFFFF",
                     "rails" = c("#DFDFDF", "#EFEFEF"),
                     "buildings" = c("#FFCEB4", "#F28D89", "#BBE3D5"),
                     "text" = "#333333",
                     "waterlines" = "#90C7DA"
                   ),
                   "paris" = list(
                     "background" = "#F5F3F1",
                     "water" = "#B4D0DE",
                     "landuse" = c("#E8D3D0", "#C2AFAE", "#BE97AD"),
                     "contours" = "#D3D3D3",
                     "streets" = "#FAFAFA",
                     "rails" = c("#D6D6D6", "#E9E9E9"),
                     "buildings" = c("#E0C6BF", "#CEB0A5", "#B89F91"),
                     "text" = "#494949",
                     "waterlines" = "#ACC8DA"
                   ),
                   # mythemes
                   # "dark" = list(
                   #   "background" = "#522258",
                   #   "water" = "#D95F59",
                   #   "landuse" = c("#522258", "#522258", "#522258"),
                   #   "contours" = "#522258",
                   #   "streets" = "#8C3061",
                   #   "rails" = c("#8C3061", "#522258"),
                   #   "buildings" = c("#522258", "#522258", "#522258"),
                   #   "text" = "#D95F59",
                   #   "waterlines" = "#D95F59"
                   # ),
                   "dark" = list(
                     "background" = "#0A1931",
                     "water" = "#185ADB",
                     "landuse" = c("#0A1931", "#0A1931", "#0A1931"),
                     "contours" = "#FFC947",
                     "streets" = "#FFC947",
                     "rails" = c("#FFC947", "#0A1931"),
                     "buildings" = c("#FEDDBE", "#FEDDBE", "#FEDDBE"),
                     "text" = "#FFC947",
                     "waterlines" = "#185ADB"
                    ),
                   "light" = list(
                     "background" = "#ffffff",
                     "water" = "#cedce9",
                     "landuse" = c("#ffffff", "#ffffff", "#ffffff"),
                     "contours" = "#ffffff",
                     "streets" = "#4b4b4b",
                     "rails" = c("#4b4b4b", "#ffffff"),
                     "buildings" = c("#ffffff", "#ffffff", "#ffffff"),
                     "text" = "#4b4b4b",
                     "waterlines" = "#cedce9"
                   ),
                   # "dark" = list(
                   #   "background" = "#006769",
                   #   "water" = "#006769",
                   #   "landuse" = c("#40A578", "#9DDE8B", "#E6FF94"),
                   #   "contours" = "#006769",
                   #   "streets" = "#9DDE8B",
                   #   "rails" = c("#9DDE8B", "#0A1931"),
                   #   "buildings" = c("#E6FF94", "#E6FF94", "#E6FF94"),
                   #   "text" = "#9DDE8B",
                   #   "waterlines" = "#006769"
                   # )
  )
  font <- switch(theme,
                 "vintage" = list(
                   "family" = "Fredericka the Great",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "modern" = list(
                   "family" = "Imbue",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "bright" = list(
                   "family" = "Damion",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "delftware" = list(
                   "family" = "Dancing Script",
                   "face" = "bold",
                   "scale" = 1
                 ),
                 "comic" = list(
                   "family" = "Rampart One",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "rouge" = list(
                   "family" = "Oswald",
                   "face" = "bold",
                   "scale" = 1
                 ),
                 "original" = list(
                   "family" = "Caveat",
                   "face" = "bold",
                   "scale" = 1
                 ),
                 "midearth" = list(
                   "family" = "American Uncial Regular",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "batik" = list(
                   "family" = "Walter Turncoat",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "vice" = list(
                   "family" = "Rage",
                   "face" = "bold",
                   "scale" = 1
                 ),
                 # Fonts for prettymaps themes
                 "default" = list(
                   "family" = "Ubuntu Mono",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "macao" = list(
                   "family" = "Ubuntu Mono",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "minimal" = list(
                   "family" = "Caveat",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "tijuca" = list(
                   "family" = "Libre Baskerville",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "oslo" = list(
                   "family" = "Libre Baskerville",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "tokyo" = list(
                   "family" = "Ubuntu Mono",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "paris" = list(
                   "family" = "Ubuntu Mono",
                   "face" = "plain",
                   "scale" = 1
                 ),
                 "dark" = list(
                   "family" = "Ubuntu Mono",
                   "face" = "plain",
                   "scale" = 1
                ),
                "light" = list(
                  "family" = "Ubuntu Mono",
                  "face" = "plain",
                  "scale" = 1
                )
  )
  size <- list()
  size[["borders"]] <- list(
    "contours" = 0.15,
    "water" = 0.4,
    "canal" = 0.5,
    "river" = 0.6
  )
  size[["streets"]] <- list(
    "path" = 0.2,
    "residential" = 0.3,
    "structure" = 0.35,
    "tertiary" = 0.4,
    "secondary" = 0.5,
    "primary" = 0.6,
    "motorway" = 0.8,
    "rails" = 0.65,
    "runway" = 3
  )
  themeOptions <- list(
    "colors" = colors,
    "font" = font,
    "size" = size
  )
  return(themeOptions)
}


