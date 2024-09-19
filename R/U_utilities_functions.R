
# general utilities ----
## find limits ----

#' Find Limits for Variable Scaling
#'
#' This function computes an upper limit for scaling variables for display purposes, useful for squishing variable scales to a specified quantile or standard deviation.
#' The limit is determined as the minimum of two values: a mean plus a multiple of standard deviation or a weighted quantile.
#'
#' @param var Numeric vector. The variable for which limits are being calculated.
#' @param quant.weights Numeric vector. Weights for each observation in `var`, used for calculating the weighted quantile. Default is equal weights.
#' @param consider Logical vector. Indicator of which elements in `var` should be considered. Default is all elements.
#' @param quant.prob Numeric. Probability for the quantile calculation. Default is 0.98.
#' @param sd.mult Numeric. Multiple of the standard deviation to add to the mean. Default is 3.
#'
#' @return Numeric. The upper limit for the variable scaling.
#'
#' @examplesIf requireNamespace("Hmisc", quietly = TRUE)
#' var <- rnorm(100)
#' find.lims(var)
#' @import Hmisc
#' @importFrom Hmisc wtd.quantile
#' @export
find.lims <- function(var, quant.weights = rep(1, length(var)), consider = rep(TRUE, length(var)), quant.prob = 0.98, sd.mult = 3) {
  # Ensure 'quant.weights' and 'consider' are numeric and logical respectively
  quant.weights <- as.numeric(quant.weights)
  consider <- as.logical(consider)


  # Calculate mean plus a multiple of standard deviation
  mean_limit <- mean(var[consider], na.rm = TRUE) + sd.mult * sd(var[consider], na.rm = TRUE)

  # Calculate weighted quantile
  quantile_limit <- Hmisc::wtd.quantile(x = var[consider], probs = quant.prob, weights = quant.weights[consider], na.rm = TRUE)

  # Return the minimum of the two limits
  min(mean_limit, quantile_limit)
}

## pad.lim ----
#' Pad Range of Numeric Vector with Percentage Padding
#'
#' This function pads the range of a numeric vector by a specified percentage. The padding is added to both the minimum and maximum values of the vector.
#'
#' @param x Numeric vector. The data for which the range is to be padded.
#' @param map.pad Numeric. The percentage of the range to add as padding to both ends. Default is 0.05 (5%).
#'
#' @return Numeric vector of length 2. The padded range, with the specified percentage added to both ends.
#'
#' @examples
#' pad.lim(c(10, 15, 20, 25))
#' pad.lim(c(50, 60, 70, 80), map.pad = 0.1)
#'
#' @export
pad.lim <- function(x, map.pad = 0.05) {
  if (!is.numeric(x) || length(x) < 2) {
    stop("'x' must be a numeric vector with at least two elements.")
  }

  range_diff <- diff(range(x, na.rm = TRUE))         # Calculate the difference between max and min
  c(min(x, na.rm = T) - range_diff * map.pad, max(x, na.rm = T) + range_diff * map.pad)
}


# keep.only.letters ----

#' Remove Non-Alphabetic Characters
#'
#' This function removes all non-alphabetic characters from a string, leaving only letters (both uppercase and lowercase).
#'
#' @param x Character vector. The input string(s) from which non-alphabetic characters will be removed.
#'
#' @return Character vector. The input string(s) with all non-alphabetic characters removed.
#'
#' @examplesIf requireNamespace("stringr", quietly = TRUE)
#' keep.only.letters("Hello, World! 123") # Returns "HelloWorld"
#' keep.only.letters(c("Test!@#123", "Another $tring!!")) # Returns c("Test", "Anothertring")
#'
#' @importFrom stringr str_replace_all regex
#' @export
keep.only.letters <- function(x) {
  if (!is.character(x)) {
    stop("'x' must be a character vector.")
  }
  stringr::str_replace_all(x, stringr::regex("[^a-zA-Z]"), "")
}



# PLoting utilities ----

## theme map ----

#' Custom ggplot2 Map Theme
#'
#' This function provides a customized `ggplot2` theme designed for map visualizations.
#' It includes options to control the inclusion of titles and subtitles, and allows
#' customization of legend size and position.
#'
#' @param leg.tit.size Numeric. Size of the legend title text. Default is 8.
#' @param legend.position Character or numeric vector. Position of the legend.
#'   Can be one of "none", "left", "right", "bottom", "top", or a numeric vector
#'   of length two. Default is "bottom".
#' @param include.title Logical. If `TRUE`, includes plot title and subtitle with
#'   the default text size. If `FALSE`, both title and subtitle are removed. Default is `TRUE`.
#' @param ... Additional arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 `theme` object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy)) +
#'     ggplot2::geom_point() +
#'     theme_map(leg.tit.size = 10, legend.position = "right", include.title = FALSE)
#' }
#'
#' @import ggplot2
#' @export
theme_map <- function(leg.tit.size = 8,
                      legend.position = "bottom",
                      include.title = TRUE,
                      ...) {

  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "sans", color = "#22211d"),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.title = if (include.title) ggplot2::element_text(size = 11, hjust = 0.5) else ggplot2::element_blank(),
      plot.subtitle = if (include.title) ggplot2::element_text(size = 8, hjust = 0.5) else ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = leg.tit.size),
      panel.grid.major = ggplot2::element_line(color = "transparent", size = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.position = legend.position,
      panel.border = ggplot2::element_blank(),
      ...
    )
}




## colour.brks ----

#' Generate Color Breaks with Transformations
#'
#' This function generates a sequence of evenly spaced numeric breaks within the specified limits.
#' It supports various transformations (log, sqrt, identity) and rounding to specific values.
#'
#' @param lims Numeric vector of length 2. Specifies the minimum and maximum limits for the breaks.
#' @param n Integer. The number of breaks to generate. Default is 5.
#' @param round_to Numeric. The value to which the breaks should be rounded. Default is 100.
#' @param just_pretty Logical. Whether to use pretty breaks or evenly spaced breaks. Default is TRUE.
#' @param transformation Character. The transformation to apply, one of "identity", "log10", or "sqrt". Default is "identity".
#'
#' @return A numeric vector of breaks, rounded to the specified value.
#'
#' @examples
#' lims <- c(1, 1000)
#' colour.brks(lims, n = 5, round_to = 100, just_pretty = TRUE, transformation = "identity")
#' colour.brks(lims, n = 5, round_to = 100, just_pretty = TRUE, transformation = "log10")
#' colour.brks(lims, n = 5, round_to = 100, just_pretty = TRUE, transformation = "sqrt")
#'
#' @export

colour.brks <- function(lims,
                        n = 5,
                        round_to = 100,
                        just_pretty = TRUE,
                        transformation = "identity") {
  # Handle the case for zero in limits
  include_zero <- 0 %in% lims

  if (just_pretty) {
    if (transformation == "log10") {
      if (any(lims < 0)) stop("Log transformation requires non-negative limits.")

      # Transform zero to a very small positive value if included
      log_lims <- log10(pmax(lims, .Machine$double.eps))  # avoid log10(0)
      breaks <- pretty(log_lims, n = n)

      # Transform back to original scale and round
      rounded_breaks <- round(10^breaks / round_to) * round_to
      if (include_zero) {
        rounded_breaks <- c(0, unique(rounded_breaks))
      }
      return(unique(rounded_breaks))

    } else if (transformation == "sqrt") {
      # Generate pretty breaks on the sqrt scale
      sqrt_lims <- sqrt(lims)
      breaks <- pretty(sqrt_lims, n = n)

      # Transform back to original scale and round
      rounded_breaks <- round(breaks^2 / round_to) * round_to
      if (include_zero) {
        rounded_breaks <- c(0, unique(rounded_breaks))
      }
      return(unique(rounded_breaks))

    } else {
      # For identity and other transformations
      breaks <- pretty(lims, n = n)
      rounded_breaks <- round(breaks / round_to) * round_to
      if (include_zero) {
        rounded_breaks <- c(0, unique(rounded_breaks))
      }
      return(unique(rounded_breaks))
    }
  } else {
    # Generate evenly spaced breaks on the transformed scale (log/sqrt/identity)
    if (transformation == "log10") {
      if (any(lims < 0)) stop("Log transformation requires non-negative limits.")

      # Handle zero explicitly
      log_lims <- log10(pmax(lims, .Machine$double.eps))
      breaks <- seq(from = min(log_lims), to = max(log_lims), length.out = n)
      rounded_breaks <- round(10^breaks / round_to) * round_to
      if (include_zero) {
        rounded_breaks <- c(0, unique(rounded_breaks))
      }
      return(unique(rounded_breaks))

    } else if (transformation == "sqrt") {
      sqrt_lims <- sqrt(lims)
      breaks <- seq(from = min(sqrt_lims), to = max(sqrt_lims), length.out = n)
      rounded_breaks <- round(breaks^2 / round_to) * round_to
      if (include_zero) {
        rounded_breaks <- c(0, unique(rounded_breaks))
      }
      return(unique(rounded_breaks))

    } else {
      # For identity or other transformations
      breaks <- seq(from = min(lims), to = max(lims), length.out = n)
      rounded_breaks <- round(breaks / round_to) * round_to
      if (include_zero) {
        rounded_breaks <- c(0, unique(rounded_breaks))
      }
      return(unique(rounded_breaks))
    }
  }
}


## colour.lable ----

#' Generate  Color Break Labels
#'
#' This function generates labeled breaks for color scales based on provided limits.
#' It supports appending a "+" symbol to the highest break if the data exceeds the specified limits.
#'
#' @param x Numeric vector. The data to be used for determining if the "+" symbol should be added to the highest break.
#' @param lims Numeric vector of length 2. Specifies the minimum and maximum limits for the breaks.
#' @param n Integer. The number of breaks to generate. Default is 5.
#' @param round_to Integer. The number to round the breaks to (e.g., 1 for rounding to the nearest whole number). Default is 1.
#' @param just_pretty Logical. Whether to make the breaks look "pretty" using the `pretty` function. Default is TRUE.
#' @param transformation Character. A transformation to apply to the breaks, such as "identity", "log", or "sqrt". Default is "identity".
#'
#' @return A character vector of labels for the breaks, with the highest value possibly having a "+" symbol if `max(x)` exceeds `max(lims)`.
#'
#' @export

colour.lable <- function(x,
                         lims,
                         n = 5,
                         round_to = 1,
                         just_pretty = T,
                         transformation = "identity") {
  breaks <- colour.brks(lims,
                        n = n,
                        round_to = round_to,
                        just_pretty = just_pretty,
                        transformation = transformation)
  # Format the breaks to remove scientific notation and add commas
  formatted_breaks <- trimws(format(breaks, big.mark = ",", scientific = FALSE))

  if (max(lims, na.rm = TRUE) < max(x, na.rm = TRUE)) {
    paste(formatted_breaks, c(rep("", times = length(formatted_breaks) - 1), "+"))
  } else {
    paste(formatted_breaks)
  }
}


## map plotter ----

#' Custom Map Plotter with ggplot2
#'
#' This function creates a map visualization using `ggplot2` and supports either
#' a `viridis` color scale or a custom gradient color scale, defined by the user. It allows for detailed
#' customization of color breaks, labels, and plot titles.
#'
#' @param fill.scale.title Character. The title for the fill scale (legend).
#' @param main.title Character. The main title of the plot.
#' @param sub.title Character. The subtitle of the plot.
#' @param fillground sf object. The spatial data to be filled and plotted.
#' @param fillground2 sf object. A secondary spatial data layer for plotting outlines such as surrounding landmass (default is the same as `fillground`).
#' @param pltly.text Character or NULL. Text information to be displayed in tooltips (for interactive plots).
#' @param transformation Character. Transformation to be applied to the fill scale. Default is "identity".
#' @param col.limits Numeric vector. The limits for the color scale. Default is `c(0, max(variable))`.
#' @param to.plot Numeric vector. The data values to be plotted.
#' @param clr.breaks Numeric vector. The breaks for the color scale. Default is generated by `colour.brks`.
#' @param clr.labels Character vector. Labels for the color breaks. Default is generated by `colour.lable`.
#' @param use.viridis Logical. If `TRUE`, the `viridis` color scale is used. If `FALSE`, a custom gradient scale from `low.col` to `high.col` is used. Default is `TRUE`.
#' @param low.col Character. The low-end color for the gradient scale (used when `use.viridis = FALSE`). Default is "white".
#' @param high.col Character. The high-end color for the gradient scale (used when `use.viridis = FALSE`). Default is "red".
#'
#' @return A `ggplot` object.
#'
#' @examples
#' # Example with viridis color scale
#'
#' # Example with custom gradient color scale
#'
#' @export
map.ploter <- function(fill.scale.title,
                       main.title,
                       sub.title,
                       fillground = COUNTRY.ATI.shp,
                       background = COUNTRY.ATI.shp,
                       pltly.text = NULL,
                       transformation = "identity",
                       to.plot = variable,
                       col.limits = c(0, max(variable)),
                       use.viridis = TRUE,
                       low.col = "white",
                       high.col = "red",
                       fill.line_size = 0.05,
                       fill.line_colour = "grey90",
                       background.fill = "grey90",
                       background.size = 0.05,
                       background.colour = "black",
                       n.breaks = 5,
                       round_to = 1,
                       just_pretty = T
                       ) {
  clr.breaks = colour.brks(lims = col.limits,
                           n = n.breaks,
                           round_to = round_to,
                           just_pretty = just_pretty,
                           transformation = transformation)

  clr.labels = colour.lable(x = to.plot,
                            lims = col.limits,
                            n = n.breaks,
                            round_to = round_to,
                            just_pretty = just_pretty,
                            transformation = transformation)

  ggplot() +
    geom_sf(data = background, fill = background.fill, size = background.size, colour = background.colour) +
    geom_sf(data = fillground, mapping = aes(fill = to.plot, text = pltly.text), colour = fill.line_colour,
            size = fill.line_size) +
    geom_sf(data = background, fill = NA, size = background.size, colour = background.colour) +

    {
      if (use.viridis) {
        scale_fill_viridis_c(trans = transformation,
                             name = fill.scale.title,
                             limits = col.limits,
                             oob = scales::squish,
                             breaks = clr.breaks,
                             labels = clr.labels,
                             guide = guide_colorbar(
                               direction = "horizontal", barheight = unit(2, units = "mm"),
                               barwidth = unit(50, units = "mm"), draw.ulim = F,
                               title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))
      } else {
        scale_fill_gradient(low = low.col, high = high.col,
                            name = fill.scale.title,
                            limits = col.limits,
                            oob = scales::squish,
                            breaks = clr.breaks,
                            labels = clr.labels,
                            guide = guide_colorbar(
                              direction = "horizontal", barheight = unit(2, units = "mm"),
                              barwidth = unit(50, units = "mm"), draw.ulim = F,
                              title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))
      }
    } +
    labs(x = NULL, y = NULL, title = main.title, subtitle = sub.title) +
    theme_map() +
    theme(legend.position = "bottom")
}


###########################################################################
# spatial curation --------------------------------------------------------
###########################################################################

## hexgrid over landscape ----


#' Generate an Efficient Target Hexagonal Grid
#'
#' This function creates a hexagonal grid over a specified target area by intersecting it with a master grid mask.
#' The resulting hexagonal grid is associated with grid IDs and calculates the area of each hexagon in hectares.
#' This optimized version reduces redundant operations and improves spatial computation efficiency.
#'
#' @param master.grid.mask sf object. A spatial feature representing the area over which the initial grid is generated. Default is `countries`.
#' @param grid.size Numeric vector of length 2. Specifies the horizontal and vertical distances between the centers of adjacent hexagons.
#'        Example: `c(hexdist.h, hexdist.v)`.
#' @param target sf object. The target area to intersect with the hexagonal grid.
#'
#' @return An `sf` object representing the hexagonal grid intersected with the target area. The object contains the grid ID and the area of each hexagon in hectares.
#'
#' @importFrom sf st_make_grid st_sf st_polygon st_intersection st_make_valid st_area st_cast st_geometry
#' @importFrom units set_units
#' @import magrittr
#' @export
Umake.target.hexgrid <- function(master.grid.mask = countries,
                                 grid.size = c(hexdist.h, hexdist.v),
                                 target) {
  # Ensure inputs are valid sf objects
  if (!inherits(master.grid.mask, "sf")) stop("master.grid.mask must be an sf object.")
  if (!inherits(target, "sf")) stop("target must be an sf object.")

  # Create a hexagonal grid over the master grid mask
  hex.grid <- sf::st_make_grid(master.grid.mask, grid.size, what = "polygons", square = FALSE)
  hex.grid <- sf::st_sf(hex.grid)
  hex.grid$grid_id <- seq_len(nrow(hex.grid))

  # Perform intersection and ensure valid geometries
  hex.grid.valid <- sf::st_make_valid(hex.grid)
  target.valid <- sf::st_make_valid(target)
  target.hexgrid <- sf::st_intersection(hex.grid.valid, target.valid)

  # Handle the case where intersection might be empty
  if (nrow(target.hexgrid) == 0) {
    warning("The intersection resulted in an empty grid.")
    return(target.hexgrid)
  }

  # Calculate the area of each polygon in hectares and add as a column
  area_ha <- as.numeric(units::set_units(sf::st_area(target.hexgrid), "ha"))
  target.hexgrid$hex.ha <- area_ha

  # Cast to POLYGON to ensure proper format
  target.hexgrid <- sf::st_cast(target.hexgrid, "MULTIPOLYGON", do_split = TRUE)
  target.hexgrid <- sf::st_cast(target.hexgrid, "POLYGON")

  return(target.hexgrid)
}


## st_erase ----

#' Erase Geometries from a Spatial Object
#'
#' A helper function that erases all of y from x. This function removes all geometries in one spatial object (`y`) from another spatial object (`x`).
#' It performs a spatial difference operation, effectively erasing the area covered by `y` from `x`.
#'
#' @param x An `sf` object. The spatial object from which geometries will be removed.
#' @param y An `sf` object. The spatial object containing geometries to be erased from `x`.
#'
#' @return An `sf` object. The resulting spatial object after erasing the geometries in `y` from `x`.
#'
#' @importFrom sf st_difference st_union st_combine
#' @export
st_erase <- function(x, y) {
  if (!inherits(x, "sf") || !inherits(y, "sf")) {
    stop("'x' and 'y' must be 'sf' objects.")
  }

  # Perform spatial difference operation to erase y from x
  sf::st_difference(x, sf::st_union(sf::st_combine(y)))
}

## st_first.spatial.curation ----

#' Perform Initial Spatial Curation on Polygon Data
#'
#' This function applies a series of spatial operations to clean and simplify polygon data. It transforms the coordinate reference system, simplifies the geometry, buffers to address tiny gaps, and makes the geometries valid.
#'
#' @param x An `sf` object of type `POLYGON` or `MULTIPOLYGON`. The spatial data to be curated.
#' @param tolerance Numeric. The tolerance level for geometry simplification. Higher values result in more simplification. Default is 10.
#' @param tiny.buff Numeric. The distance (in the CRS units) to buffer polygons to address tiny gaps. Default is 0.0001.
#' @param smallest.hole Numeric. The minimum area of holes to be removed. Currently not used in this implementation but can be added for additional filtering.
#'
#' @return An `sf` object of type `POLYGON` or `MULTIPOLYGON`. The curated spatial data after applying the transformations.
#'
#' @examplesIf requireNamespace("sf", quietly = TRUE)
#'
#' @importFrom sf st_transform st_simplify st_buffer st_make_valid st_read st_geometry
#' @export
st_first.spatial.curation <- function(x, tolerance = 10, tiny.buff = 0.0001, smallest.hole = 5000) {
  if (!inherits(x, "sf")) {
    stop("'x' must be an 'sf' object.")
  }

  x %>%
    sf::st_transform(crs = 27700) %>%                   # Transform to British National Grid (or another CRS)
    sf::st_simplify(preserveTopology = TRUE, dTolerance = tolerance) %>%  # Simplify geometry
    sf::st_buffer(dist = tiny.buff) %>%                  # Buffer to address tiny gaps
    sf::st_simplify(preserveTopology = TRUE, dTolerance = tolerance) %>%  # Simplify geometry again
    sf::st_make_valid()                                 # Make geometries valid
}


###########################################################################
# add rows that total all english regions and UK CARs ---------------------
###########################################################################

# add_eng_uk_tots ---------------------------------------------------------
#' Add England and UK Totals
#'
#' This function adds England and UK total rows to a data frame, summing up values as needed.
#'
#' @param df Data frame to which totals will be added.
#' @param of_colmn Column name to sum.
#' @param by_var Optional grouping variable.
#' @param cars.areas Optional vector of car area values.
#' @param cars.names Optional vector of car names.
#'
#' @return Data frame with England and UK totals added.
#' @export
add_eng_uk_tots <- function(df, of_colmn, by_var = NULL, cars.areas = NULL, cars.names = NULL) {
  if (!("ha.cars" %in% names(df))) {
    stop("Column 'ha.cars' is required if 'cars.areas' and 'cars.names' are provided.")
  }

  print.order <- data.frame(Name = c("Central", "North", "South East", "South West", "England Total", "Northern Ireland", "Scotland", "Wales", "UK Total"))
  print.order$order <- 1:nrow(print.order)

  if (!is.null(cars.areas) && !is.null(cars.names)) {
    df <- bind_rows(df,
                    data.frame(Name = cars.names[!cars.names %in% df$Name],
                               ha.car_rainfor = 0,
                               ha.cars = cars.areas[!cars.names %in% df$Name]))
  }

  uk.dat <- df %>%
    group_by(across({{ by_var }})) %>%
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>%
    mutate(Name = "UK Total")
  names(uk.dat) <- str_remove(names(uk.dat), "_sum")

  eng.regions.dat <- df %>%
    filter(Name %in% c("Central", "North", "South East", "South West")) %>%
    group_by(across({{ by_var }})) %>%
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>%
    mutate(Name = "England Total")
  names(eng.regions.dat) <- str_remove(names(eng.regions.dat), "_sum")

  if ("ha.cars" %in% names(df)) {
    eng.regions.dat$ha.cars <- sum(cars.areas[!duplicated(cars.names) & (cars.names %in% c("Central", "North", "South East", "South West"))])
    uk.dat$ha.cars <- sum(cars.areas[!duplicated(cars.names)])
  }

  nu.df <- bind_rows(df, eng.regions.dat, uk.dat) %>%
    full_join(print.order, by = "Name") %>%
    arrange(across({{ by_var }})) %>%
    arrange(order) %>%
    select(1:(ncol(df) + 1))

  nu.df
}


## sum_group_by_country - Just sum cars by country ----
#' Sum Values by Country and Add UK Total
#'
#' This function sums values by country and adds totals for England and the UK.
#'
#' @param df Data frame to process.
#' @param of_colmn Column name to sum.
#' @param by_var Optional grouping variable.
#' @param cars.areas Optional vector of car area values.
#' @param cars.names Optional vector of car names.
#'
#' @return Data frame with sums by country and UK total.
#' @export
sum_group_by_country <- function(df, of_colmn, by_var = NULL, cars.areas = NULL, cars.names = NULL) {
  print.order <- data.frame(Name = c(sort(countries.names), "UK Total"))
  print.order$order <- 1:nrow(print.order)

  not.eng <- df %>%
    filter(Name %in% countries.names)

  eng <- df %>%
    filter(Name %in% england.cars) %>%
    group_by(across({{ by_var }})) %>%
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>%
    mutate(Name = "England")
  names(eng) <- str_remove(names(eng), "_sum")

  uk.dat <- df %>%
    group_by(across({{ by_var }})) %>%
    summarise(across({{ of_colmn }}, list(sum = ~ sum(., na.rm = TRUE)))) %>%
    mutate(Name = "UK Total")
  names(uk.dat) <- str_remove(names(uk.dat), "_sum")

  if ("ha.cars" %in% names(df)) {
    eng$ha.cars <- sum(cars.areas[!duplicated(cars.names) & (cars.names %in% england.cars)])
    uk.dat$ha.cars <- sum(cars.areas[!duplicated(cars.names)])
  }

  nu.df <- bind_rows(not.eng, eng, uk.dat) %>%
    full_join(print.order, by = "Name") %>%
    arrange(across({{ by_var }})) %>%
    arrange(order) %>%
    select(1:(ncol(df) + 1))

  nu.df
}



# Print with commas -------------------------------------------------------
#' Format Vector with Commas
#'
#' This function formats a vector of strings with commas and "and" for readability.
#'
#' @param vec Character vector.
#'
#' @return Formatted string.
#' @export
print_with_commas <- function(vec) {
  if (!is.character(vec)) {
    stop("'vec' must be a character vector.")
  }

  n <- length(vec)
  if (n == 0) {
    return("")
  } else if (n == 1) {
    return(vec[1])
  } else if (n == 2) {
    return(paste(vec, collapse = " and "))
  } else {
    return(paste(paste(vec[-n], collapse = ", "), vec[n], sep = ", and "))
  }
}


# map_cat - map categorical sequential covar -----
#' Create a Categorical Map with ggplot2
#'
#' This function generates a categorical map using the `ggplot2` package to visualize spatial data. It allows for customization of titles, fill scale, and includes world map outlines (only ne countries, can edidt this).
#'
#' @param grid An `sf` object containing the spatial data to be visualized. Default is `pred.nwss.grid %>% st_simplify(dTolerance = 100)`.
#' @param var.name The name of the column in `grid` to be used for categorization. Default is `"grid.vet.type"`.
#' @param main.title The main title of the map. Default is `"NWSS veteran records"`.
#' @param sub.title Optional. The subtitle of the map. Default is `NULL`.
#' @param fill.scale.title Optional. Title for the fill scale (legend). Default is `NULL`.
#'
#' @return A `ggplot` object containing the generated map.
#'
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_d labs coord_sf theme
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_transform st_bbox
#' @export
map_cat <- function(grid = pred.nwss.grid %>% sf::st_simplify(dTolerance = 100),
                    var.name = "grid.vet.type",
                    main.title = "NWSS veteran records",
                    sub.title = NULL,
                    fill.scale.title = NULL) {
  # Ensure required packages are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required but not installed.")
  }
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("The 'rnaturalearth' package is required but not installed.")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required but not installed.")
  }


  # Retrieve world data
  world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

  # Extract the variable for filling
  var <- as.data.frame(grid)[[var.name]]

  # Create the map
  var.map <- ggplot2::ggplot(data = grid) +
    ggplot2::geom_sf(data = world %>% sf::st_transform(27700), size = 0.1) +
    ggplot2::geom_sf(mapping = ggplot2::aes(fill = var), colour = NA, size = 0) +
    ggplot2::scale_fill_viridis_d(name = fill.scale.title, na.value = "transparent",
                                  guide = ggplot2::guide_legend(title.position = "top", title.hjust = 0)) +
    ggplot2::geom_sf(data = world %>% sf::st_transform(27700), size = 0.1, fill = NA) +
    ggplot2::labs(x = NULL, y = NULL, title = main.title, subtitle = sub.title) +
    ggplot2::coord_sf(xlim = pad.lim(sf::st_bbox(grid)[c(1, 3)]),
                      ylim = pad.lim(sf::st_bbox(grid)[c(2, 4)]),
                      expand = FALSE) +
    theme_map() +
    ggplot2::theme(legend.position = "right")

  return(var.map)
}


# create_ordered_table  -----

#' Create Ordered Table
#'
#' This function creates a table based on the provided row and column variables, and orders
#' the rows by their total sum in descending order. You can choose to create a frequency table
#' or a proportion table. The table can also be rounded to a specified number of digits.
#'
#' @param data A data frame containing the data.
#' @param row_var The variable name (as a string) to be used for the rows.
#' @param col_var The variable name (as a string) to be used for the columns.
#' @param type The type of table to create. It can be either "frequency" or "proportion". Default is "frequency".
#' @param round_digits The number of digits to round the proportions to. Default is 3.
#'
#' @return A table with ordered rows and margins added.
#'
#' @export

create_ordered_table <- function(data, row_var, col_var, type = c("frequency", "proportion"), round_digits = 3) {
  # Create the table using the specified row and column variables
  table_data <- table(data[[row_var]], data[[col_var]])

  # Order the rows by the sum of each row, in descending order
  ordered_table <- table_data[order(rowSums(table_data), decreasing = TRUE), ]

  # Check if the user wants a proportion table
  if (type == "proportion") {
    ordered_table <- prop.table(ordered_table) %>% # Convert to proportions
      addmargins(, margin = c(1, 2))
    ordered_table <- round(ordered_table, round_digits) # Round proportions

    return(ordered_table)

  }else{
    ordered_table <- ordered_table %>%
      addmargins(, margin = c(1, 2))
    ordered_table <- round(ordered_table, round_digits) # Round proportions

    return(ordered_table)
  }


}


# replace roudned zeros with "<" string -----
#' Replace Rounded Zeros with a String
#'
#' This function rounds numeric data to a specified precision and replaces values
#' greater than 0 but rounded to 0 with a string indicating a value smaller than the threshold.
#'
#' @param data A numeric vector or matrix.
#' @param precision The number of decimal places to round the data. Default is 3.
#'
#' @return A vector or matrix with values replaced by a string if they are greater than 0
#' but round to 0, and the rest rounded to the specified precision.
#'
#' @examples
#' data <- c(0.0004, 0.00003, 0.5, 1)
#' replace_rounded_zeros_with_string(data, precision = 3)
#'
#' @export
replace_rounded_zeros_with_string <- function(data, precision = 3) {

  # Round the data
  rounded_data <- round(data, precision)

  # Find the threshold value for the precision (e.g., 0.001 for precision = 3)
  threshold <- 1 / (10^precision)
  replacement_str <- paste0("<", format(threshold, nsmall = precision))

  # Replace values greater than 0 but rounded to 0 with the replacement string
  adjusted_data <- ifelse(data > 0 & rounded_data == 0, replacement_str, rounded_data)

  return(adjusted_data)
}
