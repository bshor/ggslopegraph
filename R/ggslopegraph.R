#' Plot a slopegraph using dplyr and ggplot2
#'
#' Creates a "slopegraph" as conceptualized by Edward Tufte. Slopegraphs are minimalist
#' and efficient presentations of your data that can simultaneously convey the relative rankings,
#' the actual numeric values, and the changes and directionality of the data over time.
#' Takes a dataframe as input, with three named columns being used to draw the plot.
#' Makes the required adjustments to the ggplot2 parameters and returns the plot.
#'
#' @param dataframe a dataframe or an object that can be coerced to a dataframe.
#' Basic error checking is performed, to include ensuring that the named columns
#' exist in the dataframe.
#' @param Times a column inside the dataframe that will be plotted on the x axis.
#' Traditionally this is some measure of time.  The function accepts a column of class
#' ordered, factor or character.  NOTE if your variable is currently a "date" class
#' you must convert before using the function with \code{as.character(variablename)}.
#' @param Measurement a column inside the dataframe that will be plotted on the y axis.
#' Traditionally this is some measure such as a percentage.  Currently the function
#' accepts a column of type integer or numeric.  The slopegraph will be most effective
#' when the measurements are not too disparate.
#' @param Grouping a column inside the dataframe that will be used to group and
#' distinguish measurements.
#' @param Data.label an optional column inside the dataframe that will be used
#'   as the label for the data points plotted.  Can be complex strings and
#'   have `NA` values but must be of class `chr`.  By default `Measurement` is
#'   converted to `chr` and used.
#' @param Title Optionally the title to be displayed. Title = NULL will remove it
#' entirely. Title = "" will provide an empty title but retain the spacing.
#' @param SubTitle Optionally the sub-title to be displayed.  SubTitle = NULL
#' will remove it entirely. SubTitle = "" will provide and empty title but retain
#' the spacing.
#' @param Caption Optionally the caption to be displayed. Caption = NULL will remove
#' it entirely. Caption = "" will provide and empty title but retain the spacing.
#' @param XTextSize Optionally the font size for the X axis labels to be displayed. XTextSize = 12 is the default must be a numeric. Note that X & Y axis text are on different scales
#' @param YTextSize Optionally the font size for the Y axis labels to be displayed.
#' YTextSize = 3 is the default must be a numeric. Note that X & Y axis text are on
#' different scales
#' @param TitleTextSize Optionally the font size for the Title to be displayed.
#' TitleTextSize = 14 is the default must be a numeric.
#' @param SubTitleTextSize Optionally the font size for the SubTitle to be displayed.
#' SubTitleTextSize = 10 is the default must be a numeric.
#' @param CaptionTextSize Optionally the font size for the Caption to be displayed.
#' CaptionTextSize = 8 is the default must be a numeric.
#' @param TitleJustify Justification of title can be either a character "L",
#'   "R" or "C" or use the \code{hjust = } notation from \code{ggplot2} with
#'   a numeric value between `0` (left) and `1` (right).
#' @param SubTitleJustify Justification of subtitle can be either a character "L",
#'   "R" or "C" or use the \code{hjust = } notation from \code{ggplot2} with
#'   a numeric value between `0` (left) and `1` (right).
#' @param CaptionJustify Justification of caption can be either a character "L",
#'   "R" or "C" or use the \code{hjust = } notation from \code{ggplot2} with
#'   a numeric value between `0` (left) and `1` (right).
#' @param LineThickness Optionally the thickness of the plotted lines that
#' connect the data points. LineThickness = 1 is the default must be a numeric.
#' @param DataTextSize Optionally the font size of the plotted data points. DataTextSize = 2.5
#' is the default must be a numeric.
#' @param DataTextColor Optionally the font color of the plotted data points. `"black"`
#' is the default can be either `colors()` or hex value e.g. "#FF00FF".
#' @param DataLabelPadding Optionally the amount of space between the plotted
#'   data point numbers and the label "box". By default very small = 0.05 to
#'   avoid overlap. Must be a numeric. Too large a value will risk "hiding"
#'   datapoints.
#' @param DataLabelLineSize Optionally how wide a line to plot around the data
#'   label box. By default = 0 to have no visible border line around the
#'   label. Must be a numeric.
#' @param DataLabelFillColor Optionally the fill color or background of the
#'   plotted data points. `"white"` is the default can be any of the `colors()`
#'   or hex value e.g. "#FF00FF".
#' @param LineColor Optionally the color of the plotted lines. By default it will use
#' the ggplot2 color palette for coloring by \code{Grouping}. The user may override
#' with \bold{one} valid color of their choice e.g. "black" (see colors() for choices)
#' \bold{OR}
#' they may provide a vector of colors such as c("gray", "red", "green", "gray", "blue")
#' \bold{OR} a named vector like c("Green" = "gray", "Liberal" = "red", "NDP" = "green",
#' "Others" = "gray", "PC" = "blue"). Any input must be character, and the length
#' of a vector \bold{should} equal the number of levels in \code{Grouping}. If the
#' user does not provide enough colors they will be recycled.
#' @param WiderLabels logical, set this value to \code{TRUE} if your "labels" or
#' \code{Grouping} variable values tend to be long. This setting will give them more
#' room in the same plot size.
#' @param ReverseYAxis logical, set this value to \code{TRUE} if you want
#' to reverse the Y scale, especially useful for rankings when you want #1 on
#' top.
#' @param ReverseXAxis logical, set this value to \code{TRUE} if you want
#' to reverse the **factor levels** on the X scale.
#' @param RemoveMissing logical, by default set to \code{TRUE} so that if any \code{Measurement}
#' is missing \bold{all rows} for that \code{Grouping} are removed. If set to \code{FALSE} then
#' the function will try to remove and graph what data it does have. \bold{N.B.} missing values
#' for \code{Times} and \code{Grouping} are never permitted and will generate a fatal error with
#' a warning.
#' @param ThemeChoice character, by default set to \bold{"bw"} the other
#' choices are \bold{"ipsum"}, \bold{"econ"}, \bold{"wsj"}, \bold{"gdocs"},
#' and \bold{"tufte"}.
#'
#'
#' @return A ggplot object.
#' @export
#' @importFrom dplyr filter group_by %>%
#' @importFrom ggplot2 aes element_blank element_text expand_limits
#' @importFrom ggplot2 geom_label geom_line ggplot labs scale_color_manual
#' @importFrom ggplot2 scale_x_discrete scale_y_reverse theme theme_bw theme_set
#' @importFrom ggrepel geom_text_repel
#' @importFrom forcats fct_rev
#' @importFrom grid unit
#' @importFrom methods hasArg is
#' @importFrom rlang .data enquo !!
#'
#' @author Thomas J. Leeper and Chuck Powell
#' @references
#' Leeper, Thomas J. slopegraph: Edward Tufte-Inspired Slopegraphs.
#' \url{https://github.com/leeper/slopegraph}
#'
#' Based on: Edward Tufte, Beautiful Evidence (2006), pages 174-176.
#' @examples
#' example <- data.frame(
#'   year = ordered(rep(c("2020", "2024"), 3), levels = c("2020", "2024")),
#'   value = c(10, 14, 8, 7, 15, 18),
#'   group = rep(c("A", "B", "C"), each = 2)
#' )
#'
#' ggslopegraph(
#'   example,
#'   year,
#'   value,
#'   group,
#'   Title = "Example slopegraph",
#'   SubTitle = NULL,
#'   Caption = NULL
#' )
ggslopegraph <- function(dataframe,
                        Times,
                        Measurement,
                        Grouping,
                        Data.label = NULL,
                        Title = "No title given",
                        SubTitle = "No subtitle given",
                        Caption = "No caption given",
                        XTextSize = 12,
                        YTextSize = 3,
                        TitleTextSize = 14,
                        SubTitleTextSize = 10,
                        CaptionTextSize = 8,
                        TitleJustify = "left",
                        SubTitleJustify = "left",
                        CaptionJustify = "right",
                        LineThickness = 1,
                        LineColor = "ByGroup",
                        DataTextSize = 2.5,
                        DataTextColor = "black",
                        DataLabelPadding = 0.05,
                        DataLabelLineSize = 0,
                        DataLabelFillColor = "white",
                        WiderLabels = FALSE,
                        ReverseYAxis = FALSE,
                        ReverseXAxis = FALSE,
                        RemoveMissing = TRUE,
                        ThemeChoice = "bw") {

  # ---------------- theme selection ----------------------------

  if (ThemeChoice == "bw") {
    theme_set(theme_bw())
  } else if (ThemeChoice == "ipsum") {
    require_suggested("hrbrthemes", "ipsum")
    theme_set(hrbrthemes::theme_ipsum_rc())
  } else if (ThemeChoice == "econ") {
    require_suggested("ggthemes", "econ")
    theme_set(ggthemes::theme_economist()) ## background = "#d5e4eb"
    if (DataLabelFillColor == "white") {
      DataLabelFillColor <- "#d5e4eb"
    }
  } else if (ThemeChoice == "wsj") {
    require_suggested("ggthemes", "wsj")
    theme_set(ggthemes::theme_wsj()) ## background = "#f8f2e4"
    if (DataLabelFillColor == "white") {
      DataLabelFillColor <- "#f8f2e4"
    }
    TitleTextSize <- TitleTextSize - 1
    SubTitleTextSize <- SubTitleTextSize + 1
  } else if (ThemeChoice == "gdocs") {
    require_suggested("ggthemes", "gdocs")
    theme_set(ggthemes::theme_gdocs())
  } else if (ThemeChoice == "tufte") {
    require_suggested("ggthemes", "tufte")
    theme_set(ggthemes::theme_tufte())
  } else {
    theme_set(theme_bw())
  }

  # ---------------- ggplot setup work ----------------------------

  # Since ggplot2 objects are just regular R objects, put them in a list
  MySpecial <- list(
    # Format tweaks
    scale_x_discrete(position = "top"), # move the x axis labels up top
    theme(legend.position = "none"), # Remove the legend
    theme(panel.border = element_blank()), # Remove the panel border
    theme(axis.title.y = element_blank()), # Remove just about everything from the y axis
    theme(axis.text.y = element_blank()),
    theme(panel.grid.major.y = element_blank()),
    theme(panel.grid.minor.y = element_blank()),
    theme(axis.title.x = element_blank()), # Remove a few things from the x axis
    theme(panel.grid.major.x = element_blank()),
    theme(axis.text.x.top = element_text(size = XTextSize, face = "bold")), # and increase font size
    theme(axis.ticks = element_blank()), # Remove x & y tick marks
    theme(plot.title = element_text(
      size = TitleTextSize,
      face = "bold",
      hjust = justifyme(TitleJustify)
    )),
    theme(plot.subtitle = element_text(
      size = SubTitleTextSize,
      hjust = justifyme(SubTitleJustify)
    )),
    theme(plot.caption = element_text(
      size = CaptionTextSize,
      hjust = justifyme(CaptionJustify)
    ))
  )

  # ---------------- input checking ----------------------------

  # error checking and setup
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed requires a dataframe, plus at least three variables")
  }
  argList <- as.list(match.call()[-1])
  if (!hasArg(dataframe)) {
    stop("You didn't specify a dataframe to use", call. = FALSE)
  }

  NTimes <- deparse(substitute(Times)) # name of Times variable
  NMeasurement <- deparse(substitute(Measurement)) # name of Measurement variable
  NGrouping <- deparse(substitute(Grouping)) # name of Grouping variable

  if(is.null(argList$Data.label)) {
    NData.label <- deparse(substitute(Measurement))
    Data.label <- argList$Measurement
  } else {
    NData.label <- deparse(substitute(Data.label))
    #     Data.label <- argList$Data.label
  }

  Ndataframe <- argList$dataframe # name of dataframe
  if (!is(dataframe, "data.frame")) {
    stop(paste0("'", Ndataframe, "' does not appear to be a data frame"))
  }
  if (!NTimes %in% names(dataframe)) {
    stop(paste0("'", NTimes, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (anyNA(dataframe[[NTimes]])) {
    stop(paste0("'", NTimes, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!NMeasurement %in% names(dataframe)) {
    stop(paste0("'", NMeasurement, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (!NGrouping %in% names(dataframe)) {
    stop(paste0("'", NGrouping, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (!NData.label %in% names(dataframe)) {
    stop(paste0("'", NData.label, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (anyNA(dataframe[[NGrouping]])) {
    stop(paste0("'", NGrouping, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!class(dataframe[[NMeasurement]]) %in% c("integer", "numeric")) {
    stop(paste0("Sorry I need the measured variable '", NMeasurement, "' to be a number"), call. = FALSE)
  }
  if (!"ordered" %in% class(dataframe[[NTimes]])) { # keep checking
    if (!"character" %in% class(dataframe[[NTimes]])) { # keep checking
      if ("factor" %in% class(dataframe[[NTimes]])) { # impose order
        message(paste0("\nConverting '", NTimes, "' to an ordered factor\n"))
        dataframe[[NTimes]] <- factor(dataframe[[NTimes]], ordered = TRUE)
      } else {
        stop(paste0("Sorry I need the variable '", NTimes, "' to be of class character, factor or ordered"), call. = FALSE)
      }
    }
  }

  Times <- enquo(Times)
  Measurement <- enquo(Measurement)
  Grouping <- enquo(Grouping)
  Data.label <- enquo(Data.label)

  # ---------------- handle some special options ----------------------------

  if (ReverseXAxis) {
    dataframe[[NTimes]] <- forcats::fct_rev(dataframe[[NTimes]])
  }

  NumbOfLevels <- nlevels(factor(dataframe[[NTimes]]))
  if (WiderLabels) {
    MySpecial <- c(MySpecial, expand_limits(x = c(0, NumbOfLevels + 1)))
  }

  if (ReverseYAxis) {
    MySpecial <- c(MySpecial, scale_y_reverse())
  }

  if (length(LineColor) > 1) {
    if (length(LineColor) < length(unique(dataframe[[NGrouping]]))) {
      message(paste0("\nYou gave me ", length(LineColor), " colors I'm recycling colors because you have ", length(unique(dataframe[[NGrouping]])), " ", NGrouping, "s\n"))
      LineColor <- rep(LineColor, length.out = length(unique(dataframe[[NGrouping]])))
    }
    LineGeom <- list(geom_line(aes(color = !!Grouping), linewidth = LineThickness), scale_color_manual(values = LineColor))
  } else {
    if (LineColor == "ByGroup") {
      LineGeom <- list(geom_line(aes(color = !!Grouping, alpha = 1), linewidth = LineThickness))
    } else {
      LineGeom <- list(geom_line(linewidth = LineThickness, color = LineColor))
    }
  }

  # logic to sort out missing values if any
  if (anyNA(dataframe[[NMeasurement]])) { # are there any missing
    if (RemoveMissing) { # which way should we handle them
      dataframe <- dataframe %>%
        group_by(!!Grouping) %>%
        filter(!anyNA(!!Measurement)) %>%
        droplevels()
    } else {
      dataframe <- dataframe %>%
        filter(!is.na(!!Measurement))
    }
  }

  left_labels <- dataframe %>%
    filter(!!Times == min(!!Times))

  right_labels <- dataframe %>%
    filter(!!Times == max(!!Times))

  # ---------------- main ggplot routine ----------------------------

  dataframe %>%
    ggplot(aes(group = !!Grouping, y = !!Measurement, x = !!Times)) +
    LineGeom +
    # left side y axis labels
    geom_text_repel(
      data = left_labels,
      aes(label = !!Grouping),
      hjust = "left",
      box.padding = 0.10,
      point.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "bold",
      size = YTextSize,
      nudge_x = -1.95,
      direction = "y",
      force = .5,
      max.iter = 3000
    ) +
    # right side y axis labels
    geom_text_repel(
      data = right_labels,
      aes(label = !!Grouping),
      hjust = "right",
      box.padding = 0.10,
      point.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "bold",
      size = YTextSize,
      nudge_x = 1.95,
      direction = "y",
      force = .5,
      max.iter = 3000
    ) +
    # data point labels
    geom_label(aes(label = .data[[NData.label]]),
               size = DataTextSize,
               # label.padding controls fill padding
               label.padding = unit(DataLabelPadding, "lines"),
               # linewidth controls width of line around label box
               # 0 = no box line
               linewidth = DataLabelLineSize,
               # color = text color of label
               color = DataTextColor,
               # fill background color for data label
               fill = DataLabelFillColor
    ) +
    MySpecial +
    labs(
      title = Title,
      subtitle = SubTitle,
      caption = Caption
    )

  # implicitly return plot object
} # end of function
