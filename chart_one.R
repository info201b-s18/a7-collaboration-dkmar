library("dplyr")
library("ggplot2")
library("plotly")
library("lintr")
library("ggthemes")

# Source in the sumaary table
source("summary.R")

# Function for drawing the chart 
chart_one_function <- function(data, x, y, fill_y) {
  # setting the color for the chart
  color_select <- c(c(rgb(254/255, 67/255, 101/255), 
                      rgb(252/255, 157/255, 154/255),
                      rgb(249/255, 205/255, 173/255),
                      rgb(248/255, 202/255, 0/255),
                      rgb(131/255, 175/255, 155/255)))
  # Factor class standing and experience level so the order of x-asix of a chart
  # can be rearrange by desired order
  sorted_table$class_standing <- factor(sorted_table$class_standing,
                                        levels = c("Freshman", "Sophomore",
                                                   "Junior", "Senior",
                                                   "Not Specified"))
  sorted_table$coding_experience <- factor(sorted_table$coding_experience,
                                           levels = c("Lots", "Experimented",
                                                      "Moderate", "Never",
                                                      "Not Specified"))
  # making the chart using ggplot2 and ggplotly
  text <- paste0("Number of Students: ", data[[y]])
  chart <- ggplot() + theme_bw() +
    geom_bar(aes(y = data[[y]], x = data[[x]], fill = data[[fill_y]]),
             data = data, stat = "identity", label = text) +
    scale_fill_manual(values = color_select) +
    theme(legend.title = element_text(size = 8),
          legend.background = element_rect(fill = "white",
                                           size = 0.5,
                                           linetype = "solid",
                                           colour = "black"),
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    labs(x =gsub("_", " ", x),
         y = gsub("_", " ", y),
         fill = gsub("_", " ", fill_y)) +
    ggtitle(paste0(gsub("_", " ", fill_y), " by ", gsub("_", " ", x)))
  return(ggplotly(chart, tooltip = c("label")))
}

chart_one_function(sorted_table, "class_standing",
                   "number_of_students", "coding_experience")