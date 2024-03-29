# Function for drawing the chart
chart_one_function <- function(data, x, y, fill_y) {
  # setting the color for the chart
  color_select <- c(c(rgb(254 / 255, 67 / 255, 101 / 255),
                      rgb(252 / 255, 157 / 255, 154 / 255),
                      rgb(249 / 255, 205 / 255, 173 / 255),
                      rgb(248 / 255, 202 / 255, 0 / 255),
                      rgb(131 / 255, 175 / 255, 155 / 255)))
  # making the chart using ggplot2 and ggplotly
  info <- paste0("Number of Students: ", data[[y]])
  chart <- ggplot() + theme_bw() +
    geom_bar(aes(y = data[[y]], x = data[[x]],
                 fill = data[[fill_y]], label = info),
             data = data, stat = "identity") +
    scale_fill_manual(values = color_select) +
    theme(legend.title = element_text(size = 8),
          legend.background = element_rect(fill = "white",
                                           size = 0.5,
                                           linetype = "solid",
                                           colour = "black"),
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    labs(x = gsub("_", " ", x),
         y = gsub("_", " ", y),
         fill = gsub("_", " ", fill_y)) +
    ggtitle(paste0(gsub("_", " ", fill_y), " by ", gsub("_", " ", x)))
  return(ggplotly(chart, tooltip = c("info")))
}