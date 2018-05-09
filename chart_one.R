library("dplyr")
library("ggplot2")
library("plotly")
library("lintr")
library("ggthemes")

# read in the data 
survey_data <- read.csv("data/intro-survey.csv", as.is = TRUE)

# wrangling with the data 
survey_data_sorted <- survey_data %>%
  rename("class_standing" = What.is.your.current.class.standing.,
         "coding_experience" =
           How.would.you.describe.your.coding.programming.experience.) %>%
  mutate(coding_experience = gsub( " .*$", "", coding_experience)) %>%
  replace(. == "", "Not Specified") %>%
  group_by(class_standing, coding_experience) %>%
  tally(sort = TRUE) %>%
  arrange(match(class_standing, c("Freshman", "Sophomore", "Junior", "Senior",
                                  "Not Specified")),
          match(coding_experience, c("Never", "Experimented", "Moderate",
                                     "Lots", "Not Specified")))

# Function called to draw the chart 
chart_one_function <- function(data) {
  # First factor class standing and experience level to rearrange
  # the order of x-asix
  class <- factor(data$class_standing,
                  levels = c("Freshman", "Sophomore", "Junior", "Senior",
                             "Not Specified"))
  group <- factor(data$coding_experience,
                  levels = c("Not Specified", "Lots", "Moderate",
                             "Experimented", "Never"))
  chart <- ggplot() + theme_bw() +
    geom_bar(aes(y = n, x = class, fill = group),
             data = survey_data_sorted, stat = "identity") +
    scale_fill_manual(values = c("coral", "gold", "deepskyblue",
                                 "seagreen1", "pink1")) +
    geom_text(data = survey_data_sorted,
              aes(x = class, y = n, label = paste0(n, " students")),
              size = 3, position = position_stack(vjust = 0.5)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size = 8),
          legend.background = element_rect(fill = "white",
                                           size = 0.5,
                                           linetype = "solid",
                                           colour = "black"),
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    labs(x = "Class Standing", y = "Number \n of \n Students",
         fill = "Coding Experience") +
    ggtitle("Programing Experience by Class Standing")
  return(chart)
}

# Testing the function
chart_one_function(survey_data_sorted)