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
  rename("number_of_students" = n) %>%
  arrange(match(class_standing, c("Freshman", "Sophomore", "Junior", "Senior",
                                  "Not Specified")),
          match(coding_experience, c("Lots", "Experimented", "Moderate",
                                     "Never", "Not Specified"))) 

# Factor class standing and experience level so the order of x-asix of a chart
# can be rearrange by desired order
survey_data_sorted$class_standing <- factor(survey_data_sorted$class_standing,
                levels = c("Freshman", "Sophomore", "Junior", "Senior",
                           "Not Specified"))
survey_data_sorted$coding_experience <- factor(survey_data_sorted$coding_experience,
                levels = c("Lots", "Experimented", "Moderate", "Never",
                           "Not Specified"))

# Function called to draw the chart 
chart_one_function <- function(data, x, y, fill_y) {
  # setting the color for the chart
  color_select <- c(c(rgb(254/255, 67/255, 101/255), 
                      rgb(252/255, 157/255, 154/255),
                      rgb(249/255, 205/255, 173/255),
                      rgb(248/255, 202/255, 0/255),
                      rgb(131/255, 175/255, 155/255)))
  text <- paste0("Number of Students: ", data[[y]])
  # making the chart using ggplot2 and ggplotly
  chart <- ggplot() + theme_bw() +
    geom_bar(aes(y = data[[y]], x = data[[x]], fill = data[[fill_y]],
                 label = text),
             data = data, stat = "identity") +
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

chart_one_function(survey_data_sorted, "class_standing", "number_of_students", "coding_experience")
