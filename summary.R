library(dplyr)
library(stringr)
library(data.table)

data <- read.csv("data/intro-survey.csv", stringsAsFactors = FALSE)

#Just rename, should View(data)
colnames(data) <- c("class_standing", 
                    "major_interest", 
                    "using_os",
                    "using_cmd",
                    "using_git",
                    "using_markdown",
                    "using_r",
                    "coding_experience",
                    "num_country",
                    "is_born_wa",
                    "num_sibling",
                    "inches_tall",
                    "favorite_pet",
                    "is_seahawks_fan")
data$using_cmd <- gsub(" .*$", "", data$using_cmd)
data$using_git <- gsub(" .*$", "", data$using_git)
data$using_markdown <- gsub(" .*$", "", data$using_markdown)
data$using_r <- gsub(" .*$", "", data$using_r)
data$coding_experience <- gsub(" .*$", "", data$coding_experience)
data$favorite_pet[data$favorite_pet %like% "dog"] <- "Dog"
data$favorite_pet[data$favorite_pet %like% "cat"] <- "Cat"
data[data == ""] <- "Not Specified"

#Variable to call, tell me to add more if you need....
total_students <- nrow(data) 

#Chloe's requested table
sorted_table <- data %>%
  group_by(class_standing, coding_experience) %>%
  summarise(n = n()) %>%
  rename("number_of_students" = n) %>%
  arrange(match(class_standing, class_order),
          match(coding_experience, exp_order))
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

#Function....








