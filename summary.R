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
data[data == ""] <- "Not Specified"
data$using_cmd <- gsub(" .*$", "", data$using_cmd)
data$using_git <- gsub(" .*$", "", data$using_git)
data$using_markdown <- gsub(" .*$", "", data$using_markdown)
data$using_r <- gsub(" .*$", "", data$using_r)
data$coding_experience <- gsub(" .*$", "", data$coding_experience)
data$favorite_pet[data$favorite_pet %like% "dog"] <- "Dog"
data$favorite_pet[data$favorite_pet %like% "cat"] <- "Cat"

#Variable to call, tell me to add more if you need....
num_total_students <- nrow(data) 

#Chloe's requested table
class_order <- c("Freshman", "Sophomore", "Junior", "Senior", "Not Specified")
exp_order   <- c("Lots", "Experimented", "Moderate", "Never")
sorted_table <- data %>%
  group_by(class_standing, coding_experience) %>%
  summarise(n = n()) %>%
  arrange(match(class_standing, class_order),
          match(coding_experience, exp_order))

#Function
vector_store_stuff <- c(num_total_students, data, sorted_table)
get_summary_stuff <- function(){
  return(vector_store_stuff)
}











