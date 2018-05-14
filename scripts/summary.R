summarizing_data <- function(data){
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
  num_total_students <- nrow(data)
  num_never_using_r <- data %>%
    filter(using_r == "Never") %>%
    summarize(percent_r_never =
              round(n() / num_total_students * 100, digits = 0)) %>%
    .$percent_r_never
  
  num_never_using_git <- data %>%
    filter(using_git == "Never") %>%
    summarize(percent_git_never =
              round(n() / num_total_students * 100, digits = 0)) %>%
    .$percent_git_never
  
  num_never_coding_exp <- data %>%
    filter(using_git == "Never") %>%
    summarize(percent_never_coding =
              round(n() / num_total_students * 100, digits = 0)) %>%
    .$percent_never_coding
  
  #Chloe's requested table
  class_order <- c("Freshman", "Sophomore", "Junior", "Senior", "Not Specified")
  exp_order   <- c("Lots", "Moderate", "Experimented", "Never", "Not Specified")
  
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
  
  # Return a list that store desired objects
  ret <- list("total_students" = num_total_students,
              "no_exp_r" = num_never_using_r,
              "no_exp_git" = num_never_using_git,
              "no_exp_coding" = num_never_coding_exp,
              "modified_table" = data,
              "sorted_data" = sorted_table)
  return(ret)
}
