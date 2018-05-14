# Correlation between coding experience and use of git
chart_two_function <- function(data) {
  code_xp_git <- count(data, coding_experience, using_git)

  # Define ordering of Git experience (E.g. Expert after Intermediate)
  git_xp_levels <- c(
    "Not Specified", "Never", "Have",
    "Intermediate", "Expert"
  )

  # Define ordering of coding experience (E.g. Lots after Moderate)
  code_xp_levels <- c(
    "Not Specified", "Never",
    "Experimented", "Moderate", "Lots"
  )

  # Implement above orderings
  code_xp_git <- code_xp_git %>%
    mutate(
      using_git = factor(using_git, git_xp_levels),
      coding_experience = factor(coding_experience, code_xp_levels)
    ) %>%
    arrange(coding_experience, using_git)

  # Color scale for chart
  color_select <- c(c(rgb(254 / 255, 67 / 255, 101 / 255),
                      rgb(252 / 255, 157 / 255, 154 / 255),
                      rgb(249 / 255, 205 / 255, 173 / 255),
                      rgb(248 / 255, 202 / 255, 0 / 255),
                      rgb(131 / 255, 175 / 255, 155 / 255)))

  ggplot(code_xp_git) +
    geom_mosaic(aes(
      weight = n,
      x = product(coding_experience),
      fill = using_git
    )) +
    scale_fill_manual(
      guide = guide_legend(title = "Git Usage", reverse = TRUE),
      values = rev(color_select)) +
    labs(x = "Coding Experience",
         title = "Git Usage by Coding Experience Level") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.margin = margin(b = 20, l = 5, r = 5),
      axis.text.x = element_text(
        angle = 45,
        margin = margin(t = 25),
        size = 11),
      axis.title.x = element_text(
        margin = margin(t = -15),
        size = 14,
        hjust = 0.5))
}
