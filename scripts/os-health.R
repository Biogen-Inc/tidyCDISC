library(glue)
library(dplyr)
library(GithubMetrics)

repos <- "Biogen-Inc/tidyCDISC"

# commits
commits <- gh_commits_get(
  repos,
  days_back = 365*10
)

# issues
issues <- gh_issues_get(repos, days_back =  365*10)

issues_enriched <- issues %>%
  mutate(
    days_open = as.numeric(Sys.Date() - as.Date(created)),
    days_no_activity = as.numeric(Sys.Date() - as.Date(updated))
  ) %>%
  select(
    full_name, state, days_open, days_no_activity
  )

commits_enriched <- commits %>%
  mutate(
    date = as.Date(datetime)
  ) %>%
  select(full_name, date, author)

scoring <- tibble(
  full_name = unique(commits$full_name)
) %>%
  left_join(
    gh_metric_issues(issues_enriched), by = "full_name"
  ) %>%
  left_join(
    gh_metric_commits_days_since_commit(commits_enriched), by = "full_name"
  ) %>%
  left_join(
    gh_metric_commits_prepost_midpoint(commits_enriched), by = "full_name"
  ) %>%
  left_join(
    gh_metric_commits_authors_ever(commits_enriched), by = "full_name"
  ) %>%
  left_join(
    gh_metric_commits_authors_prepost_midpoint(commits_enriched), by = "full_name"
  ) %>%
  gh_score()

os_health <- scoring$score

template <- "https://img.shields.io/badge/{label}-{value}-{colour}"

badge_health = case_when(
  is.na(os_health) ~ as.character(glue(
    template, 
    label = "OS Health",
    colour = "red",
    value = 0
  )),
  os_health > 90 ~ as.character(glue(
    template, 
    label = "OS Health",
    colour = "brightgreen",
    value = os_health
  )),
  os_health > 80 ~ as.character(glue(
    template, 
    label = "OS Health",
    colour = "green",
    value = os_health
  )),
  os_health > 60 ~ as.character(glue(
    template, 
    label = "OS Health",
    colour = "yellowgreen",
    value = os_health
  )),
  os_health > 50 ~ as.character(glue(
    template, 
    label = "OS Health",
    colour = "yellow",
    value = os_health
  )),
  os_health > 40 ~ as.character(glue(
    template, 
    label = "OS Health",
    colour = "orange",
    value = os_health
  )),
  TRUE ~ as.character(glue(
    template, 
    label = "OS Health",
    colour = "red",
    value = os_health
  ))
)
