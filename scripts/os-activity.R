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

os_activity <- scoring$score

template <- "https://img.shields.io/badge/{label}-{value}-{colour}"

badge_activity = case_when(
  is.na(os_activity) ~ as.character(glue(
    template, 
    label = "OS Activity",
    colour = "red",
    value = 0
  )),
  os_activity > 90 ~ as.character(glue(
    template, 
    label = "OS Activity",
    colour = "brightgreen",
    value = os_activity
  )),
  os_activity > 80 ~ as.character(glue(
    template, 
    label = "OS Activity",
    colour = "green",
    value = os_activity
  )),
  os_activity > 60 ~ as.character(glue(
    template, 
    label = "OS Activity",
    colour = "yellowgreen",
    value = os_activity
  )),
  os_activity > 50 ~ as.character(glue(
    template, 
    label = "OS Activity",
    colour = "yellow",
    value = os_activity
  )),
  os_activity > 40 ~ as.character(glue(
    template, 
    label = "OS Activity",
    colour = "orange",
    value = os_activity
  )),
  TRUE ~ as.character(glue(
    template, 
    label = "OS Activity",
    colour = "red",
    value = os_activity
  ))
)
