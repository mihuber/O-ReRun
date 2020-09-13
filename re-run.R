library(tidyverse)
library(readxl)
library(cowplot)

top_time_percent = 0.25

top_time <- function(c, p) {
    n = round(length(c) * p)
    t = sort(c, decreasing = FALSE)[1:n]
    return(mean(t))
}

mean_density <- function(c) {
    d = density(c)
    return(mean(d$y))
}

median_density <- function(c) {
    d = density(c)
    return(median(d$y))
}

max_density <- function(c) {
    d = density(c)
    return(max(d$y))
}

###

raw_data_run = read_excel("/Users/Michael/Dropbox/Orienteering/Re-run/data1.xlsx") %>%
    gather(key = Posten, value = Time, -Runner) %>%
    mutate(Run = "run")

raw_data_re_run = read_excel("/Users/Michael/Dropbox/Orienteering/Re-run/data2.xlsx")  %>%
    gather(key = Posten, value = Time, -Runner) %>%
    mutate(Run = "re_run")

tidy_data = rbind(raw_data_run, raw_data_re_run) %>%
    group_by(Posten) %>%
    mutate(TopTime = top_time(Time, top_time_percent)) %>%
    mutate(Index = TopTime/Time*100) %>%
    group_by(Runner, Run) %>%
    mutate(density = median_density(Index))

results = tidy_data %>%
    group_by(Runner, Run) %>%
    sample_n(1) %>%
    select(Runner, Run, density) %>%
    spread(key = Run, value = density) %>%
    mutate(error = 100 - run/`re_run`*100) %>%
    arrange(error)

p = tidy_data %>%
    ggplot(aes(x = Index, group = Run, color = Run)) +
    geom_density() +
    facet_wrap(~Runner) +
    geom_vline(xintercept = 100) +
    theme_set(theme_cowplot()) +
    background_grid() +
    panel_border()

p
