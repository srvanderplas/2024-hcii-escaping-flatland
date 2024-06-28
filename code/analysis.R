library(tidyverse)

source("code/data_cleaning.R")

results <- results |>
  group_by(participantID,db) |>
  arrange(plotStartTime) |>
  mutate(trial_no = 1:n())


library(lme4)

ggplot(results, aes(x = log2diff)) + geom_density() + facet_wrap(~plot)
results <- results |>
  mutate(plot_lab = as.character(plot))

mod <- lmer(data = results, log2diff ~ (1|participantID) + (1|db) +
              factor(ratioLabel) + trial_secs + graphtype + plot_lab)

summary(mod)


library(mgcv)
gammod <- gam(absdiff ~ treatment:transf_time +
      s(subject, bs = 're') +
      s(subject, transf_time, bs = 're'),
    data = rats, method = 'REML')
library(modelsummary)
modelsummary(mod,
             fmt = 1,
             estimate  = "{estimate} [{conf.low}, {conf.high}]",
             statistic = "t")
