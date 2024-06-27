library(readr)
source("../code/data_cleaning.R")

library(ggpcp)

results %>%
  # filter(plot != "3D Print") %>%
  ggplot(aes(x = ratioLabel, y = byHowMuch)) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_jitter(alpha = .25) +
  geom_violin(aes(group = factor(ratioLabel)), alpha = .5, draw_quantiles = c(.25, .5, .75)) +
  facet_wrap(~plot) + theme_bw() +
  scale_x_continuous("True Bar Ratio", breaks = round(unique(results$true_ratio*100)), minor_breaks = NULL, limits = c(0, 100)) +
  scale_y_continuous("Estimated Bar Ratio", breaks = seq(0, 100, by = 10), minor_breaks = seq(0, 100, by = 5), limits = c(0, 100)) +
  coord_fixed()

duplicates <- results |>
  dplyr::summarise(n = dplyr::n(), .by = c(participantID, participantUnique, plot, graphtype, ratioLabel)) |>
  dplyr::filter(n > 1L)
results |>
  pivot_wider(id_cols = c(participantID, ratioLabel), names_from = plot,
              values_from = byHowMuch, values_fn = mean, names_sort = T) |>
  pcp_select(-c(participantID, ratioLabel)) |>
  pcp_arrange() |>
  ggplot(aes_pcp()) + geom_pcp_axes() + geom_pcp(alpha = .5) + facet_wrap(~ratioLabel)
