# How having by-participant random effects makes plotting the model estimates
# directly not very visually informative, for a review response

library(tidyverse)
library(magrittr)
library(broom.mixed)
library(ggtext)
library(insight)

load("all-analyses.RData")

# Model predictions-----
## Calculate on response scale (probability)----
exp1_d_predicted_pr <- exp1_m_nameGender %>%
  get_predicted(predict = "expectation", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(filter(exp1_d, Condition != "last")) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, She, Predicted
  ) %>%
  rename("She_Female" = "She", "Predicted_Pr" = "Predicted")

exp2_d_predicted_pr <- exp2_m_nameGender %>%
  get_predicted(predict = "expectation", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(filter(exp2_d, Condition != "last")) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, Female, Predicted
  ) %>%
  rename("She_Female" = "Female", "Predicted_Pr" = "Predicted")

exp3_d_predicted_pr <- exp3_m_all %>%
  get_predicted(predict = "expectation", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(exp3_d) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, She, Predicted
  ) %>%
  rename("She_Female" = "She", "Predicted_Pr" = "Predicted")

exp4_d_predicted_pr <- exp4_m_all %>%
  get_predicted(predict = "expectation", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(exp4_d) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, Female, Predicted
  ) %>%
  rename("She_Female" = "Female", "Predicted_Pr" = "Predicted")

all_d_predicted_pr <- bind_rows(
  .id = "Experiment",
  "Experiment 1" = exp1_d_predicted_pr,
  "Experiment 2" = exp2_d_predicted_pr,
  "Experiment 3" = exp3_d_predicted_pr,
  "Experiment 4" = exp4_d_predicted_pr
)

## Calculate on model scale (log odds)----
exp1_d_predicted_log <- exp1_m_nameGender %>%
  get_predicted(predict = "link", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(filter(exp1_d, Condition != "last")) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, She, Predicted
  ) %>%
  rename("She_Female" = "She", "Predicted_Log" = "Predicted")

exp2_d_predicted_log <- exp2_m_nameGender %>%
  get_predicted(predict = "link", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(filter(exp2_d, Condition != "last")) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, Female, Predicted
  ) %>%
  rename("She_Female" = "Female", "Predicted_Log" = "Predicted")

exp3_d_predicted_log <- exp3_m_all %>%
  get_predicted(predict = "link", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(exp3_d) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, She, Predicted
  ) %>%
  rename("She_Female" = "She", "Predicted_Log" = "Predicted")

exp4_d_predicted_log <- exp4_m_all %>%
  get_predicted(predict = "link", include_random = TRUE) %>%
  as.data.frame() %>%
  add_column(exp4_d) %>%
  select(
    Condition, Participant, Item, GenderRatingCentered, Female, Predicted
  ) %>%
  rename("She_Female" = "Female", "Predicted_Log" = "Predicted")

all_d_predicted_log <- bind_rows(
  .id = "Experiment",
  "Experiment 1" = exp1_d_predicted_log,
  "Experiment 2" = exp2_d_predicted_log,
  "Experiment 3" = exp3_d_predicted_log,
  "Experiment 4" = exp4_d_predicted_log
)

## Plot log odds----
all_p_pred_log <- ggplot(all_d_predicted_log,
  aes(x = GenderRatingCentered, y = Predicted_Log)) +
  geom_line(
    aes(group = Participant),
    color = "red", linewidth = 0.25
  ) +
  geom_line(
    data = . %>%
      group_by(Experiment, Condition, GenderRatingCentered, Item) %>%
      summarise(Predicted_Log = mean(Predicted_Log)),
    color = "black"
  ) +
  facet_grid(
    Experiment ~ Condition,
    labeller = labeller(Condition = str_to_title)
  ) +
  scale_x_continuous(limits = c(-3, 3), n.breaks = 7, expand = c(0.02, 0.02)) +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    plot.title = element_markdown(),
    strip.text = element_text(size = 16)
  ) +
  labs(title =
    "Log Odds of a <i>She</i>/<i>Female</i> Response Across Experiments",
    x     = "Masculine - Feminine",
    y     = "By-Participant Model Predictions"
  )
all_p_pred_log

## Plot probabilities with mean----
all_p_pred_pr <- ggplot(all_d_predicted_pr,
  aes(x = GenderRatingCentered)) +
  geom_line(
    aes(y = Predicted_Pr, group = Participant),
    color = "red", linewidth = 0.25
  ) +
  geom_smooth(
    aes(y = She_Female),
    method = "glm", method.args = list(family = binomial),
    color = "black"
  ) +
  facet_grid(
    Experiment ~ Condition,
    labeller = labeller(Condition = str_to_title)
  ) +
  scale_x_continuous(limits = c(-3, 3), n.breaks = 7, expand = c(0.02, 0.02)) +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    plot.title = element_markdown(),
    strip.text = element_text(size = 16)
  ) +
  labs(title =
    "Probability of a <i>She</i>/<i>Female</i> Response Across Experiments",
    x = "Masculine - Feminine",
    y = "By-Participant Model Predictions"
  )
all_p_pred_pr

# Save----
ggsave(
  plot = all_p_pred_log, path = "extras/",
  filename = "all_predicted_log.png",
  width = 8, height = 10, unit = "in", device = "png"
)
ggsave(
  plot = all_p_pred_pr, path = "extras/",
  filename = "all_predicted_pr.png",
  width = 8, height = 10, unit = "in", device = "png"
)
