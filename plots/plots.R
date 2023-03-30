library(tidyverse)
library(magrittr)
library(broom.mixed)  # Model  results
library(insight)  # Model predictions
library(ggtext)
library(ggrepel)
library(patchwork)

load("all-analyses.RData")  # Includes plots

# Response by first name gender rating, with by-name means

# Experiment 1----
exp1_d_long <- exp1_d_FF %>%
  pivot_longer(
    cols      = c(He, She, Other),
    names_to  = "Pronoun",
    values_to = "Response"
  )
exp1_d_long$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp1_d_long$Condition) <- c("First", "Full")

exp1_d_itemMeans <- exp1_d_FF %>%
  group_by(Condition, GenderRatingCentered) %>%
  summarise(
    He    = mean(He),
    She   = mean(She),
    Other = mean(Other)
  ) %>%
  pivot_longer(
    cols     = c(He, She, Other),
    names_to = "Pronoun",
    values_to = "Mean"
  )

exp1_d_itemMeans$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp1_d_itemMeans$Condition) <- c("First", "Full")

exp1_p <- ggplot(exp1_d_long,
  aes(x = GenderRatingCentered, color = Pronoun, fill = Pronoun)) +
  geom_vline(xintercept = -0.21, linetype = "dashed") +
  geom_point(data = exp1_d_itemMeans, aes(y = Mean)) +  # points are item means
  geom_smooth(aes(y = Response)) +  # but smooth is on full data
  facet_wrap(~Condition) +
  theme_classic() +
  scale_x_continuous(
    limits = c(-3, 3), expand = c(0.02, 0.02),
    breaks = c(-3, -2, -1, 0, 1, 2, 3)
  ) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 5) +
  scale_color_manual(values = c("grey70", "blue", "red")) +
  scale_fill_manual(values = c("grey70", "blue", "red")) +
  theme(
    text = element_text(size = 16),
    legend.margin = margin(l = -10)) +
  labs(
    title = "Experiment 1: Pronoun Used by Name Condition",
    x     = "Masculine - Feminine",
    y     = "Prop Gendered Pronoun"
  )
exp1_p

# Experiment 2----
exp2_d_long <- exp2_d_FF %>%
  pivot_longer(
    cols      = c(Male, Female, Other),
    names_to  = "Gender",
    values_to = "Response"
  )
exp2_d_long$Gender %<>% as.factor() %>% relevel("Other")
levels(exp2_d_long$Condition) <- c("First", "Full")

exp2_d_itemMeans <- exp2_d_FF %>%
  group_by(Condition, GenderRatingCentered) %>%
  summarise(
    Male   = mean(Male),
    Female = mean(Female),
    Other  = mean(Other)
  ) %>%
  pivot_longer(
    cols      = c(Male, Female, Other),
    names_to  = "Gender",
    values_to = "Mean"
  )
exp2_d_itemMeans$Gender %<>% as.factor() %>%
  relevel("Male") %>%
  relevel("Other")
levels(exp2_d_itemMeans$Condition) <- c("First", "Full")

exp2_p <- ggplot(exp2_d_long,
  aes(x = GenderRatingCentered, color = Gender, fill = Gender)) +
  geom_point(data = exp2_d_itemMeans, aes(y = Mean)) +  # points are item means
  geom_smooth(aes(y = Response)) +  # but smooth is on full data
  geom_vline(xintercept = -0.21, linetype = "dashed") +
  facet_wrap(~Condition) +
  theme_classic() +
  scale_x_continuous(
    limits = c(-3.1, 3), expand = c(0.02, 0.02),
    breaks = c(-3, -2, -1, 0, 1, 2, 3)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.05, 1), n.breaks = 5) +
  scale_color_manual(values = c("grey70", "blue", "red")) +
  scale_fill_manual(values = c("grey70", "blue", "red")) +
  theme(
    text = element_text(size = 16),
    legend.margin = margin(l = -10)
  ) +
  labs(
    title = "Experiment 2: Gender Recalled by Name Condition",
    x     = "Masculine - Feminine",
    y     = "Prop Gender Recalled",
    color = "Gender \nRecalled",
    fill  = "Gender \nRecalled"
  )
exp2_p

# Experiment 3----
exp3_d_long <- exp3_d %>%
  pivot_longer(
    cols      = c(He, She, Other),
    names_to  = "Pronoun",
    values_to = "Response"
  )
exp3_d_long$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp3_d_long$Condition) <- c("First", "Full", "Last")

exp3_d_itemMeans <- exp3_d %>%
  group_by(Condition, GenderRatingCentered) %>%
  summarise(
    He    = mean(He),
    She   = mean(She),
    Other = mean(Other)
  ) %>%
  pivot_longer(
    cols      = c(He, She, Other),
    names_to  = "Pronoun",
    values_to = "Mean"
  )
exp3_d_itemMeans$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp3_d_itemMeans$Condition) <- c("First", "Full", "Last")

exp3_p <- ggplot(exp3_d_long,
  aes(x = GenderRatingCentered, color = Pronoun, fill = Pronoun)) +
  geom_point(data = exp3_d_itemMeans, aes(y = Mean)) +  # points are item means
  geom_smooth(aes(y = Response)) +  # but smooth is on full data
  geom_vline(xintercept = -0.21, linetype = "dashed") +
  facet_wrap(~Condition) +
  theme_classic() +
  scale_x_continuous(
    limits = c(-3.1, 3), expand = c(0.02, 0.02),
    breaks = c(-3, -2, -1, 0, 1, 2, 3)
  ) +
  scale_y_continuous(limits = c(-0.05, 1), expand = c(0, 0), n.breaks = 5) +
  scale_color_manual(values = c("grey70", "blue", "red")) +
  scale_fill_manual(values = c("grey70", "blue", "red")) +
  theme(
    text = element_text(size = 16),
    legend.margin = margin(l = -10)
  ) +
  labs(
    title = "Experiment 3: Pronoun Used by Name Condition",
    x     = "Masculine - Feminine",
    y     = "Prop Gendered Pronoun"
  )
exp3_p

# Experiment 4----
exp4_d_long <- exp4_d %>%
  pivot_longer(
    cols      = c(Male, Female, Other),
    names_to  = "Gender",
    values_to = "Response"
  )
exp4_d_long$Gender %<>% as.factor() %>% relevel("Other")
levels(exp4_d_long$Condition) <- c("First", "Full", "Last")

exp4_d_itemMeans <- exp4_d %>%
  group_by(Condition, GenderRatingCentered) %>%
  summarise(
    Male   = mean(Male),
    Female = mean(Female),
    Other  = mean(Other)
  ) %>%
  pivot_longer(
    cols      = c(Male, Female, Other),
    names_to  = "Gender",
    values_to = "Mean"
  )
exp4_d_itemMeans$Gender %<>% as.factor() %>%
  relevel("Male") %>%
  relevel("Other")
levels(exp4_d_itemMeans$Condition) <- c("First", "Full", "Last")

exp4_p <- ggplot(exp4_d_long,
  aes(x = GenderRatingCentered, color = Gender, fill = Gender)) +
  geom_point(data = exp4_d_itemMeans, aes(y = Mean)) +  # points are item means
  geom_smooth(aes(y = Response)) +  # but smooth is on full data
  geom_vline(xintercept = -0.21, linetype = "dashed") +
  facet_wrap(~Condition) +
  theme_classic() +
  scale_x_continuous(
    limits = c(-3.1, 3), expand = c(0.02, 0.02),
    breaks = c(-3, -2, -1, 0, 1, 2, 3)
  ) +
  scale_y_continuous(limits = c(-0.05, 1), expand = c(0, 0), n.breaks = 5) +
  scale_color_manual(values = c("grey70", "blue", "red")) +
  scale_fill_manual(values = c("grey70", "blue", "red")) +
  theme(
    text = element_text(size = 16),
    legend.margin = margin(l = -10)
  ) +
  labs(
    title = "Experiment 4: Gender Recalled by Name Condition",
    x     = "Masculine - Feminine",
    y     = "Prop Gender Recalled",
    color = "Gender \nRecalled",
    fill  = "Gender \nRecalled"
  )
exp4_p



# Save----
# Main
ggsave(
  plot = exp1_p, path = "plots/",
  filename = "exp1_gender-rating-itemMeans.png",
  width = 8, height = 4, unit = "in", device = "png"
)
ggsave(
  plot = exp2_p, path = "plots/",
  filename = "exp2_gender-rating-itemMeans.png",
  width = 8, height = 4, unit = "in", device = "png"
)
ggsave(
  plot = exp3_p, path = "plots/",
  filename = "exp3_gender-rating-itemMeans.png",
  width = 8, height = 4, unit = "in", device = "png"
)
ggsave(
  plot = exp4_p, path = "plots/",
  filename = "exp4_gender-rating-itemMeans.png",
  width = 8, height = 4, unit = "in", device = "png"
)
