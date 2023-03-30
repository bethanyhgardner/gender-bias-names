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

# Odds ratios----
## Setup----
all_odds <- purrr::map_df(  # Pull beta estimates and SE from models
  .x = list(
    # Main models (just Condition models in Exp1&2 to get all conditions)
    "Exp 1_Main" = exp1_m_cond, "Exp 2_Main" = exp2_m_cond,
    "Exp 3_Main" = exp3_m_all, "Exp 4_Main" = exp4_m_all,
    # Dummy coded models to get response bias just in Last
    "Exp 1_Last" = exp1_m_L, "Exp 2_Last" = exp2_m_L,
    "Exp 3_Last" = exp3_m_cond_L, "Exp 4_Last" = exp4_m_all_L,
    # Dummy coded models to get response bias just in First + Full
    "Exp 1_FF" = exp1_m_FF, "Exp 2_FF" = exp2_m_FF,
    "Exp 3_FF" = exp3_m_cond_FF, "Exp 4_FF" = exp4_m_cond_FF,
    # Equivalents excluding other responses
    "Exp 1_Main_No Other" = exp1_m_cond_noOther,
    "Exp 2_Main_No Other" = exp2_m_cond_noOther,
    "Exp 3_Main_No Other" = exp3_m_noOther,
    "Exp 4_Main_No Other" = exp4_m_noOther,
    "Exp 1_Last_No Other" = exp1_m_L_noOther,
    "Exp 2_Last_No Other" = exp2_m_L_noOther,
    "Exp 3_Last_No Other" = exp3_m_noOther_L,
    "Exp 4_Last_No Other" = exp4_m_noOther_L,
    "Exp 1_FF_No Other" = exp1_m_FF_noOther,
    "Exp 2_FF_No Other" = exp2_m_FF_noOther,
    "Exp 3_FF_No Other" = exp3_m_noOther_FF,
    "Exp 4_FF_No Other" = exp4_m_noOther_FF
  ),
  .f = tidy,
  .id = "exp"
) %>%
  filter(effect == "fixed") %>%  # just keep fixed effects
  separate_wider_delim(  # get model variables from variable name
    exp,
    delim = "_",
    names = c("Experiment", "Conditions", "Other"),
    too_few = "align_start"
  ) %>%
  mutate(Other = case_when(  # labels for other/no other
    Other == "No Other" ~ "Without Other Responses",
    is.na(Other) ~ "With Other Responses"
  )) %>%
  mutate(.after = Other, Outcome = case_when(  # outcome variables
    str_detect(Experiment, "1|3") ~ "She | He",
    str_detect(Experiment, "2|4") ~ "Female | Male"
  )) %>%
  mutate(Outcome = ifelse(
    Other == TRUE, str_c(Outcome, " + Other"), Outcome
  )) %>%
  mutate(.after = std.error,  # calculate CI
    CI_lower = estimate - std.error,
    CI_upper = estimate + std.error,
    logOdds = exp(estimate)
  ) %>%
  mutate(.after = std.error,  # convert log-odds beta estimate to odds ratio
    OddsRatio = exp(estimate)) %>%
  rename("LogOdds" = "estimate") %>%
  filter(  # keep intercepts, both condition contrasts, gender rating
    !(Conditions != "Main" & str_detect(term, "Condition") |
    str_detect(term, "Condition2|first vs full") |
    str_detect(term, "GenderRating"))
  ) %>%
  mutate(.after = term, Label = case_when(  # label estimates
    term == "(Intercept)" & Conditions == "Main" ~
      "All\nConditions",  # intercepts in main model = mean across conditions
    term == "Conditionlast vs first/full" | term == "Condition1" ~
      "Last vs\nFirst + Full",  # last vs first+full contrast
    Conditions == "Last" ~ # last name only (dummy coded intercept)
      "Last",
    Conditions == "FF" ~   # first + full names only (dummy coded intercept)
      "First + Full"
  )) %>%
  select(-effect, -group, -term, -statistic, -p.value, -std.error) %>%
  mutate(.after = OddsRatio,
         OddsRatio_Split = OddsRatio >= 1.35) # split for plot axes

# Labels for plots
all_odds$Label %<>% factor(
  levels = c("Last vs\nFirst + Full", "First + Full", "Last", "All\nConditions")
)

all_odds$Other %<>% as.factor() %>% fct_rev()

## OR w/o other responses----
all_p_oddsRatio <- ggplot(
  data = all_odds %>% filter(Other == "With Other Responses"),
  aes(y = Label, x = OddsRatio, color = Experiment, fill = Experiment)) +
  geom_point(size = 2, key_glyph = "rect") +
  facet_wrap(~OddsRatio_Split, scales = "free_x") +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 7, 10, 13, 16),
    expand = c(0.05, 0.05)
  ) +
  guides(color = guide_none(), fill = guide_legend(byrow = TRUE)) +
  theme_classic() +
  theme(
    text             = element_text(size = 16),
    axis.text.y      = element_text(size = 16),
    axis.ticks.y     = element_blank(),
    legend.position  = c(0.80, 0.75),
    legend.spacing.y = unit(10, "pt"),
    legend.text      = element_text(size = 16),
    panel.spacing.x  = unit(25, "pt"),
    plot.title       = element_markdown(),
    strip.text       = element_blank()
  ) +
  labs(
    title = paste("Odds Ratio of a <i>She</i>/<i>Female</i> vs ",
                  "<i>He</i>/<i>Male</i> or <i>Other</i><br>Response",
                  "Across Experiments"),
    x     = "Odds Ratio",
    y     = element_blank(),
    fill  = element_blank()
  )
all_p_oddsRatio

## Comparing analysis w/ and w/o other responses----
all_p_withOther <- ggplot(all_odds,
  aes(
    x = LogOdds, xmin = CI_lower, xmax = CI_upper,
    y = Label,
    color = Experiment,
    group = Other, shape = Other
  )) +
  geom_pointrange(
    size = 0.75, linewidth = 0.75,
    position = position_dodge(width = 0.9)
  ) +
  geom_vline(xintercept = 0) +
  scale_color_brewer(palette = "Spectral") +
  scale_x_continuous(limits = c(-4, 4)) +
  guides(shape = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.ticks.y = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.box.background = element_rect(fill = "grey90", color = "grey90"),
    legend.margin = margin(t = -8, b = 2, l = 5, r = 5),
    legend.position = c(0.80, 0.75),
    legend.spacing = unit(0, "pt"),
    plot.title = element_markdown(),
    plot.title.position = "plot"
  ) +
  labs(title =
    "Likelihood of a <i>She</i>/<i>Female</i> Response Across Experiments",
    x = "Model Estimate",
    color = element_blank(), shape = element_blank(), y = element_blank()
  )
all_p_withOther



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
ggsave(
  plot = all_p_oddsRatio, path = "plots/",
  filename = "all_oddsRatio.png",
  width = 8, height = 4, unit = "in", device = "png"
)
# Extras
ggsave(
  plot = all_p_withOther, path = "extras/",
  filename = "plot_OR_other.png",
  width = 8, height = 4, unit = "in", device = "png"
)
