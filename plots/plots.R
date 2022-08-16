library(tidyverse)
library(magrittr)
library(ggtext)

load("all-analyses.RData")

#Response by first name gender rating, with by-name means

#Experiment 1----

exp1_d_itemMeans <- exp1_d_FF %>% 
  group_by(Condition, GenderRating) %>%
  summarise(He    = mean(He),
            She   = mean(She),
            Other = mean(Other)) %>%
  pivot_longer(cols=c(He, She, Other),
               names_to="Pronoun", 
               values_to="Mean")
exp1_d_itemMeans$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp1_d_itemMeans$Condition) <- c("First", "Full")  

exp1_p <- ggplot(exp1_d_itemMeans, 
  aes(x=GenderRating, y=Mean, color=Pronoun, fill=Pronoun)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept=4) +
  facet_wrap(~Condition) +
  theme_classic() +
  scale_color_manual(values=c("grey70", "blue", "red")) + 
  scale_fill_manual(values=c("grey70", "blue", "red")) + 
  theme(text=element_text(size=16)) +
  labs(title="Experiment 1: Pronoun Used by Name Condition", 
       x="Masculine - Feminine", y="Prop Gendered Pronoun")

#Experiment 2----

exp2_d_itemMeans <- exp2_d_FF %>% 
  group_by(Condition, GenderRating) %>%
  summarise(Male   = mean(Male),
            Female = mean(Female),
            Other  = mean(Other)) %>%
  pivot_longer(cols=c(Male, Female, Other),
               names_to="Pronoun", 
               values_to="Mean")
exp2_d_itemMeans$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp2_d_itemMeans$Condition) <- c("First", "Full")  

exp2_p <- ggplot(exp2_d_itemMeans, 
  aes(x=GenderRating, y=Mean, color=Pronoun, fill=Pronoun)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept=4) +
  facet_wrap(~Condition) +
  theme_classic() +
  scale_color_manual(values=c("grey70", "blue", "red")) + 
  scale_fill_manual(values=c("grey70", "blue", "red")) + 
  theme(text=element_text(size=16)) +
  labs(title="Experiment 2: Gender Recalled by Name Condition", 
       x="Masculine - Feminine", y="Prop Gender Recalled",
       color="Gender \nRecalled", fill="Gender \nRecalled")

#Experiment 3----

exp3_d_itemMeans <- exp3_d %>% 
  group_by(Condition, GenderRating) %>%
  summarise(He    = mean(He),
            She   = mean(She),
            Other = mean(Other)) %>%
  pivot_longer(cols=c(He, She, Other),
               names_to="Pronoun", 
               values_to="Mean")
exp3_d_itemMeans$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp3_d_itemMeans$Condition) <- c("First", "Full", "Last")

exp3_p <- ggplot(exp3_d_itemMeans, 
  aes(x=GenderRating, y=Mean, color=Pronoun, fill=Pronoun)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept=4) +
  facet_wrap(~Condition) +
  theme_classic() +
  scale_color_manual(values=c("grey70", "blue", "red")) + 
  scale_fill_manual(values=c("grey70", "blue", "red")) + 
  theme(text=element_text(size=16)) +
  labs(title="Experiment 3: Pronoun Used by Name Condition", 
       x="Masculine - Feminine", y="Prop Gendered Pronoun")

#Experiment 4----

exp4_d_itemMeans <- exp4_d %>% 
  group_by(Condition, GenderRating) %>%
  summarise(Male   = mean(Male),
            Female = mean(Female),
            Other  = mean(Other)) %>%
  pivot_longer(cols=c(Male, Female, Other),
               names_to="Pronoun", 
               values_to="Mean")
exp4_d_itemMeans$Pronoun %<>% as.factor() %>% relevel("Other")
levels(exp4_d_itemMeans$Condition) <- c("First", "Full", "Last") 

exp4_p <- ggplot(exp4_d_itemMeans, 
  aes(x=GenderRating, y=Mean, color=Pronoun, fill=Pronoun)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept=4) +
  facet_wrap(~Condition) +
  theme_classic() +
  scale_color_manual(values=c("grey70", "blue", "red")) + 
  scale_fill_manual(values=c("grey70", "blue", "red")) + 
  theme(text=element_text(size=16)) +
  labs(title="Experiment 4: Gender Recalled by Name Condition", 
       x="Masculine - Feminine", y="Prop Gender Recalled",
       color="Gender \nRecalled", fill="Gender \nRecalled")

#Save----

ggsave(plot=exp1_p, 
       path="plots/", 
       filename="exp1_gender-rating-itemMeans.png", 
       width=8, height=4, unit="in", device="png")
ggsave(plot=exp2_p, 
       path="plots/", 
       filename="exp2_gender-rating-itemMeans.png", 
       width=8, height=4, unit="in", device="png")
ggsave(plot=exp3_p, 
       path="plots/", 
       filename="exp3_gender-rating-itemMeans.png", 
       width=8, height=4, unit="in", device="png")
ggsave(plot=exp4_p, 
       path="plots/", 
       filename="exp4_gender-rating-itemMeans.png", 
       width=8, height=4, unit="in", device="png")


