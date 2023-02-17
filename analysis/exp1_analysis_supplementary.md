Experiment 1: Supplementary Analyses
================
2023-02-07

- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#without-other-responses"
  id="toc-without-other-responses">Without <em>Other</em> Responses</a>
  - <a href="#model-1-condition-without-other-responses"
    id="toc-model-1-condition-without-other-responses">Model 1: Condition
    without <em>Other</em> Responses</a>
    - <a href="#odds-ratios-intercept" id="toc-odds-ratios-intercept">Odds
      Ratios: Intercept</a>
    - <a href="#odds-ratios-last-vs-firstfull"
      id="toc-odds-ratios-last-vs-firstfull">Odds Ratios: Last vs
      First+Full</a>
    - <a href="#odds-ratios-last-only" id="toc-odds-ratios-last-only">Odds
      Ratios: Last Only</a>
    - <a href="#odds-ratios-first-and-full-only"
      id="toc-odds-ratios-first-and-full-only">Odds Ratios: First and Full
      Only</a>
  - <a href="#model-2-condition--name-gender-wo-other-responses"
    id="toc-model-2-condition--name-gender-wo-other-responses">Model 2:
    Condition * Name Gender w/o <em>Other</em> Responses</a>
- <a href="#quadratic-name-gender-rating"
  id="toc-quadratic-name-gender-rating">Quadratic Name Gender Rating</a>
  - <a href="#model-3-quadratic" id="toc-model-3-quadratic">Model 3:
    Quadratic</a>
- <a href="#participant-gender" id="toc-participant-gender">Participant
  Gender</a>
  - <a href="#setupdata-summary" id="toc-setupdata-summary">Setup/Data
    Summary</a>
  - <a href="#model-4-condition--participant-gender"
    id="toc-model-4-condition--participant-gender">Model 4: Condition *
    Participant Gender</a>
  - <a href="#model-5-condition--name-gender--participant-gender"
    id="toc-model-5-condition--name-gender--participant-gender">Model 5:
    Condition * Name Gender * Participant Gender</a>

# Setup

Variable names:

- Experiment: exp1

- Type

  - d = data
  - m = model
  - est = log odds estimate from model
  - OR = odds ratio converted from est

- Analysis

  - count =sums of response types
  - cond = effect of Condition (Last vs First+Full)
  - nameGender = effects of Condition (First vs Full) and Name Gender
    Rating
  - recentered = center name gender rating by scale (at 4)

- Subset

  - all = including *other* responses

  - noOther = excluding *other* responses

  - FF = First and Full Name conditions only

  - Last = Last Name condition only

Load data and select columns used in model. See data/exp1_data_about.txt
for more details.

``` r
exp1_d <- read.csv("../data/exp1_data.csv", 
                   stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="NameShown") %>%
  select(Participant, SubjGenderMale, 
         Condition, GenderRating, 
         Item, He, She, Other)
str(exp1_d)
```

    ## 'data.frame':    9564 obs. of  8 variables:
    ##  $ Participant   : Factor w/ 457 levels "Exp1_P1","Exp1_P10",..: 443 443 443 443 443 443 443 443 443 443 ...
    ##  $ SubjGenderMale: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Condition     : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating  : num  1.21 1.24 1.28 2.12 2.41 2.61 3.61 3.75 3.87 4.22 ...
    ##  $ Item          : Factor w/ 104 levels "Ashley","Ashley Cook",..: 64 11 43 18 95 29 88 71 79 92 ...
    ##  $ He            : int  1 1 1 1 0 1 1 0 1 1 ...
    ##  $ She           : int  0 0 0 0 1 0 0 1 0 0 ...
    ##  $ Other         : int  0 0 0 0 0 0 0 0 0 0 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp1_d %<>% mutate(GenderRatingCentered=
    scale(GenderRating, scale=FALSE))
```

Set contrasts for name conditions.

``` r
contrasts(exp1_d$Condition) = cbind(
  "last vs first/full"=c(.33,.33,-0.66), 
  "first vs full"=c(-.5,.5,0))
contrasts(exp1_d$Condition)
```

    ##       last vs first/full first vs full
    ## first               0.33          -0.5
    ## full                0.33           0.5
    ## last               -0.66           0.0

Subset for gender rating effects (First and Full conditions only).

``` r
exp1_d_FF <- exp1_d %>% filter(Condition!="last") 
exp1_d_FF$Condition <- droplevels(exp1_d_FF$Condition)
contrasts(exp1_d_FF$Condition) = cbind(
  "first vs full"=c(-.5,.5)) #add contrast back
contrasts(exp1_d_FF$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

# Without *Other* Responses

The first supplementary analysis tests if excluding *other* responses
(7.12% of total responses) affects the pattern of results.

``` r
sum(exp1_d$Other) 
```

    ## [1] 681

``` r
sum(exp1_d$Other)/length(exp1_d$Other) 
```

    ## [1] 0.07120452

Exclude *other* responses.

``` r
exp1_d_noOther <- exp1_d %>% filter(Other==0)
exp1_d_FF_noOther<- exp1_d_FF %>% filter(Other==0)
```

## Model 1: Condition without *Other* Responses

Effect of Condition (first name, last name, full name) on likelihood of
a *she* response, as opposed to a *he* response, with *other* responses
excluded. Participant and Item are again included as random intercepts,
with items defined as the unique first, last and first + last name
combinations.

``` r
exp1_m_cond_noOther <- glmer(
  She ~ Condition + (1|Participant) + (1|Item), 
  exp1_d_noOther, family=binomial)
summary(exp1_m_cond_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: exp1_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5795.0   5830.5  -2892.5   5785.0     8878 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.1910 -0.2987 -0.1446  0.1682 10.5933 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.823    0.9072  
    ##  Item        (Intercept) 9.099    3.0164  
    ## Number of obs: 8883, groups:  Participant, 456; Item, 104
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -1.1304     0.3432  -3.294 0.000988 ***
    ## Conditionlast vs first/full   2.9905     0.7830   3.819 0.000134 ***
    ## Conditionfirst vs full        0.5548     0.7823   0.709 0.478192    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/
    ## Cndtnvfrst/ -0.179       
    ## Cndtnfrstvf -0.363 -0.241

No differences in results.

### Odds Ratios: Intercept

``` r
exp1_est_noOther_intercept <- exp1_m_cond_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric() 

exp(exp1_est_noOther_intercept)
```

    ## [1] 0.3229123

``` r
exp(-exp1_est_noOther_intercept)
```

    ## [1] 3.096817

``` r
#Save this for the table comparing all 4 experiments
exp1_OR_noOther_I <- exp(exp1_est_noOther_intercept) %>%
  round(2)
```

0.32x less likely to use to use *she* overall (or: 3.10x more likely to
use *he* overall), p\<.001

### Odds Ratios: Last vs First+Full

``` r
exp1_est_noOther_LFF <- exp1_m_cond_noOther %>% 
  tidy() %>%
  filter(term=="Conditionlast vs first/full") %>%
  select(estimate) %>% as.numeric() 
exp(exp1_est_noOther_LFF)
```

    ## [1] 19.89544

``` r
#Save this for the table comparing all 4 experiments
exp1_OR_noOther_LFF <- exp(exp1_est_noOther_LFF) %>%
  round(2)
```

19.89x more likely to use *she* in First + Full compared to Last (or:
19.89x times more likely to use *he* and *other* in Last than in First +
Full), p\<.001

### Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp1_d_noOther %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
exp1_d_noOther$Condition_Last %<>% as.factor()
```

``` r
exp1_m_L_noOther <- glmer(
  She ~ Condition_Last + (1|Participant) + (1|Item), 
  data=exp1_d_noOther, family=binomial)
summary(exp1_m_L_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp1_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5793.5   5821.9  -2892.8   5785.5     8879 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.3061 -0.2987 -0.1443  0.1687 10.4488 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.8231   0.9072  
    ##  Item        (Intercept) 9.1123   3.0187  
    ## Number of obs: 8883, groups:  Participant, 456; Item, 104
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -3.1042     0.6701  -4.632 3.61e-06 ***
    ## Condition_Last1   3.0923     0.7530   4.107 4.01e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.890

``` r
exp1_est_L_noOther <- exp1_m_L_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp1_est_L_noOther)
```

    ## [1] 0.04486108

``` r
exp(-exp1_est_L_noOther)
```

    ## [1] 22.29104

``` r
#Save this for the table comparing all 4 experiments
exp1_OR_noOther_L <- exp(exp1_est_L_noOther) %>% 
  round(2)
```

0.04x times less likely to use *she* in the Last Name condition (or:
22.29x more likely to use *he* in the Last Name condition), p\<.001

### Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp1_d_noOther %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
exp1_d_noOther$Condition_FF %<>% as.factor()
```

``` r
exp1_m_FF_noOther <- glmer(
  She ~ Condition_FF + (1|Participant) + (1|Item), 
  data=exp1_d_noOther, family=binomial)
summary(exp1_m_FF_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp1_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5793.5   5821.9  -2892.8   5785.5     8879 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.3061 -0.2987 -0.1443  0.1687 10.4488 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.8231   0.9072  
    ##  Item        (Intercept) 9.1123   3.0187  
    ## Number of obs: 8883, groups:  Participant, 456; Item, 104
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.0119     0.3436  -0.035    0.972    
    ## Condition_FF1  -3.0923     0.7535  -4.104 4.06e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.456

``` r
exp1_est_FF_noOther <- exp1_m_FF_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp1_est_FF_noOther)
```

    ## [1] 0.9881717

``` r
exp(-exp1_est_FF_noOther)
```

    ## [1] 1.01197

``` r
#Save this for the table comparing all 4 experiments
exp1_OR_noOther_FF <- exp(exp1_est_FF_noOther) %>% 
  round(2)
```

0.99x times less likely to use *she* in the First and Full Name
conditions (or: 1.01x more likely to use *he* in the n the First and
Full Name conditions) p=0.97

## Model 2: Condition \* Name Gender w/o *Other* Responses

Effects of Condition (first name, full name) and the first name’s Gender
Rating (centered, positive=more feminine) on the likelihood of a *she*
response as opposed to a *he* response, with *other* responses excluded.
In Experiment 1, the Last Name condition does not include any instances
of the gendered first name, so it is not included here. Participant and
Item are again included as random intercepts.

``` r
exp1_m_nameGender_noOther <- glmer(
  She ~ Condition * GenderRatingCentered + 
    (1|Participant) + (1|Item), 
  exp1_d_FF_noOther, family=binomial)
summary(exp1_m_nameGender_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp1_d_FF_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4107.5   4147.7  -2047.7   4095.5     6010 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.4773 -0.3329 -0.0363  0.2865 16.3570 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.5808   0.7621  
    ##  Item        (Intercept) 0.6272   0.7920  
    ## Number of obs: 6016, groups:  Participant, 304; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.22358    0.12769  -1.751
    ## Conditionfirst vs full                       0.41016    0.25609   1.602
    ## GenderRatingCentered                         1.74039    0.08444  20.612
    ## Conditionfirst vs full:GenderRatingCentered -0.25145    0.16068  -1.565
    ##                                             Pr(>|z|)    
    ## (Intercept)                                    0.080 .  
    ## Conditionfirst vs full                         0.109    
    ## GenderRatingCentered                          <2e-16 ***
    ## Conditionfirst vs full:GenderRatingCentered    0.118    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC
    ## Cndtnfrstvf -0.387              
    ## GndrRtngCnt -0.115  0.113       
    ## Cvfll:GndRC  0.096 -0.122 -0.426

Compared to the main analysis including *other* responses, the intercept
is trending instead of significant, the gender rating effect the same,
and the small First vs Full effect is no longer significant.

# Quadratic Name Gender Rating

The second supplementary analysis tested the effect of squared name
gender rating, such that larger values meant names with stronger gender
associations (masc or fem), and smaller values meant names with weaker
gender associations.

``` r
exp1_d_FF %<>% mutate(GenderRatingSquared=GenderRatingCentered^2)
```

## Model 3: Quadratic

No quadratic effects.

``` r
exp1_m_nameGenderQuad <- glmer(
  She ~ Condition*GenderRatingCentered + Condition*GenderRatingSquared +
    (1|Participant) + (1|Item), 
  exp1_d_FF, family="binomial")
summary(exp1_m_nameGenderQuad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## She ~ Condition * GenderRatingCentered + Condition * GenderRatingSquared +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: exp1_d_FF
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4658.9   4712.9  -2321.4   4642.9     6364 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.1928 -0.3535 -0.0574  0.3125 14.0580 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.8910   0.9440  
    ##  Item        (Intercept) 0.4795   0.6925  
    ## Number of obs: 6372, groups:  Participant, 305; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.59972    0.15023  -3.992
    ## Conditionfirst vs full                       0.38504    0.30017   1.283
    ## GenderRatingCentered                         1.59657    0.07187  22.214
    ## GenderRatingSquared                          0.03687    0.03923   0.940
    ## Conditionfirst vs full:GenderRatingCentered -0.16052    0.13726  -1.169
    ## Conditionfirst vs full:GenderRatingSquared   0.06049    0.07828   0.773
    ##                                             Pr(>|z|)    
    ## (Intercept)                                 6.55e-05 ***
    ## Conditionfirst vs full                         0.200    
    ## GenderRatingCentered                         < 2e-16 ***
    ## GenderRatingSquared                            0.347    
    ## Conditionfirst vs full:GenderRatingCentered    0.242    
    ## Conditionfirst vs full:GenderRatingSquared     0.440    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC GndrRS Cvf:GRC
    ## Cndtnfrstvf -0.374                             
    ## GndrRtngCnt -0.112  0.024                      
    ## GndrRtngSqr -0.617  0.267 -0.041               
    ## Cvfll:GndRC  0.025 -0.107 -0.408  0.111        
    ## Cvfll:GndRS  0.266 -0.618  0.120 -0.439 -0.030

# Participant Gender

## Setup/Data Summary

The third supplementary analysis looks at participant gender: if male
participants show a larger bias towards HE responses than non-male
participants.

Participants entered their gender in a free-response box.

``` r
exp1_d %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   198 |
|              1 |   244 |
|             NA |    15 |

For this analysis, we exclude participants who did not respond (N=15).
Because there are not enough participants to create 3 groups, we compare
male to non-male participants. Male participants (N=244) coded as 1 and
female (N=196), genderfluid (N=1), and nonbinary participants (N=1)
coded as 0.

``` r
exp1_d_gender <- exp1_d %>% filter(!is.na(SubjGenderMale))

exp1_d_gender %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   198 |
|              1 |   244 |

Summary of responses by condition and participant gender.

``` r
exp1_d_gender %<>% mutate(ResponseAll=case_when(
       He==1 ~ "He",
       She==1 ~ "She", 
       Other==1 ~ "Other"))

exp1_d_count_gender <- exp1_d_gender %>% 
  group_by(Condition, ResponseAll, SubjGenderMale) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=c(ResponseAll),
              values_from=n) %>%
  mutate(She_HeOther = She / (He+Other),
         She_He = She / He) %>%
  rename("ParticipantGender"="SubjGenderMale") 
exp1_d_count_gender$ParticipantGender %<>% recode("0"="Non-male", "1"="Male")
kable(exp1_d_count_gender, digits=3)
```

| Condition | ParticipantGender |   He | Other | She | She_HeOther | She_He |
|:----------|:------------------|-----:|------:|----:|------------:|-------:|
| first     | Non-male          |  680 |    83 | 644 |       0.844 |  0.947 |
| first     | Male              |  830 |   131 | 698 |       0.726 |  0.841 |
| full      | Non-male          |  656 |    58 | 648 |       0.908 |  0.988 |
| full      | Male              |  823 |    71 | 842 |       0.942 |  1.023 |
| last      | Non-male          | 1114 |   134 | 138 |       0.111 |  0.124 |
| last      | Male              | 1418 |   176 | 107 |       0.067 |  0.075 |

Participant gender is mean centered effects coded, comparing non-male
participants to male participants.

``` r
exp1_d_gender$SubjGenderMale %<>% as.factor()
contrasts(exp1_d_gender$SubjGenderMale)=cbind("NM_M"=c(-.5,.5)) 
contrasts(exp1_d_gender$SubjGenderMale)
```

    ##   NM_M
    ## 0 -0.5
    ## 1  0.5

Subset First and Full conditions.

``` r
exp1_d_FF_gender <- exp1_d_gender %>% filter(Condition!="last")
exp1_d_FF_gender$Condition <- droplevels(exp1_d_FF_gender$Condition)
contrasts(exp1_d_FF_gender$Condition) = 
  cbind("first vs full"=c(-.5,.5)) #add contrast back
contrasts(exp1_d_FF_gender$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

## Model 4: Condition \* Participant Gender

Effect of Condition (first name, last name, full name) and Participant
Gender (non-male vs male) on likelihood of a *she* response, as opposed
to a *he* or *other* response. Participant and Item are again included
as random intercepts.

``` r
exp1_m_cond_gender <- glmer(
  She ~ Condition * SubjGenderMale + (1|Participant) + (1|Item), 
  exp1_d_gender, family=binomial)
summary(exp1_m_cond_gender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition * SubjGenderMale + (1 | Participant) + (1 | Item)
    ##    Data: exp1_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6209.6   6266.7  -3096.8   6193.6     9243 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.9913 -0.2996 -0.1427  0.2145 10.1263 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 1.018    1.009   
    ##  Item        (Intercept) 7.202    2.684   
    ## Number of obs: 9251, groups:  Participant, 442; Item, 104
    ## 
    ## Fixed effects:
    ##                                                Estimate Std. Error z value
    ## (Intercept)                                     -1.4241     0.3071  -4.637
    ## Conditionlast vs first/full                      2.8079     0.7018   4.001
    ## Conditionfirst vs full                           0.5860     0.6993   0.838
    ## SubjGenderMaleNM_M                              -0.2637     0.1209  -2.182
    ## Conditionlast vs first/full:SubjGenderMaleNM_M   0.3958     0.2700   1.466
    ## Conditionfirst vs full:SubjGenderMaleNM_M        0.4309     0.2830   1.522
    ##                                                Pr(>|z|)    
    ## (Intercept)                                    3.53e-06 ***
    ## Conditionlast vs first/full                    6.31e-05 ***
    ## Conditionfirst vs full                           0.4021    
    ## SubjGenderMaleNM_M                               0.0291 *  
    ## Conditionlast vs first/full:SubjGenderMaleNM_M   0.1427    
    ## Conditionfirst vs full:SubjGenderMaleNM_M        0.1279    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/ Cndtvf SGMNM_ Cvf/:S
    ## Cndtnvfrst/ -0.181                            
    ## Cndtnfrstvf -0.360 -0.239                     
    ## SbjGndMNM_M -0.014 -0.006 -0.004              
    ## Cvf/:SGMNM_ -0.006 -0.009 -0.003 -0.117       
    ## Cvf:SGMNM_M -0.005 -0.002 -0.020 -0.008 -0.005

Male participants are less likely to respond *she* overall than non-male
participants, but this is not significant after correcting for multiple
comparisons. Neither interaction with Condition is significant.

## Model 5: Condition \* Name Gender \* Participant Gender

Effects of Condition (first name, full name), the first name’s Gender
Rating (centered, positive=more feminine), and Participant Gender
(non-male vs. male) on the likelihood of a *she* response as opposed to
a *he* or *other* responses. In Experiment 1, the Last Name condition
does not include any instances of the gendered first name, so it is not
included here. The model with random intercepts does not converge with
glmer, but does when using buildmer to find the maximal model (?).

``` r
exp1_m_nameGender_gender <- buildmer(
  formula = She ~ Condition * GenderRatingCentered * SubjGenderMale + 
            (1|Participant) + (1|Item), 
  data = exp1_d_FF_gender, family = binomial, 
  buildmerControl(direction = c("order"), quiet = TRUE))
summary(exp1_m_nameGender_gender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ 1 + GenderRatingCentered + Condition + SubjGenderMale +  
    ##     Condition:SubjGenderMale + GenderRatingCentered:SubjGenderMale +  
    ##     GenderRatingCentered:Condition + GenderRatingCentered:Condition:SubjGenderMale +  
    ##     (1 | Item)
    ##    Data: exp1_d_FF_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4708.6   4769.1  -2345.3   4690.6     6155 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.7492 -0.3848 -0.0928  0.3758 16.6793 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Item   (Intercept) 0.3698   0.6081  
    ## Number of obs: 6164, groups:  Item, 83
    ## 
    ## Fixed effects:
    ##                                                                Estimate
    ## (Intercept)                                                    -0.47422
    ## GenderRatingCentered                                            1.41461
    ## Conditionfirst vs full                                          0.45444
    ## SubjGenderMaleNM_M                                             -0.17431
    ## Conditionfirst vs full:SubjGenderMaleNM_M                       0.48353
    ## GenderRatingCentered:SubjGenderMaleNM_M                         0.13909
    ## GenderRatingCentered:Conditionfirst vs full                    -0.09910
    ## GenderRatingCentered:Conditionfirst vs full:SubjGenderMaleNM_M -0.26972
    ##                                                                Std. Error
    ## (Intercept)                                                       0.09401
    ## GenderRatingCentered                                              0.06258
    ## Conditionfirst vs full                                            0.18844
    ## SubjGenderMaleNM_M                                                0.07759
    ## Conditionfirst vs full:SubjGenderMaleNM_M                         0.15518
    ## GenderRatingCentered:SubjGenderMaleNM_M                           0.06684
    ## GenderRatingCentered:Conditionfirst vs full                       0.12233
    ## GenderRatingCentered:Conditionfirst vs full:SubjGenderMaleNM_M    0.13367
    ##                                                                 z value
    ## (Intercept)                                                    -5.04411
    ## GenderRatingCentered                                           22.60429
    ## Conditionfirst vs full                                          2.41158
    ## SubjGenderMaleNM_M                                             -2.24641
    ## Conditionfirst vs full:SubjGenderMaleNM_M                       3.11592
    ## GenderRatingCentered:SubjGenderMaleNM_M                         2.08105
    ## GenderRatingCentered:Conditionfirst vs full                    -0.81009
    ## GenderRatingCentered:Conditionfirst vs full:SubjGenderMaleNM_M -2.01777
    ##                                                                Pr(>|z|)
    ## (Intercept)                                                       0.000
    ## GenderRatingCentered                                              0.000
    ## Conditionfirst vs full                                            0.016
    ## SubjGenderMaleNM_M                                                0.025
    ## Conditionfirst vs full:SubjGenderMaleNM_M                         0.002
    ## GenderRatingCentered:SubjGenderMaleNM_M                           0.037
    ## GenderRatingCentered:Conditionfirst vs full                       0.418
    ## GenderRatingCentered:Conditionfirst vs full:SubjGenderMaleNM_M    0.044
    ##                                                                Pr(>|t|)    
    ## (Intercept)                                                    4.56e-07 ***
    ## GenderRatingCentered                                            < 2e-16 ***
    ## Conditionfirst vs full                                          0.01588 *  
    ## SubjGenderMaleNM_M                                              0.02468 *  
    ## Conditionfirst vs full:SubjGenderMaleNM_M                       0.00183 ** 
    ## GenderRatingCentered:SubjGenderMaleNM_M                         0.03743 *  
    ## GenderRatingCentered:Conditionfirst vs full                     0.41789    
    ## GenderRatingCentered:Conditionfirst vs full:SubjGenderMaleNM_M  0.04362 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC Cndtvf SGMNM_ Cvf:SG GRC:SG GnRC:Cvf
    ## GndrRtngCnt -0.207                                            
    ## Cndtnfrstvf -0.414  0.131                                     
    ## SbjGndMNM_M  0.005 -0.032 -0.056                              
    ## Cvf:SGMNM_M -0.055  0.052  0.005 -0.043                       
    ## GRC:SGMNM_M -0.037  0.027  0.057 -0.285  0.088                
    ## GndrRtC:Cvf  0.116 -0.374 -0.199  0.053 -0.032 -0.089         
    ## GRC:Cvf:SGM  0.057 -0.088 -0.037  0.088 -0.285 -0.056  0.026

- Participant Gender: n.s.

- Condition (First vs Full) \* Participant Gender: There is a larger
  difference between the First and Full Name conditions for male
  participants (see means above), but this is n.s. after correcting for
  multiple comparisons.

- Name Gender \* Participant Gender: There is a stronger effect of the
  first name gender rating for male participants, but this is n.s. after
  correction for multiple comparisons.

- Condition (First vs Full) \* Name Gender \* Participant Gender:
  trending

