Experiment 2: Supplementary Analyses
================
2023-02-07

- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#without-other-responses"
  id="toc-without-other-responses">Without <em>Other</em> Responses</a>
  - <a href="#model-1-condition-wo-other-responses"
    id="toc-model-1-condition-wo-other-responses">Model 1: Condition w/o
    <em>Other</em> Responses</a>
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
    - <a href="#interaction" id="toc-interaction">Interaction</a>
  - <a href="#model-5-condition--name-gender--participant-gender"
    id="toc-model-5-condition--name-gender--participant-gender">Model 5:
    Condition * Name Gender * Participant Gender</a>
- <a href="#gender-rating-centering"

# Setup

Variable names:

- Experiment: exp2

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

- Subset

  - all = including *other* responses

  - noOther = excluding *other* responses

  - FF = First and Full Name conditions only

  - Last = Last Name condition only

Load data and select columns used in model. See data/exp2_data_about.txt
for more details.

``` r
exp2_d <- read.csv("../data/exp2_data.csv", 
                   stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="NameShown") %>%
  select(Participant, SubjGenderMale, Condition, 
         GenderRating, 
         Item, Male, Female, Other)
str(exp2_d)
```

    ## 'data.frame':    9457 obs. of  8 variables:
    ##  $ Participant   : Factor w/ 1351 levels "Exp2_P1","Exp2_P10",..: 501 501 501 501 501 501 501 14 14 14 ...
    ##  $ SubjGenderMale: int  1 1 1 1 1 1 1 0 0 0 ...
    ##  $ Condition     : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating  : num  5.59 4.22 2.12 6.73 3.61 4.73 1.21 6.24 4.39 2.61 ...
    ##  $ Item          : Factor w/ 105 levels "Ashley","Ashley Cook",..: 51 91 18 60 87 55 63 1 47 29 ...
    ##  $ Male          : int  1 1 0 1 1 0 1 0 0 1 ...
    ##  $ Female        : int  0 0 1 0 0 1 0 1 1 0 ...
    ##  $ Other         : int  0 0 0 0 0 0 0 0 0 0 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp2_d %<>% mutate(GenderRatingCentered=
            scale(GenderRating, scale=FALSE))
```

Set contrasts for name conditions.

``` r
contrasts(exp2_d$Condition) = cbind(
  "last vs first/full"=c(.33,.33,-0.66), 
  "first vs full"=c(-.5,.5,0))
contrasts(exp2_d$Condition)
```

    ##       last vs first/full first vs full
    ## first               0.33          -0.5
    ## full                0.33           0.5
    ## last               -0.66           0.0

Subset for gender rating effects (First and Full conditions only).

``` r
exp2_d_FF <- exp2_d %>% filter(Condition!="last") 
exp2_d_FF$Condition <- droplevels(exp2_d_FF$Condition)
contrasts(exp2_d_FF$Condition) = cbind(
  "first vs full"=c(-.5,.5)) #add contrast back
contrasts(exp2_d_FF$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

# Without *Other* Responses

The first supplementary analysis tests if excluding OTHER responses
(3.72% of total responses) affects the pattern of results.

``` r
sum(exp2_d$Other) 
```

    ## [1] 352

``` r
sum(exp2_d$Other)/length(exp2_d$Other) 
```

    ## [1] 0.03722111

Exclude *other* responses.

``` r
exp2_d_noOther <- exp2_d %>% filter(Other==0)
exp2_d_FF_noOther<- exp2_d_FF %>% filter(Other==0)
```

## Model 1: Condition w/o *Other* Responses

Effect of Name Condition (first name, last name, full name) on
likelihood of a *female* response, as opposed to a *male* response, with
*other* responses excluded. Participant and Item are again included as
random intercepts, with items defined as the unique first, last and
first + last name combinations.

``` r
exp2_m_cond_noOther <- glmer(
  Female ~ Condition + (1|Participant) + (1|Item), 
  exp2_d_noOther, family=binomial)
summary(exp2_m_cond_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8950.7   8986.3  -4470.3   8940.7     9100 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8774 -0.4708 -0.3082  0.5527  4.7451 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1024   0.320   
    ##  Item        (Intercept) 1.7798   1.334   
    ## Number of obs: 9105, groups:  Participant, 1322; Item, 105
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -0.7854     0.1505  -5.220 1.79e-07 ***
    ## Conditionlast vs first/full   1.9225     0.3423   5.616 1.96e-08 ***
    ## Conditionfirst vs full       -0.2003     0.3441  -0.582    0.561    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/
    ## Cndtnvfrst/ -0.170       
    ## Cndtnfrstvf -0.361 -0.241

No differences.

### Odds Ratios: Intercept

``` r
exp2_OR_noOther_I <- exp2_m_cond_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric() 
exp(exp2_OR_noOther_I)
```

    ## [1] 0.4559352

``` r
exp(-exp2_OR_noOther_I)
```

    ## [1] 2.193294

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_I <- exp(exp2_OR_noOther_I) %>%
  round(2) 
```

0.45x less likely to recall as female overall (or: 2.21x more likely to
recall as male/other overall), p\<.001

### Odds Ratios: Last vs First+Full

``` r
exp2_est_noOther_LFF <- exp2_m_cond_noOther %>% 
  tidy() %>%
  filter(term=="Conditionlast vs first/full") %>%
  select(estimate) %>% as.numeric() 
exp(exp2_est_noOther_LFF)
```

    ## [1] 6.838129

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_LFF <- exp(exp2_est_noOther_LFF) %>%
  round(2)
```

6.75x more likely to use *she* in First + Full compared to Last (or:
6.75x times more likely to use *he* and *other* in Last than in First +
Full), p\<.001

### Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp2_d_noOther %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
exp2_d_noOther$Condition_Last %<>% as.factor()
```

``` r
exp2_m_L_noOther <- glmer(
  Female ~ Condition_Last + (1|Participant) + (1|Item), 
  data=exp2_d_noOther, family=binomial)
summary(exp2_m_L_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8949.0   8977.5  -4470.5   8941.0     9101 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8705 -0.4718 -0.3082  0.5518  4.7752 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1024   0.320   
    ##  Item        (Intercept) 1.7849   1.336   
    ## Number of obs: 9105, groups:  Participant, 1322; Item, 105
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -2.0543     0.2923  -7.027 2.11e-12 ***
    ## Condition_Last1   1.8558     0.3293   5.636 1.74e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.888

``` r
exp2_est_L_noOther <- exp2_m_L_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp2_est_L_noOther)
```

    ## [1] 0.1281845

``` r
exp(-exp2_est_L_noOther)
```

    ## [1] 7.801258

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_L <- exp(exp2_est_L_noOther) %>% 
  round(2) 
```

0.12x times less likely to recall as female in the Last Name condition
(or: 7.80x more likely to recall as male in the Last Name condition),
p\<.001

### Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp2_d_noOther %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
exp2_d_noOther$Condition_FF %<>% as.factor()
```

``` r
exp2_m_FF_noOther <- glmer(
  Female ~ Condition_FF + (1|Participant) + (1|Item), 
  data=exp2_d_noOther, family=binomial)
summary(exp2_m_FF_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8949.0   8977.5  -4470.5   8941.0     9101 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8705 -0.4718 -0.3082  0.5518  4.7752 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1024   0.320   
    ##  Item        (Intercept) 1.7849   1.336   
    ## Number of obs: 9105, groups:  Participant, 1322; Item, 105
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.1985     0.1517  -1.308    0.191    
    ## Condition_FF1  -1.8558     0.3293  -5.635 1.75e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.460

``` r
exp2_est_FF_noOther <- exp2_m_FF_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp2_est_FF_noOther)
```

    ## [1] 0.8199412

``` r
exp(-exp2_est_FF_noOther)
```

    ## [1] 1.2196

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_FF <- exp(exp2_est_FF_noOther) %>% 
  round(2)
```

0.81x times less likely o recall as female in the First and Full Name
conditions (or: 1.24x more likely to use *he* in the n the First and
Full Name conditions), p=.17

## Model 2: Condition \* Name Gender w/o *Other* Responses

Effects of Name Condition (first name, full name) and the first name’s
Gender Rating (centered, positive=more feminine) on the likelihood of a
*female* response as opposed to a *male* response, with *other*
responses excluded. In Experiment 2, the Last Name condition does not
include any instances of the gendered first name, so it is not included
here. Participant and Item are again included as random intercepts.

``` r
exp2_m_nameGender_noOther <- glmer(
  Female ~ Condition * GenderRatingCentered + (1|Participant) + (1|Item), 
  exp2_d_FF_noOther, family=binomial)
summary(exp2_m_nameGender_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp2_d_FF_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6599.9   6640.3  -3294.0   6587.9     6195 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8767 -0.6104 -0.2386  0.6019  4.2441 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.02864  0.1692  
    ##  Item        (Intercept) 0.13866  0.3724  
    ## Number of obs: 6201, groups:  Participant, 897; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.12585    0.05754  -2.187
    ## Conditionfirst vs full                      -0.19186    0.11493  -1.669
    ## GenderRatingCentered                         0.78391    0.03496  22.423
    ## Conditionfirst vs full:GenderRatingCentered -0.06121    0.06855  -0.893
    ##                                             Pr(>|z|)    
    ## (Intercept)                                   0.0287 *  
    ## Conditionfirst vs full                        0.0951 .  
    ## GenderRatingCentered                          <2e-16 ***
    ## Conditionfirst vs full:GenderRatingCentered   0.3719    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC
    ## Cndtnfrstvf -0.344              
    ## GndrRtngCnt -0.058 -0.013       
    ## Cvfll:GndRC -0.011 -0.052 -0.293

Compared to the main analysis including *other* responses, the intercept
has a larger p-value, the difference between the First and Full Name
conditions is no longer trending, and the Name Gender Rating is the
same.

# Quadratic Name Gender Rating

The second supplementary analysis tested the effect of squared name
gender rating, such that larger values meant names with stronger gender
associations (masc or fem), and smaller values meant names with weaker
gender associations.

``` r
exp2_d_FF %<>% mutate(GenderRatingSquared=GenderRatingCentered^2)
```

## Model 3: Quadratic

No quadratic effects.

``` r
exp2_m_nameGenderQuad <- glmer(
  Female ~ Condition*GenderRatingCentered + Condition*GenderRatingSquared +
    (1|Participant) + (1|Item), 
  exp2_d_FF, family="binomial")
summary(exp2_m_nameGenderQuad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ Condition * GenderRatingCentered + Condition * GenderRatingSquared +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_FF
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6779.2   6833.2  -3381.6   6763.2     6313 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8991 -0.6319 -0.2351  0.6235  4.4716 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1137   0.3372  
    ##  Item        (Intercept) 0.1409   0.3754  
    ## Number of obs: 6321, groups:  Participant, 903; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.13892    0.08064  -1.723
    ## Conditionfirst vs full                      -0.25775    0.16129  -1.598
    ## GenderRatingCentered                         0.77906    0.03558  21.895
    ## GenderRatingSquared                         -0.01296    0.01963  -0.661
    ## Conditionfirst vs full:GenderRatingCentered -0.06125    0.06977  -0.878
    ## Conditionfirst vs full:GenderRatingSquared   0.01120    0.03921   0.286
    ##                                             Pr(>|z|)    
    ## (Intercept)                                   0.0849 .  
    ## Conditionfirst vs full                        0.1100    
    ## GenderRatingCentered                          <2e-16 ***
    ## GenderRatingSquared                           0.5089    
    ## Conditionfirst vs full:GenderRatingCentered   0.3800    
    ## Conditionfirst vs full:GenderRatingSquared    0.7752    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC GndrRS Cvf:GRC
    ## Cndtnfrstvf -0.375                             
    ## GndrRtngCnt -0.164  0.052                      
    ## GndrRtngSqr -0.687  0.258  0.168               
    ## Cvfll:GndRC  0.058 -0.165 -0.306 -0.087        
    ## Cvfll:GndRS  0.258 -0.688 -0.083 -0.331  0.175

# Participant Gender

## Setup/Data Summary

The third supplementary analysis looks at participant gender: if male
participants show a larger bias to recall the character as male than
non-male participants.

Participants entered their gender in a free-response box.

``` r
exp2_d %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   569 |
|              1 |   694 |
|             NA |    88 |

For this analysis, we exclude participants who did not respond (N=88).
Because there are not enough participants to create 3 groups, we compare
male to non-male participants. Male participants (N=694) are coded as 1,
and female (N=566), nonbinary (N=2), and genderqueer (N=1) participants
are coded as 0.

Summary of responses by condition and participant gender:

``` r
exp2_d_gender <- exp2_d %>% 
  filter(!is.na(SubjGenderMale)) %>%
  mutate(ResponseAll=case_when(
    Male==1 ~ "Male",
    Female==1 ~ "Female", 
    Other==1 ~ "Other"))

exp2_d_count_gender <- exp2_d_gender %>% 
  group_by(Condition, ResponseAll, SubjGenderMale) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=c(ResponseAll),
              values_from=n) %>%
  mutate(Female_MaleOther = Female / (Male + Other),
         Female_Male = Female / Male) %>%
  rename("ParticipantGender"="SubjGenderMale") 
exp2_d_count_gender$ParticipantGender %<>% 
  recode("0"="Non-male", "1"="Male")
kable(exp2_d_count_gender, digits=3)
```

| Condition | ParticipantGender | Female | Male | Other | Female_MaleOther | Female_Male |
|:----------|:------------------|-------:|-----:|------:|-----------------:|------------:|
| first     | Non-male          |    693 |  609 |    21 |            1.100 |       1.138 |
| first     | Male              |    784 |  847 |    14 |            0.911 |       0.926 |
| full      | Non-male          |    601 |  609 |    43 |            0.922 |       0.987 |
| full      | Male              |    734 |  893 |    32 |            0.794 |       0.822 |
| last      | Non-male          |    170 | 1146 |    91 |            0.137 |       0.148 |
| last      | Male              |    212 | 1223 |   119 |            0.158 |       0.173 |

Participant gender is mean centered effects coded, comparing non-male
participants to male participants.

``` r
exp2_d_gender$SubjGenderMale %<>% as.factor()
contrasts(exp2_d_gender$SubjGenderMale)=cbind(
  "NM_M"=c(-.5,.5)) 
contrasts(exp2_d_gender$SubjGenderMale)
```

    ##   NM_M
    ## 0 -0.5
    ## 1  0.5

Subset First and Full conditions.

``` r
exp2_d_FF_gender <- exp2_d_gender %>% filter(Condition!="last")
exp2_d_FF_gender$Condition <- droplevels(exp2_d_FF_gender$Condition)
contrasts(exp2_d_FF_gender$Condition) = 
  cbind("first vs full"=c(-.5,.5)) #add contrast back
contrasts(exp2_d_FF_gender$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

## Model 4: Condition \* Participant Gender

Effect of Name Condition (first name, last name, full name) and
Participant Gender (non-male vs male) on likelihood of a *female*
response, as opposed to a *male* response or *other* response.
Participant and Item are again included as random intercepts.

``` r
exp2_m_cond_gender <- glmer(
  Female ~ Condition * SubjGenderMale + (1|Participant) + (1|Item), 
  exp2_d_gender, family=binomial)
summary(exp2_m_cond_gender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * SubjGenderMale + (1 | Participant) + (1 |  
    ##     Item)
    ##    Data: exp2_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8549.0   8605.6  -4266.5   8533.0     8833 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6665 -0.4635 -0.2879  0.5557  4.6794 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1828   0.4275  
    ##  Item        (Intercept) 1.8258   1.3512  
    ## Number of obs: 8841, groups:  Participant, 1263; Item, 105
    ## 
    ## Fixed effects:
    ##                                                Estimate Std. Error z value
    ## (Intercept)                                    -0.85062    0.15276  -5.568
    ## Conditionlast vs first/full                     2.02308    0.34754   5.821
    ## Conditionfirst vs full                         -0.20021    0.34946  -0.573
    ## SubjGenderMaleNM_M                             -0.12522    0.06205  -2.018
    ## Conditionlast vs first/full:SubjGenderMaleNM_M -0.41779    0.14249  -2.932
    ## Conditionfirst vs full:SubjGenderMaleNM_M       0.09225    0.14019   0.658
    ##                                                Pr(>|z|)    
    ## (Intercept)                                    2.57e-08 ***
    ## Conditionlast vs first/full                    5.84e-09 ***
    ## Conditionfirst vs full                          0.56669    
    ## SubjGenderMaleNM_M                              0.04357 *  
    ## Conditionlast vs first/full:SubjGenderMaleNM_M  0.00337 ** 
    ## Conditionfirst vs full:SubjGenderMaleNM_M       0.51051    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/ Cndtvf SGMNM_ Cvf/:S
    ## Cndtnvfrst/ -0.170                            
    ## Cndtnfrstvf -0.359 -0.240                     
    ## SbjGndMNM_M -0.022  0.001 -0.002              
    ## Cvf/:SGMNM_  0.003 -0.023 -0.001 -0.196       
    ## Cvf:SGMNM_M -0.002 -0.001 -0.024 -0.002 -0.001

- Male participants are less likely to recall the character as female
  overall, but this is not significant after correction for multiple
  comparisons.

- The interaction between Condition (Last vs. First + Full) and
  Participant Gender is significant.

### Interaction

Dummy code to get the Participant Gender effect just for First and Full
Name conditions.

``` r
exp2_d_gender$FFdummy = as.numeric(exp2_d_gender$Condition) 
exp2_d_gender$FFdummy[exp2_d_gender$FFdummy == 1] <- 0
exp2_d_gender$FFdummy[exp2_d_gender$FFdummy == 2] <- 0
exp2_d_gender$FFdummy[exp2_d_gender$FFdummy == 3] <- 1
with(exp2_d_gender, tapply(FFdummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     0     0     1

``` r
exp2_m_cond_genderFF <- glmer(Female ~ 
    FFdummy*SubjGenderMale + (1|Participant) + (1|Item), 
    data=exp2_d_gender, family=binomial)
summary(exp2_m_cond_genderFF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ FFdummy * SubjGenderMale + (1 | Participant) + (1 |  
    ##     Item)
    ##    Data: exp2_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8545.7   8588.2  -4266.8   8533.7     8835 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7198 -0.4648 -0.2879  0.5527  4.6970 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1832   0.4281  
    ##  Item        (Intercept) 1.8318   1.3535  
    ## Number of obs: 8841, groups:  Participant, 1263; Item, 105
    ## 
    ## Fixed effects:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -0.22873    0.15432  -1.482 0.138294    
    ## FFdummy                    -1.95738    0.33433  -5.855 4.78e-09 ***
    ## SubjGenderMaleNM_M         -0.26325    0.07015  -3.753 0.000175 ***
    ## FFdummy:SubjGenderMaleNM_M  0.41378    0.14108   2.933 0.003357 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) FFdmmy SGMNM_
    ## FFdummy     -0.461              
    ## SbjGndMNM_M -0.028  0.015       
    ## FFd:SGMNM_M  0.014 -0.023 -0.497

Then dummy code to get the participant gender effect just for Last Name
condition.

``` r
exp2_d_gender$Ldummy = as.numeric(exp2_d_gender$Condition) 
exp2_d_gender$Ldummy[exp2_d_gender$Ldummy == 1] <- 1
exp2_d_gender$Ldummy[exp2_d_gender$Ldummy == 2] <- 1
exp2_d_gender$Ldummy[exp2_d_gender$Ldummy == 3] <- 0
with(exp2_d_gender, tapply(Ldummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     1     1     0

``` r
exp2_m_cond_genderL <- glmer(Female ~ 
    Ldummy*SubjGenderMale + (1|Participant) + (1|Item), 
    data=exp2_d_gender, family=binomial)
summary(exp2_m_cond_genderL)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Ldummy * SubjGenderMale + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8545.7   8588.2  -4266.8   8533.7     8835 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7198 -0.4648 -0.2879  0.5527  4.6970 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1832   0.4281  
    ##  Item        (Intercept) 1.8318   1.3535  
    ## Number of obs: 8841, groups:  Participant, 1263; Item, 105
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -2.1861     0.2967  -7.367 1.75e-13 ***
    ## Ldummy                      1.9574     0.3344   5.854 4.80e-09 ***
    ## SubjGenderMaleNM_M          0.1505     0.1224   1.230  0.21876    
    ## Ldummy:SubjGenderMaleNM_M  -0.4138     0.1411  -2.933  0.00336 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ldummy SGMNM_
    ## Ldummy      -0.887              
    ## SbjGndMNM_M -0.021  0.019       
    ## Ldm:SGMNM_M  0.019 -0.023 -0.868

- Beta for subj gender in First + Full: -0.26325
- Beta for subj gender in Last: 0.1505 NS

–\> Male participants were less likely to recall the referent as female
than non-male participants in the First and Full Name conditions. No
participant gender difference in the Last Name condition.

## Model 5: Condition \* Name Gender \* Participant Gender

Effects of Name Condition (first name, full name), the first name’s
Gender Rating (centered, positive=more feminine), and Participant Gender
(non-male vs. male) on the likelihood of a *female* response as opposed
to *male* or *other* responses. In Experiment 2, the Last Name condition
does not include any instances of the gendered first name, so it is not
included here.

``` r
exp2_m_nameGender_gender <- buildmer(
  formula = Female ~ Condition * GenderRatingCentered * SubjGenderMale + 
            (1|Participant) + (1|Item), 
  data = exp2_d_FF_gender, family = binomial, 
  buildmerControl(direction = "order", quiet = TRUE))
summary(exp2_m_nameGender_gender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ 1 + GenderRatingCentered + SubjGenderMale + Condition +  
    ##     GenderRatingCentered:SubjGenderMale + GenderRatingCentered:Condition +  
    ##     SubjGenderMale:Condition + GenderRatingCentered:SubjGenderMale:Condition +  
    ##     (1 | Item) + (1 | Participant)
    ##    Data: exp2_d_FF_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6256.0   6322.8  -3118.0   6236.0     5870 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3843 -0.6313 -0.2289  0.6377  4.3983 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.09857  0.3140  
    ##  Item        (Intercept) 0.14889  0.3859  
    ## Number of obs: 5880, groups:  Participant, 840; Item, 83
    ## 
    ## Fixed effects:
    ##                                                                Estimate
    ## (Intercept)                                                    -0.15983
    ## GenderRatingCentered                                            0.80814
    ## SubjGenderMaleNM_M                                             -0.22830
    ## Conditionfirst vs full                                         -0.20337
    ## GenderRatingCentered:SubjGenderMaleNM_M                        -0.14941
    ## GenderRatingCentered:Conditionfirst vs full                    -0.05830
    ## SubjGenderMaleNM_M:Conditionfirst vs full                       0.12761
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full -0.11630
    ##                                                                Std. Error
    ## (Intercept)                                                       0.06058
    ## GenderRatingCentered                                              0.03677
    ## SubjGenderMaleNM_M                                                0.06826
    ## Conditionfirst vs full                                            0.12106
    ## GenderRatingCentered:SubjGenderMaleNM_M                           0.04500
    ## GenderRatingCentered:Conditionfirst vs full                       0.07191
    ## SubjGenderMaleNM_M:Conditionfirst vs full                         0.13645
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full    0.08997
    ##                                                                 z value
    ## (Intercept)                                                    -2.63817
    ## GenderRatingCentered                                           21.98051
    ## SubjGenderMaleNM_M                                             -3.34465
    ## Conditionfirst vs full                                         -1.67990
    ## GenderRatingCentered:SubjGenderMaleNM_M                        -3.31991
    ## GenderRatingCentered:Conditionfirst vs full                    -0.81074
    ## SubjGenderMaleNM_M:Conditionfirst vs full                       0.93524
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full -1.29269
    ##                                                                Pr(>|z|)
    ## (Intercept)                                                       0.008
    ## GenderRatingCentered                                              0.000
    ## SubjGenderMaleNM_M                                                0.001
    ## Conditionfirst vs full                                            0.093
    ## GenderRatingCentered:SubjGenderMaleNM_M                           0.001
    ## GenderRatingCentered:Conditionfirst vs full                       0.418
    ## SubjGenderMaleNM_M:Conditionfirst vs full                         0.350
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full    0.196
    ##                                                                Pr(>|t|)    
    ## (Intercept)                                                    0.008336 ** 
    ## GenderRatingCentered                                            < 2e-16 ***
    ## SubjGenderMaleNM_M                                             0.000824 ***
    ## Conditionfirst vs full                                         0.092977 .  
    ## GenderRatingCentered:SubjGenderMaleNM_M                        0.000900 ***
    ## GenderRatingCentered:Conditionfirst vs full                    0.417513    
    ## SubjGenderMaleNM_M:Conditionfirst vs full                      0.349664    
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full 0.196119    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC SGMNM_ Cndtvf GRC:SG GRC:Cvf SGMNvf
    ## GndrRtngCnt -0.062                                           
    ## SbjGndMNM_M -0.092 -0.010                                    
    ## Cndtnfrstvf -0.329 -0.013 -0.017                             
    ## GRC:SGMNM_M -0.011 -0.147 -0.126  0.024                      
    ## GndrRtC:Cvf -0.011 -0.276  0.021 -0.056 -0.033               
    ## SGMNM_M:Cvf -0.018  0.022  0.010 -0.094 -0.036 -0.003        
    ## GRC:SGMNMvf  0.024 -0.035 -0.035 -0.011 -0.003 -0.144  -0.127

- Male participants are less likely to recall the character as female
  overall. This matches the results of the interaction in the
  condition-only model.
- The interaction between participant gender and first name gender
  rating is significant. Smaller effect of name gender rating in male
  participants.
- Interaction with Condition, three-way interaction with Name Gender and
  Condition n.s.
