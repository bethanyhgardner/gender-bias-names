Experiment 4: Supplementary Analyses
================
2022-08-16

-   <a href="#setup" id="toc-setup">Setup</a>
-   <a href="#without-other-responses"
    id="toc-without-other-responses">Without <em>Other</em> Responses</a>
    -   <a href="#odds-ratios-intercept" id="toc-odds-ratios-intercept">Odds
        Ratios: Intercept</a>
    -   <a href="#odds-ratios-last-vs-firstfull"
        id="toc-odds-ratios-last-vs-firstfull">Odds Ratios: Last vs
        First+Full</a>
    -   <a href="#odds-ratios-last-only" id="toc-odds-ratios-last-only">Odds
        Ratios: Last Only</a>
    -   <a href="#odds-ratios-first-and-full-only"
        id="toc-odds-ratios-first-and-full-only">Odds Ratios: First and Full
        Only</a>
-   <a href="#quadratic-name-gender-rating"
    id="toc-quadratic-name-gender-rating">Quadratic Name Gender Rating</a>
-   <a href="#participant-gender" id="toc-participant-gender">Participant
    Gender</a>
    -   <a href="#setupdata-summary" id="toc-setupdata-summary">Setup/Data
        Summary</a>
    -   <a href="#model-condition--name-gender--participant-gender"
        id="toc-model-condition--name-gender--participant-gender">Model:
        Condition * Name Gender * Participant Gender</a>

# Setup

-   Variable names:

    -   Experiment: exp4

    -   Type

        -   d = data
        -   m = model
        -   est = log odds estimate from model
        -   OR = odds ratio converted from est

    -   Analysis

        -   count =sums of response types
        -   cond = effect of Condition (Last vs First+Full)
        -   nameGender = effects of Condition (First vs Full) and Name
            Gender Rating

    -   Subset

        -   all = including *other* responses

        -   noOther = excluding *other* responses

        -   FF = First and Full Name conditions only

        -   Last = Last Name condition only

Load data and select columns used in model. See data/exp4_data_about.txt
for more details.

``` r
exp4_d <- read.csv("../data/exp4_data.csv", 
                   stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="Name") %>%
  select(Participant, Condition, SubjGenderMale, 
         GenderRating, Item, Male, Female, Other)
str(exp4_d)
```

    ## 'data.frame':    8771 obs. of  8 variables:
    ##  $ Participant   : Factor w/ 1253 levels "Exp4_P1","Exp4_P10",..: 520 520 520 520 520 520 520 1143 1143 1143 ...
    ##  $ Condition     : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SubjGenderMale: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating  : num  6.24 2.61 6.82 5.34 1.28 4.39 3.87 5.22 1.24 5.86 ...
    ##  $ Item          : Factor w/ 63 levels "Ashley Cook",..: 1 18 21 22 25 28 50 5 7 15 ...
    ##  $ Male          : int  0 1 0 0 1 1 1 1 1 0 ...
    ##  $ Female        : int  1 0 1 1 0 0 0 0 0 1 ...
    ##  $ Other         : int  0 0 0 0 0 0 0 0 0 0 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp4_d %<>% mutate(GenderRatingCentered=
                     scale(GenderRating, scale=FALSE))
```

Set contrasts for name conditions, now weighted to account for uneven
sample sizes. This uses Scott Fraundorf’s function for weighted
contrasts. (The psycholing package version doesn’t support doing 2v1
comparisons, only 1v1.) Condition1 is Last vs First+Full. Condition2 is
First vs Full.

``` r
source("centerfactor.R")
contrasts(exp4_d$Condition) <- centerfactor(exp4_d$Condition, c("last","first"))
contrasts(exp4_d$Condition)
```

    ##             [,1]         [,2]
    ## first  0.3312051 -0.497605746
    ## full   0.3312051  0.502394254
    ## last  -0.6687949  0.002394254

# Without *Other* Responses

The first supplementary analysis tests if excluding *other* responses
(2.99% of total responses) affects the pattern of results.

``` r
sum(exp4_d$Other) 
```

    ## [1] 262

``` r
sum(exp4_d$Other)/length(exp4_d$Other) 
```

    ## [1] 0.02987117

Exclude *other* responses.

``` r
exp4_d_noOther <- exp4_d %>% filter(Other==0)
```

Effect of Name Condition (first name, last name, full name) and first
name Gender Rating on likelihood of a *female* response, as opposed to a
*male* response, with *other* responses excluded. Participant and Item
are again included as random intercepts, with items defined as the
unique first, last and first + last name combinations.

``` r
exp4_m_noOther <- glmer(
  Female ~ Condition * GenderRatingCentered + 
    (1|Participant) + (1|Item), 
  data=exp4_d_noOther, family=binomial)
summary(exp4_m_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8737.3   8793.7  -4360.6   8721.3     8501 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4146 -0.5648 -0.2574  0.5646  4.7423 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.05031  0.2243  
    ##  Item        (Intercept) 0.36891  0.6074  
    ## Number of obs: 8509, groups:  Participant, 1232; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.16411    0.08195  -2.002   0.0452 *  
    ## Condition1                       0.13514    0.05783   2.337   0.0194 *  
    ## Condition2                       0.11302    0.06837   1.653   0.0983 .  
    ## GenderRatingCentered             0.76972    0.04650  16.554   <2e-16 ***
    ## Condition1:GenderRatingCentered  0.13700    0.03522   3.890   0.0001 ***
    ## Condition2:GenderRatingCentered -0.09189    0.04315  -2.129   0.0332 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC C1:GRC
    ## Condition1   0.016                            
    ## Condition2  -0.010 -0.012                     
    ## GndrRtngCnt -0.022  0.005  0.013              
    ## Cndtn1:GnRC  0.004 -0.101  0.020  0.038       
    ## Cndtn2:GnRC  0.012  0.020 -0.085 -0.025 -0.038

Compared to the main model:

-   Intercept and Condition2:GenderRatingCentered (difference between
    Last Name and First+Full name conditions) potentially smaller
    differences

-   Condition2 now trending

## Odds Ratios: Intercept

``` r
exp4_OR_noOther_I <- exp4_m_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()
exp(exp4_OR_noOther_I)
```

    ## [1] 0.8486509

``` r
exp(-exp4_OR_noOther_I)
```

    ## [1] 1.178341

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_noOther_I <- exp(exp4_OR_noOther_I) %>%
  round(2)
```

0.84x less likely to recall as female overall (or: 1.18x more likely to
recall as male overall), p\<.05

## Odds Ratios: Last vs First+Full

``` r
exp4_est_noOther_LFF <- exp4_m_noOther %>% 
  tidy() %>%
  filter(term=="Condition1") %>%
  select(estimate) %>% as.numeric()
exp(exp4_est_noOther_LFF)
```

    ## [1] 1.144701

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_noOther_LFF <- exp(exp4_est_noOther_LFF) %>% 
  round(2)
```

1.14x more likely to recall as female in First + Full compared to Last,
p\<.05

## Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp4_d_noOther %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
exp4_d_noOther$Condition_Last %<>% as.factor()
```

``` r
exp4_m_noOther_L <- glmer(
  Female ~ Condition_Last + (1|Participant) + (1|Item), 
  data=exp4_d_noOther, family=binomial)
summary(exp4_m_noOther_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp4_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8858.6   8886.8  -4425.3   8850.6     8505 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0926 -0.5697 -0.2723  0.5620  4.0294 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.0476   0.2182  
    ##  Item        (Intercept) 2.2803   1.5101  
    ## Number of obs: 8509, groups:  Participant, 1232; Item, 63
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)     -0.27235    0.19639  -1.387  0.16553   
    ## Condition_Last1  0.16052    0.05804   2.766  0.00568 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.198

``` r
exp4_est_noOther_L <- exp4_m_noOther_L %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp4_est_noOther_L)
```

    ## [1] 0.7615914

``` r
exp(-exp4_est_noOther_L)
```

    ## [1] 1.31304

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_noOther_L <- exp(exp4_est_noOther_L) %>% 
  round(2)
```

0.76x times less likely to recall as female in the Last Name condition
(or: 1.31x more likely to recall as male in the Last Name condition),
p=.17

## Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp4_d_noOther %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
exp4_d_noOther$Condition_FF %<>% as.factor()
```

``` r
exp4_m_noOther_FF <- glmer(
  Female ~ Condition_FF + (1|Participant) + (1|Item), 
  data=exp4_d_noOther, family=binomial)
summary(exp4_m_noOther_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp4_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8858.6   8886.8  -4425.3   8850.6     8505 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0926 -0.5697 -0.2723  0.5620  4.0294 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.04761  0.2182  
    ##  Item        (Intercept) 2.28035  1.5101  
    ## Number of obs: 8509, groups:  Participant, 1232; Item, 63
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)   -0.11182    0.19348  -0.578  0.56331   
    ## Condition_FF1 -0.16052    0.05804  -2.766  0.00568 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.099

``` r
exp4_est_noOther_FF <- exp4_m_noOther_FF %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp4_est_noOther_FF)
```

    ## [1] 0.894209

``` r
exp(-exp4_est_noOther_FF)
```

    ## [1] 1.118307

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_noOther_FF <- exp(exp4_est_noOther_FF) %>% 
  round(2)
```

0.89x less likely to recall as female in First and Full Name conditions
(or: 1.12x more likely to recall as male in First and Full Name
conditions), p=0.56

# Quadratic Name Gender Rating

The second supplementary analysis tested the effect of squared name
gender rating, such that larger values meant names with stronger gender
associations (masc or fem), and smaller values meant names with weaker
gender associations.

``` r
exp4_d %<>% mutate(GenderRatingSquared=GenderRatingCentered^2)

exp4_m_nameGenderQuad <- glmer(
  Female ~ Condition*GenderRatingCentered + Condition*GenderRatingSquared +
    (1|Participant) + (1|Item), 
  exp4_d, family="binomial")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00243397 (tol = 0.002, component 1)

``` r
summary(exp4_m_nameGenderQuad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ Condition * GenderRatingCentered + Condition * GenderRatingSquared +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9142.9   9220.8  -4560.4   9120.9     8760 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2148 -0.5741 -0.2557  0.5736  5.9909 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2036   0.4512  
    ##  Item        (Intercept) 0.3482   0.5901  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.36857    0.11558  -3.189 0.001428 ** 
    ## Condition1                       0.16071    0.08015   2.005 0.044956 *  
    ## Condition2                      -0.07638    0.09273  -0.824 0.410097    
    ## GenderRatingCentered             0.77987    0.04638  16.814  < 2e-16 ***
    ## GenderRatingSquared              0.03432    0.02628   1.306 0.191611    
    ## Condition1:GenderRatingCentered  0.13227    0.03480   3.800 0.000144 ***
    ## Condition2:GenderRatingCentered -0.09191    0.04261  -2.157 0.030996 *  
    ## Condition1:GenderRatingSquared  -0.01424    0.01933  -0.737 0.461257    
    ## Condition2:GenderRatingSquared   0.05968    0.02351   2.538 0.011137 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC GndrRS C1:GRC C2:GRC C1:GRS
    ## Condition1   0.002                                                 
    ## Condition2  -0.003 -0.002                                          
    ## GndrRtngCnt -0.173  0.003 -0.005                                   
    ## GndrRtngSqr -0.717 -0.005  0.005  0.212                            
    ## Cndtn1:GnRC  0.001 -0.165 -0.009  0.038 -0.003                     
    ## Cndtn2:GnRC -0.004 -0.008 -0.154 -0.027  0.029 -0.043              
    ## Cndtn1:GnRS -0.005 -0.636  0.007 -0.003  0.029  0.117  0.056       
    ## Cndtn2:GnRS  0.005  0.006 -0.620  0.034 -0.027  0.058  0.095 -0.045
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.00243397 (tol = 0.002, component 1)

-   Condition (F v F) \* Quadratic Gender Rating interaction, but n.s.
    after correction for multiple comparisons, so not making a big deal
    of it

# Participant Gender

## Setup/Data Summary

The third supplementary analysis looks at participant gender: if male
participants show a larger bias to recall the character as male than
non-male participants.

Participants entered their gender in a free-response box.

``` r
exp4_d %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>% kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   558 |
|              1 |   604 |
|             NA |    91 |

For this analysis, we exclude participants who did not respond (N=91).
Because there are not enough participants to create 3 groups, we compare
male to non-male participants. Male (N=602) and transgender male (N=1)
are coded as 1, and female (N=555), nonbinary (N=3), and transgender
female (N=1) are coded as 0.

Summary of responses by condition and participant gender:

``` r
exp4_d_gender <- exp4_d %>% 
  filter(!is.na(SubjGenderMale)) %>%
  mutate(ResponseAll=case_when(
    Male   == 1 ~ "Male",
    Female == 1 ~ "Female", 
    Other  == 1 ~ "Other"))

exp4_d_gender %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   558 |
|              1 |   604 |

Participant gender is mean centered effects coded, comparing non-male
participants to male participants.

``` r
exp4_d_gender$SubjGenderMale %<>% as.factor()
contrasts(exp4_d_gender$SubjGenderMale)=cbind("NM_M"=c(-.5,.5)) 
contrasts(exp4_d_gender$SubjGenderMale)
```

    ##   NM_M
    ## 0 -0.5
    ## 1  0.5

## Model: Condition \* Name Gender \* Participant Gender

Effects of Name Condition (first name, full name), the first name’s
Gender Rating (centered, positive=more feminine), and Participant Gender
(non-male vs. male) on the likelihood of a *female* response as opposed
to *male* or *other* responses.

``` r
exp4_m_gender <- glmer(Female ~ 
      Condition * GenderRatingCentered * SubjGenderMale + 
      (1|Participant) + (1|Item), 
      data=exp4_d_gender, family=binomial)
summary(exp4_m_gender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered * SubjGenderMale +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: exp4_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8483.2   8581.2  -4227.6   8455.2     8120 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4044 -0.5729 -0.2630  0.5804  4.8532 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1822   0.4268  
    ##  Item        (Intercept) 0.3674   0.6061  
    ## Number of obs: 8134, groups:  Participant, 1162; Item, 63
    ## 
    ## Fixed effects:
    ##                                                    Estimate Std. Error z value
    ## (Intercept)                                        -0.24985    0.08273  -3.020
    ## Condition1                                          0.14938    0.06380   2.341
    ## Condition2                                          0.07758    0.07463   1.040
    ## GenderRatingCentered                                0.76493    0.04662  16.407
    ## SubjGenderMaleNM_M                                 -0.20190    0.06077  -3.322
    ## Condition1:GenderRatingCentered                     0.09613    0.03622   2.654
    ## Condition2:GenderRatingCentered                    -0.09863    0.04345  -2.270
    ## Condition1:SubjGenderMaleNM_M                      -0.01408    0.12800  -0.110
    ## Condition2:SubjGenderMaleNM_M                      -0.14462    0.14937  -0.968
    ## GenderRatingCentered:SubjGenderMaleNM_M            -0.02007    0.03494  -0.574
    ## Condition1:GenderRatingCentered:SubjGenderMaleNM_M  0.04139    0.07267   0.570
    ## Condition2:GenderRatingCentered:SubjGenderMaleNM_M -0.05276    0.08694  -0.607
    ##                                                    Pr(>|z|)    
    ## (Intercept)                                        0.002528 ** 
    ## Condition1                                         0.019214 *  
    ## Condition2                                         0.298556    
    ## GenderRatingCentered                                < 2e-16 ***
    ## SubjGenderMaleNM_M                                 0.000893 ***
    ## Condition1:GenderRatingCentered                    0.007961 ** 
    ## Condition2:GenderRatingCentered                    0.023222 *  
    ## Condition1:SubjGenderMaleNM_M                      0.912408    
    ## Condition2:SubjGenderMaleNM_M                      0.332962    
    ## GenderRatingCentered:SubjGenderMaleNM_M            0.565754    
    ## Condition1:GenderRatingCentered:SubjGenderMaleNM_M 0.568932    
    ## Condition2:GenderRatingCentered:SubjGenderMaleNM_M 0.543950    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC SGMNM_ Cn1:GRC Cn2:GRC C1:SGM C2:SGM
    ## Condition1   0.009                                                          
    ## Condition2  -0.007 -0.007                                                   
    ## GndrRtngCnt -0.029  0.007  0.012                                            
    ## SbjGndMNM_M -0.012  0.041 -0.042 -0.018                                     
    ## Cndtn1:GnRC  0.005 -0.122  0.017  0.025 -0.017                              
    ## Cndtn2:GnRC  0.012  0.017 -0.104 -0.024  0.000 -0.035                       
    ## Cn1:SGMNM_M  0.014 -0.066 -0.028 -0.002  0.019 -0.019   0.000               
    ## Cn2:SGMNM_M -0.014 -0.031 -0.005 -0.002 -0.011  0.000  -0.041  -0.007       
    ## GRC:SGMNM_M -0.015 -0.018  0.001 -0.013 -0.112  0.060  -0.052   0.009  0.024
    ## C1:GRC:SGMN -0.004 -0.020  0.000  0.016  0.010 -0.086  -0.036  -0.123  0.018
    ## C2:GRC:SGMN -0.001  0.000 -0.041 -0.018  0.024 -0.038   0.002   0.017 -0.103
    ##             GRC:SG C1:GRC:
    ## Condition1                
    ## Condition2                
    ## GndrRtngCnt               
    ## SbjGndMNM_M               
    ## Cndtn1:GnRC               
    ## Cndtn2:GnRC               
    ## Cn1:SGMNM_M               
    ## Cn2:SGMNM_M               
    ## GRC:SGMNM_M               
    ## C1:GRC:SGMN  0.056        
    ## C2:GRC:SGMN -0.050 -0.034

-   Male participants less likely to recall character as female than
    non-male participants overall.

-   No other interactions with participant gender significant.
