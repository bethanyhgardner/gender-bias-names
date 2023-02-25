Experiment 4: Main Analyses
================
2023-02-24

- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#data-summary" id="toc-data-summary">Data Summary</a>
- <a href="#main-model" id="toc-main-model">Main Model</a>
  - <a href="#l-v-ff-interaction" id="toc-l-v-ff-interaction">L v F+F
    Interaction</a>
  - <a href="#f-v-f-interaction" id="toc-f-v-f-interaction">F v F
    Interaction</a>
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

# Setup

Variable names:

- Experiment: exp4\_
- Data (\_d\_)
  - d = main df
  - count =sums of response types
- Models (\_m\_)
  - count =sums of response types
  - all = effect of Condition and Name Gender Rating, including *other*
    responses
  - cond = effect of Condition only
  - FF = dummy coded with First + Full Name conditions as 0, Last Name
    condition as 1
  - L = dummy coded with Last Name condition as 0, First + Full Name
    conditions as 1
  - first = dummy coded with First Name condition as 0, Full Name and
    Last Name conditions as 1
  - full = dummy coded with Full Name condition as 0, First Name and
    Last Name conditions as 1

Load data and select columns used in model. See data/exp4_data_about.txt
for more details.

``` r
exp4_d <- read.csv("../data/exp4_data.csv",
                   stringsAsFactors = TRUE) %>%
  rename("Participant" = "SubjID", "Item" = "Name") %>%
  select(Participant, Condition, GenderRating, 
         Item, Male, Female, Other)
str(exp4_d)
```

    ## 'data.frame':    8771 obs. of  7 variables:
    ##  $ Participant : Factor w/ 1253 levels "Exp4_P1","Exp4_P10",..: 520 520 520 520 520 520 520 1143 1143 1143 ...
    ##  $ Condition   : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating: num  6.24 2.61 6.82 5.34 1.28 4.39 3.87 5.22 1.24 5.86 ...
    ##  $ Item        : Factor w/ 63 levels "Ashley Cook",..: 1 18 21 22 25 28 50 5 7 15 ...
    ##  $ Male        : int  0 1 0 0 1 1 1 1 1 0 ...
    ##  $ Female      : int  1 0 1 1 0 0 0 0 0 1 ...
    ##  $ Other       : int  0 0 0 0 0 0 0 0 0 0 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp4_d %<>% mutate(GenderRatingCentered =
             scale(GenderRating, scale = FALSE))
```

Set contrasts for name conditions, now weighted to account for uneven
sample sizes. This uses Scott Fraundorf’s function for weighted
contrasts. (The psycholing package version doesn’t support doing 2v1
comparisons, only 1v1.) Condition1 is Last vs First+Full. Condition2 is
First vs Full.

``` r
source("centerfactor.R")
contrasts(exp4_d$Condition) <- centerfactor(
  exp4_d$Condition, c("last","first"))
contrasts(exp4_d$Condition)
```

    ##             [,1]         [,2]
    ## first  0.3312051 -0.497605746
    ## full   0.3312051  0.502394254
    ## last  -0.6687949  0.002394254

# Data Summary

Responses by condition.

``` r
exp4_d %<>% mutate(ResponseAll = case_when(
  Male   == 1 ~ "Male",
  Female == 1 ~ "Female", 
  Other  == 1 ~ "Other"))

exp4_d_count <- exp4_d %>% 
  group_by(Condition, ResponseAll) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from  = ResponseAll,
              values_from = n) %>%
  mutate(Female_MaleOther = Female / (Male+Other),
         Female_Male      = Female / Male)

kable(exp4_d_count)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Condition
</th>
<th style="text-align:right;">
Female
</th>
<th style="text-align:right;">
Male
</th>
<th style="text-align:right;">
Other
</th>
<th style="text-align:right;">
Female_MaleOther
</th>
<th style="text-align:right;">
Female_Male
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
first
</td>
<td style="text-align:right;">
1381
</td>
<td style="text-align:right;">
1511
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
0.8779402
</td>
<td style="text-align:right;">
0.9139643
</td>
</tr>
<tr>
<td style="text-align:left;">
full
</td>
<td style="text-align:right;">
1380
</td>
<td style="text-align:right;">
1416
</td>
<td style="text-align:right;">
116
</td>
<td style="text-align:right;">
0.9007833
</td>
<td style="text-align:right;">
0.9745763
</td>
</tr>
<tr>
<td style="text-align:left;">
last
</td>
<td style="text-align:right;">
1292
</td>
<td style="text-align:right;">
1529
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
0.8009919
</td>
<td style="text-align:right;">
0.8449967
</td>
</tr>
</tbody>
</table>

- First name condition has second-most (slightly) *female* responses
- Full name condition has most *female* responses
- Last name condition has fewest *female* responses

# Main Model

Because Experiment 4 always introduces the character with a full name,
then manipulates the name form in the subsequent 3 references, the main
analysis is 1 model, as opposed to the 2 for Experiments 1 and 2.

Effects of Name Condition (first name, last name, full name) and first
name Gender Rating (centered, + fem, -masc) on the likelihood of
*female* responses, as opposed to *male* and *other* responses.
Participant and Item are included as random intercepts, with items
defined as the unique first, last and first + last name combinations.
Condition1 is the contrast between last and first+full. Condition2 is
the contrast between first and full.

``` r
exp4_m_all <- glmer(
  Female ~ Condition * GenderRatingCentered + (1|Participant) + (1|Item), 
  data = exp4_d, family = binomial)
summary(exp4_m_all)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9145.4   9202.1  -4564.7   9129.4     8763 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4531 -0.5754 -0.2627  0.5724  5.4529 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2014   0.4488  
    ##  Item        (Intercept) 0.3599   0.5999  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.25606    0.08161  -3.138 0.001703 ** 
    ## Condition1                       0.12636    0.06170   2.048 0.040565 *  
    ## Condition2                       0.06835    0.07245   0.943 0.345471    
    ## GenderRatingCentered             0.76408    0.04590  16.647  < 2e-16 ***
    ## Condition1:GenderRatingCentered  0.13147    0.03451   3.809 0.000139 ***
    ## Condition2:GenderRatingCentered -0.10288    0.04204  -2.447 0.014401 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC C1:GRC
    ## Condition1   0.012                            
    ## Condition2  -0.012 -0.016                     
    ## GndrRtngCnt -0.028  0.002  0.011              
    ## Cndtn1:GnRC  0.001 -0.121  0.016  0.035       
    ## Cndtn2:GnRC  0.011  0.016 -0.112 -0.030 -0.046

- Less likely to recall character as female overall

- More likely to recall character as female in the First and Full Name
  conditions than in the Last Name condition

- More likely to recall character as female as first names become more
  feminine

**Double check the directions of the interactions:**

## L v F+F Interaction

Dummy code to get the gender rating effect for just the First and Full
Name conditions.

``` r
exp4_d %<>% mutate(Condition_FF = case_when(
  Condition == "first" ~ 0,
  Condition == "full"  ~ 0,
  Condition == "last"  ~ 1))
exp4_d$Condition_FF %<>% as.factor()

exp4_m_all_FF <- glmer(
  Female ~ Condition_FF * GenderRatingCentered + 
    (1|Participant) + (1|Item),
  data = exp4_d, family = binomial)
summary(exp4_m_all_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9147.7   9190.2  -4567.8   9135.7     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2676 -0.5751 -0.2669  0.5734  4.9543 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2005   0.4477  
    ##  Item        (Intercept) 0.3603   0.6002  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                        -0.21216    0.08437  -2.515 0.011917 *  
    ## Condition_FF1                      -0.12797    0.06164  -2.076 0.037889 *  
    ## GenderRatingCentered                0.80513    0.04766  16.893  < 2e-16 ***
    ## Condition_FF1:GenderRatingCentered -0.12944    0.03446  -3.756 0.000172 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cn_FF1 GndrRC
    ## Conditn_FF1 -0.253              
    ## GndrRtngCnt -0.032  0.026       
    ## Cnd_FF1:GRC  0.027 -0.120 -0.272

Then dummy code to get the gender rating effect just in the Last Name
condition.

``` r
exp4_d %<>% mutate(Condition_Last = case_when(
  Condition == "first" ~ 1,
  Condition == "full"  ~ 1,
  Condition == "last"  ~ 0))
exp4_d$Condition_Last %<>% as.factor()

exp4_m_all_L <- glmer(
  Female ~ Condition_Last * GenderRatingCentered +
    (1|Participant) + (1|Item),
  data = exp4_d, family = binomial)
summary(exp4_m_all_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9147.7   9190.2  -4567.8   9135.7     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2676 -0.5751 -0.2669  0.5734  4.9543 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2005   0.4477  
    ##  Item        (Intercept) 0.3603   0.6002  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -0.34013    0.09101  -3.737 0.000186 ***
    ## Condition_Last1                       0.12797    0.06164   2.076 0.037896 *  
    ## GenderRatingCentered                  0.67569    0.05066  13.338  < 2e-16 ***
    ## Condition_Last1:GenderRatingCentered  0.12944    0.03446   3.757 0.000172 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cnd_L1 GndrRC
    ## Condtn_Lst1 -0.442              
    ## GndrRtngCnt -0.049  0.057       
    ## Cndt_L1:GRC  0.056 -0.120 -0.425

``` r
exp4_m_all_FF %>% tidy() %>%
  filter(term == "GenderRatingCentered") %>% pull(estimate)
```

    ## [1] 0.8051317

``` r
exp4_m_all_L %>% tidy() %>%
  filter(term == "GenderRatingCentered") %>% pull(estimate)
```

    ## [1] 0.6756906

Interaction indicates Gender Rating has a larger effect in the First and
Full Name conditions (0.81) than in the Last Name condition (0.67). This
makes sense because the gendered first name is repeated all 4x in the
First and Full name conditions, but only once in the Last Name
condition.

## F v F Interaction

Dummy code to get the gender rating effect for just the First Name
condition.

``` r
exp4_d %<>% mutate(Condition_First = case_when(
  Condition == "first" ~ 0,
  Condition == "full"  ~ 1,
  Condition == "last"  ~ 1))
exp4_d$Condition_First %<>% as.factor()

exp4_m_all_first <- glmer(
  Female ~ Condition_First * GenderRatingCentered +
    (1|Participant) + (1|Item),
  data = exp4_d, family = binomial)
summary(exp4_m_all_first)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_First * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9151.6   9194.0  -4569.8   9139.6     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4660 -0.5784 -0.2629  0.5803  5.4716 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2059   0.4538  
    ##  Item        (Intercept) 0.3592   0.5994  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                           -0.24885    0.09227  -2.697  0.00699 ** 
    ## Condition_First1                      -0.01315    0.06303  -0.209  0.83475    
    ## GenderRatingCentered                   0.85944    0.05280  16.277  < 2e-16 ***
    ## Condition_First1:GenderRatingCentered -0.14454    0.03661  -3.948 7.88e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cnd_F1 GndrRC
    ## Cndtn_Frst1 -0.468              
    ## GndrRtngCnt -0.055  0.065       
    ## Cndt_F1:GRC  0.064 -0.125 -0.497

Dummy code to get the gender rating effect for just the Full Name
condition.

``` r
exp4_d %<>% mutate(Condition_Full = case_when(
  Condition == "first" ~ 1,
  Condition == "full"  ~ 0,
  Condition == "last"  ~ 1))
exp4_d$Condition_Full %<>% as.factor()

exp4_m_all_full <- glmer(
  Female ~ Condition_Full * GenderRatingCentered + 
    (1|Participant) + (1|Item),
  data = exp4_d, family = binomial)
summary(exp4_m_all_full)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Full * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9164.2   9206.7  -4576.1   9152.2     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0410 -0.5746 -0.2710  0.5694  4.7906 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2015   0.4489  
    ##  Item        (Intercept) 0.3602   0.6001  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -0.180223   0.091385  -1.972   0.0486 *  
    ## Condition_Full1                      -0.114778   0.062004  -1.851   0.0642 .  
    ## GenderRatingCentered                  0.755873   0.051389  14.709   <2e-16 ***
    ## Condition_Full1:GenderRatingCentered  0.006064   0.035112   0.173   0.8629    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cnd_F1 GndrRC
    ## Condtn_Fll1 -0.450              
    ## GndrRtngCnt -0.035  0.038       
    ## Cndt_F1:GRC  0.038 -0.104 -0.451

``` r
exp4_m_all_first %>% tidy() %>%
  filter(term == "GenderRatingCentered") %>% pull(estimate)
```

    ## [1] 0.8594397

``` r
exp4_m_all_full %>% tidy() %>%
  filter(term == "GenderRatingCentered") %>% pull(estimate)
```

    ## [1] 0.7558726

The effect of name gender rating is larger in the First Name condition
(0.86) than in the Full Name condition (0.76).

## Odds Ratios: Intercept

``` r
exp(get_intercept(exp4_m_all))
```

    ## [1] 0.7740991

``` r
exp(-get_intercept(exp4_m_all))
```

    ## [1] 1.291824

0.77x less likely to recall as female overall (or: 1.29x more likely to
recall as male overall), p\<.01

## Odds Ratios: Last vs First+Full

``` r
exp4_m_all %>% tidy() %>% filter(term == "Condition1") %>%
  pull(estimate) %>% exp()
```

    ## [1] 1.134692

1.13x more likely to recall as female in First + Full compared to Last,
p\<.05

## Odds Ratios: Last Only

Model with just Condition (to more directly compare to Exp 2).

``` r
exp4_m_cond_L <- glmer(
  Female ~ Condition_Last + (1|Participant) + (1|Item), 
  data = exp4_d, family = binomial)
summary(exp4_m_cond_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9265.9   9294.2  -4628.9   9257.9     8767 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0475 -0.5940 -0.2737  0.5750  4.4732 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1961   0.4428  
    ##  Item        (Intercept) 2.2443   1.4981  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)     -0.36280    0.19565  -1.854   0.0637 .
    ## Condition_Last1  0.15844    0.06154   2.574   0.0100 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.211

``` r
exp(get_intercept(exp4_m_cond_L))
```

    ## [1] 0.695726

``` r
exp(-get_intercept(exp4_m_cond_L))
```

    ## [1] 1.437347

0.17x times less likely to recall as female in the Last Name condition
(or: 5.72x more likely to recall as male in the Last Name condition),
p=0.06

## Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions. Model with just Condition (to more directly
compare to Exp 2).

``` r
exp4_m_cond_FF <- glmer(
  Female ~ Condition_FF + (1|Participant) + (1|Item), 
  data = exp4_d, family = binomial)
summary(exp4_m_cond_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9265.9   9294.2  -4628.9   9257.9     8767 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0475 -0.5940 -0.2737  0.5750  4.4732 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1961   0.4428  
    ##  Item        (Intercept) 2.2443   1.4981  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)   -0.20436    0.19231  -1.063    0.288  
    ## Condition_FF1 -0.15843    0.06154  -2.574    0.010 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.105

``` r
exp(get_intercept(exp4_m_cond_FF))
```

    ## [1] 0.8151685

``` r
exp(-get_intercept(exp4_m_cond_FF))
```

    ## [1] 1.22674

0.82x less likely to recall as female in First and Full Name conditions
(or: 1.23x more likely to recall as male in First and Full Name
conditions), p=.29
