Experiment 2: Main Analysis
================
2023-03-26

- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#data-summary" id="toc-data-summary">Data Summary</a>
- <a href="#model-1-condition" id="toc-model-1-condition">Model 1:
  Condition</a>
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
- <a href="#model-2-condition--name-gender"
  id="toc-model-2-condition--name-gender">Model 2: Condition * Name
  Gender</a>

# Setup

Variable names:

- Experiment: exp2\_
- Data (\_d\_)
  - d = main df
  - count = sums of response types
  - FF = First + Full Name conditions only
- Models (\_m\_)
  - cond = effect of Condition (Last vs First+Full)
  - nameGender = effects of Condition (First vs Full) and Name Gender
    Rating
  - FF = dummy coded with First + Full Name conditions as 0, Last Name
    condition as 1
  - L = dummy coded with Last Name condition as 0, First + Full Name
    conditions as 1

Load data and select columns used in model. See data/exp2_data_about.txt
for more details.

``` r
exp2_d <- read.csv("../data/exp2_data.csv",
                   stringsAsFactors = TRUE) %>%
  rename("Participant" = "SubjID", "Item" = "NameShown") %>%
  select(
    Participant, SubjGenderMale,
    Condition, GenderRating,
    Item, Male, Female, Other
  )
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
exp2_d %<>% mutate(GenderRatingCentered = scale(GenderRating, scale = FALSE))
```

Set contrasts for name conditions.

``` r
contrasts(exp2_d$Condition) <- cbind(
  "last vs first/full" = c(.33, .33, -0.66),
  "first vs full"      = c(-.5, .5, 0)
)
contrasts(exp2_d$Condition)
```

    ##       last vs first/full first vs full
    ## first               0.33          -0.5
    ## full                0.33           0.5
    ## last               -0.66           0.0

Subset for gender rating effects (First and Full conditions only).

``` r
exp2_d_FF <- exp2_d %>% filter(Condition != "last")
exp2_d_FF$Condition %<>% droplevels()

contrasts(exp2_d_FF$Condition) <- cbind(
  "first vs full" = c(-.5, .5)
) # add contrast back
contrasts(exp2_d_FF$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

# Data Summary

Responses by condition.

``` r
exp2_d %<>% mutate(ResponseAll = case_when(
  Male   == 1 ~ "Male",
  Female == 1 ~ "Female",
  Other  == 1 ~ "Other"
))

exp2_d_count <- exp2_d %>%
  group_by(Condition, ResponseAll) %>%
  summarise(n = n()) %>%
  pivot_wider(
    names_from = ResponseAll,
    values_from = n
  ) %>%
  mutate(
    Female_MaleOther = Female / (Male + Other),
    Female_Male = Female / Male
  )

kable(exp2_d_count, digits = 3)
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
1579
</td>
<td style="text-align:right;">
1543
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
1.001
</td>
<td style="text-align:right;">
1.023
</td>
</tr>
<tr>
<td style="text-align:left;">
full
</td>
<td style="text-align:right;">
1446
</td>
<td style="text-align:right;">
1633
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
0.842
</td>
<td style="text-align:right;">
0.885
</td>
</tr>
<tr>
<td style="text-align:left;">
last
</td>
<td style="text-align:right;">
406
</td>
<td style="text-align:right;">
2498
</td>
<td style="text-align:right;">
232
</td>
<td style="text-align:right;">
0.149
</td>
<td style="text-align:right;">
0.163
</td>
</tr>
</tbody>
</table>

- First name condition has most *female* responses
- Full name condition has second-most *female* responses
- Last name condition has fewest *female* responses

# Model 1: Condition

Effect of Condition (first name, last name, full name) on likelihood of
a *female* response, as opposed to a *male* or *other* response.
Participant and Item are included as random intercepts, with items
defined as the unique first, last and first + last name combinations.
Because the condition manipulations were fully between-subject and
between-item, fitting a random slope model was not possible.

``` r
exp2_m_cond <- glmer(
  Female ~ Condition + (1 | Participant) + (1 | Item),
  data = exp2_d, family = binomial
)
summary(exp2_m_cond)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9181.9   9217.7  -4585.9   9171.9     9452 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9521 -0.4649 -0.2948  0.5485  4.8434 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.196    0.4427  
    ##  Item        (Intercept) 1.782    1.3351  
    ## Number of obs: 9457, groups:  Participant, 1351; Item, 105
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -0.8608     0.1508  -5.710 1.13e-08 ***
    ## Conditionlast vs first/full   2.0002     0.3429   5.833 5.44e-09 ***
    ## Conditionfirst vs full       -0.2304     0.3449  -0.668    0.504    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/
    ## Cndtnvfrst/ -0.170       
    ## Cndtnfrstvf -0.360 -0.241

- Less likely overall to recall character as female.

- Less likely to recall character as female in the Last Name condition
  as compared to the First and Full Name conditions.

## Odds Ratios: Intercept

``` r
exp(get_intercept(exp2_m_cond))
```

    ## [1] 0.4228053

``` r
exp(-get_intercept(exp2_m_cond))
```

    ## [1] 2.365155

0.42x less likely to recall as female overall (or: 2.37x more likely to
recall as male/other overall), p\<.001

## Odds Ratios: Last vs First+Full

``` r
exp2_m_cond %>%
  tidy() %>%
  filter(term == "Conditionlast vs first/full") %>%
  pull(estimate) %>%
  exp()
```

    ## [1] 7.390569

7.39x more likely to recall as female in First + Full compared to Last
(or: 7.39 more likely to recall as male in Last than in First + Full),
p\<.001

## Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp2_d %<>% mutate(Condition_Last = case_when(
  Condition == "first" ~ 1,
  Condition == "full"  ~ 1,
  Condition == "last"  ~ 0
))
exp2_d$Condition_Last %<>% as.factor()
```

``` r
exp2_m_L <- glmer(
  Female ~ Condition_Last + (1 | Participant) + (1 | Item),
  data = exp2_d, family = binomial
)
summary(exp2_m_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9180.3   9209.0  -4586.2   9172.3     9453 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9615 -0.4649 -0.2948  0.5475  4.8790 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.196    0.4427  
    ##  Item        (Intercept) 1.789    1.3376  
    ## Number of obs: 9457, groups:  Participant, 1351; Item, 105
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -2.1811     0.2927  -7.450 9.32e-14 ***
    ## Condition_Last1   1.9257     0.3298   5.839 5.24e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.887

``` r
exp(get_intercept(exp2_m_L))
```

    ## [1] 0.1129223

``` r
exp(-get_intercept(exp2_m_L))
```

    ## [1] 8.855648

0.11x times less likely to recall as female in the Last Name condition
(or: 8.86x more likely to recall as male in the Last Name condition),
p\<.001

## Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp2_d %<>% mutate(Condition_FF = case_when(
  Condition == "first" ~ 0,
  Condition == "full"  ~ 0,
  Condition == "last"  ~ 1
))
exp2_d$Condition_FF %<>% as.factor()
```

``` r
exp2_m_FF <- glmer(
  Female ~ Condition_FF + (1 | Participant) + (1 | Item),
  data = exp2_d, family = binomial
)
summary(exp2_m_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9180.3   9209.0  -4586.2   9172.3     9453 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9615 -0.4649 -0.2948  0.5475  4.8790 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.196    0.4427  
    ##  Item        (Intercept) 1.789    1.3376  
    ## Number of obs: 9457, groups:  Participant, 1351; Item, 105
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.2554     0.1522  -1.678   0.0933 .  
    ## Condition_FF1  -1.9257     0.3299  -5.837 5.32e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.460

``` r
exp(get_intercept(exp2_m_FF))
```

    ## [1] 0.7746395

``` r
exp(-get_intercept(exp2_m_FF))
```

    ## [1] 1.290923

0.77x times less likely to recall characters as female in the First and
Full Name conditions (or: 1.29x more likely to use recall characters as
male in the First and Full Name conditions), p=.07

# Model 2: Condition \* Name Gender

Effects of Condition (first name, full name) and the first name’s Gender
Rating (centered, positive=more feminine) on the likelihood of a
*female* response, as opposed to a *male* or *other* response. In
Experiment 2, the Last Name condition does not include any instances of
the gendered first name, so it is not included here. Participant and
Item are again included as random intercepts.

``` r
exp2_m_nameGender <- glmer(
  Female ~ Condition * GenderRatingCentered + (1 | Participant) + (1 | Item),
  data = exp2_d_FF, family = binomial
)
summary(exp2_m_nameGender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp2_d_FF
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6775.6   6816.1  -3381.8   6763.6     6315 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9386 -0.6285 -0.2404  0.6240  4.3728 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1137   0.3372  
    ##  Item        (Intercept) 0.1414   0.3760  
    ## Number of obs: 6321, groups:  Participant, 903; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.17570    0.05858  -2.999
    ## Conditionfirst vs full                      -0.22313    0.11701  -1.907
    ## GenderRatingCentered                         0.78322    0.03506  22.338
    ## Conditionfirst vs full:GenderRatingCentered -0.06596    0.06862  -0.961
    ##                                             Pr(>|z|)    
    ## (Intercept)                                  0.00271 ** 
    ## Conditionfirst vs full                       0.05654 .  
    ## GenderRatingCentered                         < 2e-16 ***
    ## Conditionfirst vs full:GenderRatingCentered  0.33644    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC
    ## Cndtnfrstvf -0.338              
    ## GndrRtngCnt -0.064 -0.014       
    ## Cvfll:GndRC -0.011 -0.057 -0.297

- Less likely overall to recall character as female in the First and
  Full Name conditions.

- Somewhat more likely to recall the character as female in the First
  Name condition as compared to the Full Name condition (trending).

- More likely to recall character as female as first name becomes more
  feminine.

- No interaction between name condition and first name gender rating.
