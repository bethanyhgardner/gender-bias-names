Experiment 1: Main Analyses
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

- Experiment: exp1\_
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

Load data and select columns used in model. See data/exp1_data_about.txt
for more details.

``` r
exp1_d <- read.csv("../data/exp1_data.csv",
                   stringsAsFactors = TRUE) %>%
  rename("Participant" = "SubjID", "Item" = "NameShown") %>%
  select(
    Participant, SubjGenderMale, Condition, GenderRating,
    Item, He, She, Other
  )
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
exp1_d %<>% mutate(GenderRatingCentered = scale(GenderRating, scale = FALSE))
```

Set contrasts for name conditions.

``` r
contrasts(exp1_d$Condition) <- cbind(
  "last vs first/full" = c(.33, .33, -0.66),
  "first vs full"      = c(-.5, .5, 0)
)
contrasts(exp1_d$Condition)
```

    ##       last vs first/full first vs full
    ## first               0.33          -0.5
    ## full                0.33           0.5
    ## last               -0.66           0.0

Subset for gender rating effects (First and Full conditions only).

``` r
exp1_d_FF <- exp1_d %>% filter(Condition != "last")
exp1_d_FF$Condition %<>% droplevels()

contrasts(exp1_d_FF$Condition) <- cbind(
  "first vs full" = c(-.5, .5)
) # add contrast back
contrasts(exp1_d_FF$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

# Data Summary

Responses by condition.

``` r
exp1_d %<>% mutate(ResponseAll = case_when(
  He    == 1 ~ "He",
  She   == 1 ~ "She",
  Other == 1 ~ "Other"
))

exp1_d_count <- exp1_d %>%
  group_by(Condition, ResponseAll) %>%
  summarise(n = n()) %>%
  pivot_wider(
    names_from = ResponseAll,
    values_from = n
  ) %>%
  mutate(
    She_HeOther = She / (He + Other),
    She_He = She / He
  ) %>%
  select(She, He, Other, She_HeOther, She_He)
```

    ## Adding missing grouping variables: `Condition`

``` r
kable(exp1_d_count, digits = 3, align = "c")
```

<table>
<thead>
<tr>
<th style="text-align:center;">
Condition
</th>
<th style="text-align:center;">
She
</th>
<th style="text-align:center;">
He
</th>
<th style="text-align:center;">
Other
</th>
<th style="text-align:center;">
She_HeOther
</th>
<th style="text-align:center;">
She_He
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
first
</td>
<td style="text-align:center;">
1395
</td>
<td style="text-align:center;">
1572
</td>
<td style="text-align:center;">
225
</td>
<td style="text-align:center;">
0.776
</td>
<td style="text-align:center;">
0.887
</td>
</tr>
<tr>
<td style="text-align:center;">
full
</td>
<td style="text-align:center;">
1535
</td>
<td style="text-align:center;">
1514
</td>
<td style="text-align:center;">
131
</td>
<td style="text-align:center;">
0.933
</td>
<td style="text-align:center;">
1.014
</td>
</tr>
<tr>
<td style="text-align:center;">
last
</td>
<td style="text-align:center;">
251
</td>
<td style="text-align:center;">
2616
</td>
<td style="text-align:center;">
325
</td>
<td style="text-align:center;">
0.085
</td>
<td style="text-align:center;">
0.096
</td>
</tr>
</tbody>
</table>

- First name condition has second-most *she* responses
- Full name condition has most *she* responses
- Last name condition has fewest *she* responses

# Model 1: Condition

Effect of Condition (first name, last name, full name) on likelihood of
a *she* response, as opposed to a *he* or *other* response. Participant
and Item are included as random intercepts, with items defined as the
unique first, last and first + last name combinations. Because the
condition manipulations were fully between-subject and between-item,
fitting a random slope model was not possible.

``` r
exp1_m_cond <- glmer(
  She ~ Condition + (1 | Participant) + (1 | Item),
  data = exp1_d, family = binomial
)
summary(exp1_m_cond)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: exp1_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6406.5   6442.3  -3198.2   6396.5     9559 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -8.9618 -0.3029 -0.1438  0.2164 10.0122 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 1.029    1.014   
    ##  Item        (Intercept) 7.234    2.690   
    ## Number of obs: 9564, groups:  Participant, 457; Item, 104
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -1.4284     0.3074  -4.647 3.38e-06 ***
    ## Conditionlast vs first/full   2.8241     0.7003   4.033 5.51e-05 ***
    ## Conditionfirst vs full        0.6197     0.6998   0.886    0.376    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/
    ## Cndtnvfrst/ -0.180       
    ## Cndtnfrstvf -0.361 -0.238

- Fewer *she* responses overall

- First+Full have more *she* responses than Last. Full has more *she*
  responses than First (n.s. but matches ratios).

## Odds Ratios: Intercept

``` r
exp(get_intercept(exp1_m_cond))
```

    ## [1] 0.2396997

``` r
exp(-get_intercept(exp1_m_cond))
```

    ## [1] 4.171887

0.24x less likely to use to use *she* overall (or: 4.17x more likely to
use *he* or *other* overall), p\<.001

## Odds Ratios: Last vs First+Full

``` r
exp1_m_cond %>%
  tidy() %>%
  filter(term == "Conditionlast vs first/full") %>%
  pull(estimate) %>%
  exp()
```

    ## [1] 16.84624

16.85x more likely to use *she* in First + Full compared to Last (or:
16.85 times more likely to use *he* and *other* in Last than in First +
Full), p\<.001

## Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp1_d %<>% mutate(Condition_Last = case_when(
  Condition == "first" ~ 1,
  Condition == "full"  ~ 1,
  Condition == "last"  ~ 0
))
exp1_d$Condition_Last %<>% as.factor()
```

``` r
exp1_m_L <- glmer(
  She ~ Condition_Last + (1 | Participant) + (1 | Item),
  data = exp1_d, family = binomial
)
summary(exp1_m_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp1_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6405.2   6433.9  -3198.6   6397.2     9560 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0006 -0.3022 -0.1440  0.2163  9.8408 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 1.030    1.015   
    ##  Item        (Intercept) 7.274    2.697   
    ## Number of obs: 9564, groups:  Participant, 457; Item, 104
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -3.2926     0.6021  -5.469 4.54e-08 ***
    ## Condition_Last1   2.9424     0.6765   4.349 1.37e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.890

``` r
exp(get_intercept(exp1_m_L))
```

    ## [1] 0.03715806

``` r
exp(-get_intercept(exp1_m_L))
```

    ## [1] 26.91206

0.04x times less likely to use *she* in the Last Name condition (or:
26.91x more likely to use *he* and *other* in the Last Name condition),
p\<.001

## Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp1_d %<>% mutate(Condition_FF = case_when(
  Condition == "first" ~ 0,
  Condition == "full"  ~ 0,
  Condition == "last"  ~ 1
))
exp1_d$Condition_FF %<>% as.factor()
```

``` r
exp1_m_FF <- glmer(
  She ~ Condition_FF + (1 | Participant) + (1 | Item),
  data = exp1_d, family = binomial
)
summary(exp1_m_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp1_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6405.2   6433.9  -3198.6   6397.2     9560 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0006 -0.3022 -0.1440  0.2163  9.8409 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 1.030    1.015   
    ##  Item        (Intercept) 7.274    2.697   
    ## Number of obs: 9564, groups:  Participant, 457; Item, 104
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.3502     0.3090  -1.133    0.257    
    ## Condition_FF1  -2.9424     0.6769  -4.347 1.38e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.456

``` r
exp(get_intercept(exp1_m_FF))
```

    ## [1] 0.7045629

``` r
exp(-get_intercept(exp1_m_FF))
```

    ## [1] 1.41932

0.70x times less likely to use *she* in the First and Full Name
conditions (or: 1.42x more likely to use *he* and *other* in the First
and Full Name conditions), p=.26

# Model 2: Condition \* Name Gender

Effects of Condition (first name, full name) and the first name’s Gender
Rating (centered, positive=more feminine) on the likelihood of a *she*
response, as opposed to a *he* or *other* response. In Experiment 1, the
Last Name condition does not include any instances of the gendered first
name, so only the First and Full Name conditions are analyzed here.
Participant and Item are again included as random intercepts.

``` r
exp1_m_nameGender <- glmer(
  She ~ Condition * GenderRatingCentered + (1 | Participant) + (1 | Item),
  exp1_d_FF,
  family = binomial
)
summary(exp1_m_nameGender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp1_d_FF
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4657.4   4698.0  -2322.7   4645.4     6366 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.1567 -0.3548 -0.0551  0.3126 14.3200 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.889    0.9429  
    ##  Item        (Intercept) 0.501    0.7078  
    ## Number of obs: 6372, groups:  Participant, 305; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.51325    0.11987  -4.282
    ## Conditionfirst vs full                       0.53204    0.23994   2.217
    ## GenderRatingCentered                         1.59330    0.07253  21.967
    ## Conditionfirst vs full:GenderRatingCentered -0.17493    0.13917  -1.257
    ##                                             Pr(>|z|)    
    ## (Intercept)                                 1.86e-05 ***
    ## Conditionfirst vs full                        0.0266 *  
    ## GenderRatingCentered                         < 2e-16 ***
    ## Conditionfirst vs full:GenderRatingCentered   0.2088    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC
    ## Cndtnfrstvf -0.346              
    ## GndrRtngCnt -0.179  0.122       
    ## Cvfll:GndRC  0.111 -0.172 -0.409

- More *she* responses as first names become more feminine.

- Difference between First and Full is now significant (as compared to
  condition-only model).
