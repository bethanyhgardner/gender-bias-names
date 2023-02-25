Experiment 3: Supplementary Analyses
================
2023-02-24

- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#quadratic-name-gender-rating"
  id="toc-quadratic-name-gender-rating">Quadratic Name Gender Rating</a>
  - <a href="#model" id="toc-model">Model</a>
  - <a href="#main-quadratic-effect" id="toc-main-quadratic-effect">Main
    quadratic effect</a>
  - <a href="#quadratic-interaction"
    id="toc-quadratic-interaction">Quadratic interaction</a>
- <a href="#participant-gender" id="toc-participant-gender">Participant
  Gender</a>
  - <a href="#setupdata-summary" id="toc-setupdata-summary">Setup/Data
    Summary</a>
  - <a href="#model-1" id="toc-model-1">Model</a>
- <a href="#gender-rating-centering"
  id="toc-gender-rating-centering">Gender Rating Centering</a>

# Setup

Variable names:

- Experiment: exp3\_
- Data (\_d\_)
  - d = main df
- Models (\_m\_)
  - FF = dummy coded with First + Full Name conditions as 0, Last Name
    condition as 1
  - L = dummy coded with Last Name condition as 0, First + Full Name
    conditions as 1
  - quad = quadratic effect of Name Gender
  - subjGender = participant gender
  - recenter= center name gender rating by scale (at 4)
- Plots (\_p\_)

Load data and select columns used in model. See data/exp3_data_about.txt
for more details.

``` r
exp3_d <- read.csv("../data/exp3_data.csv", stringsAsFactors = TRUE) %>%
  rename("Participant" = "SubjID", "Item" = "Name") %>%
  select(Participant, SubjGenderMale, Condition, 
         GenderRating, Item, He, She, Other)

str(exp3_d)
```

    ## 'data.frame':    8904 obs. of  8 variables:
    ##  $ Participant   : Factor w/ 1272 levels "Exp3_P1","Exp3_P10",..: 974 974 974 974 974 974 974 330 330 330 ...
    ##  $ SubjGenderMale: int  1 1 1 1 1 1 1 0 0 0 ...
    ##  $ Condition     : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating  : num  5.22 1.24 5.86 3.75 6.78 4.34 2.41 6.24 2.61 6.82 ...
    ##  $ Item          : Factor w/ 63 levels "Ashley Cook",..: 6 9 13 43 47 52 62 2 16 20 ...
    ##  $ He            : int  0 1 0 0 0 0 1 0 1 0 ...
    ##  $ She           : int  0 0 1 0 1 1 0 0 0 1 ...
    ##  $ Other         : int  1 0 0 1 0 0 0 1 0 0 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp3_d %<>% mutate(GenderRatingCentered =
            scale(GenderRating, scale = FALSE))
```

Set contrasts for name conditions. This uses Scott Fraundorf’s function
for weighted contrasts. (The psycholing package version doesn’t support
doing 2v1 comparisons, only 1v1.) Condition1 is Last vs First+Full.
Condition2 is First vs Full.

``` r
source("centerfactor.R") 
contrasts(exp3_d$Condition) <- centerfactor(
  exp3_d$Condition, c("last","first"))
contrasts(exp3_d$Condition)
```

    ##             [,1]        [,2]
    ## first  0.4009434 -0.48113208
    ## full   0.4009434  0.51886792
    ## last  -0.5990566  0.01886792

# Quadratic Name Gender Rating

The second supplementary analysis tested the quadratic effect of name
gender rating, such that larger values meant names with stronger gender
associations (masc or fem), and smaller values meant names with weaker
gender associations.

``` r
exp3_d %<>% mutate(GenderRatingSquared = GenderRatingCentered^2)
```

## Model

Quadratic name gender effect on the likelihood of *she* responses, as
opposed to *he* and *other* responses. The maximal model includes random
intercepts by item, but not by participant.

``` r
exp3_m_quad <- buildmer(
  formula = She ~ Condition*GenderRatingCentered +
            Condition*GenderRatingSquared +
            (1|Participant) + (1|Item), 
  data = exp3_d, family = binomial,
  buildmerControl(direction = "order", quiet = TRUE))
summary(exp3_m_quad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ 1 + GenderRatingCentered + GenderRatingSquared + Condition +  
    ##     GenderRatingCentered:Condition + GenderRatingSquared:Condition +  
    ##     (1 | Item)
    ##    Data: exp3_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7979.5   8050.4  -3979.7   7959.5     8894 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1120 -0.5443 -0.1467  0.6532 15.2267 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Item   (Intercept) 0.3002   0.5479  
    ## Number of obs: 8904, groups:  Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error  z value Pr(>|z|) Pr(>|t|)
    ## (Intercept)                     -1.09643    0.11101 -9.87643    0.000  < 2e-16
    ## GenderRatingCentered             1.06982    0.05554 19.26236    0.000  < 2e-16
    ## GenderRatingSquared             -0.11378    0.03102 -3.66732    0.000 0.000245
    ## Condition1                       0.23784    0.07935  2.99748    0.003 0.002722
    ## Condition2                       0.05570    0.09965  0.55893    0.576 0.576208
    ## GenderRatingCentered:Condition1  0.22179    0.06115  3.62713    0.000 0.000287
    ## GenderRatingCentered:Condition2 -0.11288    0.08816 -1.28044    0.200 0.200391
    ## GenderRatingSquared:Condition1  -0.09635    0.02976 -3.23767    0.001 0.001205
    ## GenderRatingSquared:Condition2   0.03866    0.04184  0.92410    0.355 0.355432
    ##                                    
    ## (Intercept)                     ***
    ## GenderRatingCentered            ***
    ## GenderRatingSquared             ***
    ## Condition1                      ** 
    ## Condition2                         
    ## GenderRatingCentered:Condition1 ***
    ## GenderRatingCentered:Condition2    
    ## GenderRatingSquared:Condition1  ** 
    ## GenderRatingSquared:Condition2     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC GndrRS Cndtn1 Cndtn2 GRC:C1 GRC:C2 GRS:C1
    ## GndrRtngCnt -0.093                                                 
    ## GndrRtngSqr -0.638 -0.252                                          
    ## Condition1  -0.005 -0.007  0.007                                   
    ## Condition2  -0.007  0.014 -0.005  0.029                            
    ## GndrRtnC:C1  0.021  0.212 -0.189 -0.143  0.018                     
    ## GndrRtnC:C2  0.001 -0.099  0.078  0.018 -0.146 -0.090              
    ## GndrRtnS:C1 -0.018 -0.191  0.175 -0.424 -0.032 -0.657  0.078       
    ## GndrRtnS:C2  0.003  0.086 -0.073 -0.031 -0.361  0.081 -0.723 -0.053

## Main quadratic effect

To make this easier to understand, plot the data converted to log odds.
This includes just what the model is testing: *she* responses, no
effects of Condition included yet.

``` r
exp3_p_log <- exp3_d %>% 
  group_by(GenderRatingCentered, Item) %>%
  summarise(She.Mean   = mean(She)) %>%
  mutate(She.Log = log(She.Mean)) %>%
  ggplot(aes(x = GenderRatingCentered, y = She.Log)) +
  geom_smooth(fill="red", color ="red") +
  geom_point(fill="red", color ="red") +
  theme_classic() +
  labs(title = "Experiment 3: Log Odds of *She* Responses", 
       x     = "Masculine - Feminine", 
       y     = "Log Odds (Item Means)") +
  theme(text = element_text(size = 16),
        plot.title = element_markdown()) 
exp3_p_log
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](exp3_analysis_supplementary_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

At the masculine end of the scale, *she* responses decrease more
linearly. At the feminine end of the scale, *she* responses level off at
around 5.5 (mostly feminine), then don’t ever reach 0. Fewer *she*
responses in 6-7 range than *he* responses in 1-2 range.

## Quadratic interaction

Now, plot the comparison for the Last vs First+Full condition
interaction.

``` r
exp3_p_quadCond <- exp3_d %>% 
  mutate(Condition_Model = case_when(
    Condition == "first" ~ "First + Full",
    Condition == "full"  ~ "First + Full",
    Condition == "last"  ~ "Last")) %>%
  group_by(Condition_Model, Item, GenderRatingCentered) %>%
  summarise(She.Mean = mean(She)) %>%
  mutate(She.Log = log(She.Mean)) %>%
  ggplot(aes(x = GenderRatingCentered, y = She.Log)) +
  geom_smooth(fill="red", color ="red") +
  geom_point(fill="red", color ="red") +
  theme_classic() +
  labs(title = "Experiment 3: Log Odds of *She* Responses", 
       x = "Masculine - Feminine", 
       y = "Log Odds (Item Means)") +
  theme(text = element_text(size = 16),
        plot.title = element_markdown()) 
exp3_p_quadCond
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](exp3_analysis_supplementary_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Dummy code to get the quadratic effect just for First and Full Name
conditions.

``` r
exp3_d %<>% mutate(Condition_FF = case_when(
  Condition == "first" ~ 0,
  Condition == "full"  ~ 0,
  Condition == "last"  ~ 1))
exp3_d$Condition_FF %<>% as.factor()

exp3_m_FF_quad <- glmer(
  She ~ 1 + GenderRatingCentered + GenderRatingSquared + 
    Condition_FF + GenderRatingCentered:Condition_FF + 
    GenderRatingSquared:Condition_FF + (1|Participant) + (1|Item), 
  data = exp3_d, family = binomial)

summary(exp3_m_FF_quad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ 1 + GenderRatingCentered + GenderRatingSquared + Condition_FF +  
    ##     GenderRatingCentered:Condition_FF + GenderRatingSquared:Condition_FF +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: exp3_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7806.7   7863.5  -3895.4   7790.7     8896 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9048 -0.4902 -0.1312  0.5281 18.6250 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.7876   0.8875  
    ##  Item        (Intercept) 0.3740   0.6116  
    ## Number of obs: 8904, groups:  Participant, 1272; Item, 63
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                        -1.15152    0.13250  -8.691  < 2e-16 ***
    ## GenderRatingCentered                1.28256    0.07173  17.881  < 2e-16 ***
    ## GenderRatingSquared                -0.15083    0.03826  -3.942 8.09e-05 ***
    ## Condition_FF1                      -0.27707    0.09986  -2.774 0.005529 ** 
    ## GenderRatingCentered:Condition_FF1 -0.24465    0.06485  -3.773 0.000162 ***
    ## GenderRatingSquared:Condition_FF1   0.10652    0.03171   3.359 0.000782 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC GndrRS Cn_FF1 GRC:C_
    ## GndrRtngCnt -0.101                            
    ## GndrRtngSqr -0.575 -0.359                     
    ## Conditn_FF1 -0.294  0.039  0.124              
    ## GndRC:C_FF1  0.018 -0.549  0.385 -0.122       
    ## GndRS:C_FF1  0.131  0.403 -0.494 -0.378 -0.641

Dummy code to get the quadratic effect just for Last Name condition.

``` r
exp3_d %<>% mutate(Condition_Last = case_when(
  Condition == "first" ~ 1,
  Condition == "full"  ~ 1,
  Condition == "last"  ~ 0))
exp3_d$Condition_Last %<>% as.factor()

exp3_m_L_quad <- glmer(
  She ~ 1 + GenderRatingCentered + GenderRatingSquared + 
    Condition_Last + GenderRatingCentered:Condition_Last + 
    GenderRatingSquared:Condition_Last + (1|Participant) + (1|Item), 
  data = exp3_d, family = binomial)
summary(exp3_m_L_quad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## She ~ 1 + GenderRatingCentered + GenderRatingSquared + Condition_Last +  
    ##     GenderRatingCentered:Condition_Last + GenderRatingSquared:Condition_Last +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: exp3_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7806.7   7863.5  -3895.4   7790.7     8896 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9048 -0.4902 -0.1312  0.5281 18.6243 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.7876   0.8875  
    ##  Item        (Intercept) 0.3740   0.6116  
    ## Number of obs: 8904, groups:  Participant, 1272; Item, 63
    ## 
    ## Fixed effects:
    ##                                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -1.42860    0.14054 -10.165  < 2e-16 ***
    ## GenderRatingCentered                  1.03790    0.06514  15.933  < 2e-16 ***
    ## GenderRatingSquared                  -0.04431    0.03565  -1.243 0.213896    
    ## Condition_Last1                       0.27707    0.09987   2.774 0.005531 ** 
    ## GenderRatingCentered:Condition_Last1  0.24465    0.06485   3.773 0.000162 ***
    ## GenderRatingSquared:Condition_Last1  -0.10651    0.03171  -3.359 0.000782 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC GndrRS Cnd_L1 GRC:C_
    ## GndrRtngCnt -0.144                            
    ## GndrRtngSqr -0.616 -0.186                     
    ## Condtn_Lst1 -0.434  0.078  0.204              
    ## GndrRC:C_L1  0.070 -0.391  0.157 -0.122       
    ## GndrRS:C_L1  0.145  0.194 -0.359 -0.378 -0.641

``` r
exp3_m_FF_quad %>% tidy() %>%
  filter(term == "GenderRatingSquared") %>% pull(estimate)
```

    ## [1] -0.1508263

``` r
exp3_m_L_quad %>% tidy() %>%
  filter(term == "GenderRatingSquared") %>% pull(estimate)
```

    ## [1] -0.04430599

- Beta for quadratic gender rating in First + Full: -0.15\*\*\*

- Beta for quadratic gender rating in Last: -0.05508 .

# Participant Gender

## Setup/Data Summary

The third supplementary analysis looks at participant gender: if male
participants show a larger bias towards *he* responses than non-male
participants.

Participants entered their gender in a free-response box.

``` r
exp3_d %>% group_by(SubjGenderMale) %>% 
  summarise(total = n_distinct(Participant)) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:right;">
SubjGenderMale
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
642
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
514
</td>
</tr>
<tr>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
116
</td>
</tr>
</tbody>
</table>

For this analysis, we exclude participants who did not respond (N=116)..
Because there are not enough participants to create 3 groups, we compare
male to non-male participants. Male participants (N=514) are coded as 1
and female (N=638), nonbinary (N=2), agender (N=1), and asexual (N=1)
participants are coded as 0.

Summary of responses by condition and participant gender:

``` r
exp3_d_subjGender <- exp3_d %>% filter(!is.na(SubjGenderMale))
exp3_d_subjGender %<>% mutate(ResponseAll = case_when(
  He    == 1 ~ "He",
  She   == 1 ~ "She", 
  Other == 1 ~ "Other"))
```

Participant gender is mean centered effects coded, comparing non-male
participants to male participants.

``` r
exp3_d_subjGender$SubjGenderMale %<>% as.factor()
contrasts(exp3_d_subjGender$SubjGenderMale) = cbind("NM_M"=c(-.5, .5)) 
contrasts(exp3_d_subjGender$SubjGenderMale)
```

    ##   NM_M
    ## 0 -0.5
    ## 1  0.5

## Model

Effects of Name Condition (first name, full name), the first name’s
Gender Rating (centered, positive=more feminine), and Participant Gender
(non-male vs. male) on the likelihood of a *she* response as opposed to
*he* or *other* responses. The maximal model contains random intercepts
by item and by participant.

``` r
exp3_m_subjGender  <- buildmer(
  formula = She ~ Condition * GenderRatingCentered * SubjGenderMale + 
            (1|Participant) + (1|Item), 
  data = exp3_d_subjGender, family = binomial,
  buildmerControl(direction = "order", quiet = TRUE))

summary(exp3_m_subjGender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ 1 + GenderRatingCentered + SubjGenderMale + Condition +  
    ##     GenderRatingCentered:Condition + SubjGenderMale:Condition +  
    ##     GenderRatingCentered:SubjGenderMale + GenderRatingCentered:SubjGenderMale:Condition +  
    ##     (1 | Item) + (1 | Participant)
    ##    Data: exp3_d_subjGender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7061.6   7159.6  -3516.8   7033.6     8078 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0837 -0.4716 -0.1392  0.5318  9.8645 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.7527   0.8676  
    ##  Item        (Intercept) 0.4478   0.6692  
    ## Number of obs: 8092, groups:  Participant, 1156; Item, 63
    ## 
    ## Fixed effects:
    ##                                                     Estimate Std. Error
    ## (Intercept)                                         -1.58004    0.10544
    ## GenderRatingCentered                                 1.14763    0.06289
    ## SubjGenderMaleNM_M                                  -0.33908    0.09599
    ## Condition1                                           0.19527    0.09835
    ## Condition2                                           0.13618    0.12205
    ## GenderRatingCentered:Condition1                      0.11311    0.05254
    ## GenderRatingCentered:Condition2                     -0.07906    0.06655
    ## SubjGenderMaleNM_M:Condition1                        0.12026    0.19712
    ## SubjGenderMaleNM_M:Condition2                        0.04672    0.24324
    ## GenderRatingCentered:SubjGenderMaleNM_M             -0.01729    0.05160
    ## GenderRatingCentered:SubjGenderMaleNM_M:Condition1   0.09438    0.10525
    ## GenderRatingCentered:SubjGenderMaleNM_M:Condition2  -0.04578    0.13264
    ##                                                      z value Pr(>|z|) Pr(>|t|)
    ## (Intercept)                                        -14.98481    0.000  < 2e-16
    ## GenderRatingCentered                                18.24838    0.000  < 2e-16
    ## SubjGenderMaleNM_M                                  -3.53250    0.000 0.000412
    ## Condition1                                           1.98537    0.047 0.047104
    ## Condition2                                           1.11579    0.265 0.264510
    ## GenderRatingCentered:Condition1                      2.15269    0.031 0.031343
    ## GenderRatingCentered:Condition2                     -1.18805    0.235 0.234816
    ## SubjGenderMaleNM_M:Condition1                        0.61010    0.542 0.541795
    ## SubjGenderMaleNM_M:Condition2                        0.19208    0.848 0.847682
    ## GenderRatingCentered:SubjGenderMaleNM_M             -0.33513    0.738 0.737525
    ## GenderRatingCentered:SubjGenderMaleNM_M:Condition1   0.89671    0.370 0.369876
    ## GenderRatingCentered:SubjGenderMaleNM_M:Condition2  -0.34513    0.730 0.729996
    ##                                                       
    ## (Intercept)                                        ***
    ## GenderRatingCentered                               ***
    ## SubjGenderMaleNM_M                                 ***
    ## Condition1                                         *  
    ## Condition2                                            
    ## GenderRatingCentered:Condition1                    *  
    ## GenderRatingCentered:Condition2                       
    ## SubjGenderMaleNM_M:Condition1                         
    ## SubjGenderMaleNM_M:Condition2                         
    ## GenderRatingCentered:SubjGenderMaleNM_M               
    ## GenderRatingCentered:SubjGenderMaleNM_M:Condition1    
    ## GenderRatingCentered:SubjGenderMaleNM_M:Condition2    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) GndrRC SbGMNM_M Cndtn1 Cndtn2 GRC:C1 GRC:C2 SGMNM_M:C1
    ## GndrRtngCnt    -0.300                                                       
    ## SbjGndMNM_M     0.090 -0.063                                                
    ## Condition1     -0.016  0.005 -0.054                                         
    ## Condition2     -0.032  0.029  0.012    0.002                                
    ## GndrRtnC:C1    -0.001  0.014  0.022   -0.519  0.015                         
    ## GndrRtnC:C2     0.030 -0.042  0.001    0.014 -0.507 -0.015                  
    ## SbGMNM_M:C1    -0.027  0.012 -0.036    0.191  0.017 -0.125 -0.004           
    ## SbGMNM_M:C2     0.005  0.002 -0.047    0.017  0.133 -0.004 -0.100 -0.001    
    ## GnRC:SGMNM_M   -0.062  0.074 -0.517    0.023  0.001 -0.039  0.008  0.019    
    ## GRC:SGMNM_M:C1  0.008 -0.011  0.018   -0.125 -0.003  0.176  0.012 -0.520    
    ## GRC:SGMNM_M:C2  0.003  0.000  0.042   -0.004 -0.100  0.013  0.129  0.015    
    ##                SGMNM_M:C2 GnRC:SGMNM_M GRC:SGMNM_M:C1
    ## GndrRtngCnt                                          
    ## SbjGndMNM_M                                          
    ## Condition1                                           
    ## Condition2                                           
    ## GndrRtnC:C1                                          
    ## GndrRtnC:C2                                          
    ## SbGMNM_M:C1                                          
    ## SbGMNM_M:C2                                          
    ## GnRC:SGMNM_M    0.042                                
    ## GRC:SGMNM_M:C1  0.015     -0.001                     
    ## GRC:SGMNM_M:C2 -0.506     -0.066       -0.016

- Male participants less likely to produce *she* responses overall

- No interactions with participant gender significant

# Gender Rating Centering

The first name gender ratings aren’t perfectly centered, partially
because mostly-feminine/somewhat-masculine names are much less common
than mostly-masculine/somewhat-feminine names.

``` r
mean(exp3_d$GenderRating, na.rm = TRUE)
```

    ## [1] 4.206009

Does it make a difference if we center it on 4, the mean of the scale,
instead of 4.21, the mean of the items?

``` r
exp3_d %<>% mutate(GenderRating4 = GenderRating - 4)
```

``` r
exp3_m_recenter <- glmer(
  She ~ Condition * GenderRating4 + (1|Participant) + (1|Item), 
  exp3_d, family = binomial)
summary(exp3_m_recenter)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition * GenderRating4 + (1 | Participant) + (1 | Item)
    ##    Data: exp3_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7825.8   7882.5  -3904.9   7809.8     8896 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0250 -0.4836 -0.1394  0.5355  9.7282 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.7931   0.8905  
    ##  Item        (Intercept) 0.4209   0.6488  
    ## Number of obs: 8904, groups:  Participant, 1272; Item, 63
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -1.76079    0.10526 -16.728   <2e-16 ***
    ## Condition1                0.13163    0.09692   1.358   0.1744    
    ## Condition2                0.10279    0.12280   0.837   0.4026    
    ## GenderRating4             1.14844    0.06039  19.017   <2e-16 ***
    ## Condition1:GenderRating4  0.10498    0.04875   2.153   0.0313 *  
    ## Condition2:GenderRating4 -0.05627    0.06294  -0.894   0.3713    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrR4 C1:GR4
    ## Condition1   0.001                            
    ## Condition2  -0.017  0.021                     
    ## GenderRtng4 -0.394 -0.006  0.018              
    ## Cndtn1:GnR4 -0.011 -0.572 -0.001  0.025       
    ## Cndtn2:GnR4  0.019 -0.001 -0.566 -0.023  0.009

Here, the beta estimate for the intercept has a larger absolute value
(-1.76 vs -1.52), and the beta estimates for the condition effects is
slightly different (0.13 vs 0.15; 0.10 vs 0.09).
