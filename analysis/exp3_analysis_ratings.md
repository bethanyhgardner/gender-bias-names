Experiment 3: Ratings Analysis
================
2023-02-24

- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#likeability" id="toc-likeability">Likeability</a>
- <a href="#accomplishment" id="toc-accomplishment">Accomplishment</a>
- <a href="#importance" id="toc-importance">Importance</a>
- <a href="#all-ratings" id="toc-all-ratings">All Ratings</a>

# Setup

Variable names:

- Experiment: exp3\_
- Data (\_d\_)
  - d = main df
- Models (\_m\_)
  - like = Likeability ratings
  - acc = Accomplishment ratings
  - imp = Importance ratings
  - ratings = build model with all 3 ratings

Load data and select columns used in model. See data/exp3_data_about.txt
for more details.

``` r
exp3_d <- read.csv("../data/exp3_data.csv", stringsAsFactors = TRUE) %>%
  rename("Participant" = "SubjID", "Item" = "Name") %>%
  select(Participant, Condition, GenderRating, Item, 
         He, She, Other,
         Likeable, Accomplished, Important)
str(exp3_d)
```

    ## 'data.frame':    8904 obs. of  10 variables:
    ##  $ Participant : Factor w/ 1272 levels "Exp3_P1","Exp3_P10",..: 974 974 974 974 974 974 974 330 330 330 ...
    ##  $ Condition   : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating: num  5.22 1.24 5.86 3.75 6.78 4.34 2.41 6.24 2.61 6.82 ...
    ##  $ Item        : Factor w/ 63 levels "Ashley Cook",..: 6 9 13 43 47 52 62 2 16 20 ...
    ##  $ He          : int  0 1 0 0 0 0 1 0 1 0 ...
    ##  $ She         : int  0 0 1 0 1 1 0 0 0 1 ...
    ##  $ Other       : int  1 0 0 1 0 0 0 1 0 0 ...
    ##  $ Likeable    : int  2 2 2 2 2 1 2 2 1 2 ...
    ##  $ Accomplished: int  2 1 1 2 1 1 2 3 1 1 ...
    ##  $ Important   : int  2 1 2 2 1 1 2 2 1 1 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp3_d %<>% mutate(GenderRatingCentered =
  scale(GenderRating, scale = FALSE))
```

Set contrasts for name conditions, now weighted to account for uneven
sample sizes. This uses Scott Fraundorf’s function for weighted
contrasts. (The psycholing package version doesn’t support doing 2v1
comparisons, only 1v1.) Condition1 is Last vs First+Full. Condition2 is
First vs Full.

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

Flip ratings from 1=most likeable/accomplished/important to 7=most
L/A/I, to make interpreting models easier, then mean-center.

``` r
exp3_d %<>% mutate(
  LikeableFlip = recode(Likeable, 
      '1' = 7, '2' = 6, '3' = 5, '4' = 4, '5' = 3, '6' = 2, '7' = 1),
  AccomplishedFlip = recode(Accomplished,
      '1' = 7, '2' = 6, '3' = 5, '4' = 4, '5' = 3, '6' = 2, '7' = 1),
  ImportantFlip = recode(Important,
      '1' = 7, '2' = 6, '3' = 5, '4' = 4, '5' = 3, '6' = 2, '7' = 1),
  LikeableCentered = scale(LikeableFlip, scale = FALSE),
  AccomplishedCentered = scale(AccomplishedFlip, scale = FALSE),
  ImportantCentered = scale(ImportantFlip, scale = FALSE))
str(exp3_d)
```

    ## 'data.frame':    8904 obs. of  17 variables:
    ##  $ Participant         : Factor w/ 1272 levels "Exp3_P1","Exp3_P10",..: 974 974 974 974 974 974 974 330 330 330 ...
    ##  $ Condition           : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..- attr(*, "contrasts")= num [1:3, 1:2] 0.401 0.401 -0.599 -0.481 0.519 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:3] "first" "full" "last"
    ##   .. .. ..$ : NULL
    ##  $ GenderRating        : num  5.22 1.24 5.86 3.75 6.78 4.34 2.41 6.24 2.61 6.82 ...
    ##  $ Item                : Factor w/ 63 levels "Ashley Cook",..: 6 9 13 43 47 52 62 2 16 20 ...
    ##  $ He                  : int  0 1 0 0 0 0 1 0 1 0 ...
    ##  $ She                 : int  0 0 1 0 1 1 0 0 0 1 ...
    ##  $ Other               : int  1 0 0 1 0 0 0 1 0 0 ...
    ##  $ Likeable            : int  2 2 2 2 2 1 2 2 1 2 ...
    ##  $ Accomplished        : int  2 1 1 2 1 1 2 3 1 1 ...
    ##  $ Important           : int  2 1 2 2 1 1 2 2 1 1 ...
    ##  $ GenderRatingCentered: num [1:8904, 1] 1.014 -2.966 1.654 -0.456 2.574 ...
    ##   ..- attr(*, "scaled:center")= num 4.21
    ##  $ LikeableFlip        : num  6 6 6 6 6 7 6 6 7 6 ...
    ##  $ AccomplishedFlip    : num  6 7 7 6 7 7 6 5 7 7 ...
    ##  $ ImportantFlip       : num  6 7 6 6 7 7 6 6 7 7 ...
    ##  $ LikeableCentered    : num [1:8904, 1] 0.271 0.271 0.271 0.271 0.271 ...
    ##   ..- attr(*, "scaled:center")= num 5.73
    ##  $ AccomplishedCentered: num [1:8904, 1] 0.147 1.147 1.147 0.147 1.147 ...
    ##   ..- attr(*, "scaled:center")= num 5.85
    ##  $ ImportantCentered   : num [1:8904, 1] 0.585 1.585 0.585 0.585 1.585 ...
    ##   ..- attr(*, "scaled:center")= num 5.42

# Likeability

Summary statistics:

``` r
summary(exp3_d$Likeable)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   2.271   3.000   7.000

``` r
summary(exp3_d$LikeableFlip)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   5.000   6.000   5.729   7.000   7.000

``` r
sd(exp3_d$Likeable)
```

    ## [1] 1.316101

Does the Likeability rating of the character predict the likelihood of
*she* as opposed to *he* and *other* responses? The maximal model
includes all interactions, then random intercepts by item but not by
participant.

``` r
exp3_m_like <- buildmer(
  formula = She ~ Condition * GenderRatingCentered * 
            LikeableCentered + (1|Participant) + (1|Item),
  data = exp3_d, family = binomial, 
  buildmerControl(direction = "order", quiet = TRUE))

summary(exp3_m_like)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ 1 + GenderRatingCentered + LikeableCentered + Condition +  
    ##     GenderRatingCentered:Condition + GenderRatingCentered:LikeableCentered +  
    ##     LikeableCentered:Condition + GenderRatingCentered:LikeableCentered:Condition +  
    ##     (1 | Item)
    ##    Data: exp3_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7977.9   8070.1  -3975.9   7951.9     8891 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6061 -0.5430 -0.1465  0.6311 10.0732 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Item   (Intercept) 0.3524   0.5936  
    ## Number of obs: 8904, groups:  Item, 63
    ## 
    ## Fixed effects:
    ##                                                   Estimate Std. Error   z value
    ## (Intercept)                                       -1.37455    0.08926 -15.39978
    ## GenderRatingCentered                               1.03162    0.05487  18.80241
    ## LikeableCentered                                   0.08004    0.02805   2.85361
    ## Condition1                                         0.13853    0.07103   1.95029
    ## Condition2                                         0.06855    0.08957   0.76530
    ## GenderRatingCentered:Condition1                    0.08800    0.04529   1.94319
    ## GenderRatingCentered:Condition2                   -0.06416    0.05848  -1.09717
    ## GenderRatingCentered:LikeableCentered              0.02698    0.01736   1.55386
    ## LikeableCentered:Condition1                       -0.06365    0.05656  -1.12521
    ## LikeableCentered:Condition2                        0.06998    0.06931   1.00957
    ## GenderRatingCentered:LikeableCentered:Condition1   0.06193    0.03485   1.77725
    ## GenderRatingCentered:LikeableCentered:Condition2  -0.03563    0.04395  -0.81076
    ##                                                  Pr(>|z|) Pr(>|t|)    
    ## (Intercept)                                         0.000  < 2e-16 ***
    ## GenderRatingCentered                                0.000  < 2e-16 ***
    ## LikeableCentered                                    0.004  0.00432 ** 
    ## Condition1                                          0.051  0.05114 .  
    ## Condition2                                          0.444  0.44409    
    ## GenderRatingCentered:Condition1                     0.052  0.05199 .  
    ## GenderRatingCentered:Condition2                     0.273  0.27257    
    ## GenderRatingCentered:LikeableCentered               0.120  0.12022    
    ## LikeableCentered:Condition1                         0.261  0.26050    
    ## LikeableCentered:Condition2                         0.313  0.31270    
    ## GenderRatingCentered:LikeableCentered:Condition1    0.076  0.07553 .  
    ## GenderRatingCentered:LikeableCentered:Condition2    0.418  0.41750    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC LkblCn Cndtn1 Cndtn2 GRC:C1 GRC:C2 GnRC:LC LkC:C1
    ## GndrRtngCnt -0.287                                                         
    ## LikeblCntrd -0.005 -0.003                                                  
    ## Condition1   0.000 -0.005  0.051                                           
    ## Condition2  -0.017  0.018 -0.072  0.017                                    
    ## GndrRtnC:C1  0.001  0.016 -0.044 -0.601  0.000                             
    ## GndrRtnC:C2  0.016 -0.023  0.048  0.001 -0.586  0.008                      
    ## GndrRtnC:LC  0.002  0.005 -0.623 -0.044  0.050  0.044 -0.090               
    ## LkblCntr:C1  0.029 -0.030 -0.054 -0.073 -0.055  0.052  0.037  0.044        
    ## LkblCntr:C2 -0.034  0.028  0.077 -0.056 -0.013  0.038  0.000 -0.042   0.094
    ## GndRC:LC:C1 -0.029  0.032  0.044  0.053  0.039 -0.047 -0.071 -0.006  -0.626
    ## GndRC:LC:C2  0.023 -0.042 -0.041  0.038  0.001 -0.073  0.003  0.093  -0.051
    ##             LkC:C2 GRC:LC:C1
    ## GndrRtngCnt                 
    ## LikeblCntrd                 
    ## Condition1                  
    ## Condition2                  
    ## GndrRtnC:C1                 
    ## GndrRtnC:C2                 
    ## GndrRtnC:LC                 
    ## LkblCntr:C1                 
    ## LkblCntr:C2                 
    ## GndRC:LC:C1 -0.052          
    ## GndRC:LC:C2 -0.594  0.108

- Characters who are rated as more Likeable are more likely to be
  referred to with *she*

# Accomplishment

Summary statistics:

``` r
summary(exp3_d$Accomplished)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   2.147   3.000   7.000

``` r
summary(exp3_d$AccomplishedFlip)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   5.000   6.000   5.853   7.000   7.000

``` r
sd(exp3_d$Accomplished)
```

    ## [1] 1.27504

Does the Accomplishment rating of the character predict the likelihood
of *she* as opposed to *he* and *other* responses? The maximal model
includes all interactions, then random intercepts by item but not by
participant.

``` r
exp3_m_acc <- buildmer(
  formula = She ~ Condition * GenderRatingCentered * 
            AccomplishedCentered + (1|Participant) + (1|Item),
  data = exp3_d, family = binomial, 
  buildmerControl(direction = "order", quiet = TRUE))

summary(exp3_m_acc)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ 1 + GenderRatingCentered + AccomplishedCentered + Condition +  
    ##     GenderRatingCentered:Condition + GenderRatingCentered:AccomplishedCentered +  
    ##     AccomplishedCentered:Condition + GenderRatingCentered:AccomplishedCentered:Condition +  
    ##     (1 | Item)
    ##    Data: exp3_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7975.7   8068.0  -3974.9   7949.7     8891 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6466 -0.5469 -0.1450  0.6222 10.8637 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Item   (Intercept) 0.3595   0.5996  
    ## Number of obs: 8904, groups:  Item, 63
    ## 
    ## Fixed effects:
    ##                                                        Estimate Std. Error
    ## (Intercept)                                           -1.372294   0.089919
    ## GenderRatingCentered                                   1.034110   0.055238
    ## AccomplishedCentered                                   0.072453   0.028342
    ## Condition1                                             0.139473   0.070835
    ## Condition2                                             0.073638   0.089326
    ## GenderRatingCentered:Condition1                        0.090036   0.045170
    ## GenderRatingCentered:Condition2                       -0.057030   0.058311
    ## GenderRatingCentered:AccomplishedCentered              0.029731   0.017426
    ## AccomplishedCentered:Condition1                       -0.083766   0.058528
    ## AccomplishedCentered:Condition2                       -0.069941   0.069994
    ## GenderRatingCentered:AccomplishedCentered:Condition1   0.084687   0.035961
    ## GenderRatingCentered:AccomplishedCentered:Condition2   0.002789   0.043500
    ##                                                         z value Pr(>|z|)
    ## (Intercept)                                          -15.261381    0.000
    ## GenderRatingCentered                                  18.720891    0.000
    ## AccomplishedCentered                                   2.556329    0.011
    ## Condition1                                             1.968970    0.049
    ## Condition2                                             0.824375    0.410
    ## GenderRatingCentered:Condition1                        1.993281    0.046
    ## GenderRatingCentered:Condition2                       -0.978038    0.328
    ## GenderRatingCentered:AccomplishedCentered              1.706129    0.088
    ## AccomplishedCentered:Condition1                       -1.431211    0.152
    ## AccomplishedCentered:Condition2                       -0.999249    0.318
    ## GenderRatingCentered:AccomplishedCentered:Condition1   2.354960    0.019
    ## GenderRatingCentered:AccomplishedCentered:Condition2   0.064123    0.949
    ##                                                      Pr(>|t|)    
    ## (Intercept)                                            <2e-16 ***
    ## GenderRatingCentered                                   <2e-16 ***
    ## AccomplishedCentered                                   0.0106 *  
    ## Condition1                                             0.0490 *  
    ## Condition2                                             0.4097    
    ## GenderRatingCentered:Condition1                        0.0462 *  
    ## GenderRatingCentered:Condition2                        0.3281    
    ## GenderRatingCentered:AccomplishedCentered              0.0880 .  
    ## AccomplishedCentered:Condition1                        0.1524    
    ## AccomplishedCentered:Condition2                        0.3177    
    ## GenderRatingCentered:AccomplishedCentered:Condition1   0.0185 *  
    ## GenderRatingCentered:AccomplishedCentered:Condition2   0.9489    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC AccmpC Cndtn1 Cndtn2 GRC:C1 GRC:C2 GnRC:AC AcC:C1
    ## GndrRtngCnt -0.284                                                         
    ## AccmplshdCn -0.006  0.003                                                  
    ## Condition1   0.001 -0.006  0.060                                           
    ## Condition2  -0.017  0.018 -0.013  0.015                                    
    ## GndrRtnC:C1 -0.001  0.018 -0.054 -0.601  0.001                             
    ## GndrRtnC:C2  0.016 -0.023  0.002  0.001 -0.586  0.005                      
    ## GndrRtnC:AC  0.006  0.006 -0.623 -0.055  0.002  0.060 -0.022               
    ## AccmplsC:C1  0.041 -0.045 -0.088 -0.045 -0.008  0.031  0.001  0.080        
    ## AccmplsC:C2 -0.001 -0.003  0.021 -0.006  0.026 -0.001 -0.033  0.012   0.055
    ## GndRC:AC:C1 -0.040  0.048  0.079  0.032  0.001 -0.009 -0.016 -0.069  -0.633
    ## GndRC:AC:C2 -0.005 -0.002  0.011 -0.001 -0.033 -0.015  0.062  0.023  -0.013
    ##             AcC:C2 GRC:AC:C1
    ## GndrRtngCnt                 
    ## AccmplshdCn                 
    ## Condition1                  
    ## Condition2                  
    ## GndrRtnC:C1                 
    ## GndrRtnC:C2                 
    ## GndrRtnC:AC                 
    ## AccmplsC:C1                 
    ## AccmplsC:C2                 
    ## GndRC:AC:C1 -0.013          
    ## GndRC:AC:C2 -0.592  0.056

- Characters who are rated as more Accomplished are more likely to be
  referred to with *she*, but this is n.s. after correction for multiple
  comparisons.

- Interaction between Accomplishment, Name Gender Rating, and Condition
  (L vs F+F), but this is n.s. after correction for multiple
  comparisons, so I’m not going to dig into it.

# Importance

Summary statistics:

``` r
summary(exp3_d$Important)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   2.585   3.000   7.000

``` r
summary(exp3_d$ImportantFlip)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   5.000   6.000   5.415   7.000   7.000

``` r
sd(exp3_d$Important)
```

    ## [1] 1.366153

Does the Importance rating of the character predict the likelihood of
*she* as opposed to *he* and *other* responses The maximal model
includes all interactions, then random intercepts by item but not by
participant.

``` r
exp3_m_imp <- buildmer(
  formula = She ~ Condition * GenderRatingCentered * 
            ImportantCentered + (1|Participant) + (1|Item),
  data = exp3_d, family = binomial, 
  buildmerControl(direction = "order", quiet = TRUE))

summary(exp3_m_imp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ 1 + GenderRatingCentered + Condition + ImportantCentered +  
    ##     GenderRatingCentered:Condition + GenderRatingCentered:ImportantCentered +  
    ##     Condition:ImportantCentered + GenderRatingCentered:Condition:ImportantCentered +  
    ##     (1 | Item)
    ##    Data: exp3_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7998.6   8090.8  -3986.3   7972.6     8891 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4475 -0.5401 -0.1474  0.6287  9.9493 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Item   (Intercept) 0.3588   0.599   
    ## Number of obs: 8904, groups:  Item, 63
    ## 
    ## Fixed effects:
    ##                                                     Estimate Std. Error
    ## (Intercept)                                        -1.374939   0.089891
    ## GenderRatingCentered                                1.033388   0.055223
    ## Condition1                                          0.136688   0.070834
    ## Condition2                                          0.076283   0.089473
    ## ImportantCentered                                   0.041717   0.026130
    ## GenderRatingCentered:Condition1                     0.087122   0.045131
    ## GenderRatingCentered:Condition2                    -0.054814   0.058223
    ## GenderRatingCentered:ImportantCentered              0.003465   0.016691
    ## Condition1:ImportantCentered                       -0.059863   0.052359
    ## Condition2:ImportantCentered                        0.078479   0.063684
    ## GenderRatingCentered:Condition1:ImportantCentered   0.065849   0.033480
    ## GenderRatingCentered:Condition2:ImportantCentered  -0.028646   0.041322
    ##                                                      z value Pr(>|z|) Pr(>|t|)
    ## (Intercept)                                       -15.295564    0.000   <2e-16
    ## GenderRatingCentered                               18.712849    0.000   <2e-16
    ## Condition1                                          1.929679    0.054   0.0536
    ## Condition2                                          0.852574    0.394   0.3939
    ## ImportantCentered                                   1.596514    0.110   0.1104
    ## GenderRatingCentered:Condition1                     1.930431    0.054   0.0536
    ## GenderRatingCentered:Condition2                    -0.941446    0.346   0.3465
    ## GenderRatingCentered:ImportantCentered              0.207623    0.836   0.8355
    ## Condition1:ImportantCentered                       -1.143320    0.253   0.2529
    ## Condition2:ImportantCentered                        1.232319    0.218   0.2178
    ## GenderRatingCentered:Condition1:ImportantCentered   1.966845    0.049   0.0492
    ## GenderRatingCentered:Condition2:ImportantCentered  -0.693251    0.488   0.4882
    ##                                                      
    ## (Intercept)                                       ***
    ## GenderRatingCentered                              ***
    ## Condition1                                        .  
    ## Condition2                                           
    ## ImportantCentered                                    
    ## GenderRatingCentered:Condition1                   .  
    ## GenderRatingCentered:Condition2                      
    ## GenderRatingCentered:ImportantCentered               
    ## Condition1:ImportantCentered                         
    ## Condition2:ImportantCentered                         
    ## GenderRatingCentered:Condition1:ImportantCentered *  
    ## GenderRatingCentered:Condition2:ImportantCentered    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC Cndtn1 Cndtn2 ImprtC GnRC:C1 GnRC:C2 GRC:IC Cn1:IC
    ## GndrRtngCnt -0.285                                                          
    ## Condition1   0.002 -0.006                                                   
    ## Condition2  -0.017  0.018  0.014                                            
    ## ImprtntCntr -0.022  0.026  0.051 -0.053                                     
    ## GndrRtnC:C1 -0.001  0.017 -0.601  0.001 -0.052                              
    ## GndrRtnC:C2  0.015 -0.021  0.002 -0.588  0.030  0.008                       
    ## GndrRtnC:IC  0.026 -0.025 -0.052  0.030 -0.585  0.043  -0.059               
    ## Cndtn1:ImpC  0.033 -0.038 -0.042 -0.039 -0.070  0.046   0.021   0.064       
    ## Cndtn2:ImpC -0.027  0.022 -0.041  0.019  0.041  0.023  -0.017  -0.001  0.065
    ## GndRC:C1:IC -0.036  0.038  0.046  0.022  0.065 -0.030  -0.046  -0.048 -0.590
    ## GndRC:C2:IC  0.019 -0.035  0.023 -0.017 -0.001 -0.048   0.021   0.048 -0.018
    ##             Cn2:IC GRC:C1:
    ## GndrRtngCnt               
    ## Condition1                
    ## Condition2                
    ## ImprtntCntr               
    ## GndrRtnC:C1               
    ## GndrRtnC:C2               
    ## GndrRtnC:IC               
    ## Cndtn1:ImpC               
    ## Cndtn2:ImpC               
    ## GndRC:C1:IC -0.018        
    ## GndRC:C2:IC -0.552  0.074

- Interaction between Important, Name Gender Rating, and Condition (L vs
  F+F), but this is way too small to be significant after correction for
  multiple comparisons

# All Ratings

``` r
exp3_m_ratings <- buildmer(
  formula = She ~ Condition * GenderRatingCentered * LikeableCentered * 
            AccomplishedCentered * ImportantCentered +
            (1|Participant) + (1|Item),
  data = exp3_d,
  family = binomial, 
  buildmerControl(direction = c("order", "backward"), quiet = TRUE))

summary(exp3_m_ratings)
```

    ## 
    ## Call:
    ## stats::glm(formula = She ~ 1 + GenderRatingCentered + LikeableCentered + 
    ##     Condition + ImportantCentered + AccomplishedCentered + Condition:AccomplishedCentered + 
    ##     Condition:ImportantCentered + LikeableCentered:Condition + 
    ##     LikeableCentered:AccomplishedCentered + LikeableCentered:ImportantCentered + 
    ##     ImportantCentered:AccomplishedCentered + LikeableCentered:ImportantCentered:AccomplishedCentered + 
    ##     LikeableCentered:Condition:AccomplishedCentered + LikeableCentered:Condition:ImportantCentered + 
    ##     Condition:ImportantCentered:AccomplishedCentered + Condition:LikeableCentered:AccomplishedCentered:ImportantCentered, 
    ##     family = binomial, data = exp3_d)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9790  -0.7231  -0.3095   0.7728   2.9943  
    ## 
    ## Coefficients:
    ##                                                                     Estimate
    ## (Intercept)                                                        -1.183679
    ## GenderRatingCentered                                                0.895923
    ## LikeableCentered                                                    0.110380
    ## Condition1                                                          0.192791
    ## Condition2                                                         -0.061019
    ## ImportantCentered                                                  -0.070138
    ## AccomplishedCentered                                                0.053692
    ## Condition1:AccomplishedCentered                                     0.043019
    ## Condition2:AccomplishedCentered                                    -0.239237
    ## Condition1:ImportantCentered                                       -0.035665
    ## Condition2:ImportantCentered                                        0.100167
    ## LikeableCentered:Condition1                                        -0.052403
    ## LikeableCentered:Condition2                                         0.022351
    ## LikeableCentered:AccomplishedCentered                               0.008091
    ## LikeableCentered:ImportantCentered                                  0.005932
    ## ImportantCentered:AccomplishedCentered                              0.000364
    ## LikeableCentered:ImportantCentered:AccomplishedCentered             0.009798
    ## LikeableCentered:Condition1:AccomplishedCentered                    0.007850
    ## LikeableCentered:Condition2:AccomplishedCentered                    0.017325
    ## LikeableCentered:Condition1:ImportantCentered                       0.008035
    ## LikeableCentered:Condition2:ImportantCentered                       0.078234
    ## Condition1:ImportantCentered:AccomplishedCentered                   0.023376
    ## Condition2:ImportantCentered:AccomplishedCentered                   0.029479
    ## LikeableCentered:Condition1:ImportantCentered:AccomplishedCentered  0.022956
    ## LikeableCentered:Condition2:ImportantCentered:AccomplishedCentered  0.057829
    ##                                                                    Std. Error
    ## (Intercept)                                                          0.038223
    ## GenderRatingCentered                                                 0.021163
    ## LikeableCentered                                                     0.032207
    ## Condition1                                                           0.069638
    ## Condition2                                                           0.087625
    ## ImportantCentered                                                    0.029469
    ## AccomplishedCentered                                                 0.034803
    ## Condition1:AccomplishedCentered                                      0.071294
    ## Condition2:AccomplishedCentered                                      0.089899
    ## Condition1:ImportantCentered                                         0.059845
    ## Condition2:ImportantCentered                                         0.077861
    ## LikeableCentered:Condition1                                          0.065543
    ## LikeableCentered:Condition2                                          0.084333
    ## LikeableCentered:AccomplishedCentered                                0.021795
    ## LikeableCentered:ImportantCentered                                   0.022602
    ## ImportantCentered:AccomplishedCentered                               0.023053
    ## LikeableCentered:ImportantCentered:AccomplishedCentered              0.008109
    ## LikeableCentered:Condition1:AccomplishedCentered                     0.043915
    ## LikeableCentered:Condition2:AccomplishedCentered                     0.058238
    ## LikeableCentered:Condition1:ImportantCentered                        0.045577
    ## LikeableCentered:Condition2:ImportantCentered                        0.060974
    ## Condition1:ImportantCentered:AccomplishedCentered                    0.047445
    ## Condition2:ImportantCentered:AccomplishedCentered                    0.058217
    ## LikeableCentered:Condition1:ImportantCentered:AccomplishedCentered   0.016068
    ## LikeableCentered:Condition2:ImportantCentered:AccomplishedCentered   0.022892
    ##                                                                    z value
    ## (Intercept)                                                        -30.968
    ## GenderRatingCentered                                                42.335
    ## LikeableCentered                                                     3.427
    ## Condition1                                                           2.768
    ## Condition2                                                          -0.696
    ## ImportantCentered                                                   -2.380
    ## AccomplishedCentered                                                 1.543
    ## Condition1:AccomplishedCentered                                      0.603
    ## Condition2:AccomplishedCentered                                     -2.661
    ## Condition1:ImportantCentered                                        -0.596
    ## Condition2:ImportantCentered                                         1.286
    ## LikeableCentered:Condition1                                         -0.800
    ## LikeableCentered:Condition2                                          0.265
    ## LikeableCentered:AccomplishedCentered                                0.371
    ## LikeableCentered:ImportantCentered                                   0.262
    ## ImportantCentered:AccomplishedCentered                               0.016
    ## LikeableCentered:ImportantCentered:AccomplishedCentered              1.208
    ## LikeableCentered:Condition1:AccomplishedCentered                     0.179
    ## LikeableCentered:Condition2:AccomplishedCentered                     0.297
    ## LikeableCentered:Condition1:ImportantCentered                        0.176
    ## LikeableCentered:Condition2:ImportantCentered                        1.283
    ## Condition1:ImportantCentered:AccomplishedCentered                    0.493
    ## Condition2:ImportantCentered:AccomplishedCentered                    0.506
    ## LikeableCentered:Condition1:ImportantCentered:AccomplishedCentered   1.429
    ## LikeableCentered:Condition2:ImportantCentered:AccomplishedCentered   2.526
    ##                                                                    Pr(>|z|)    
    ## (Intercept)                                                         < 2e-16 ***
    ## GenderRatingCentered                                                < 2e-16 ***
    ## LikeableCentered                                                    0.00061 ***
    ## Condition1                                                          0.00563 ** 
    ## Condition2                                                          0.48621    
    ## ImportantCentered                                                   0.01731 *  
    ## AccomplishedCentered                                                0.12289    
    ## Condition1:AccomplishedCentered                                     0.54624    
    ## Condition2:AccomplishedCentered                                     0.00779 ** 
    ## Condition1:ImportantCentered                                        0.55121    
    ## Condition2:ImportantCentered                                        0.19827    
    ## LikeableCentered:Condition1                                         0.42398    
    ## LikeableCentered:Condition2                                         0.79098    
    ## LikeableCentered:AccomplishedCentered                               0.71047    
    ## LikeableCentered:ImportantCentered                                  0.79297    
    ## ImportantCentered:AccomplishedCentered                              0.98740    
    ## LikeableCentered:ImportantCentered:AccomplishedCentered             0.22694    
    ## LikeableCentered:Condition1:AccomplishedCentered                    0.85814    
    ## LikeableCentered:Condition2:AccomplishedCentered                    0.76610    
    ## LikeableCentered:Condition1:ImportantCentered                       0.86006    
    ## LikeableCentered:Condition2:ImportantCentered                       0.19947    
    ## Condition1:ImportantCentered:AccomplishedCentered                   0.62223    
    ## Condition2:ImportantCentered:AccomplishedCentered                   0.61260    
    ## LikeableCentered:Condition1:ImportantCentered:AccomplishedCentered  0.15310    
    ## LikeableCentered:Condition2:ImportantCentered:AccomplishedCentered  0.01153 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11191.3  on 8903  degrees of freedom
    ## Residual deviance:  8193.3  on 8879  degrees of freedom
    ## AIC: 8243.3
    ## 
    ## Number of Fisher Scoring iterations: 5
