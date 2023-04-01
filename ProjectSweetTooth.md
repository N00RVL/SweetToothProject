<pre>
> library(broom)
> library(corrplot)
> library(fivethirtyeight)
> data("candy_rankings")
> glimpse(candy_rankings)
Rows: 85
Columns: 13

$ competitorname   <chr> "100 Grand", "3 Musketeers", "One dime", "One quarter", "Air Heads", "Almond Joy"…

$ chocolate        <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FA…

$ fruity           <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, …

$ caramel          <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, …

$ peanutyalmondy   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, …

$ nougat           <lgl> FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, …

$ crispedricewafer <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…

$ hard             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…

$ bar              <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FA…

$ pluribus         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, …

$ sugarpercent     <dbl> 0.732, 0.604, 0.011, 0.011, 0.906, 0.465, 0.604, 0.313, 0.906, 0.604, 0.604, 0.73…

$ pricepercent     <dbl> 0.860, 0.511, 0.116, 0.511, 0.511, 0.767, 0.767, 0.511, 0.325, 0.325, 0.511, 0.51…

$ winpercent       <dbl> 66.97173, 67.60294, 32.26109, 46.11650, 52.34146, 50.34755, 56.91455, 23.41782, 3…

> $ peanutyalmondy   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE...
> $ nougat           <lgl> FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE...
> $ crispedricewafer <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
> $ hard             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
> $ bar              <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, ...
> $ pluribus         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
> $ sugarpercent     <dbl> 0.732, 0.604, 0.011, 0.011, 0.906, 0.465, 0.6...
> $ pricepercent     <dbl> 0.860, 0.511, 0.116, 0.511, 0.511, 0.767, 0.7...
> $ winpercent       <dbl> 66.97173, 67.60294, 32.26109, 46.11650, 52.34...
> candy_rankings_long <- gather(data = candy_rankings, key = feature, value = value, chocolate:pluribus)
> ggplot(candy_rankings_long, aes(x = value, fill = feature)) +
+   geom_bar() +
+   xlab("Value") +
+   ylab("Feature") +
+   facet_wrap(.~feature) +
+   theme(legend.position = "none")
> ggplot(candy_rankings, aes(x = reorder(competitorname, pricepercent), y = pricepercent)) +
+   geom_segment(aes(xend = reorder(competitorname, pricepercent), yend = 0), size = 1, color = "orange") +
+   geom_point(color = "purple", size = 1, alpha=0.4) +
+   xlab("Competitor Name") +
+   ylab("Price per Cent") +
+   coord_flip()
> ggplot(candy_rankings, aes(x = winpercent)) +
+   geom_histogram()
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
> ggplot(candy_rankings, aes(x = reorder(competitorname, winpercent), y = winpercent)) +
+   geom_segment(aes(xend = reorder(competitorname, winpercent), yend = 0), color = "green") +
+   geom_point(color = "blue", alpha = 0.4) +
+   xlab("Competitor Name") +
+   ylab("Preferences") +
+   coord_flip()  
> corrplot(cor(candy_rankings[,2:13]))
> win_mod <- lm(winpercent ~ ., candy_rankings[,2:13])
> summary(win_mod)

Call:
lm(formula = winpercent ~ ., data = candy_rankings[, 2:13])

Residuals:
     Min       1Q   Median       3Q      Max 
-20.2244  -6.6247   0.1986   6.8420  23.8680 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)           34.5340     4.3199   7.994 1.44e-11 ***
chocolateTRUE         19.7481     3.8987   5.065 2.96e-06 ***
fruityTRUE             9.4223     3.7630   2.504  0.01452 *  
caramelTRUE            2.2245     3.6574   0.608  0.54493    
peanutyalmondyTRUE    10.0707     3.6158   2.785  0.00681 ** 
nougatTRUE             0.8043     5.7164   0.141  0.88849    
crispedricewaferTRUE   8.9190     5.2679   1.693  0.09470 .  
hardTRUE              -6.1653     3.4551  -1.784  0.07852 .  
barTRUE                0.4415     5.0611   0.087  0.93072    
pluribusTRUE          -0.8545     3.0401  -0.281  0.77945    
sugarpercent           9.0868     4.6595   1.950  0.05500 .  
pricepercent          -5.9284     5.5132  -1.075  0.28578    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.7 on 73 degrees of freedom
Multiple R-squared:  0.5402,	Adjusted R-squared:  0.4709 
F-statistic: 7.797 on 11 and 73 DF,  p-value: 9.504e-09


> augment(win_mod) %>%
+   ggplot(aes(x = .fitted, y = .resid)) +
+   geom_point(color = "green", size = 2, alpha = 0.8) +
+   geom_hline(yintercept = 0, color = "red")

> choc_mod <- glm(chocolate ~ ., candy_rankings[, 2:13], family = "binomial" )
Warning message:
glm.fit: fitted probabilities numerically 0 or 1 occurred 
> summary(choc_mod)

Call:
glm(formula = chocolate ~ ., family = "binomial", data = candy_rankings[, 
    2:13])

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.72224  -0.17612  -0.02787   0.01954   2.57898  

Coefficients:
                       Estimate Std. Error z value Pr(>|z|)   
(Intercept)           -10.29370    4.12040  -2.498  0.01248 * 
fruityTRUE             -6.75305    2.20462  -3.063  0.00219 **
caramelTRUE            -1.85093    1.66750  -1.110  0.26700   
peanutyalmondyTRUE     -4.11907    2.98175  -1.381  0.16715   
nougatTRUE            -16.74818 3520.13323  -0.005  0.99620   
crispedricewaferTRUE   14.98331 4725.35051   0.003  0.99747   
hardTRUE                1.83504    1.80742   1.015  0.30997   
barTRUE                19.06799 3520.13379   0.005  0.99568   
pluribusTRUE            0.22804    1.45457   0.157  0.87542   
sugarpercent            0.12168    2.07707   0.059  0.95329   
pricepercent            1.76626    2.24816   0.786  0.43208   
winpercent              0.23019    0.08593   2.679  0.00739 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 116.407  on 84  degrees of freedom
Residual deviance:  25.802  on 73  degrees of freedom
AIC: 49.802

Number of Fisher Scoring iterations: 19


> preds <- augment(choc_mod, type.predict = "response") %>% 
+   mutate(prediction = .fitted > .5)
> conf_mat <- table(preds$chocolate, preds$prediction)
> accuracy <- sum(diag(conf_mat))/sum(conf_mat)
</pre>
