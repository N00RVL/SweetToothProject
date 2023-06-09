<pre>
# SweetToothProject
#Choose Your Candy Project Created by Noorul.
# Load required packages
library(tidyverse)
library(broom)
library(corrplot)
library(fivethirtyeight)

# Load the candy_rankings dataset from fivethirtyeight package
data("candy_rankings")

# Take a look at the structure of the dataset
glimpse(candy_rankings)
 Observations: 85
 Variables: 13
<pre>
 $ competitorname   <chr> "100 Grand", "3 Musketeers", "One dime", "One...
 
 $ chocolate        <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, ...
 
 $ fruity           <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALS...
 
 $ caramel          <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE...
 
 $ peanutyalmondy   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE...
 
 $ nougat           <lgl> FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE...
 
 $ crispedricewafer <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS...
 
 $ hard             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
 
 $ bar              <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, ...
 
 $ pluribus         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL...
 
 $ sugarpercent     <dbl> 0.732, 0.604, 0.011, 0.011, 0.906, 0.465, 0.6...
 
 $ pricepercent     <dbl> 0.860, 0.511, 0.116, 0.511, 0.511, 0.767, 0.7...
 
 $ winpercent       <dbl> 66.97173, 67.60294, 32.26109, 46.11650, 52.34...
</pre>
# Gather the columns of the dataset into long format
candy_rankings_long <- gather(data = candy_rankings, key = feature, value = value, chocolate:pluribus)

# Create a bar chart for the distribution of values for each candy feature
ggplot(candy_rankings_long, aes(x = value, fill = feature)) +
  geom_bar() +
  xlab("Value") +
  ylab("Feature") +
  facet_wrap(.~feature) +
  theme(legend.position = "none")

![image](https://user-images.githubusercontent.com/111958524/229280676-2b37993b-2c7c-4410-a74d-7ab322fbd30f.png)

# Create a horizontal bar chart for the price per cent of each candy
ggplot(candy_rankings, aes(x = reorder(competitorname, pricepercent), y = pricepercent)) +
  geom_segment(aes(xend = reorder(competitorname, pricepercent), yend = 0), size = 1, color = "orange") +
  geom_point(color = "purple", size = 1, alpha=0.4) +
  xlab("Competitor Name") +
  ylab("Price per Cent") +
  coord_flip()

![image](https://user-images.githubusercontent.com/111958524/229280739-47b678c8-194d-496c-8fc6-6fe5e82a73f0.png)

# Create a histogram for the distribution of win percentages of the candies
ggplot(candy_rankings, aes(x = winpercent)) +
geom_histogram()
  
![image](https://user-images.githubusercontent.com/111958524/229280753-fbaa00a5-4e99-45f2-87a4-6b3e4e414896.png)

# Create a horizontal scatter plot to show the relationship between predicted and actual win percentages
ggplot(candy_rankings, aes(x = reorder(competitorname, winpercent), y = winpercent)) +
  geom_segment(aes(xend = reorder(competitorname, winpercent), yend = 0), color = "green") +
  geom_point(color = "blue", alpha = 0.4) +
  xlab("Competitor Name") +
  ylab("Preferences") +
  coord_flip()

![image](https://user-images.githubusercontent.com/111958524/229280759-f2aba99f-d514-48ff-a09b-2fddd392e601.png)

# Create a correlation plot for the dataset
corrplot(cor(candy_rankings[,2:13]))

![image](https://user-images.githubusercontent.com/111958524/229280770-5e04887d-7f83-4eaf-b470-f6936b87bd8f.png)

# Fit a linear regression model with winpercent as the response variable and all the other variables as predictors
win_mod <- lm(winpercent ~ ., candy_rankings[,2:13])
summary(win_mod)

# Create a scatter plot to show the residuals of the linear regression model
augment(win_mod) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(color = "green", size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red")

![image](https://user-images.githubusercontent.com/111958524/229280798-7dd21391-824d-4164-ad10-d00c559f8e50.png)

# Fit a logistic regression model with chocolate as the response variable and all the other variables as predictors
choc_mod <- glm(chocolate ~ ., candy_rankings[, 2:13], family = "binomial" )
summary(choc_mod)

# Predict chocolate using the logistic regression model and calculate the confusion matrix and accuracy
preds <- augment(choc_mod, type.predict = "response") %>% 
  mutate(prediction = .fitted > .5)
conf_mat <- table(preds$chocolate, preds$prediction)
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
</pre>
[Output](/Output.md)
