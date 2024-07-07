
# Load required package ---------------------------------------------------

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

# Simple linear regression ------------------------------------------------

    # Load data 
rm(list = ls())
data <- readr::read_csv("data/income.data.csv")
names(data) <- c("id","income","happiness")

head(data)
# income is income in USD 10000
# happiness is happiness score
# Question= is happiness score increased with income?

summary(data)
    # Check Assumptions
# Independence of observation
# Only two variable and there is no auto correlation.
# Thus there is independence of observation.

# Normality
hist(data$happiness)
# Seem bell-shape and normally distributed.
shapiro.test(data$happiness) 
# But Shapiro-Wilk test said no normality.
# Let's assume it normal because of the bell-shape

# Linearity
plot(data$income, data$happiness)
# It's roughly linear

# Homoscedasticity (homogeneity of variance) will be test after fitting the model

# Perform linear regression analysis
res <- lm(happiness ~ income, data = data)
summary(res)
round(cbind(res$coefficients,confint(res)),2)

# Checking homogeneity of variance

plot(res,1)
# In residual vs fitted plot, mean of the residuals  is around zero,
# so, there is no outliers or bias that affect validity of our model
plot(res,2)
# In Q-Q residual plot, real residuals fit with theoretical residuals almost perfectly,
# so there is homogeneity of variance

# Outlier and influence

plot(res)
data$residual <- res$residuals
data$cook <- cooks.distance(res)
# The plot showed observation 87,223,247,460 are outliers
data %>%  
  filter(id %in% c(87,223,247,460)) %>% 
  select(income, happiness, residual, cook)
# Non of the cook distance is above one and there's no influential points biasing the model

# Visualize the regression
ggplot(data = data, aes(x=income,y=happiness)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm",se=T , color = "black") +
  stat_regline_equation(label.x =2 , label.y =6 ) +
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")


# Multiple linear regression ----------------------------------------------

# Load data
rm(list = ls())
data <- readr::read_csv("data/heart.data.csv")
names(data) <- c("town","biking", "smoking","heart")
# Percentages of bikers , smokers and heart disease in 498 towns
summary(data)

# Checking assumption

    # Independence of observation
cor(data$biking, data$smoking) #test relationship between independent variables
# correlation coefficient 0.015 __ small
cor <- cor(data, method = "pearson"); cor
symnum(cor)

    # Normality
hist(data$heart)
# Data is not skewed and roughly bell-shape
shapiro.test(data$heart) #Shapiro-Wilk test showed non-normality

    # Linearity
plot(data$biking,data$heart) #Linear relationship
plot(data$smoking, data$heart) #Unclear but a little bit linear

    # Performing linear regression
res <- lm(heart ~ biking + smoking, data = data)
summary(res)

    # Homogeneity of variance
plot(res,1) # mean of residuals is around zero.
plot(res,2) # almost perfectly fit between real and theoretical quantiles of residuals

    # Outlier and Influential points
plot(res)
data$residual <- res$residuals
data$cook <- cooks.distance(res)
data$dfbeta <- dfbeta(res)
data$p <- predict(res)
plot(data$dfbeta[,"biking"], data$p)

# plots showed obs: 196,231,416,426,346 are out-liers
summary(data$cook)
data %>% 
  filter(town %in% c(196,231,416,426,346))


# Visualizing regression
plot.data <- expand.grid(
  biking = seq(min(data$biking), max(data$biking), length.out =30),
  smoking = c(min(data$smoking),mean(data$smoking), max(data$smoking))
)
plot.data$predict.y <- predict.lm(res, newdata = plot.data)


plot.data$smoking <- round(plot.data$smoking,2)
plot.data$smoking <- as.factor(plot.data$smoking)


ggplot(data = data, aes(biking, heart)) +
  geom_point(color = "royalblue") +
  geom_line(data = plot.data, 
            aes( x = biking, y = predict.y, color = smoking),
            size = 1.25) +
  theme_bw() +
  labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
       x = "Biking to work (% of population)",
       y = "Heart disease (% of population)",
       color = "Smoking \n (% of population)")

# Binary logistic regression ----------------------------------------------

    # Set up and load data
library(tidyverse)
rm(list = ls())
data <- read_csv("data/lungCancer.csv")
names(data) <- c("lungCA","smoke","bmi")
# smoke = years of smoking
data$lungCA <- factor(data$lungCA,
                      levels = c(0,1),
                      labels = c("Non-CA","CA"))
data$bmi <- factor(data$bmi,
                   levels = c(0,1),
                   labels = c("Underweight","Normal"))


    # Assumptions
# Independence of observation
# No perfect multicollinearity
# Linearity

    # Fitting models

m1 <- glm(lungCA ~ smoke, data = data, family = binomial("logit"))
summary(m1)

m2 <- glm(lungCA ~ smoke + bmi, data = data, family = binomial("logit"))
summary(m2)

    # Check assumptions

# Independence of observation 
# not same person, family, residence

# Check multicollinearity 
# by using generalised vif(must be <2)
car::vif(m2) # below two

# by using correlation coefficient
hist(data$smoke)
cor <- cor(x = data$smoke,y = as.numeric(data$bmi), method = "kendall") ; cor
symnum(cor)


# Check linearity between logit of fitted values and years of smoking
logit <- log(m2$fitted.values/(1-m2$fitted.values))
plot(m2$model$smoke,logit)

linear <- data.frame(logit, smoke=m2$model$smoke)
linear %>% 
  ggplot(aes(x = smoke, y = logit)) +
  geom_point(aes(size = "Observation"), color = "gray", alpha =2)+
  geom_smooth(se = FALSE, aes(color = "Loess curve")) + 
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) + 
  theme_minimal(base_size = 14, base_family = "serif") +
  labs(x = "Years of smoking", 
       y = "Log-odds of lung cancer predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("gray50", "black")) +
  scale_size_manual(values = 1.5, name = "")

# Adjusted odds ratios
format(round(exp(m2$coefficients),2), scientific = F)
tab <- cbind(m2$coefficients, confint(m2)) 
exp(tab)

# model Chi square test
prop.table(table(m2$model$lungCA))
#deviance - how far predicted probability from true probability (0.43)
m2$null.deviance 
m2$deviance
x <- m2$null.deviance - m2$deviance ; x
df <- m2$df.null - m2$df.residual ; df
1-pchisq(x,df)

# Contingency table
data$p <- m2$fitted.values
data$pcat <- ifelse(data$p>0.5,"Yes","No")

table(data$pcat)
table(data$lungCA)
acc.tab <- table(data$pcat, data$lungCA) ; acc.tab
sensitivity <- acc.tab[2,2]/(acc.tab[1,2]+ acc.tab[2,2]) ; sensitivity
specificity <- acc.tab[1,1]/(acc.tab[1,1]+acc.tab[2,1]) ; specificity
correct <- (acc.tab[2,2]+acc.tab[1,1])/sum(acc.tab) ; correct

