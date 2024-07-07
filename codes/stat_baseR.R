
# Load data ---------------------------------------------------------------

library(tidyverse)
rm(list = ls())
data <- starwars 



# Checking data -----------------------------------------------------------

str(data)
names(data)

# Checking missing value
is.na(data$height)
sum(is.na(data$height))
data[is.na(data$height),"name"]

is.na(data$mass)
sum(is.na(data$mass))
data[is.na(data$mass),"name"]

is.na(data$sex)
sum(is.na(data$sex))
data[is.na(data$sex),"name"]

# Range check for numeric variables
summary(data$height)
max(data$height, na.rm = T)  
min(data$height, na.rm = T)

summary(data$mass)
max(data$mass, na.rm = T)  
min(data$mass, na.rm = T)


# Outliers
quantile(data$mass, na.rm = T)
IQR(data$mass, na.rm = T)
data[data$mass > 84.5+(1.5*(IQR(data$mass, na.rm = T))), c("name", "mass")]
data[data$mass < 55.6-(1.5*(IQR(data$mass, na.rm = T))), c("name", "mass")]


# Category check for categorical variables
unique(data$gender) 
unique(data$sex)


# Descriptive statistics --------------------------------------------------

# Check normality
hist(data$height)
hist(data$mass)

qqnorm(data$height)
qqnorm(data$mass)

shapiro.test(data$height)
format(shapiro.test(data$mass), scientific = F)

# Summarizing numeric variable
summary(data$height)
summary(data$mass)

mean(data$height, na.rm = T)
sd(data$height, na.rm = T)

mean.se <- function(x) sd(x)/sqrt(length(x))
x <- data[!is.na(data$height),"height"]
height <- as.double(x$height)
mean.se(height)

# Summarizing categorical variable

table(data$sex)
prop.table(table(data$sex))
round(100*prop.table(table(data$sex)),2)

table(data$gender)
prop.table(table(data$gender))
round(100*prop.table(table(data$gender)),2)



# Inferential statistics for continuous data ------------------------------
## One sample t test 

trim <- data %>% filter(height >143 & height < 215)

shapiro.test(trim$height) #Meet normality assumption
t.test(height~1, data = trim, alternative = "greater", 
       mu = 180, conf.level = 0.95, na.action = na.omit)

t.test(height~1, data = trim, alternative = "less", 
       mu = 180, conf.level = 0.95, na.action = na.omit)

t.test(height~1, data = trim, alternative = "two.sided", 
       mu = 180, conf.level = 0.95, na.action = na.omit)

# Wilcoxon sign rank test for one sample 
shapiro.test(data$mass) #Violate normality assumption
wilcox.test(mass~1, data = data, alternative = "two.sided", 
            mu = 97, conf.int = T, conf.level = 0.95, na.action = na.omit)

wilcox.test(mass~1, data = data, alternative = "less", 
            mu = 97, conf.int = T, conf.level = 0.95,correct = T, na.action = na.omit)

wilcox.test(mass~1, data = data, alternative = "greater", 
            mu = 97, conf.int = T, conf.level = 0.95,correct = F, na.action = na.omit)


# Independent t test 

subdt <- data %>% filter(height >143 & height < 215 & !is.na(gender))
  # Checking assumptions
# 1. Normality
shapiro.test(subdt[subdt$gender=="masculine",]$height)
shapiro.test(subdt[subdt$gender=="feminine",]$height)

# 2. Equal variance
var.test(height~gender, data = subdt, ratio =1, alternative = "two.sided")

t.test(height~gender, data =  subdt, var.equal = T, alternative = "two.sided")
t.test(height~gender, data =  subdt, var.equal = F, alternative = "two.sided")
t.test(height~gender, data =  subdt, var.equal = T, alternative = "less")
t.test(height~gender, data =  subdt, var.equal = T, alternative = "greater")


# Paired t test 
set.seed(111)
pair <- data.frame(before = round(rnorm(100, mean = 180, sd = 35)),
                   after = round(rnorm(100, mean = 79, sd = 29))) 

shapiro.test(pair$before) #met normality assumption
shapiro.test(pair$after)  #met normality assumption

t.test(pair$before, pair$after, paired = T, conf.level = 0.95, alternative = "two.sided")
t.test(pair$before, pair$after, paired = T, conf.level = 0.95, alternative = "less")
t.test(pair$before, pair$after, paired = T, conf.level = 0.95, alternative = "greater")


# Mann Whitney' test 
shapiro.test(data$height) #Normality assumption do not met
shapiro.test(data[data$gender == "masculine",]$height)
shapiro.test(data[data$gender == "feminine",]$height)

wilcox.test(height~gender, data =  data, correct = T, alternative = "two.sided")
wilcox.test(height~gender, data =  data, correct = F, alternative = "two.sided")

# One way ANOVA 

data <- iris
data %>% 
  group_by(Species) %>% 
  summarise(
    count = n(),
    mean = mean(Sepal.Length),
    sd = sd(Sepal.Length)
  ) 

ggplot(data = data, aes(Species,Sepal.Length)) + geom_boxplot()

  # Checking assumption
# Normality
shapiro.test(data[data$Species == "setosa",]$Sepal.Length)
shapiro.test(data[data$Species == "versicolor",]$Sepal.Length)
shapiro.test(data[data$Species == "virginica",]$Sepal.Length)


# Homogeneity of variance
bartlett.test(data$Sepal.Length ~ data$Species) #Bartlett Test of Homogeneity of Variances
fligner.test(data$Sepal.Length ~ data$Species) #Fligner-Killeen test for  homogeneity of variance in non normality distribution
car::leveneTest(Sepal.Length ~ Species, data = data) #Levene's test for homogeneity of variance in non-normal distribution

# Fitting ANOVA 
res.aov <- aov(Sepal.Length ~ Species, data = data) ; res.aov
broom::tidy(res.aov)
summary(res.aov)


# Pairwise comparisons
TukeyHSD(res.aov)
plot(TukeyHSD(res.aov))
broom::tidy(TukeyHSD(res.aov))

pairwise.t.test(data$Sepal.Length, data$Species, p.adjust.method = "bonferroni")
pairwise.t.test(data$Sepal.Length, data$Species, p.adjust.method = "BH")

# Diagnostic of assumption
plot(res.aov, 1) #Residual vs fitted for homogeneity of variance
plot(res.aov, 2) # QQ Residual plot for normality
aov_residuals <- residuals(res.aov)
shapiro.test(aov_residuals)

# Here we see observation 107, 118 and 132 are violating the assumptions
# Let's treat them just for training (We don't do it in practice)

remove <- data %>% slice(-c(107,118,132))
shapiro.test(remove[remove$Species == "setosa",]$Sepal.Length)
shapiro.test(remove[remove$Species == "versicolor",]$Sepal.Length)
shapiro.test(remove[remove$Species == "virginica",]$Sepal.Length)

bartlett.test(remove$Sepal.Length, remove$Species)

summary(aov(data = remove, formula = Sepal.Length ~ Species))
plot(aov(data = remove, formula = Sepal.Length ~ Species))

# You see, just removing outliers does not work

bartlett.test(log(data$Sepal.Length), data$Species) 
# Natural log transformation work

# Relaxing homogeneity of variance assumption

oneway.test(Sepal.Length ~ Species, data = data, var.equal = F)
broom::tidy(pairwise.t.test(data$Sepal.Length, data$Species, p.adjust.method = "BH", pool.sd = F))


oneway.test(Sepal.Length ~ Species, data = data, var.equal = T)
pairwise.t.test(data$Sepal.Length, data$Species, p.adjust.method = "BH", pool.sd = T)

# Kruskal-Wallis Rank Sum Test 

kruskal.test(Sepal.Length ~ Species, data = data)

# Correlation 

# Correlation Matrix of Multivariate sample
  #Normal data
corM1 <- cor(iris[,-5], method = "pearson") ; corM1

  #Skewed data
corM2 <- cor(iris[,-5], method = "spearman") ; corM2
corM3 <- cor(iris[,-5], method = "kendall") ; corM3

#graphical matrix
symnum(corM1) 
symnum(corM2)
symnum(corM3)

# Correlation coefficient
cor(iris$Petal.Length,iris$Petal.Width, method = "pearson")
cor(iris$Petal.Length,iris$Petal.Width, method = "kendall")
cor(iris$Petal.Length,iris$Petal.Width, method = "spearman")

#Correlation test
cor.test(iris$Petal.Length,iris$Petal.Width, method = "pearson")
cor.test(iris$Petal.Length,iris$Petal.Width, method = "kendall")
cor.test(iris$Petal.Length,iris$Petal.Width, method = "spearman")



# Inferential statistics for categorical data --------------------------------------------------


lowbwt <- readr::read_csv("c:/docs/R_teaching/data/lowbwt_csv3.csv")
lowbwt <- lowbwt %>% 
  select(id,low,smoke) %>% 
  mutate(
    low = if_else(low == "bwt<=2500g", 1L, 0L),
    smoke = if_else(smoke == "Yes", 1L, 0L)
  ) 

# Z test for one sample proportion 

table(lowbwt[lowbwt$smoke==1,]$low)
# Here, number of low birth-weight among smoking mothers is 30
sum(lowbwt$smoke)
# Total number of smoking mothers is 74

# Alternative hypothesis: proportion of low birth-weight among smokers is not equal to 0.5
prop.test(x = 30,n = 74, p = 0.5, alternative = "two.sided")

# Alternative hypothesis: proportion of low birth-weight among smokers is greater than 0.5
prop.test(30,74,0.6, alternative = "greater")

# Alternative hypothesis: proportion of low birth-weight among smokers is smaller than 0.5
prop.test(30,74,0.6, alternative = "less")

# prop.test can only be used for 2x2 table with c(1,0) or binomial distribution
prop.test(table(lowbwt[lowbwt$smoke==1,]$low), 100) #with continuity correction
prop.test(table(lowbwt[lowbwt$smoke==1,]$low), 100, correct = F) #without continuity correction



# Z test for two sample proportion
prop.test(table(lowbwt$smoke,lowbwt$low),100)
# It is better to do the proportion test step by step as followed.

lowbwt <- readr::read_csv("c:/docs/R_teaching/data/lowbwt_csv3.csv")
lowbwt <- lowbwt %>% 
  select(id,low,smoke) %>% 
  mutate(
    low = if_else(low == "bwt<=2500g", 1L, 0L),
    smoke = if_else(smoke == "Yes", 1L, 2L)
  ) 
lowbwt$low <- factor(lowbwt$low,
                     levels = c(0,1),
                     labels = c("normal","low"))
lowbwt$smoke <- factor(lowbwt$smoke,
                       levels = c(1,2),
                       labels = c("yes","no"))

table(lowbwt$smoke,lowbwt$low)
mosaicplot(table(lowbwt$smoke,lowbwt$low))
prop.test(x = c(29, 30), n = c(86+29,44+30))
prop.test(x = c(29, 30), n = c(86+29,44+30), alternative = "less")


# Chi square test of independence
table(lowbwt$smoke,lowbwt$low)
Xsq <- chisq.test(table(lowbwt$smoke,lowbwt$low)) ; Xsq
  # Checking assumption
Xsq$observed #observed frequency
Xsq$expected #expected frequency

# Chi square goodness of fit test
x <- c(89,37,30,28,2)
p <- c(40,20,20,15,5)
chisq.test(x, p = p) #error probability total>1
chisq.test(x, p = p, rescale.p = TRUE)
p <- c(0.40,0.20,0.20,0.19,0.01)
gf <- chisq.test(x, p = p); gf
gf$expected

# Fisher's exact test

lowbwt <- readr::read_csv("c:/docs/R_teaching/data/lowbwt_csv3.csv")
lowbwt <- lowbwt %>% 
  select(id,low,smoke) %>% 
  mutate(
    low = if_else(low == "bwt<=2500g", 1L, 0L),
    smoke = if_else(smoke == "Yes", 1L, 0L)
  ) 
lowbwt$low <- factor(lowbwt$low,
                     levels = c(0,1),
                     labels = c("normal","low"))
lowbwt$smoke <- factor(lowbwt$smoke,
                       levels = c(0,1),
                       labels = c("no","yes"))

table(lowbwt$smoke,lowbwt$low)
prop.table(table(lowbwt$smoke,lowbwt$low))
fisher.test(table(lowbwt$smoke,lowbwt$low))
fisher.test(table(lowbwt$smoke,lowbwt$low), alternative = "less")
fisher.test(table(lowbwt$smoke,lowbwt$low), alternative = "greater")
fisher.test(table(lowbwt$smoke,lowbwt$low), alternative = "greater", or=3)


