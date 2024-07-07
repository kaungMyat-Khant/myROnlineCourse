# mStats is a package that is very convenient for epidemiological analysis.
# It is better to use it together with magrittr & dplyr package

# Attached package -------------------------------------------------------------------

library(mStats)
library(magrittr)

getwd()
setwd("C:/docs/R_teaching/data")

# Cleaning work-space ------------------------------------------------------
# instead of rm(list=ls())
a <- 1 ; b <- "o"
clear() #clear both environment and console

# Read data ---------------------------------------------------------------

# You can use haven, rio or any package to read data depending on your dataset file type
hiv.xls <- readxl::read_excel("pone_Adherence_ART.xls")
str(hiv.xls)

# Write data --------------------------------------------------------------

# Let's say we have clean the data and write it so that we can send our clean data to a journal for publication
write.csv(hiv.xls, file = "pone_Adherence_ART.csv", na = "", row.names = F)
haven::write_dta(hiv.xls, path = "pone_Adherence_ART.dta")

# Checking data -----------------------------------------------------------

codebook(hiv.xls) #to describe data
View(hiv.xls)
# change the tibble into more flexible data frame
hiv <- data.frame(hiv.xls)
codebook(hiv)


# Data management ---------------------------------------------------------

# Subsetting
varNames <- c("Sex", "TB", "District", "WHOstage", "ARVregimen", "ARVStart",
               "ARVtablet", "Nutrition", "CD4", "Relation", "RelativeHIV",
               "SiblingsHIV", "Literacy", "Nregimens", "VLcat")
hivDF <- hiv[,varNames]
codebook(hivDF)
# Recoding values
head(hivDF)
hivDF <- recode(hivDF,TB,"y"/"Yes","n"/"No")

# Create new variable
hivDF <- generate(hivDF, ARVpi, ifelse(ARVregimen < 4,"Non-PI", "PI"))

# extended generate for numeric to factor
hivDF <- generate(hivDF,timeART,as.numeric(as.Date("2017-12-31") - as.Date(ARVStart))/365.25)
hivDF <- egen(hivDF, timeART, 
     cut = c(2.0, 6.0, 9.0),
     lbl = c("3-24M","25M-5Yr","6-8Yr","9+Yr"), yearART.gp)

# Labelling

hivDF <- label(hivDF,
      ARVpi = "PI or Non-PI", 
      timeART = "Year taking ART",
      yearART.gp = "Group of year taking ART",
      District = "1=Misungwi,2=others")

hivDF <- label(hivDF, "ART Adherence Dataset")
codebook(hivDF)


# Exploratory data analysis -----------------------------------------------


hiv <- recode(hiv, Sex, "F"/"Female","M"/"Male")

# One-way tabulation
tab(hiv,Sex)
tab(hivDF,ARVpi,yearART.gp)
tab(hivDF)

# Cross-tabulation
tab(hivDF,ARVpi, by = Sex)
tab(hivDF,Literacy, by = Sex, row.pct = F, na.rm = T, digits = 2)

# Numeric summary
summ(hiv, CD4)
summ(hivDF, CD4, timeART)
summ(hivDF, CD4, timeART, detail = T)
summ(hiv)

# Group summary
summ(hivDF, CD4, by = ARVpi, digits = 2)
summ(hivDF, CD4, by = yearART.gp, digits = 1)


# Workflow ----------------------------------------------------------------

# Set up
library(magrittr)
library(mStats)
clear()
getwd()
setwd("C:/docs/R_teaching/data")

# Read data
hiv <- readxl::read_excel("pone_Adherence_ART.xls")
str(hiv)
codebook(hiv)

# Manage data
hiv.tables <- hiv %>% 
  # Remove unnecessary variables
  dplyr::select(!c(DiagnHIV, 
                   CTX, 
                   Vitamin, 
                   IPT, 
                   Infection, 
                   Adherence, 
                   AdherenceR)) %>% 
  # Re-code categorical values to characters
  recode(Sex,"F"/"Female", "M"/"Male") %>% 
  recode(TB, "y"/"Positive", "n"/"Negative") %>% 
  recode(District, 1/"Misungwi", 2/"others") %>% 
  recode(WHOstage, 1/"I", 2/"II", 3/"III", 4/"IV") %>% 
  # Generate ARV as a new variable for abbreviation
  generate(ARV, ARVregimen) %>% 
  # Re-code ARV as character
  recode(ARV, 1/"3TC+AZT+NVP", 2/"3TC+AZT+EFV", 3/"ABC+3CT+EFV",
         4/"ABC+3TC+LPV/r", 5/"ABC+3TC+LPV/r", 6/"TDF+FDC+ATV/r",
         7/"ABC+3TC+ATV/r") %>% 
  generate(ARVpi, ifelse(ARVregimen < 4, "Non-PI", "PI")) %>% 
  # new variable to calculate time on ART: date difference / 365.25 = years
  generate(timeART, as.numeric(as.Date("2017-12-31") - as.Date(ARVStart)) / 365.25) %>% 
  # factorize time of ART into categories
  egen(timeART, 
       cut = c(2.0,6.0,9.0), 
       lbl = c("3-24M", "25M-5Yr", "6-8Yr", "9+Yr"),
       new_var = timeART.grp) %>% 
  # number of ARV tablets taken
  egen(ARVtablet, 
       cut = c(2.5,4.5,6), 
       lbl = c("<=2.4", "2.5-4.4", "4.5-5.9", "6+"),
       new_var = ARVtab.grp) %>% 
  # Re-code nutrition
  recode(Nutrition, 1/"Normal", 2/"Moderate", 3/"Severe") %>% 
  # CD4 group cut-off point 351
  egen(CD4,
       cut = c(0,351),
       lbl = c("<=350","351+"),
       new_var = cd4.grp1) %>% 
  # CD4 group cut-off point 500
  egen(CD4,
       cut = c(0,501),
       lbl = c("<=500","501+"),
       new_var = cd4.grp2) %>% 
  # re-code relation
  recode(Relation, 1/ "Mother", 2/"Father", 3/"Relative", 4/"Others") %>% 
  # re-code HIV care giver
  recode(RelativeHIV, "y"/"Yes", "n"/"No") %>% 
  # re-code siblings' HIV
  recode(SiblingsHIV, "y"/"Yes", "n"/"No") %>% 
  # re-code care giver's literacy
  recode(Literacy, "y"/"Yes", "n"/"No") %>% 
  # generate new variable for viral load suppression 
  generate(vlSuppress, ifelse(VLcat < 2, 1, 0)) %>% 
  # Keep the variables that we will use
  dplyr::select(Sex, TB, District, WHOstage, ARVpi, ARV, timeART.grp, ARVtab.grp,
                Nutrition, cd4.grp1, Relation, RelativeHIV, SiblingsHIV, Literacy,
                timeART, Nregimens, cd4.grp2, vlSuppress) %>% 
  # label the variable
  label(Sex = "Gender",
        TB = "History of TB",
        District = "District: 1=Misungwi,2=others", 
        WHOstage ="WHO Clinical Staging",
        ARVpi = "ART PI-NonPI", 
        ARV = "ARV regimens", 
        timeART.grp = "Time on ART Category",
        ARVtab.grp = "number of ARV tablet/day", 
        Nutrition = "Nutritional Status",
        cd4.grp1 = "CD4 group: 350", 
        Relation = "Relation with Caregiver", 
        RelativeHIV = "HIV+ Caregiver",
        SiblingsHIV = "HIV+ Siblings", 
        Literacy = "Literacy of caregiver",
        timeART = "Time on ART Years", 
        Nregimens = "Previous Regimens", 
        cd4.grp2 = "CD4 group: 500", 
        vlSuppress = "Viral Suppression")

# Save the output
ilog(logfile = "C:/docs/R_teaching/output.txt", append = F)

# Check the data 
codebook(hiv.tables)

# Tabulate the categorical data 
tab(hiv.tables)

# Summarize the continuous data
summ(hiv.tables)

# Cross tab
tab(hiv.tables, by = vlSuppress)

# Logistic regression
res.sex <- glm(vlSuppress ~ Sex, data = hiv.tables, family = binomial(link = "logit"))
logit(res.sex)
res.full <- glm(vlSuppress ~ Sex + District + cd4.grp2, data = hiv.tables, family = binomial(link = "logit"))
logit(res.full)

# Save the output
ilog.close()
