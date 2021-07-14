############################################################
#                                                          #
#                Coding Club 01-03-2017                    #
# Practicing modelling using  different data distributions #
#                                                          #
############################################################
#URL: https://ourcodingclub.github.io/tutorials/modelling/#generalised
#generalized linear models and data distributions

#collinearity: If two variables in your dataset are very correlated 
#with each other, chances are they will both explain similar amounts of 
#variation in your response variable - but the same variation, not different 
#or complementary aspects of it! 

# Libraries ----
library(lme4)
library(ggplot2)
library(dplyr)

# Load data ----

### For a general linear model

install.packages("agridat")
library(agridat)

#load dataset from agridat
apples <- agridat::archbold.apple
head(apples)
str(apples)

# Defining a ggplot2 theme we will use for plotting later
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

#make spacing column a factor because there are only 3 types
apples$spacing2 <- as.factor(apples$spacing)

#make a boxplot of the data
(apples.p <- ggplot(apples, aes(spacing2, yield)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, color = "#8B2323") +
    theme.clean() +
    theme(axis.text.x = element_text(size= 12, angle= 0)) +
    labs(x= "Spacing (m)", y= "Yield (kg)"))

#run a model to test if yield increases when spacing increases
apples.m <- lm(yield ~ spacing2, data= apples)
summary(apples.m)
#in the summary, there is spacing 210, which refers to spacing2, and the 10 spacing
#as spacing increases, yield increases with a low p-value

#Is the weight of lambs at weaning a function of their age at weaning?
sheep <- agridat::ilri.sheep
head(sheep)

#filter out only lambs that were from mothers in breed R
sheep <- filter(sheep, ewegen == "R")
head(sheep)

sheep.m1 <- lm(weanwt ~ weanage, data= sheep)
summary(sheep.m1)

#what if the sex of the lamb also influences weight?
sheep.m2 <- lm(weanwt ~ weanage*sex, data= sheep)
summary(sheep.m2)
#intercept is the weight of a female at age 0 (this is bc the dataset starts with females)

#scatterplot
(sheep.p <- ggplot(sheep, aes(x= weanage, y= weanwt)) +
    geom_point(aes(color= sex)) +
    labs(x= "Age at weaning (days)", y= "Wean weight (kg)") +
    stat_smooth(method = "lm", aes(fill= sex, color= sex)) +
    scale_color_manual(values = c("#FFC125", "#36648B")) +
    scale_fill_manual(values = c("#FFC125", "#36648B")) +
    theme.clean())

#check if the data meets the assumptions to use a linear model
#are the residuals normally distributed? Use the Shapiro-Wilk test
apples.resid <- resid(apples.m)
shapiro.test(apples.resid)
#the p-value is 0.87, which means we fail to reject the null and the residuals are not different
#from a normal distribution

#is there homoscedasticity? i.e. is the variance in the data around the same at all values of the predictor variable
bartlett.test(yield ~ spacing2, data= apples)
#the p-value is 0.14, which means we fail to reject the null hypo of homoscedasticity

#are the data points independent? assume yes bc we didnt collect the data

#plot model
plot(apples.m)

### For a generalized linear model (can't use Normal distribution)

#Poisson distribution example
# Using population trend data for the European Shag from the Living Planet Index
# Import the shagLPI.csv file from the project's directory

shag <- read.csv("shagLPI.csv", header = TRUE)
head(shag)
str(shag)
#transform year from integer to numeric
shag$year <- as.numeric(shag$year)

#histogram
(shag.hist <- ggplot(shag, aes(pop)) + geom_histogram() + theme.clean())

#pop variable is count data, so use Poisson
shag.m <- glm(pop ~ year, family= poisson, data= shag)
summary(shag.m)

#scatterplot
(shag.p <- ggplot(shag, aes(x= year, y= pop)) +
    geom_point(color= "#483D8B") +
    geom_smooth(method= glm, color= "#483D8B", fill = "#483D8B", alpha= 0.6) +
    scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme.clean() +
    labs(x= " ", y= "European shag abundance"))

#paused at a model with binomial distribution!!

# Using a dataset on weevil damage to Scott's pine
# Import the Weevil_damage.csv file from the project's directory


# Poisson distribution

# Binomial distribution