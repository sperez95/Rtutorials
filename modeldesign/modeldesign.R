## Intro to Model Design
# URL: https://ourcodingclub.github.io/tutorials/model-design/index.html

# Load libraries ----
library(tidyverse)  # for data manipulation (tidyr, dplyr), visualization, (ggplot2), ...
library(lme4)  # for hierarchical models
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(MCMCglmm)  # for Bayesian models
library(MCMCvis)  # to visualise Bayesian model outputs
library(stargazer)  # for tables of model outputs


# Load data ----
# Remember to set your working directory to the folder
# where you saved the workshop files
toolik_plants <- read.csv("toolik_plants.csv")

# Inspect data
head(toolik_plants)

#see general structure of data
str(toolik_plants)
#block is a character because it gives more detailed info on where the measurements were taken

#plot needs to be changed from integer to categorical because the numbers represent
#categories, not actual count data
#use mutate() from dplyr to modify columns
#use across() from dplyr to do it for multiple columns
#use as.factor to convert those columns to factors
toolik_plants <- 
  toolik_plants %>%
  mutate(across(c(Site, Block, Plot), as.factor))
str(toolik_plants)

#get the unique site names to look at data structure
unique(toolik_plants$Site)
length(unique(toolik_plants$Site))

#group the dataframe by Site to see how many blocks per site
#creates a tibble that has two columns to describe the data
#block.n is the name of the column made in the tibble
toolik_plants %>% group_by(Site) %>%
  summarise(block.n = length(unique(Block)))

#do the same with plots
toolik_plants %>% group_by(Block) %>%
  summarise(plot.n = length(unique(Plot)))

#look at how many different years are in the dataset
unique(toolik_plants$Year)

#how many species are represented in this dataset? 129!
length(unique(toolik_plants$Species))
#double check that they are all unique species
unique(toolik_plants$Species)
#there actually isnt 129 species, there are some general entries

#filter out all unnecessary entries
#you must list all of the entries you don't want, but using %in% allows you to apply it
#all for the column referenced
toolik_plants <- toolik_plants %>% 
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

#calc how many species were in each plot each year -- species richness
toolik_plants <- toolik_plants %>% 
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species))) %>%
  ungroup()
head(toolik_plants)

#make a histogram of species richness
#putting the code in brackets will automatically plot the histogram without having to call it
(hist <- ggplot(toolik_plants, aes(x=Richness)) +
    geom_histogram() +
    theme_classic())

#make a histogram of plant cover
(hist2 <- ggplot(toolik_plants, aes(x=Relative.Cover)) +
    geom_histogram() +
    theme_classic())
#the plant cover data is skewed to the right, there are a lot of low values

##General Linear Models
#assume there are no random effects
#other assumptions: normal distribution, data points are independent, there is a linear relationship between the variables
#I(Year-2007) transformed the year data by subracting by 2007 so that it starts from year 1-5
plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants)
summary(plant_m)
#think back to the histograms, the data is not normally distributed!

#Model convergence is whether or not the model has worked, whether it has 
#estimated your response variable (and random effects) - basically whether the 
#underlying mathematics have worked or have “broken” in some way.

#plot the model, which gives you:
#1) residuals vs fitted
#2) Q-Q plot
#3) square roots of standardized residuals versus fitted values
#4) a plot of residuals versus leverage that adds bands corresponding to Cook’s distances of 0.5 and 1
plot(plant_m)
#there are definite outliers!!

##Hierarchical models using lme4
#model only with site as a random effect; no temporal replication or plots within blocks
plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data=toolik_plants)
summary(plant_m_plot)
#look at effect sizes, under Fixed effects
#there is an annual decrease of 0.7 species

#add blocks to model
plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data=toolik_plants)
summary(plant_m_plot2)
#effect size has gotten slightly more negative

#add blocks and plot to model
plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data=toolik_plants)
summary(plant_m_plot3)
#This final model answers our question about how plant species richness has changed over time, whilst also 
#accounting for the hierarchical structure of the data.

#check residuals of this model
plot(plant_m_plot3)
#the points are evenly distributed over zero, which means that the assumptions are not violated

#visualize the results
#set a theme for the graphs
set_theme(base = theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units =, "cm")))

#visualize random effects
#see the estimate for our fixed effect: Year
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))
#package glmmTMB is required to plot random effects, but was discontinued

#look at the effect of mean temp on richness, adding year as a random effect
plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data=toolik_plants)
summary(plant_m_temp)

#plot for fixed effect: mean temp
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE))
#confidence interval way wider! high uncertainty about the effect of temp on richness
#assumptions not accounted for: spatial and temporal autocorrelation

##Random slopes vs random intercepts
#how does temp influence species richness? make each plot have its own relationship with temp
plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year), 
                   data=toolik_plants)
summary(plant_m_rs)
#we get certain messages that prove the model is too complicated and will not converge

#simplify the model
plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site) + (1|Year), data=toolik_plants)
summary(plant_m_rs)
#output says the model failed to converge, still too complex
#AND ignore the heirarchical data, combined block and plot

#visualize results
(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))
#CI pretty large

#calculate model predictions and plot them using ggeffects
#First, we calculate the overall predictions for the relationship between species richness and temperature.
#Then, we calculate the predictions for each plot, thus visualising the among-plot variation.
ggpredict(plant_m_rs, terms= c("Mean.Temp")) %>% plot()

#now plot with sites added
ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re") %>% plot() +
  theme(legend.position = "bottom")
#note that the y axis doesn't start at zero, which means the pattern you see is not very true

#plot predictions manually to see the true trends
#this is just for mean temp
predictions <- ggpredict(plant_m_rs, terms = c("Mean.Temp"))
(pred_plot1 <- ggplot(predictions, aes(x, predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=0.1) +
    scale_y_continuous(limits = c(0, 35)) +
    labs(x= "\nMean annual temperature", y= "Predicted species richness\n"))
#now the relationship between temp and richness doesn't look strong anymore!

#repeat, but take the random effect into account
predictions_rs_ri <- ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re")
(pred_plot2 <- ggplot(predictions_rs_ri, aes(x=x, y=predicted, color=group)) +
    stat_smooth(method = "lm", se = FALSE) +
    scale_y_continuous(limits = c(0, 35)) +
    theme(legend.position = "bottom") +
    labs(x= "\nMean annual temperature", y= "Predicted species richness\n"))

#not as zoomed in bc removed the scale limits
(pred_plot2 <- ggplot(predictions_rs_ri, aes(x=x, y=predicted, color=group)) +
    stat_smooth(method = "lm", se = FALSE) +
    theme(legend.position = "bottom") +
    labs(x= "\nMean annual temperature", y= "Predicted species richness\n"))

##Hierarchical models using MCMCglmm
#this package fits generalized linear mixed-effects models using a 
#Markhov chain Monte Carlo approach under a Bayesian statistical framework
#compared to lme4, you can give your model additional info (priors) that is taken in account
#when the model runs
#good modeling for when there are a long of zeros in the data or if it is highly skewed
plant_mcmc <- MCMCglmm(Richness ~ I(Year - 2007), random = ~Site, 
                       family= "poisson", data = toolik_plants)
#get an error to use a stronger prior
#warning messages because we were using tibbles before

#now use block and plot as random intercepts
plant_mcmc <- MCMCglmm(Richness ~ I(Year - 2007), random = ~Block + Plot, 
                       family= "poisson", data = toolik_plants)
summary(plant_mcmc)
#the posterior mean for the Year term is -0.07 (on log scale bc Poisson), which means
#species richness has decreased over time

#check if the model converged! Use trace plots
#look at random effects
plot(plant_mcmc$VCV)
#doesnt look good enough for convergence
#look at fixed effects
plot(plant_mcmc$Sol)
#same output

#estimate changes in the cover of one species: Betula nana
#use Poisson dist, below uses parameter-expanded priors to help the model converge
# Set weakly informative priors
prior2 <- list(R = list(V = 1, nu = 0.002),
               G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                        G2 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                        G3 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000)))

# Extract just the Betula nana data
betula <- filter(toolik_plants, Species == "Bet nan")

betula_m <- MCMCglmm(round(Relative.Cover*100) ~ Year, random = ~Site + Block + Plot,
                     family = "poisson", prior = prior2, data = betula)

summary(betula_m)
#effect size for year is very small for Betula
plot(betula_m$VCV)
plot(betula_m$Sol)
#trace plots look a little better(more like caterpillars), but if you increase iterations
#the models will have improved convergence

#visualize the previous model outputs
MCMCplot(betula_m$VCV)
#if the credible intervals overlap zero, those effects are not significant
MCMCplot(betula_m$Sol)
#Betula nana cover hasn't changed over the years
