#R tutorial on functions from Youtube
# https://www.youtube.com/watch?v=Q2hOBOm8x1c

##template for functions
#functiontut <- function(inputs) {
#  outputvalue <- dosomething(inputs)
#  return(outputvalue)
#}
#the brackets in a function indicate that the code within them run as a group

#example

calc_shrub_vol <- function(length, width, height) {
  volume <- length*width*height
  return(volume)
}

#creates the function in the environment

calc_shrub_vol(0.8, 1.6, 2.0)

#create default argument

calc_shrub_vol <- function(length=1, width=1, height=1) {
  volume <- length*width*height
  return(volume)
}
calc_shrub_vol()

#if you add one of the elements into the function, it overwrites the default value given before
calc_shrub_vol(width=2)
#will overwrite if you put all 3 values in the same order as the function
calc_shrub_vol(0.8, 1.6, 2.0)

#combine functions
est_shrub_mass <- function(volume) {
  mass <- 2.65 * volume^0.9
}

shrub_volume <- calc_shrub_vol(0.8, 1.6, 2.0)
shrub_mass <- est_shrub_mass(shrub_volume)

#use pipes with our functions
library(dplyr)

shrub_mass <- calc_shrub_vol(0.8, 1.6, 2.0) %>%
  est_shrub_mass()

#nesting functions, usually don't do more than 2 nests

shrub_mass <- est_shrub_mass(calc_shrub_vol(0.8, 1.6, 2.0))

#call functions from inside other functions

est_shrub_mass_dim <- function(length, width, height) {
  volume <- calc_shrub_vol(length, width, height)
  mass <- est_shrub_mass(volume)
  return(mass)
}

#at the bottom of the script window there is a button "(Top Level)" that lists what functions you have made

est_shrub_mass_dim(0.8, 1.6, 2.0)
