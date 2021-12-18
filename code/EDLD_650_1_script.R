######################################################
## Script Name: EDLD_650_1_script.R
## Project Title: EDLD 650 Winter 2022
## Author: David Liebowitz
## Created: 12/17/21
## Last update: 12/17/21
## Purpose: this script imports the SfA data, does light variable cleaning and estimates the effects of randomized assignment of the 
##                      school-level treatment of SfA. To do so, it fits a multi-level random effects model to adjust for school-level differences in achievement
## Inputs: ch7_sfa.dta           

######################################################
# Load necessary packages

library(pacman)

# These are the packages you will need for the analyses 
p_load(here, tidyverse, lme4, haven)

# You will want to have created a folder for the course and an R project within that folder
# Then, create a folder within your course folder entitled "code"
# Finally, save this R script within the code folder

# This command tells R where your script is and allows you to point it to other folders within the directory
i_am("code/EDLD_650_1_script.R")


# Load the data. In this case, it is in .dta format which is the data file format for Stata 
ch7_sfa <- read_dta(here("data/ch7_sfa.dta"))

# View the head (or tail) of the data 
head(ch7_sfa, n=3)
tail(ch7_sfa, n=3)

# We can also get a sense of the data structure
str(ch7_sfa)

## SfA and school IDs are currently defined as double (numeric); in fact, these describe categories, so we want to convert them to factor variables

# Convert to factor variables
ch7_sfa$sfa <- as_factor(ch7_sfa$sfa)
ch7_sfa$schid <- as_factor(ch7_sfa$schid)


# Estimate a random-effects model (includes both a fixed-effect component, sfa, and a random-effect component, schid)
sfa <- lmer(wattack ~ sfa + (1 | schid), data=ch7_sfa)
summary(sfa)