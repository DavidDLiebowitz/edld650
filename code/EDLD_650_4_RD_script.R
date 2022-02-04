######################################################
## Script Name: EDLD_650_4_RD_script.R
## Project Title: EDLD 650 Winter 2022
## Author: David Liebowitz
## Created: 1/24/22
## Last update: 2/4/22
## Purpose: This script imports the Angrist and Lavy data and does light variable cleaning. It conducts descriptive analysis to test the 
##          big three assumptions for RD and then estimates the intent-to-treat (ITT) effects of being assigned to a small class 
## Inputs: ch9_angrist.dta 
######################################################
# Load necessary packages

# Define graphing colors
red_pink <- "#e64173"
turquoise <- "#20B2AA"
orange <- "#FFA500"
red <- "#fb6107"
blue <- "#3b3b9a"
green <- "#8bb174"
grey_light <- "grey70"
grey_mid <- "grey50"
grey_dark <- "grey20"
purple <- "#6A5ACD"
slate <- "#314f4f"

library(pacman)

# These are the packages you will need for the analyses 
p_load(here, tidyverse, DT, ggplot2, xaringan, knitr, kableExtra, modelsummary, stargazer, xaringanthemer, gganimate, ggthemes, fixest, haven)

# You will want to have created a folder for the course and an R project within that folder
# Then, create a folder within your course folder entitled "code"
# Finally, save this R script within the code folder

# This command tells R where your script is and allows you to point it to other folders within the directory
i_am("code/EDLD_650_4_RD_script.R")

####################################
## The Angrist and Lavy Maimonides' Rule data
#####################################

# Read the data in; again in .dta format
maimonides <- read_dta(here("data/ch9_angrist.dta"))
d <- select(maimonides, 
            read, size, intended_classize, observed_classize)

# Examine summary statistics
summary(d)

# Calculate particular summary statistics
sapply(d, sd, na.rm=TRUE)


##################################
## "The Big Three (plus one)"
###################################


#################################
### 0. Examine variation in treatment likelihood by forcing variable
###    Does the forcing variable predict (some) change in the probability of treatment?

treat <- ggplot() +
  geom_jitter(data=maimonides, aes(x=size, y=observed_classize), color=grey_mid,alpha=0.4, shape=16) + 
  geom_line(data=maimonides, aes(x=size, y=intended_classize), color=red_pink, linetype="dashed", size = 1.5) +
  theme_pander(base_size = 18) + scale_x_continuous("Cohort size") +
  scale_y_continuous("Class Size")

treat


################################
### 1. Examine whether there is evidence of manipulation at the discontinuity
###    Bunching: examine whether there is bunching at the discontinuity (density of observations aroudn discontinuity)


# Zoom in on just the first cut (above/below 40)
d <- filter(maimonides, size>=29 & size<=53)

# Examine bunching
# Could do this with several other plots, including stat_count
bunch <- ggplot() +
  geom_histogram(data=d, aes(size), fill=blue, binwidth = 1)  +
  geom_vline(xintercept = 40.5, color = slate, size = 1.5, alpha = 0.5) +       
  theme_pander(base_size = 18) + 
  scale_x_continuous("Cohort size", breaks=seq(29, 53, by = 1)) 
bunch


###################################
### 2. Manipulation:
###    Examine whether pre-treatment characteristics differ around the policy discontinuity (binned scatterplot of covariates around discontinuity)

# Differences in Family SES
sort <- ggplot() +
  geom_boxplot(data=d, aes(x=as.factor(size), y=ses), fill=red_pink, alpha=0.4) +
  theme_pander(base_size = 20) +
  xlab("Cohort size") + scale_y_continuous("Family income")

sort

# Another way
quantile <- ggplot() +
  geom_quantile(data=filter(d, size<41), aes(size, ses), quantiles=0.5, color=purple) + 
  geom_quantile(data=filter(d, size>=41), aes(size, ses), quantiles=0.5, color=red_pink) +
  geom_vline(xintercept = 40.5, color = slate, size = 1.5, alpha = 0.5) +
  theme_pander(base_size = 18) +
  xlab("Cohort size") + scale_y_continuous("Family income") + expand_limits(y=c(-3,3))

quantile

##################################
### 3. Does treatment predict change in the outcome in some discontinuous fashion?
###    (binned scatterplot of outcome against forcing variable)

fx <- ggplot() +
  geom_point(data=d, aes(x=size, y=read), color=blue, alpha=0.8, shape=16) +
  geom_vline(xintercept = 40.5, color = slate, size = 1.5, alpha = 0.5) +
  theme_pander(base_size = 18) +
  xlab("Cohort size") + ylab("Verbal score") 

fx

### Sort of hard to see here....


# Create grouping category called "small" for better visualization
d <- d %>% mutate(small = ifelse(size >= 41,TRUE,FALSE))

fx2 <- ggplot() + 
  geom_point(data=d, aes(x=size, y=read, colour=small), alpha=0.8, shape=16) +
  geom_vline(xintercept = 40.5, color = slate, size = 1.5, alpha = 0.5) +
  theme_pander(base_size = 18) + xlab("Cohort size") + ylab("Verbal score") + 
  scale_color_manual(values = c(purple, red_pink)) +
  theme(legend.position = "none")

fx2

## Bin outcome by cohort size (this takes the mean of the outcome at each value of the forcing variable)
bin <- d %>% group_by(size) %>% 
     summarise(across(c("read", "small"), mean))

# Note that you might also want to round to more discrete values on the forcing variable.
# We don't do this with the Angrist data, but an optional extension activity for the DARE is to do this

d <- d %>%
  mutate(bin = plyr::round_any(size, 3, round))


binned_plot <- ggplot() + 
  geom_point(data=bin, aes(x=size, y=read, colour=as.factor(small)), alpha=0.8, shape=16, size=3) +
  geom_vline(xintercept = 40.5, color = slate, size = 1.5, alpha = 0.5) + 
  theme_pander(base_size = 18) + xlab("Cohort size") + ylab("Verbal score") + 
  scale_color_manual(values = c(purple, red_pink)) +
  expand_limits(y=c(35,90)) +
  theme(legend.position = "none")

binned_plot

## Include fitted lines on the binned plot
## You can easily do this as part of the ggplot functionality on the raw data
## But need to use this approach when plotting the binned data and the estimation using the full underlying data
## Which is what you'll eventually want to do


#The predicted values approach:
lm_tmp <- lm(read ~ size + I(size>40.5), data = d)
lm_fun <- function(size) predict(lm_tmp, data.frame(size = size))

binned_plot +
  stat_function(
    data = data.frame(x = c(29, 53)),
    aes(size = size),
    fun = lm_fun,
    xlim = c(29,40.4),
    color = purple,
    size = 1.5
  ) +
  stat_function(
    data = data.frame(x = c(29, 53)),
    aes(size = size),
    fun = lm_fun,
    xlim = c(40.6,53),
    color = red_pink,
    size = 1.5)

## Diferent slopes

fx2 + 
  geom_smooth(data=d, aes(x=size, y=read, colour=small), method = lm, se=FALSE)

## Different bandwidth

fx2 + 
  geom_smooth(data=filter(d, size>=36 & size<=46), aes(x=size, y=read, colour=small), method = lm, se=FALSE)

## Include confidence interval shading
fx2 + 
  geom_smooth(data=d, aes(x=size, y=read, colour=small), method = lm, se=TRUE)

## Allow for non-linear (quadratic) forcing variable trends
fx2 + 
  geom_smooth(data=d, aes(x=size, y=read, colour=small), method = lm, formula=y ~ poly(x,2), se=TRUE)


#######################
### Estimate RD in regression framework

# Again, note that you can use lots of different table packages and export to various different formats

linear_const <- lm(read ~ size + I(size>40.5), d)
linear_diff <- lm(read ~ size * I(size>40.5), d)
quadratic <- lm(read ~ poly(size,2) + I(size>40.5), d)

modelsummary(list(linear_const, linear_diff, quadratic), 
             stars=T,
             coef_rename = c("(Intercept)" = "Intercept", "size" = "Intended size", "I(size > 40.5)TRUE" = "Intended small class",
                             "size:I(size > 40.5)TRUE" = "Size x Small", 
                             "poly(size, 2)1" = "Intended size", "poly(size, 2)2" = "(Intended size)^2"),
             gof_omit = "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std|F"
            )

