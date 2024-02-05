###############################################
##  Project: Textbooks and performance
##  Script Name: EDLD_DARE_2_code.R
##  Author: David D. Liebowitz
##  Date created: 1/24/22
##  Last update: 2/1/24
##  Inputs: EDLD_650_CA_schools_es.dta
###############################################

# Define pretty graphing colors
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

#load necessary packages
library(pacman)

p_load(here, haven, tidyverse, ggplot2, ggthemes, fixest, modelsummary, kableExtra, broom, stargazer, rdrobust)


# I would suggest creating a project for the course. Within this project, create a folder entitled 'assignments' and a sub-folder entitled 'DARE_1'
#In your working folder for the assignment, I would set-up FOUR sub-directories for each of the following:
#data
#code
#figures
#tables

# Place the .dta file from the course website into the data folder
# (You can also, of course, use json to load directly from the course website)

#Set location 
i_am("assignments/DARE_2/code/EDLD_650_DARE_2_code.R")


#import data
full_data <- read_dta(here("assignments/DARE_2/data/EDLD_650_CA_schools_es.dta"))

#######################################################
## (A) Assumption Tests
#######################################################

#view data structure
str(full_data)

####Optional#######################
##Define bin categories for binned data visualizations

# Following, Holden, round to nearest 3
full_data <- full_data %>%
  mutate(bin = plyr::round_any(norm, 3, round))

test <- filter(full_data, bin==0)
View(test)

###Using a bin of 3 lumps schools w/ APIs of -1, 0 and 1 into single bin==0
###Holden sets norm=0 to missing... this seems wrong
###Here, we can put norm=1 in bin 3 and norm=0 and norm==-1 to bin=-3

full_data <- full_data %>%
  mutate(bin=case_when(
    norm== -1 ~ -3, 
    norm== 0 ~ -3,
    norm== 1 ~ 3,
    TRUE ~ bin))

test <- filter(full_data, bin==-3 | bin==0 | bin==3)
View(test)

###############
# QUESTION A1 #
###############

treat <- ggplot() + geom_point(data=subset(full_data, year==2005), aes(norm, receive_williams),
          color=red_pink, alpha=0.6, shape=16, size=1) +
          geom_vline(xintercept = 0, color = slate, linewidth = 1.5, alpha = 0.5) +  
          theme_pander(base_size = 12) + 
          scale_x_continuous("API rank, centered at intended cut-off for Williams (643)") +
          scale_y_continuous("Actual Receipt of Williams")

treat

ggsave(filename="assignments/DARE_2/figures/williams_treat.png", width=15, height=12, units=c("cm"))

###Binned data using mean treatment probability by bin
bin_data <- full_data %>% filter(year==2005) %>% 
            group_by(bin) %>% 
            summarise(mean_rec_williams = mean(receive_williams))
 

bin_treat <- ggplot() + geom_point(data=bin_data, aes(bin, mean_rec_williams),
                               color=red_pink, alpha=0.6, shape=16, size=1) +
                  geom_vline(xintercept = 0, color = slate, linewidth = 1.5, alpha = 0.5) +  
                  theme_pander(base_size = 12) + 
                  scale_x_continuous("API rank, centered at intended cut-off for Williams (643)") +
                  scale_y_continuous("Actual Receipt of Williams")

bin_treat

ggsave(filename="assignments/DARE_2/figures/williams_treat_bin.png", width=15, height=12, units=c("cm"))


###############
# QUESTION A2 #
###############

###Test for bunching to receive grant funding
bunch <- ggplot() + 
         geom_histogram(data=filter(full_data, year==2003), aes(norm), 
         binwidth=3, fill=blue, color=grey_mid, alpha=0.8, boundary=0) +
         geom_vline(xintercept = 0, color = slate, linewidth = 1, alpha = 0.5) +  
         theme_pander(base_size = 12) + 
         scale_x_continuous("API rank, centered at intended cut-off for Williams (643)") +
         scale_y_continuous("Count of schools")

bunch

ggsave(filename="assignments/DARE_2/figures/williams_bunch.png", width=15, height=12, units=c("cm"))


###Test for sorting of schools with different observable characteristics
sort <- ggplot() + 
            geom_quantile(data=filter(full_data, year==2003 & norm<=0), aes(x=norm, y=percentfrl),
            quantiles=0.5, color=purple) + 
            geom_quantile(data=filter(full_data, year==2003 & norm>0), aes(x=norm, y=percentfrl),
            quantiles=0.5, color=red_pink) +
            geom_vline(xintercept = 0, color = slate, linewidth = 1, alpha = 0.5) +  
            theme_pander(base_size = 12) + 
            scale_x_continuous("API rank, centered at intended cut-off for Williams (643)") +
            scale_y_continuous("Avg. Free- and Reduced-Price Lunch (%)")

sort

ggsave(filename="assignments/DARE_2/figures/williams_sort.png", width=15, height=12, units=c("cm"))

####Could do the same for various other demographic characteristics

###Here is one approach that involves binning
binned2 <- full_data %>% filter(year==2003) %>% 
              group_by(bin) %>% summarise(mean_frl = mean(percentfrl))

bin_sort <- ggplot() + 
            geom_point(data=filter(binned2, bin<=0), aes(x=bin, y=mean_frl), color=purple) + 
            geom_point(data=filter(binned2, bin>0), aes(x=bin, y=mean_frl), color=red_pink) +
            geom_vline(xintercept = 0, color = slate, linewidth = 1, alpha = 0.5) +  
            theme_pander(base_size = 12) + 
            scale_x_continuous("API rank, centered at intended cut-off for Williams (643)") +
            scale_y_continuous("Avg. Free- and Reduced-Price Lunch (%)")

bin_sort

ggsave(filename="assignments/DARE_2/figures/williams_bin_sort.png", width=15, height=12, units=c("cm"))


######################################
#  OPTIONAL EXTENSION. QUESTION A3  ##
######################################

descriptives <- select(full_data, c("readingscore", "mathscore", "api_rank", "total", "pct_hi", "pct_wh", "percentfrl", "yrs_teach",
                      "classsize"))

stargazer(as.data.frame(descriptives), type="latex", summary=T, summary.logical=T, summary.stat=c("n", "mean", "sd"),
          covariate.labels = c("Reading Score", "Math Score", "API rank in 2003", "Total enrollment", "Pct. Hisp.", 
          "Pct. White", "Pct. FRL", "Avg. teach. exp", "Avg. class size"),
          digits=2, digit.separator = ",", notes=c("Notes: This table presents school-year means", "and standard deviations for elementary schools", " from 2003-2009."), notes.append = F, 
          title="Descriptive Statistics", out="assignments/DARE_2/tables/descriptives_table.tex")



#######################################################
## (B) Replication and Extension
#######################################################

#Defining subset of data in which to fit estimates
# Visual evidence of outcome discontinuity uses +/- 50
# Formal test uses +/- 19.1 drawn from Holden 2016 (using Calonico, Cattaneo and Titiunik)

graph_sample <- filter(full_data, year>=2005 & norm>=-50 & norm<=50)
ana_sample <- filter(full_data, year>=2005 & norm>-19.1 & norm<19.1)

###############
# QUESTION B1 #
###############



effect <- ggplot() + geom_point(data=graph_sample, aes(x=norm, y=api_mean, color=as.factor(ind))) +
  geom_vline(xintercept = 0, color = slate, linewidth = 1, alpha = 0.5) +  
  theme_pander(base_size = 12) + 
  scale_x_continuous("API rank, centered at intended cut-off for Williams (643)") +
  scale_y_continuous("Mean, standardized scale score") + 
  scale_color_manual(values = c(purple, red_pink), name="Receive Williams")
effect

ggsave(filename="assignments/DARE_2/figures/main_RD_effect.png", width=15, height=12, units=c("cm"))

###Preferrable do it with binned data

binned3 <- filter(full_data, year>=2005 & norm>=-50 & norm<=50) %>% group_by(bin) %>% 
  summarise(across(c("api_mean", "ind"), mean))

bin_effect <- ggplot() + geom_point(data=binned3, aes(x=bin, y=api_mean, color=as.factor(ind))) +
  geom_vline(xintercept = 0, color = slate, linewidth = 1, alpha = 0.5) +  
  theme_pander(base_size = 12) + 
  scale_x_continuous("API rank, centered at intended cut-off for Williams (643)") +
  scale_y_continuous("Mean, standardized scale score") + 
  scale_color_manual(values = c(purple, red_pink), name="Receive Williams")

bin_effect

ggsave(filename="assignments/DARE_2/figures/main_RD_effect_bin.png", width=15, height=12, units=c("cm"))


###############
# QUESTION B2 #
###############

rdd1 <- lm(average_score ~ ind + norm, ana_sample)
rdd2 <- lm(average_score ~ ind + norm + ind_norm, ana_sample)
rdd3 <- lm(average_score ~ ind + norm, filter(full_data, year>=2005 & norm>=-25 & norm<=25))
rdd4 <- lm(average_score ~ ind + poly(norm, 2), ana_sample)

stargazer(rdd1, rdd2, rdd3, rdd4, type='latex', out="assignments/DARE_2/tables/RD_results.tex", dep.var.caption="", dep.var.labels = "", 
          column.labels =c("Linear, same slope", "Linear, diff slope", "Linear (+/- 25 API)", "Quadratic"),
          omit.stat = c("ser", "adj.rsq", "f"), notes.append=F, 
          notes = c("*p$<$0.05, **p$<$0.01, ***p$<$0.001. Cells report coefficients and associated standard errors. Estimates pool years from 2005 to 2009."),
          star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", omit=c("Constant"),
          covariate.labels=c("Receive Williams", "API Rank", "Receive Williams $\\times$ API Rank", "API Rank", "API Rank sq."), 
          title="Regression discontinuity estimates of the effects of instructional material funding on math/reading test scores")



###############
# QUESTION B3 #
###############

##In written response



########################
# OPTIONAL QUESTION B4 #
########################

##Incorporated in B2 above