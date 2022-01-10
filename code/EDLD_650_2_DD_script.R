######################################################
## Script Name: EDLD_650_2_DD_script.R
## Project Title: EDLD 650 Winter 2022
## Author: David Liebowitz
## Created: 1/7/21
## Last update: 1/7/21
## Purpose: This script imports the Dynarski data, does light variable cleaning and estimates the effects of the offer of financial odd
##            on college-going outcomes in a difference-in-differences framework.
##            It also imports the Liebowitz data, estimates the main effects of the end of desegregation, provides examples
##            for how to create relative-time policy variables, conduct assumption tests and estimate an event study
## Inputs: ch8_dynarski.dta | edld_650_class_2_eepa_data.dta

######################################################
# Load necessary packages

library(pacman)

# These are the packages you will need for the analyses 
p_load(here, tidyverse, fixest, DT, modelsummary, haven)

# You will want to have created a folder for the course and an R project within that folder
# Then, create a folder within your course folder entitled "code"
# Finally, save this R script within the code folder

# This command tells R where your script is and allows you to point it to other folders within the directory
i_am("code/EDLD_650_2_DD_script.R")


########################################################
###           Dynarski, two-period DD
########################################################

# Read the data in; again in .dta format
dynarski <- read_dta(here("data/ch8_dynarski.dta"))


# Viewing the data

head(dynarski)

#Another way of viewing the data
datatable(dynarski[,c(1, 4:8)], fillContainer = FALSE, options = 
            list(pageLength = 7))


# See a few summary statistics
x <- select(dynarski, coll, hgc23, fatherdec, offer)
summary(x)

# Are there missing values of coll?
sum(is.na(coll))

# Convert coll and fatherdec from a numeric dummy to a factor variable
dynarski$fac_coll <- as.factor(dynarski$coll)
dynarski$fac_coll <- factor(dynarski$fac_coll, levels = c(0,1),labels=c("No College", "College"))
dynarski$fac_fatherdec <- as.factor(dynarski$fatherdec)
dynarski$fac_fatherdec <- factor(dynarski$fac_fatherdec, levels=c(0,1), labels=c("Father not deceased", "Father deceased"))

# Create a two-way table
college <- table(dynarski$fac_fatherdec, dynarski$fac_coll)
college


# Plot highest grade completed in a ggplot histogram
hg <- ggplot(dynarski, aes(hgc23)) + geom_histogram(binwidth=1) 
hg + scale_x_continuous(name="Highest-grade completed at 23", 
                        breaks=c(10, 12, 14, 16, 18, 20))

# Create summary statistics table
# Note that you can change the type to export the table into different formats
# There are lots of different table packages, and I admit I have not found one for summary statistics that is as straightforward as I would like

stargazer(as.data.frame(x), type="html", summary=T, summary.logical=T, summary.stat=(c("n", "mean", "sd")),
          covariate.labels = c("Attend college at 23", "Years schooling at 23", "Father deceased", "Offer"),
          notes="Notes: This table presents unweighted means and standard deviations from the NLSY poverty and random samples used in the                     Dynarski (2003) paper.",
          digits=2, notes.append = F, title="Table 1. Descriptive Statistics")

###############
### Create a summary of descriptive evidence on changes in outcome relative to the policy

#Switch designation given that offer is actually earlier in time
dynarski <- mutate(dynarski, post = ifelse(offer==1,0,1))

dd <- ggplot(dynarski, aes(post,coll)) + aes(color=factor(fatherdec)) + 
  stat_summary(fun=mean, geom="line") + labs(y="Attend college (%)", 
                                             color="Father Deceased") + scale_x_discrete(name="Year", 
                                                                                         breaks=c(0, 1), limits=c(0, 1), labels=c("0"= "pre-1981", "1"= "post-1981")) +
  expand_limits(y=c(0.35, 0.6), x=c(-0.25, 1)) +
  annotate("text", x = 1, y = 0.45, label = "NOT deceased father", color = "#F8766D", size = 6) +
  annotate("text", x = 1, y = 0.39, label = "DECEASED father", color = "#00BFC4", size = 6) +
  theme_pander(base_size = 20) +
  theme(legend.position = "none") 

dd

#####################
#### Estimate classic two-period DD

lm(coll ~ fatherdec*offer + fatherdec + offer, data=dynarski, 
   weights=dynarski$wt88)

# Look at what `lm` stores as output
est_dynarski %>% names()

# One way to look at output
est_dynarski %>% tidy()

# The summary command (from the `broom` package, which is part of the tidyverse) is your workhorse for viewing regression output
summary(est_dynarski)

# Outputting to a table; again, you can change the type
stargazer(est_dynarski, type='html')


####################################################################################
####          Liebowitz, staggered implementation panel DD with assumption checks and event study framework

# Read the data in
desegregation <- read_dta(here("data/edld_650_class_2_eepa_data.dta"))

# View the data
datatable(desegregation[c(7:9, 13:15,319:321),c(1:3, 7, 10, 24)], 
              fillContainer = FALSE, height=175, options = list(pageLength = 9))



# Estimating a two-way fixed effect model (note that these models all use WLS, which is non-typical)

ols_unitary1 <- feols(sd_dropout_prop_b ~ unitary |                          ## this is the main model with outcome and predictors
                      year + leaid,                                          ## after the | you place the fixed effects
                      data=desegregation,                                    ## designate data source as usual
                      vcov = "iid", weights=desegregation$sd_t_1619_b)       ## this first specification, for pedagogical purposes, specifies errors as uncorrelated (iid)
summary(ols_unitary1)

# Without specifying the clustering variable, the default is to cluster on the first fixed effect listed
ols_unitary2 <- feols(sd_dropout_prop_b ~ unitary | leaid + year, 
                      data=desegregation, 
                      weights=desegregation$sd_t_1619_b)

# I've specified the clustering approaches in the slides by doing them directly in the models for pedagogical purposes
# In fact, one of the amazing things about `fixest` is that you can change the inference approach post-estimation
# This allows you to cluster in multiple different ways after the fact and speeds up the initial estimation in large data sets substantially

summary(ols_unitary2, vcov = "cluster")
summary(ols_unitary2, cluster = ~ leaid^year)
summary(ols_unitary2, vcov = "twoway")          # OR
summary(ols_unitary2, cluster = ~ leaid+year)

# Here's our preferred time-invariant DD specification
ols_unitary3 <- feols(sd_dropout_prop_b ~ unitary | leaid + year, 
                      data=desegregation, 
                      vcov = ~ leaid^year,
                      weights=desegregation$sd_t_1619_b)

# Switching to dynamic treatment effects and tests of parallel trends

# Create a "relative time" variable, centered at the year of the policy change for each unit
desegregation <- desegregation %>%
  mutate(run_time = case_when(
    !is.na(yrdiss) ~ (year - yrdiss),
    is.na(yrdiss) ~ -1 ## <-- Common practice is to set units that never experience the policy change to -1 (last year before implementation)
  ))                    ##    and to have this be the reference category. It doesn't really matter which relative-time year you choose.
                        ##    What's important is that the year fixed effects, combined with no variation in relative time for never-treated units
                        ##    mean that they do not contribute to the estimates of the year-specific effects but serve as a reference category
                        ##    for each of these other estimates.

summary(desegregation$run_time)


# Testing for parallel trends and allowing effects to differ linearly across years
ols_unitary_run <- feols(sd_dropout_prop_b ~ unitary + 
                           unitary:run_time + run_time | 
                           year + leaid, data=desegregation, 
                           vcov = ~leaid^year, weights=desegregation$sd_t_1619_b)

summary(ols_unitary_run)

# Add in an exogenous covariate (determined prior to any policy changes)

# Note that this value is time-invariant, so we interact it with each of the year indicators
desegregation$black90_yr <- desegregation$per_black_90 * desegregation$year

# Re-estimate main DD w covariate
ols_unitary4 <- feols(sd_dropout_prop_b ~ unitary + black90_yr | leaid + year, 
                      data=desegregation, 
                      vcov = ~ leaid^year,
                      weights=desegregation$sd_t_1619_b)


ols_unitary_run2 <- feols(sd_dropout_prop_b ~ unitary + unitary:run_time + run_time + black90_yr | year + leaid, 
                         data=desegregation, 
                         vcov = ~leaid^year, weights=desegregation$sd_t_1619_b)


results <- list()
results[['1']] <- ols_unitary3
results[['2']] <- ols_unitary4
results[['3']] <- ols_unitary_run2

# Add a row to indicate the inclusion of covariates
row <- tribble(~term,          ~'1',  ~'2', ~'3',
               'Covariates?', '',  'X', 'X')
attr(row, 'position') <- c(7)

modelsummary(results, 
             title = "Table 2. Effects of end of school desegregation on black dropout rate",
             stars=T,
             coef_omit = c("black90_yr"),
             coef_rename = c("unitary" = "Unitary status", "run_time" = "Pre-trend", "unitary:run_time" = "Unitary x Relative-Year"),
             estimate = "{estimate}{stars}",
             gof_omit= "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std",
             add_rows = row,
             threeparttable= T,
             notes = c("Notes: $^{+}p<0.1, ^{*}p<0.05, ^{**}p<0.01, ^{***}p<0.001$. The table displays coefficients from Equation X and district-by-year clustered standard errors in parentheses. All models include fixed effects for year and district. Models 2 and 3 adjust for the proportion of 16-19 year-olds residing in the district in 1990 who were Black, interacted with year."),
             type='html')


#############
### Event-study

# Create year-specific relative time indicators
# In this case, for statistical power, we've aggregated a few years at a time

desegregation <- mutate(desegregation, r_10minus=ifelse(run_time<=-10,1,0))
desegregation <- mutate(desegregation, r_10plus=ifelse(run_time>=10, 1, 0))

desegregation <- mutate(desegregation, r_7to9minus=case_when(
  run_time==-7 ~ 1,
  run_time==-8 ~ 1,
  run_time==-9 ~ 1,
  TRUE ~ 0))
desegregation <- mutate(desegregation, r_6to4minus=case_when(
  run_time==-6 ~ 1,
  run_time==-5 ~ 1,
  run_time==-4 ~ 1,
  TRUE ~ 0))
desegregation <- mutate(desegregation, r_3to2minus=case_when(
  run_time==-3 ~ 1,
  run_time==-2 ~ 1,
  TRUE ~ 0))
desegregation <- mutate(desegregation, r_1minus = case_when(
  run_time==-1 ~ 1,
  is.na(run_time) ~ 1,
  TRUE ~ 0))
desegregation <- mutate(desegregation, r_0to2plus=case_when(
  run_time==0 ~ 1,
  run_time==1 ~ 1,
  run_time==2 ~ 1,
  TRUE ~ 0))
desegregation <- mutate(desegregation, r_3to5plus=case_when(
  run_time==3 ~ 1,
  run_time==4 ~ 1,
  run_time==5 ~ 1,
  TRUE ~ 0))
desegregation <- mutate(desegregation, r_6to9plus=case_when(
  run_time==6 ~ 1,
  run_time==7 ~ 1,
  run_time==8 ~ 1,
  run_time==9 ~ 1,
  TRUE ~ 0))

# Estimate the event study
event_study <- feols(sd_dropout_prop_b ~ r_10minus + r_7to9minus + r_6to4minus +
                       r_3to2minus + r_0to2plus + r_3to5plus + r_6to9plus + 
                       r_10plus | year + leaid, data=desegregation,
                     vcov = ~leaid^year, weights=desegregation$sd_t_1619_b)
summary(event_study)

# Plot the coefficients and standard errors
modelplot(event_study, vcov=~leaid^year) +
  theme_pander(base_size=16) + geom_hline(aes(yintercept = 4.75), col="red", lty="dashed") +
  geom_vline(aes(xintercept=0), col="blue", lty="dashed") +
  geom_point(aes(x=0, y=4.5), size=2) +
  labs(x="Change in Hispanic Dropout Rate", y="Relative Year", title="Figure XX. Event study of effects of end of school desegregation \n on the Black dropout rate") +
  scale_y_discrete(labels=c("-10 plus", "-7to-9", "-6to-4", "-3to-2", "Unitaryto+2", "+3to+5", "+6to+9", "10 plus")) + 
  coord_flip()

# Note, that we would not typically plot/interpret the extreme values -10 plus / 10 plus because these values are censored for some
# units and so represent both treatment effects AND compositional changes. It's fine to include them in your model, though
