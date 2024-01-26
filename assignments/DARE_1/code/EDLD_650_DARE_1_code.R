#load necessary packages
library(pacman)

p_load(here, tidyverse, haven, fixest, modelsummary, kableExtra, broom, reshape2, stargazer)


# I would suggest creating a project for the course. Within this project, create a folder entitled 'assignments' and a sub-folder entitled 'DARE_1'
#In your working folder for the assignment, I would set-up FOUR sub-directories for each of the following:
#data
#code
#figures
#tables

# Place the .xlsx file from the course website into the data folder

#Set location 
i_am("assignments/DARE_1/code/EDLD_650_DARE_1_code.R")


# Table format global option
options(modelsummary_format_numeric_latex = "plain")


#import data
d <- read_csv(here("assignments/DARE_1/data/EDLD_650_DARE_1.csv"))

  # while you can, of course, read the file directly from the web, there are several reasons why it is useful to have any data that you are using stored locally (when practical)


#######################################################
## Data Management ## (A)
#######################################################

#view data structure
str(d)
# head
head(d)


###############
# QUESTION A1 #
###############


#Create race proportions
for(i in 1:length(colnames(d))) {
  if(grepl("enroll_", colnames(d)[i])) {
    d[paste(colnames(d)[i],"_prop", sep="")] <-
      d[i]/d$enroll
  }
}




###############
# QUESTION A2 #
###############

##Generating policy predictor variables. 
#This will be dichotomous variables for: 
  

#eval_year 
d <- d %>% 
  mutate(eval = case_when(
    is.na(eval_year) ~ 0,
    school_year>=eval_year ~ 1,
    TRUE ~ 0
    ))

#class_remove_year
d <- d %>% 
  mutate(class_remove = case_when(
  is.na(class_remove_year) ~ 0,
  school_year>=class_remove_year ~ 1,
  TRUE ~ 0
  ))

# suspension_year 
d <- d %>% 
  mutate(suspension = case_when(
  is.na(suspension_year) ~ 0,
  school_year>=suspension_year ~ 1,
  TRUE ~ 0
  ))

#create centered time variable, setting never-eval to -1
d <- d %>% 
  mutate(run_time= case_when(
    is.na(eval_year) ~ -1, 
    !is.na(eval_year) ~ (school_year-eval_year) 
    ))

# In this case, I'm going to collapse periods more than 6 years pre and 3 years post into a single group
# There is some debate about whether this is the right thing to do
d <- d %>%
    mutate(run_time = case_when(
      run_time<=-6 ~ -6,
      run_time>=3 ~ 3,
      TRUE ~ run_time
      ))

#create eval by year
d <- d %>% 
  mutate(evalXyear=eval*run_time)

#######################################################
## Understanding the Data and Descriptive Statistics
#######################################################

###############
# QUESTION B1 #
###############

##look at data summary to examine missingness
summary(d)

##There are 516 total state-year observations

#pull out those that have missing values we are concerned about
data_eval_na <- d %>% 
  filter(is.na(enroll) | is.na(ODR_class) | is.na(run_time))
#can explore those 46 obs to understand the common phenomenon they share
#some state-years exist in which no schools are present in those states

#Here, the missingness is driven entirely by the fact that in some years of the sample, there are no schools in this set of 46 state-years that report ODR outcomes/enrollment 

#I drop those state-years within missing enrollment or outcome data

d <- filter(d, !is.na(ODR_class) & !is.na(enroll))

##This results in a final analytic sample of 470 state-year observations

###############
# QUESTION B2 #
###############

# Can display univariate data most effectively with histograms or density plots
#Here I use the melt command to create faceted plots of all four outcomes in a straightforward way. 
#You could also plot each outcome separately and combine them into a paneled figure

# select the outcome data to plot
ODR_graph <- d %>% 
  select(c(ODR_class, ODR_other, ODR_subjective, ODR_objective))


# Only need to select one of hist or kdensity
# I'm just getting a basic feel for the data, these won't appear in any publication, so not worrying about formatting

ggplot(melt(ODR_graph),aes(x=value)) +
  geom_histogram(bins = 100) + 
  facet_wrap(~variable) + labs(y="Count", x="ODR rate per 500") 

ggsave(filename="assignments/DARE_1/figures/odr_hist.png", width=15, height=12, units=c("cm"))

ggplot(melt(ODR_graph), aes(x=value)) +
  geom_density() + 
  facet_wrap(~variable)

ggsave(filename="assignments/DARE_1/figures/odr_kdensity.png", width=15, height=12, units=c("cm"))



###############
# QUESTION B3 #
###############


#Make descriptive table

#Create dataframe to construct descriptive table
descriptives <- d

#This code can be made into a function, but want to make explicitly clear what was happening
descriptives <- mutate(descriptives, temp_FRPL=FRPL_percent*enroll)
descriptives <- mutate(descriptives, temp_AM=enroll_AM_prop*enroll)
descriptives <- mutate(descriptives, temp_ASIAN=enroll_ASIAN_prop*enroll)
descriptives <- mutate(descriptives, temp_BLACK=enroll_BLACK_prop*enroll)
descriptives <- mutate(descriptives, temp_WHITE=enroll_WHITE_prop*enroll)
descriptives <- mutate(descriptives, temp_HISP=enroll_HISP_prop*enroll)
descriptives <- mutate(descriptives, temp_ODR_class=ODR_class*enroll)
descriptives <- mutate(descriptives, temp_ODR_other=ODR_other*enroll)
descriptives <- mutate(descriptives, temp_ODR_subjective=ODR_subjective*enroll)
descriptives <- mutate(descriptives, temp_ODR_objective=ODR_objective*enroll)

descriptives <- descriptives %>% group_by(school_year) %>% mutate(state_year_enroll=mean(enroll))
descriptives <- descriptives %>% group_by(school_year) %>% mutate(year_enroll=sum(enroll))

descriptives <- mutate(descriptives, weight_FRPL=sum(temp_FRPL)/year_enroll)
descriptives <- mutate(descriptives, weight_AM=sum(temp_AM)/year_enroll)
descriptives <- mutate(descriptives, weight_ASIAN=sum(temp_ASIAN)/year_enroll)
descriptives <- mutate(descriptives, weight_BLACK=sum(temp_BLACK)/year_enroll)
descriptives <- mutate(descriptives, weight_WHITE=sum(temp_WHITE)/year_enroll)
descriptives <- mutate(descriptives, weight_HISP=sum(temp_HISP)/year_enroll)
descriptives <- mutate(descriptives, weight_ODR_class=sum(temp_ODR_class)/year_enroll)
descriptives <- mutate(descriptives, weight_ODR_other=sum(temp_ODR_other)/year_enroll)
descriptives <- mutate(descriptives, weight_ODR_subjective=sum(temp_ODR_subjective)/year_enroll)
descriptives <- mutate(descriptives, weight_ODR_objective=sum(temp_ODR_objective)/year_enroll)

descriptives <- select(descriptives, state_year_enroll, year_enroll, weight_FRPL, weight_AM, weight_ASIAN, weight_BLACK, weight_WHITE, 
                       weight_HISP, PBIS, weight_ODR_class, weight_ODR_other, weight_ODR_subjective, weight_ODR_objective)

descriptives <- select(ungroup(descriptives), -c(school_year))
ungroup(descriptives) 


 stargazer(as.data.frame(descriptives), type="latex", summary=T, summary.logical=T, summary.stat=c("n", "mean", "sd"),
           covariate.labels=c("Mean State-Year Enrollment", "Mean Yearly Enrollment", "Pct. low-income", "Pct. American Indian/Native AK",
                              "Pct. Asian/Pacific-Islander", "Pct. Black", "Pct. White Non-Hispanic", "Pct. Hispanic", "Pct. States by year Implementing PBIS",
                              "Daily Referalls per 500 students - Classroom", "Daily Referalls per 500 students - Other", "Daily Referalls per 500 students - Subjective", 
                              "Daily Referalls per 500 students - Objective"),
           digits=2, digit.separator = ",", notes="Notes: This table presents state-year means and standard deviations from 2006-2018.", notes.append = F, 
           title="Summary statistics on School-Wide Information System data, 2006-2017",
           label = "tab:descriptives",
           out="assignments/DARE_1/tables/descriptives_table.tex")



########################
# OPTIONAL QUESTION B4 #
########################



#make a rate variable 
#first make a variable to capture the total number of students who received ODRs
d <- d %>% 
  group_by(run_time) %>% 
  mutate(num_ODR_class = sum(ODR_class*enroll))

d <- d %>% 
  group_by(run_time) %>% 
  mutate(num_ODR_subj = sum(ODR_subjective*enroll))

d <- d %>% 
  group_by(run_time) %>% 
  mutate(den_ODR_class = sum(enroll))

d <- d %>% 
  group_by(run_time) %>% 
  mutate(den_ODR_subj = sum(enroll))

d <- d %>% 
  group_by(run_time) %>% 
  mutate(year_ODR_class = num_ODR_class/den_ODR_class)

d <- d %>% 
  group_by(run_time) %>% 
  mutate(year_ODR_subj = num_ODR_subj/den_ODR_subj)

#total yearly enrollment
d <- d %>% 
  group_by(school_year) %>% 
  mutate(year_enroll = sum(enroll))

#now make that an ev_eval dataset 

#create temp ever-eval var [b/c of missing eval vars] 
ev_eval <- d %>% 
  mutate(ever_eval_temp = case_when(
    is.na(eval) ~ 0,
    eval==1 ~ 1, 
    eval==0 ~ 0)
  )

#now take the max of that (1 or 0)
ev_eval <- ev_eval %>% 
  group_by(state_id) %>% 
  mutate(ever_eval = max(ever_eval_temp)) 

#add in a variable of average number of state-year averages of ODRs [classroom and classroom subjective]
ev_eval <- ev_eval %>% 
  group_by(state_id, run_time) %>% 
  mutate(avg_odr_class = mean(ODR_class)) %>% 
  mutate(avg_odr_subj = mean(ODR_subjective))



#summary graph   
ggplot(data=ev_eval) +
  stat_summary(aes(x=run_time, y=year_ODR_class, color = "Class"), fun = mean, geom = "point", show.legend=F) + 
  stat_summary(aes(x=run_time, y=year_ODR_class, color="Class"), fun = mean, geom = "line") + 
  stat_summary(aes(x=run_time, y=year_ODR_subj, color="Subjective"), fun = mean, geom = "point", show.legend=F) +
  stat_summary(aes(x=run_time, y=year_ODR_subj, color="Subjective"), fun = mean, geom = "line") +
  annotate("text", x = 2.3, y = 1.3, label = "Class", color = "#F8766D", size = 5) +
  annotate("text", x = 2.3, y = 0.8, label = "Subjective", color = "#00BFC4", size= 5) +
  labs(x="Time to Evaluation Policy", y="ODR per 500 students") +
  theme_minimal() +
  theme(legend.position = "none") 
  
  
ggsave(filename="assignments/DARE_1/figures/averages.png", width=15, height=12, units=c("cm"))

#clearly we see much larger estimates at the tails, might want to consider only looking at those close 
##in to intervention, binning, etc. 

#######################################################
## Replication and Extension
#######################################################

###############
# QUESTION C1 #
###############


########
#CLASS

#model 1 (w/o controls)
ols1 <- 
  feols(ODR_class ~ eval |
      state_id + school_year,                                           ##  fixed effects go here after the first "|"
      data = d, weights = d$enroll)


#model 2 (w/controls)
ols2 <- 
  feols(ODR_class ~ eval + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year, 
      data = d, weights = d$enroll)


#model 3 (w/ controls and linear time trend)
ols3 <- 
  feols(ODR_class ~ eval * run_time + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year,
      data = d, weights = d$enroll)


################################################
##Subjective
################################################

#model 4 (w/o controls)
ols4 <- 
  feols(ODR_subjective ~ eval |
          state_id + school_year,                                           ##  fixed effects go here after the first "|"
        data = d, weights = d$enroll)


#model 5 (w/controls)
ols5 <- 
  feols(ODR_subjective ~ eval + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year,
        data = d, weights = d$enroll)


#model 6 
ols6 <- 
  feols(ODR_subjective ~ eval * run_time + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year,
          data = d, weights = d$enroll)

row <- tribble(~term,          ~'(1)',  ~'(2)', ~'(3)', ~ '(4)', ~'(5)', ~'(6)',
               'Covariates?', '',  'X', 'X', '', 'X', 'X')
attr(row, 'position') <- c(7)

mods <- list()
mods[['(1)']] <- ols1
mods[['(2)']] <- ols2
mods[['(3)']] <- ols3
mods[['(4)']] <- ols4
mods[['(5)']] <- ols5
mods[['(6)']] <- ols6

# Using to modelsummary for table generation (though note the ease of etable below)
modelsummary(mods, 
          title = "The effect of teacher evaluation reforms on Office Disciplinary Referrals, by location and subjectivity \\label{tab:mainDD}",
          stars = T,
          estimate = "{estimate}{stars}",
          coef_omit = "FRPL|enroll",
          coef_rename = c("ODR_class" = "Classroom ODRs", "ODR_subjective" ="Subjective ODRs", "eval" = "Implement evaluation", 
                          "run_time" = "Pre-trend", "eval:run_time" = "Eval x Relative-Year"),
          vcov = ~ state_id^school_year,
          gof_omit = "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std|RMSE",
          add_rows = row,
          threeparttable= T,
          type='latex', 
          notes = c("Notes: $^{+}p<0.1, ^{*}p<0.05, ^{**}p<0.01, ^{***}p<0.001$. The table displays coefficients from Equations 1 and 2 and state-by-year-clustered standard errors in parentheses. All models include fixed effects for year and state and are weighted by state enrollment. Models 2-3 and 5-6 adjust for the proportion of FRPL-eligible students and the proportion of students of different ethnoracial backgrounds."),  
          output="assignments/DARE_1/tables/main_DD_results.tex"
          )
          



################################################
#Question C2. Robustness checks
################################################

##This is a robustness check for sensitivity to other policy reforms

########
##CLASS


c_rb1 <- feols(ODR_class ~ class_remove + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year, 
          data = d, weights = d$enroll)

c_rb2 <- feols(ODR_class ~ suspension + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year,
          data = d, weights = d$enroll)

c_rb3 <- feols(ODR_class ~ class_remove + suspension + eval +
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year, 
          data = d, weights = d$enroll)

##############
##Subjective
##############


s_rb1 <- feols(ODR_subjective ~ class_remove + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year,
          data = d, weights = d$enroll)

s_rb2 <- feols(ODR_subjective ~ suspension + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year, 
          data = d, weights = d$enroll)

s_rb3 <- feols(ODR_subjective ~ class_remove + suspension + eval +
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year,
          data = d, weights = d$enroll)

### For etable, first, you define the dictionary so variables are reported cleanly. Then the etable command follows.

setFixest_dict(c(ODR_class = "Class", ODR_subjective = "Subjective", eval = "Implement Evaluation", run_time = "Pre-trend", "eval:run_time" = "Eval x Relative-Year",
                  state_id = "State", school_year = "Year", class_remove = "Class remove reform", suspension = "Limit suspension",
                  PBIS = "Implement PBIS well", "eval:PBIS" = "Eval x PBIS", "eval:PBIS:run_time" = "Eval X PBIS x Year"))

etable(c_rb1, c_rb2, c_rb3, s_rb1, s_rb2, s_rb3, 
       cluster = ~state_id^school_year, digits="r3", digits.stats = "r3", 
       signifCode = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       keep = c("%class_remove", "%suspension", "%eval"), 
       title = "Alternative policy robustness checks", label = "tab:alt",
       style.tex = style.tex("aer"), fitstat = ~ r2 + n, tex = TRUE, placement = c("!htbp"),
       notes = c("$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. Cells report coefficients and state-by-year clustered standard errors in parentheses. All models include fixed effects for year and state, are weighted by state enrollment, and adjust for the proportion of FRPL-eligible students and the proportion of students of different ethnoracial backgrounds."),
       file = 'assignments/DARE_1/tables/robustness_results_1.tex', replace=TRUE)


##########################################################################
### There are several other robustness checks one could conduct. Here is one example of the balanced panel and ever_eval approach

# Create ever evaluation dataset
d_ever <- filter(d, !is.na(eval_year))

# Create balanced panel dataset
d_balance <- filter(d,  run_time>=-5 & run_time<2)

############
##CLASS
############

rb_c_ever <- feols(ODR_class ~ eval + 
                FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
                state_id + school_year,
                data = d_ever, weights = d_ever$enroll) 

rb_c_balance <- feols(ODR_class ~ eval + 
                        FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
                        state_id + school_year,
                        data = d_balance, weights = d_balance$enroll) 

############
##SUBJECTIVE
############

rb_s_ever <- feols(ODR_subjective ~ eval + 
                    FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
                    state_id + school_year,  
                    data = d_ever, weights = d_ever$enroll)

rb_s_balance <- feols(ODR_subjective ~ eval + 
                        FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
                        state_id + school_year, 
                        data = d_balance, weights = d_balance$enroll)


etable(rb_c_ever, rb_c_balance, rb_s_ever, rb_s_balance, 
       cluster = ~state_id^school_year, digits="r3", digits.stats = "r3", 
       signifCode = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       keep = c("Implement Evaluation"), 
       title = "Treated-only and balanced-panel robustness checks", label = "tab:robust",
       style.tex = style.tex("aer"), fitstat = ~ r2 + n, tex = TRUE, placement = c("!htbp"),
       notes = c("$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. Cells report coefficients and state-by-year clustered standard errors in parentheses. Models 1 and 3 are limited to states that ever implemented teacher evaluation reforms. Models 2 and 4 are estimated in balanced panels, restricted to state-year observations 5-years before and 1-year after evaluation reform. All models include fixed effects for year and state, are weighted by state enrollment, and adjust for the proportion of FRPL-eligible students and the proportion of students of different ethnoracial backgrounds."),
       file = 'assignments/DARE_1/tables/robustness_results_2.tex', replace=TRUE)



################################
####      OPTIONAL C4   ########
################################


#Restrict to years -5 up to +2
d1 <- filter(d, run_time>=-5 & run_time<2)

###############
# Class ODRS
###############

#regress outcome of interest 

event_study_c <- 
  feols(
    ODR_class ~ i(run_time, ref=-1) |  
    state_id + school_year,  
    data = d1, weights=d1$enroll)

iplot(event_study_c, ref.line = -0.5, xlab=c("Time to evaluation policy"), ylab=c("ODR per 500 stu"), 
        main="", grid.par = list(vert=F))

ggsave("assignments/DARE_1/figures/event_study_class.png", width=15, height=12, units=c("cm"))

###############
# Subjective ODRS
###############

event_study_subj <- 
  feols(
    ODR_subjective ~ i(run_time, ref=-1) |  
      state_id + school_year,  
    data = d1, weights=d1$enroll)

iplot(event_study_subj, ref.line = -0.5, xlab=c("Time to evaluation policy"), ylab=c("ODR per 500 stu"), 
      main="", grid.par = list(vert=F))

ggsave("assignments/DARE_1/figures/event_study_subjective.png", width=15, height=12, units=c("cm"))


################################################
##            OPTIONAL Q5
################################################

pbis_sample <- d %>% 
  filter(!is.na(PBIS))


############
###CLASS
############


#model 1 (re-estimate main model on PBIS sub-sample)
pbis1 <- 
  feols(
    ODR_class ~ eval | 
    state_id + school_year,
    data = pbis_sample, weights = pbis_sample$enroll)


#model 2 (estimate main effect w PBIS as moderator)
pbis2 <- 
  feols(
    ODR_class ~ eval * PBIS  + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
    state_id + school_year, 
    data = pbis_sample, weights = pbis_sample$enroll)

#model 3 (estimate time-varying effect w PBIS as moderator)
pbis3 <- 
  feols(
    ODR_class ~ eval*PBIS*run_time +
    FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
    state_id + school_year, 
    data = pbis_sample, weights = pbis_sample$enroll)

################################################
##Subjective
################################################

#model 4
pbis4 <- 
  feols(
    ODR_subjective ~ eval | 
    state_id + school_year, 
    data = pbis_sample, weights = pbis_sample$enroll)

#model 5
pbis5 <- 
  feols(
    ODR_subjective ~ eval * PBIS + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
    state_id + school_year, 
    data = pbis_sample, weights = pbis_sample$enroll)


#model 6
pbis6 <- 
  feols(
    ODR_subjective ~ eval*PBIS*run_time +
    FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
    state_id + school_year, 
    data = pbis_sample, weights = pbis_sample$enroll)



etable(pbis1, pbis2, pbis3, pbis4, pbis5, pbis6, 
       cluster = ~state_id^school_year, digits="r3", digits.stats = "r3", 
       signifCode = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
       keep = c("Implement Evaluation", "Eval x PBIS", "Implement PBIS well", "Eval x PBIS x Year", "Eval x Relative-Year", "Pre-trend"), 
       title = "Difference-in-differences estimates of moderating effects of successful PBIS implementation", label = "tab:pbis",
       style.tex = style.tex("aer"), fitstat = ~ r2 + n, tex = TRUE, placement = c("!htbp"),
       notes = c("$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. Cells report coefficients and state-by-year clustered standard errors in parentheses. Models 1 and 3 are limited to states that ever implemented teacher evaluation reforms. Models 2 and 4 are estimated in balanced panels, restricted to state-year observations 5-years before and 1-year after evaluation reform. All models include fixed effects for year and state, are weighted by state enrollment, and adjust for the proportion of FRPL-eligible students and the proportion of students of different ethnoracial backgrounds."),
       file = 'assignments/DARE_1/tables/pbis.tex', replace=TRUE)



