#load necessary packages
library(pacman)

p_load(here, tidyverse, haven, ggplot2, fixest, modelsummary, broom, reshape2, stargazer)


# I would suggest creating a project for the course. Within this project, create a folder entitled 'assignments' and a sub-folder entitled 'DARE_1'
#In your working folder for the assignment, I would set-up FOUR sub-directories for each of the following:
#data
#code
#figures
#tables

# Place the .xlsx file from the course website into the data folder

#Set location 
i_am("assignments/DARE_1/code/EDLD_650_DARE_1_code.R")





#import data
d <- read_csv(here("assignments/DARE_1/data/EDLD_650_DARE_1.csv"))


#######################################################
## Data Management ## (A)
#######################################################

#view data structure
str(d)


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
  #eval_year class_remove_year suspension_year 

d <- d %>% 
  mutate(eval = case_when(
    is.na(eval_year) ~ 0,
    school_year>=eval_year ~ 1,
    TRUE ~ 0
    ))
d <- d %>% 
  mutate(bargain = case_when(
  is.na(bargain_year) ~ 0,
  school_year>=bargain_year ~ 1,
  TRUE ~ 0
))

d <- d %>% 
  mutate(class_remove = case_when(
  is.na(class_remove_year) ~ 0,
  school_year>=class_remove_year ~ 1,
  TRUE ~ 0
))

d <- d %>% 
  mutate(suspension = case_when(
  is.na(suspension_year) ~ 0,
  school_year>=suspension_year ~ 1,
  TRUE ~ 0
))

#create running time variable, setting never-eval to -1
d <- d %>% 
  mutate(run_time= case_when(
    is.na(eval_year) ~ -1, 
    !is.na(eval_year) ~ (school_year-eval_year) 
  ))

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

d <- filter(d, !is.na(ODR_class) & !is.na(enroll)
          )

##This results in a final analytic sample of 470 state-year observations

###############
# QUESTION B2 #
###############

#Can display univariate data most effectively with histograms or density plots
#Here I use the melt command to create faceted plots of all four outcomes in a straightforward way. 
#You could also plot each outcome separately and combine them into a paneled figure

#select the outcome data to plot
ODR_graph <- d %>% 
  select(c(ODR_class, ODR_other, ODR_subjective, ODR_objective))


#Only need to select one of hist or kdensity

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

#This code can be made into a function, but want to make clear what was happening
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
           digits=2, digit.separator = ",", notes="Notes: This table presents state-year means and standard deviations from 2006-2018.", notes.append = F, title="Descriptive Statistics", out="assignments/DARE_1/tables/descriptives_table.tex")



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
      vcov = ~ state_id,                                                ##  we can adjust our standard errors here or post-estimation
      data = d, weights = d$enroll)


#model 2 (w/controls)
ols2 <- 
  feols(ODR_class ~ eval + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year 
      data = d, weights = d$enroll)


#model 3 
ols3 <- 
  feols(ODR_class ~ eval + evalXyear + run_time + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year | 
      data = d, weights = d$enroll)


################################################
##Subjective
################################################

#model 1 (w/o controls)
ols4 <- 
  feols(ODR_subj ~ eval |
          state_id + school_year,                                           ##  fixed effects go here after the first "|"
        vcov = ~ state_id,                                                ##  we can adjust our standard errors here or post-estimation
        data = d, weights = d$enroll)


#model 2 (w/controls)
ols5 <- 
  feols(ODR_subj ~ eval + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year 
        data = d, weights = d$enroll)


#model 3 
ols6 <- 
  feols(ODR_subj ~ eval + evalXyear + run_time + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year | 
          data = d, weights = d$enroll)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, type='latex', out="Tables/main_DD_results.tex", dep.var.caption="", dep.var.labels = c(ODR_class="Classroom ODRs", ODR_subjective="Subjective ODRs"),
          omit= c("FRPL_percent", "enroll_AM_prop", "enroll_WHITE_prop", "enroll_BLACK_prop", "enroll_HISP_prop", "enroll_ASIAN_prop"), omit.stat = c("ser", "adj.rsq"), notes.append=F, notes = c("*p$<$0.05, **p$<$0.01, ***p$<$0.001. Cells report estimates and associated", 
          "standard errors clustered at the state level in parentheses. Controls include", "proportion low-income and race/ethnicity. All models include state and year", "fixed-effects and are weighted by state enrollment"),
          star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", 
          covariate.labels=c("Implement evaluation", "Evaluation*Time Trend", "Time trend"), title="Main Difference-in-Differences estimates of the effects of teacher evaluation on Office Disciplinary Referrals (ODRs)")


################################################
#Question C2. Robustness checks
################################################

##This is a robustness check for sensitivity to other policy reforms

########
##CLASS


ols1_c_rb1 <- 
  feols(ODR_class ~ class_remove + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year 
        data = d, weights = d$enroll)

ols_c_rb2 <- 
  feols(ODR_class ~ suspension + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year 
        data = d, weights = d$enroll)

feols(ODR_class ~ class_remove + suspension + eval +
        FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
        state_id + school_year 
      data = d, weights = d$enroll)

##############
##Subjective
##############


ols1_c_rb1 <- 
  feols(ODR_subj ~ class_remove + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year 
        data = d, weights = d$enroll)

ols_c_rb2 <- 
  feols(ODR_subj ~ suspension + 
          FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
          state_id + school_year 
        data = d, weights = d$enroll)

feols(ODR_subj ~ class_remove + suspension + eval +
        FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
        state_id + school_year 
      data = d, weights = d$enroll)


stargazer(ols_c_rb1, ols_c_rb2, ols_c_rb3, ols_s_rb1, ols_s_rb2, ols_s_rb3, type='latex', out="Tables/robustness_results_1.tex", dep.var.caption="", dep.var.labels = c(ODR_class="Classroom ODRs", ODR_subjective="Subjective ODRs"),
          omit= c("FRPL_percent", "enroll_AM_prop", "enroll_WHITE_prop", "enroll_BLACK_prop", "enroll_HISP_prop", "enroll_ASIAN_prop", "school_year"), omit.stat = c("ser", "adj.rsq"), notes.append=F, notes = c("*p$<$0.05, **p$<$0.01, ***p$<$0.001. Cells report estimates and associated", 
          "standard errors clustered at the state level in parentheses. Controls include",
          "proportion low-income and race/ethnicity. All models include state and year", "fixed-effects and are weighted by state enrollment"), star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", 
          covariate.labels=c("Bargain reform", "Class Remove Reform", "Limit Suspension"), title="Alternative policy robustness checks")


##########################################################################
###There are several other robustness checks one could conduct. Here is one example of the balanced panel and ever_eval approach

###select states ever subject to evaluation

d_ever <- filter(d, !is.na(eval_year))
d_balance <-filter(d, run_time>=-5 & run_time<2)

############
##CLASS
############

rb_c_ever <- 
  felm(
    ODR_class ~ eval + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year |  0 |   state_id, data = d_ever, weights = d_ever$enroll)

rb_c_balance <- 
  felm(
    ODR_class ~ eval + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year |  0 | state_id, data = d_balance, weights = d_balance$enroll)

############
##SUBJECTIVE
############

rb_s_ever <- 
  felm(
    ODR_subjective ~ eval + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year |  0 | state_id,  data = d_ever, weights = d_ever$enroll)

rb_s_balance <- 
  felm(
    ODR_subjective ~ eval + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop |
      state_id + school_year |  0 |   state_id, data = d_balance, weights = d_balance$enroll)


stargazer(rb_c_ever, rb_c_balance, rb_s_ever, rb_s_balance, type='latex', out="Tables/robustness_results_2.tex", dep.var.caption="", column.labels=c("Ever eval", "Bal. Panel", "Ever eval", "Bal. Panel"),
          dep.var.labels = c(ODR_class="Classroom ODRs", ODR_subjective="Subjective ODRs"),
          column.separate=c(1,1,1,1), omit= c("FRPL_percent", "enroll_AM_prop", "enroll_WHITE_prop", "enroll_BLACK_prop", "enroll_HISP_prop", "enroll_ASIAN_prop"), omit.stat = c("ser", "adj.rsq"), notes.append=F, notes = c("*p$<$0.05, **p$<$0.01, ***p$<$0.001. Cells report estimates and associated", 
"standard errors clustered at the state level in parentheses. Controls include",
"proportion low-income and race/ethnicity. All models include state and year", "fixed-effects and are weighted by state enrollment"), star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", 
          covariate.labels=c("Implement Evaluation"), title="Treated only and balanced panel robustness checks")

################################
####      OPTIONAL C4   ########
################################

#make dummy vars for run time
d <- mutate(d, r6min=ifelse(run_time==-6,1,0))
d <- mutate(d, r5min=ifelse(run_time==-5,1,0))
d <- mutate(d, r4min=ifelse(run_time==-4,1,0))
d <- mutate(d, r3min=ifelse(run_time==-3,1,0))
d <- mutate(d, r2min=ifelse(run_time==-2,1,0))
d <- mutate(d, r1min=ifelse(run_time==-1,1,0))
d <- mutate(d, reval=ifelse(run_time==0,1,0))
d <- mutate(d, r1plus=ifelse(run_time==1,1,0))
d <- mutate(d, r2plus=ifelse(run_time==2,1,0))
d <- mutate(d, r3plus=ifelse(run_time==3,1,0))

d1 <- filter(d, run_time>=-5 & run_time<2)

###############
# Class ODRS
###############

#regress outcome of interest 

non_para_class <- 
  felm(
    ODR_class ~ r5min + r4min + r3min + r2min + reval + r1plus |  state_id + school_year | 
      0 | state_id,  data = d1, weights=d1$enroll)


ggcoefstats(x = non_para_class, point.color="red", stats.labels=F, caption.summary=F) +  
  theme_minimal() + geom_hline(aes(yintercept = 4.5), col="blue", lty="dashed") +
  labs(x="Change in ODR per 500 stu", y="Time to Evaluation Policy") +
  scale_y_discrete(labels=c("-5", "-4", "-3", "-2", "Eval", "+1")) + coord_flip()

ggsave("Graphs/event_study_class.png", width=15, height=12, units=c("cm"))

###############
# Subjective ODRS
###############

non_para_subj <- 
  felm(
    ODR_subjective ~ r5min + r4min + r3min + r2min + reval + r1plus | state_id + school_year | 
      0 | state_id, data = d1, weights=d1$enroll)

ggcoefstats(x = non_para_subj, point.color="red", stats.labels=F, caption.summary=F) +  
  theme_minimal() + geom_hline(aes(yintercept = 4.5), col="blue", lty="dashed") +
  labs(x="Change in ODR per 500 stu", y="Time to Evaluation Policy") +
  scale_y_discrete(labels=c("-5", "-4", "-3", "-2", "Eval", "+1")) + coord_flip()

ggsave("Graphs/event_study_subjective.png", width=15, height=12, units=c("cm"))



################################################
##            OPTIONAL Q5
################################################

# make other vars 
str(d)
d$pbisXeval <- d$PBIS * d$eval
d$pbisXevalXyear <- d$pbisXeval * d$run_time

pbis_sample <- d %>% 
  filter(!is.na(PBIS))


############
###CLASS
############


#model 1 (w/o controls)
ols_pbis1 <- 
  felm(
    ODR_class ~ eval | state_id + school_year |  0 | state_id, data = pbis_sample, weights = pbis_sample$enroll)


#model2
ols_pbis2 <- 
  felm(
    ODR_class ~ eval + pbisXeval + PBIS | state_id + school_year |   0 | state_id,  data = pbis_sample, weights = pbis_sample$enroll)


#model3
ols_pbis3 <- 
  felm(
    ODR_class ~ eval + pbisXeval + PBIS  + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
      state_id + school_year |  0 | state_id, data = pbis_sample, weights = pbis_sample$enroll)

#model4
ols_pbis4 <- 
  felm(
    ODR_class ~ eval + pbisXeval + PBIS  + pbisXevalXyear + evalXyear + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
      state_id + school_year |  0 | state_id, data = pbis_sample, weights = pbis_sample$enroll)

################################################
##Subjective
################################################
#model 5 (w/o controls)
ols_pbis5 <- 
  felm(
    ODR_subjective ~ eval | state_id + school_year| 0 | state_id, data = pbis_sample, weights = pbis_sample$enroll)

#model6
ols_pbis6 <- 
  felm(ODR_subjective ~ eval + pbisXeval  + PBIS | state_id + school_year | 0 | state_id, data = pbis_sample, weights = pbis_sample$enroll)

#model7
ols_pbis7 <- 
  felm(
    ODR_subjective ~ eval + pbisXeval + PBIS + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
      state_id + school_year |  0 | state_id, data = pbis_sample, weights = pbis_sample$enroll)


#model8
ols_pbis8 <- 
  felm(
    ODR_subjective ~ eval + pbisXeval + PBIS + pbisXevalXyear + evalXyear + FRPL_percent + enroll_AM_prop + enroll_WHITE_prop + enroll_BLACK_prop + enroll_HISP_prop + enroll_ASIAN_prop  |
      state_id + school_year | 0 |   state_id, data = pbis_sample, weights = pbis_sample$enroll)


stargazer(ols_pbis1, ols_pbis2, ols_pbis3, ols_pbis4, ols_pbis5, ols_pbis6, ols_pbis7, ols_pbis8, type='latex', out="Tables/pbis.tex", dep.var.caption="", 
          dep.var.labels = c(ODR_class="Classroom ODRs", ODR_subjective="Subjective ODRs"),
          omit= c("FRPL_percent", "enroll_AM_prop", "enroll_WHITE_prop", "enroll_BLACK_prop", "enroll_HISP_prop", "enroll_ASIAN_prop"), omit.stat = c("ser", "adj.rsq"), notes.append=F, notes = c("*p$<$0.05, **p$<$0.01, ***p$<$0.001. Cells report estimates and associated", 
        "standard errors clustered at the state level in parentheses. Controls include",
        "proportion low-income and race/ethnicity. All models include state and year", "fixed-effects and are weighted by state enrollment"), star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", 
          covariate.labels=c("Implement Evaluation", "Implement EvalxPBIS", "Implement PBIS", "PBISxEVALxYEAR", "Eval x Time Trend"), title="Difference-in-differences estimates of moderating effects of successul PBIS implementation")

