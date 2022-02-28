###############################################
##  Project: Literacy intervention IV
##  Script Name: EDLD_DARE_3_code.R
##  Author: David D. Liebowitz
##  Date created: 2/14/22
##  Last update: 2/14/22
##  Inputs: EDLD_650_DARE_3.csv
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

p_load(here, haven, tidyverse, ggplot2, ggthemes, fixest, modelsummary, kableExtra, broom, stargazer, arsenal, knitr)

# Option for latex formatting
options(modelsummary_format_numeric_latex = "plain")

# I would suggest creating a project for the course. Within this project, create a folder entitled 'assignments' and a sub-folder entitled 'DARE_1'
#In your working folder for the assignment, I would set-up FOUR sub-directories for each of the following:
#data
#code
#figures
#tables

# Place the .csv file from the course website into the data folder
# (You can also, of course, use json to load directly from the course website)

#Set location 
i_am("assignments/DARE_3/code/EDLD_650_DARE_3_code.R")


#import data
read180 <- read_csv(here("assignments/DARE_3/data/EDLD_650_DARE_3.csv"))

head(read180)

# Assign factor labels to numeric treat status to have clearer/prettier tables/figures
read180$treat_f <- factor(read180$treat,
                    levels = c(0,1),
                    labels = c("Assigned to Other After-School", "Assigned to Read180"))

#A1. Create a table comparing baseline characteristics
treatment <- tableby(treat_f ~ frpl + female + dorf, numeric.stats=c("meansd"), cat.stats=c("N", "countpct"), digits=2, data=read180)
#create labels
mylabels <- list(frpl = "FRPL-eligible", female = "Female", dorf="DIBELS ORF")
descriptives <- summary(treatment, labelTranslations = mylabels)

descriptives <- kable(descriptives, booktabs = T, format = 'latex',
                  caption = "Descriptive statistics by assigned treatment \\label{tab:descriptives}"
                  ) 
                  
save_kable(descriptives, "assignments/DARE_3/tables/descriptives.tex", keep_tex = T)


capture.output(summary(treatment, labelTranslations = mylabels), file=here("assignments/DARE_3/tables/descriptives.md"))
pandoc(here("assignments/DARE_3/tables/descriptives.md"), format='latex', config = here("assignments/DARE_3/code/latex_out.txt"))

# Joint F-test
f_test <- lm(treat ~ frpl + female + dorf, data=read180)
summary(f_test)

#B1. Naive OLS estimates of outcome on READ180 attendance

ols1 <- lm(sat10_compreh ~ read180_attend + dorf, data=read180)
ols2 <- lm(sat10_compreh ~ read180_attend + dorf + frpl + female, data=read180)
ols3 <- lm(sat10_compreh ~ read180_attend + dorf + frpl + female + as.factor(school), data=read180)

stargazer(ols1, ols2, ols3, type='latex', out="assignments/DARE_3/tables/ols_results.tex", dep.var.caption="", dep.var.labels = "", 
          omit.stat = c("ser", "adj.rsq", "f"), notes.append=F, notes = c("*p$<$0.05, **p$<$0.01, ***p$<$0.001. Cells report coefficients", "and associated standard errors"),
          omit=c("Constant", "dorf", "frpl", "female", "school"), star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", model.names=F,
          add.lines = list(c("Student Chars", "No", "Yes", "Yes"), c("School Fixed Effects", "No", "No", "Yes")),
          covariate.labels=c("Full READ180 Attend"), 
          title="Na?ve OLS estimates of the effects of READ180 attendance on reading comprehension score")


#B2. Comparison of outcomes

#Boxplot
mean <- ggplot() +
        geom_boxplot(data=read180, aes(x=as.factor(treat), y=sat10_compreh),
        fill=red_pink, alpha=0.4) +
        theme_pander(base_size = 12) +
        xlab("Assigned treatment status") + scale_y_continuous("Post-intervention, reading comprehension test")
mean

ggsave(filename="assignments/DARE_3/figures/mean_out_compare.png", width=15, height=12, units=c("cm"))

#Simple t-test


ttest <- t.test(sat10_compreh ~ treat, data=read180)
ttest

#B3. ITT estimates
itt1 <- lm(sat10_compreh ~ treat, data=read180)
itt2 <- lm(sat10_compreh ~ treat + dorf + female + frpl, data=read180)
itt3 <- lm(sat10_compreh ~ treat + dorf + frpl + female + as.factor(school), data=read180)
itt4 <- feols(sat10_compreh ~ treat + dorf +frpl + female | school, vcov="iid", data=read180)
itt5 <- feols(sat10_compreh ~ treat + dorf +frpl + female | school, data=read180)

row <- tribble(~term,          ~'1',  ~'2', ~'3', ~'4', ~'5',
               "Student Chars", "No", "Yes", "Yes", "Yes", "Yes",
               'School Fixed Effects', 'No', 'No', 'Yes', 'Yes', 'Yes',
               'School-Clustered SEs', 'No', 'No', 'No', 'No', 'Yes'
              )
attr(row, 'position') <- c(3, 4, 5)

modelsummary(list(itt1, itt2, itt3, itt4, itt5), 
          title="Intent-to-Treat estimates of random assignment to READ180 after-school intervention \\label{tab:itt}",
          stars=c('*' = 0.05, '**' = 0.01, '***' = 0.001),
          coef_omit = "(Intercept)|dorf|frpl|female|as.factor",
          coef_rename = c("treat" = "Assigned to Read180"),
          estimate = "{estimate}{stars}",
          gof_omit= "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std|F",
          add_rows = row,
          threeparttable = T,
          notes = c("Notes: Cells report coefficients and associated standard errors."),
          type = 'latex',
          output="assignments/DARE_3/tables/itt_results.tex"
          )
          

#B4. TOT estimates
tot1 <- feols(sat10_compreh ~ 1 | read180_attend ~ treat, data=read180)
tot2 <- feols(sat10_compreh ~ dorf + frpl + female | read180_attend ~ treat, data=read180)
tot3 <- feols(sat10_compreh ~ dorf + frpl + female | school | read180_attend ~ treat, vcov="iid", data=read180)
tot4 <- feols(sat10_compreh ~ dorf + frpl + female | school | read180_attend ~ treat, data=read180)

summary(tot2, stage = 1)

stargazer(tot1, tot2, tot3, tot4, type='latex', out="Tables/tot_results.tex", dep.var.caption="", dep.var.labels = "", 
          omit.stat = c("ser", "adj.rsq", "f"), notes.append=F, notes = c("*p$<$0.05, **p$<$0.01, ***p$<$0.001. Cells report coefficients and", "associated standard errors. Model 4 clusters SEs at school level"),
          omit=c("Constant", "dorf", "frpl", "female", "school"), star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", model.names=F,
          add.lines = list(c("Student Chars", "No", "Yes", "Yes", "Yes"), c("School Fixed Effects", "No", "No", "Yes", "Yes")),
          covariate.labels=c("Fitted READ180 Attend"), 
          title="Treatment-on-the-Treated estimates of full participation in a seven-month READ180 after-school intervention")

row2 <- tribble(~term,          ~'1',  ~'2', ~'3', ~'4', 
               "Student Chars", "No", "Yes", "Yes", "Yes", 
               'School Fixed Effects', 'No', 'No', 'Yes', 'Yes', 
               'School-Clustered SEs', 'No', 'No', 'No', 'Yes'
              )
attr(row2, 'position') <- c(3, 4, 5)

modelsummary(list(tot1, tot2, tot3, tot4), 
             title="Treatment-on-the-Treated estimates of full participation in a seven-month READ180 after-school intervention \\label{tab:tot}",
             stars=c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             coef_omit = "(Intercept)|dorf|frpl|female|as.factor",
             coef_rename = c("fit_read180_attend" = "Predicted Read180 Attendance"),
             estimate = "{estimate}{stars}",
             gof_omit= "Adj|Pseudo|Log|Within|AIC|BIC|FE|Std|F",
             add_rows = row2,
             threeparttable = T,
             notes = c("Notes: Cells report coefficients and associated standard errors."),
             type = 'latex',
             output="assignments/DARE_3/tables/tot_results.tex"
            )