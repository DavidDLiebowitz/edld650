###############################################
##  Project: ELL beliefs (matching)
##  Script Name: EDLD_DARE_4_code.R
##  Author: David D. Liebowitz
##  Date created: 2/14/22
##  Last update: 3/1/24
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

p_load(here, haven, tidyverse, ggplot2, ggthemes, fixest, modelsummary, kableExtra, broom, stargazer, arsenal, knitr, MatchIt, gtools)

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
i_am("assignments/DARE_4/code/EDLD_650_DARE_4_code.R")


#import data
ell <- read_dta("assignments/DARE_4/data/dumont_umansky_ECLSK.dta")
ell <- data.frame(ell)

#A1. Describe raw differences in teacher perceptions between EL and non-EL classified students

ell %>% group_by(elprgm) %>% 
            summarise (n_students= n(), mean_tchr_prcpt_m = mean(tmathk), mean_tchr_prcpt_l = mean(tlangk))

#Some descriptives
t.test(tmathk ~ elprgm, data=ell)
t.test(tlangk ~ elprgm, data=ell)
#Teacher perception is 0.365SD units lower for math skills and 0.453 units lower for students classified as EL, both are significant
sd(ell$tmathk) # both are ~1 by variable construction
sd(ell$tlangk)

ols1_math <- lm(tmathk ~ elprgm, data=ell)
ols1_lang <- lm(tlangk ~ elprgm, data=ell)
ols2_math <- lm(tmathk ~ elprgm + prelas + ebrs + ses + rural + female + hisp, data=ell)
ols2_lang <- lm(tlangk ~ elprgm + prelas + ebrs + ses + rural + female + hisp, data=ell)

####Graphical representation of raw differences in perceptions
#Math
raw_diff_m <- ell %>% group_by(elprgm) %>% 
                        summarise(n_students = n(), mean_math=mean(tmathk), se_math = sd(tmathk)/sqrt(n_students))

ggplot(raw_diff_m, aes(x=as.factor(elprgm), y=mean_math, ymin=mean_math-se_math, ymax=mean_math+se_math)) + 
  geom_col(fill=red_pink, alpha=0.4) + 
  geom_linerange() + theme_pander(base_size = 12) +  expand_limits(y=-0.4) +
  xlab("EL Classification") + scale_y_continuous("Standardized teacher perception of K math skills")
ggsave(filename="assignments/DARE_4/figures/math_percep_raw_diff.png", width=15, height=12, units=c("cm"))

#Reading
raw_diff_e <- ell %>% group_by(elprgm) %>% 
                        summarise(n_students = n(), mean_read=mean(tlangk), se_read = sd(tlangk)/sqrt(n_students))

ggplot(raw_diff_e, aes(x=as.factor(elprgm), y=mean_read, ymin=mean_read-se_read, ymax=mean_read+se_read)) + geom_col(fill=red_pink, alpha=0.4) + 
  geom_linerange() + theme_pander(base_size = 12) +
  xlab("EL Classification") + scale_y_continuous("Standardized teacher perception of K reading skills")
ggsave(filename="assignments/DARE_4/figures/read_percep_raw_diff.png", width=15, height=12, units=c("cm"))



#A2.Describe other differences in teacher perceptions of students classified as EL

# Factor elprggm variable
ell$EL <- factor(ell$elprgm,
                 levels = c(0,1),
                 labels = c("Not EL", "EL"))

# Descriptives
descriptives <- tableby(EL ~ prelas + ebrs + kread + kmath + ses + female + rural + hisp, 
                        numeric.stats=c("meansd"), cat.stats=c("N", "countpct"), digits=2, data=ell)

#create labels
mylabels <- list(distance = "Propensity score", prelas = "Language Score", ebrs = "Reading Score", kread="K Literacy Score", kmath="K Math Score",
                 ses="SES", rural="Rural school", female="Female", hisp="Hispanic/Latinx")
summary(descriptives, labelTranslations = mylabels, title='Descriptive statistics by EL classification')

capture.output(summary(descriptives, labelTranslations = mylabels), file="assignments/DARE_4/tables/descriptives.md")
pandoc("assignments/DARE_4/tables/descriptives.md", format='latex', config = 'assignments/DARE_4/code/latex_out.txt')


############################################

####     Replication/Extension         #####

############################################

#B1. Estimate propensity scores and describe common support

pscores <- feglm(elprgm ~ prelas + ebrs + ses + rural + female + hisp, 
                 family=c("logit"), data=ell)

pscore_df <- data.frame(p_score=predict(pscores, type="response"), 
                        EL = ell$elprgm)

ggplot(pscore_df, aes(p_score,fill=as.factor(EL),color=as.factor(EL))) + 
          geom_density(alpha=0.1) + theme_pander(base_size = 12) + 
          guides(fill=guide_legend(title="EL"), color=guide_legend(title="EL")) +
          xlab("Probability of EL-classification")

ggsave(filename="assignments/DARE_4/figures/base_common_support.png", width=15, height=12, units=c("cm"))


#B2. Construct matching routine using CEM

### Could do it by hand, like this:
# prelas_quints <- quantcut(ell$prelas, 5)
# table(prelas_quints)
# ebrs_quints <- quantcut(ell$ebrs, 5)
# table(ebrs_quints)
# ses_quints <- quantcut(ell$ses, 5)
# table(ses_quints)
# 
# prelas_cuts <- c(13, 16, 18, 19.9)
# ebrs_cuts <- c(7, 11, 14, 17)
# ses_cuts <- c(-1.12, -0.84, -0.52, 0.14)


# but, instead will just implement directly in MatchIt routine
cem1 <- matchit(elprgm ~ prelas + ebrs + ses + rural + female + hisp, 
                cutpoints=list(prelas = "q5",
                               ebrs = "q5",
                               ses = "q5"), 
                method="cem", verbose=T, data=ell)
df_cem1 <- match.data(cem1)
dim(df_cem1)


#B3.Assess quality of matches

summary(cem1)
#1627 (887 treat, 740 control) observations retained (versus 2166 in full sample)

##Create descriptives of CEM matched sample
cem_descriptives <- tableby(EL ~ prelas + ebrs + ses + rural + female + hisp, 
                            numeric.stats=c("meansd"), cat.stats=c("N", "countpct"), digits=2, 
                            weights = weights,
                            data=df_cem1)

#Note that the tableby command deals poorly with the weighted sample. I would look for a different cross-tab post-estimation table
prelas <- lm(prelas ~ elprgm, weight=weights, data=df_cem1)
ebrs <- lm(ebrs ~ elprgm, weight=weights, data=df_cem1)
ses <- lm(ses ~ elprgm, weight=weights, data=df_cem1)
rural <- lm(rural ~ elprgm, weight=weights, data=df_cem1)
female <- lm(female ~ elprgm, weight=weights, data=df_cem1)
hisp <- lm(hisp ~ elprgm, weight=weights, data=df_cem1)

summary(prelas)
summary(ebrs)
summary(ses)
summary(rural)
summary(female)
summary(hisp)

mypval <- data.frame(
  byvar = "EL",
  variable = c('prelas','ebrs', 'ses', 'rural', 'female', 'hisp'), 
  adj.pvalue = c(0.786, 0.909, 0.752, "1.000", "1.000", "1.000")
  )
cem_descriptives2 <- modpval.tableby(cem_descriptives, mypval, use.pname=TRUE)


#Extract values
capture.output(summary(cem_descriptives2, labelTranslations = mylabels), file="assignments/DARE_4/tables/cem_descriptives.md")

pandoc("assignments/DARE_4/tables/cem_descriptives.md", format='latex', config = 'assignments/DARE_4/code/latex_cem.txt')



##Describe common support
pscores2 <- feglm(elprgm ~ prelas + ebrs + ses + rural + female + hisp, 
                 family=c("logit"), 
                 weights = df_cem1$weights,
                 data=df_cem1)

pscore_df2 <- data.frame(p_scores=predict(pscores2, type="response"), 
                        EL = df_cem1$elprgm)

ggplot(pscore_df2, aes(p_scores, fill=as.factor(EL), color=as.factor(EL))) + 
              geom_density(alpha=0.1) + theme_pander(base_size = 12) + 
              guides(fill=guide_legend(title="EL"), color=guide_legend(title="EL")) +
              xlab("Probability of EL-classification")


ggsave(filename="assignments/DARE_4/figures/cem1_common_support.png", width=15, height=12, units=c("cm"))

#B4. Estimate CEM effects

cem_fx_math <- lm(tmathk ~ elprgm + prelas + ebrs + ses + rural + female + hisp, weights = weights, data=df_cem1)
cem_fx_lang <- lm(tlangk ~ elprgm + prelas + ebrs + ses + rural + female + hisp, weights = weights, data=df_cem1)

summary(cem_fx_math)
summary(cem_fx_lang)

#B5. Conduct PSM robustness check and estimate effects

psm1 <- matchit(elprgm ~ prelas + ebrs + ses + rural + female + hisp, 
                method="nearest", discard="both", verbose=T, replace=T, data=ell)
summary(psm1)
df_psm <- match.data(psm1)

#Common support
ggplot(df_psm, aes(distance,fill=EL,color=EL)) + 
              geom_density(alpha=0.1) + theme_pander(base_size = 12) + 
              xlab("Probability of EL-classification")
ggsave(filename="assignments/DARE_4/figures/psm_common_support.png", width=15, height=12, units=c("cm"))


psm_descriptives <- tableby(EL ~ distance + prelas + ebrs + ses + rural + female + hisp, 
                            numeric.stats=c("meansd"), cat.stats=c("N", "countpct"), digits=2, weights=weights, data=df_psm)
summary(psm_descriptives)

distance_p <- lm(distance ~ elprgm, weight=weights, data=df_psm)
prelas_p <- lm(prelas ~ elprgm, weight=weights, data=df_psm)
ebrs_p <- lm(ebrs ~ elprgm, weight=weights, data=df_psm)
ses_p <- lm(ses ~ elprgm, weight=weights, data=df_psm)
rural_p <- lm(rural ~ elprgm, weight=weights, data=df_psm)
female_p <- lm(female ~ elprgm, weight=weights, data=df_psm)
hisp_p <- lm(hisp ~ elprgm, weight=weights, data=df_psm)

summary(distance_p)
summary(prelas_p)
summary(ebrs_p)
summary(ses_p)
summary(rural_p)
summary(female_p)
summary(hisp_p)

mypval2 <- data.frame(
  byvar = "EL",
  variable = c('distance', 'prelas','ebrs', 'ses', 'rural', 'female', 'hisp'), 
  adj.pvalue = c(0.994, 0.488, 0.261, "0.560", 0.153, 0.608, "0.890")
)
psm_descriptives2 <- modpval.tableby(psm_descriptives, mypval2, use.pname=TRUE)

#create labels
summary(psm_descriptives2, labelTranslations = mylabels, title='PSM Matched descriptive statistics by EL classification')

capture.output(summary(psm_descriptives2, labelTranslations = mylabels), file="assignments/DARE_4/tables/psm_descriptives2.md")
pandoc("assignments/DARE_4/tables/psm_descriptives2.md", format='latex', config = 'assignments/DARE_4/code/latex_out_psm.txt')


att1_math <- lm(tmathk ~ elprgm + prelas + ebrs + ses + rural + female + hisp, weights = weights, data=df_psm)
att1_lang <- lm(tlangk ~ elprgm + prelas + ebrs + ses + rural + female + hisp, weights = weights, data=df_psm)

summary(att1_math)
summary(att1_lang)



stargazer(ols1_math, ols2_math, cem_fx_math, att1_math, ols1_lang, ols2_lang, cem_fx_lang, att1_lang, 
          type='latex', out='assignments/DARE_4/tables/matching_results.tex', 
          omit.stat = c("ser", "adj.rsq", "f"), omit=c("Constant", "prelas", "ebrs", "ses", "rural", "female", "hisp"), 
            dep.var.labels=c("Teacher Math Perception","Teacher Language Perception"), 
          column.labels=c("OLS", "OLS", "CEM", "PSM", "OLS", "OLS", "CEM", "PSM"), star.cutoffs=c(0.05, 0.01, 0.001), notes.align="l", 
          covariate.labels=c("EL-classified"), add.lines = list(c("Student Chars", "No", "Yes", "Yes", "Yes",  "No", "Yes", "Yes", "Yes")),
          notes=c("Table presents coefficients and standard errors in parentheses."))


