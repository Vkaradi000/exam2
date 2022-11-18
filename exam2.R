Victoria Karadimas 
Exam #2
#1
#A. 0.0357
#B. -1.7
#C. -0.39917
#D. 0.02238
#E. -0.4
#2
The hypothesis I am going to form between two educational groups is some_college and Masters.

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.9)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)

t.test (x, y = NULL, alternative = c "two sided","less","greater"),
mu = 324.5470, paired = 'FALSE', var.equal = 'FALSE',
conf.level = 0.95)
#i ran the code and recived an error but the level of significance i chose was 95%, I chose to test out the if the participant had some college degree VS. one who has a masters. I thought this comparison would be a good comparison
#4
pick_use1 <- (dat_ATUS$HH_NUMKIDS < 0 ) 
dat_use1 <- subset(dat_ATUS, pick_use1)
# i chose to make this subgroup because those that have choldren older then  18 are most likeley to have mor "free time" to watch over others.
#5 
pick_use1 <- (dat_ATUS$HH_NUMKIDS < 0 || dat_ATUS$any_t_care ) 
dat_use1 <- subset(dat_ATUS, pick_use1)
model_logit1 <- glm( dat_atus$HH_NUMKIDS ~ dat_atus$SEX dat_use1)
summary(model_logit1)
#6 
d_educ <- data.frame(model.matrix(~ dat_ATUS$EDUC))
summary(d_educ)
levels(dat_ATUS$EDUC)
require(tidyverse)
dat_ATUS$EDUC_r <- recode_factor(dat_ATUS$EDUC, "\"Less than 1st grade\"" = "ltHS", "\"1st, 2nd, 3rd, or 4th grade\"" = "ltHS", "\"5th or 6th grade\""  = "ltHS",
                                 "\"7th or 8th grade\"" = "ltHS", "\"9th grade\"" = "ltHS", "\"10th grade\"" = "ltHS", "\"11th grade\"" = "ltHS", 
                                 "\"12th grade - no diploma\"" = "ltHS",
                                 "\"High school graduate - GED\"" = "HS", "\"High school graduate - diploma\"" = "HS", "\"Some college but no degree\"" = "some_college",
                                 "\"Associate degree - occupational vocational\"" = "associate", "\"Associate degree - academic program\"" = "associate",
                                 "\"Bachelor's degree (BA, AB, BS, etc.)\"" = "bachelor", "\"Master's degree (MA, MS, MEng, MEd, MSW, etc.)\"" = "master",
                                 "\"Professional school degree (MD, DDS, DVM, etc.)\"" = "prof_or_PhD", "\"Doctoral degree (PhD, EdD, etc.)\"" = "prof_or_PhD",
                                 .default = "D")


summary(dat_ATUS$EDUC_r)
d_educ_r <- data.frame(model.matrix(~ dat_ATUS$EDUC_r))
d_marstat <- data.frame(model.matrix(~ dat_ATUS$MARST))
d_sex <- data.frame(model.matrix(~ dat_ATUS$SEX))

d_any_time_sports <- data.frame(model.matrix(~ any_time_sports)) 

dat_for_analysis_sub <- data.frame(
  d_any_time_sports[ !is.na(dat_ATUS$HISPAN) ,2],
  dat_ATUS$AGE[!is.na(dat_ATUS$HISPAN)],
  d_educ_r[!is.na(dat_ATUS$HISPAN),2:7],
  d_marstat[!is.na(dat_ATUS$HISPAN),2:6],
  d_race[!is.na(dat_ATUS$HISPAN),2:20],
  d_hispanic[,2:5],
  d_sex[!is.na(dat_ATUS$HISPAN),2],
  d_region[!is.na(dat_ATUS$HISPAN),2:4]) 



names(dat_for_analysis_sub)
names(dat_for_analysis_sub) <- sub("dat_ATUS.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "any_time_sports"
names(dat_for_analysis_sub)[2] <- "AGE"
names(dat_for_analysis_sub)[37] <- "SEX"
names(dat_for_analysis_sub)
#A.I chose to use SEX and education as predictors, they seem exogenous. I do beleive that the dummy varuables are important because they allow you to narrow your data and navagate easier.
#B.Not all of my code with through, i did recieve a couple errors. But of those taht went through, the estimates did seem plausiable and statistically significant. For high school graduate the mean was 0.02268 with a max of 1. WHich i was surprised the mean was so low compared to the max.
#C.No they shouldnt be at zero, if making the coefficent to zero will result in an inaccurate test, since we are tetsing the number of childcare.
#D.The predicted probabilities are 8.876
#E. Type 1 error (rejcting a true null hypothesis) occurred twice and Type 2  happen none.
#7
model_logit1 <- dat_use1 <- any_t_care <- (ACT_CAREHH > 0)
#A. Since we were tasked to make a logit with the subgroup we used prior the variables remain the same: SEX and education
#B. I ran into an error, my laptop canot find "ACT_CAREHH" But the estimate shouldnt be plausible based off the numbers from the data
#C. A zero coefficient does work in this case.
#D. I dont have the probabilities because I ran into an error 
#E.
#8
#Instead of using OLS or Logit im going to run a summary 
dat_ATUS <- subset(dat_ATUS), (dat_ATUS$SEX == "FEMALE") & (dat_ATUS$HH_SIZE == 1) &  (dat_ATUS$AGE <50 )
#above i am making a subset of femlale who are only living by themselves that are over the age of 50. 
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
#I ran into an error again running this code but i did want to test out how many single women over the age of 50 would be taking car eof children. 