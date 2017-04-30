
library(dplyr)
library(ggplot2)


df <- read.csv("C:/Users/MZeidan/Documents/QA/OR/big_data/algo_v2_values_chenedit.csv", header = TRUE)

df <- subset(df, timeout.flag !=  1)
df <- subset(df, intercepted.flag  !=  1)
df <- subset(df, fumble.flag  !=  1)
df <- subset(df, challenge.flag  !=  1)
df <- subset(df, kick_flag  !=  1)
df <- subset(df, punt_flag  !=  1)
df <- subset(df, YardLine  !=  100)
df <- subset(df, nextscore  <  8)
df <- subset(df, nextscore  >  -8)

df <- subset(df, Quarter == 1 | Quarter == 3)

#by_ydline <- summarize(group_by(df, ABS.Yardline), raw_ep = mean(nextscore), count = n())

#ep_model <- loess(raw_ep ~ ABS.Yardline, by_ydline, weights = count, span = .6)

#by_ydline$smooth_ep <- predict(ep_model, by_ydline)


#######################



first_downs <- read.csv('first down data.csv')

first_downs <- subset(first_downs, quarter == 1 | quarter == 3)

first_downs <- subset(first_downs, scorediff >= -10 & scorediff <= 10)

by_ydline <- summarize(group_by(first_downs, ydline), raw_ep = mean(nextscore), count = n())


#create a LOWESS non-parametric model of expected points
ep_model <- loess(raw_ep ~ ydline, by_ydline, weights = count, span = .4)

#output LOWESS results to a vector
by_ydline$smooth_ep <- predict(ep_model, by_ydline)

#view result
#plot(by_ydline$ydline, by_ydline$smooth_ep)

#ggplot example
#ggplot(by_ydline, aes(x=ydline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ydline, y=smooth_ep), color='red', size=1.3)

#2016
#runs <- df

##data_2016 <- df

#view results of avg by yard line
plot(by_ydline$ydline, by_ydline$raw_ep)
plot(by_ydline$ydline, by_ydline$count)

#create a LOWESS non-parametric model of expected points
ep_model <- loess(raw_ep ~ ydline, by_ydline, weights = count, 
                  span = .4)

#output LOWESS results to a vector
by_ydline$smooth_ep <- predict(ep_model, by_ydline)






data_2016 <- subset(df,run_or_pass = "run")



ep_model <- loess(raw_ep ~ ABS.Yardline, by_ydline, weights = count, span = .6)

by_ydline$smooth_ep <- predict(ep_model, by_ydline)


runs <- subset(first_downs, playtype == 'run')
passes <- subset(first_downs, playtype == 'pass')
data_2016 <- subset(df,run_or_pass == "run")
data_2016_pass <- subset(df,run_or_pass == "pass")

#by_ydline_2016 <- summarize(group_by(df, ABS.Yardline), raw_ep = mean(nextscore), count = n())
by_ydline_2016 <- summarize(group_by(data_2016, ABS.Yardline), raw_ep_2016 = mean(nextscore), count_2016 = n())
by_ydline_2016_pass <- summarize(group_by(data_2016_pass, ABS.Yardline), raw_ep_2016_pass = mean(nextscore), count_2016_pass = n())
run_by_yardline <- summarize(group_by(runs, ydline), raw_ep_run = mean(nextscore), count_run = n())
pass_by_yardline <- summarize(group_by(passes, ydline), raw_ep_pass = mean(nextscore), count_pass = n())

model_2016 = loess(raw_ep_2016 ~ ABS.Yardline, by_ydline_2016, weights = count_2016, span = .5)
model_2016_pass = loess(raw_ep_2016_pass ~ ABS.Yardline, by_ydline_2016_pass, weights = count_2016_pass, span = .5)
run_model <- loess(raw_ep_run ~ ydline, run_by_yardline, weights = count_run, span = .5)
pass_model <- loess(raw_ep_pass ~ ydline, pass_by_yardline, weights = count_pass, span = .5)

#output LOWESS results to a vector
by_ydline$data_2016_pass = predict(model_2016_pass,by_ydline_2016_pass)
by_ydline$data_2016 = predict(model_2016,by_ydline_2016)
by_ydline$smooth_run <- predict(run_model, run_by_yardline)
by_ydline$smooth_pass <- predict(pass_model, pass_by_yardline)

ggplot(by_ydline, aes(x=ydline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ydline, y=smooth_run), color='red', size=1.3) + 
  geom_line(aes(x=ydline, y=smooth_pass), color='green', size=1.3) + geom_line(aes(x=ydline, y=data_2016), color='black', size=1.3)  + geom_line(aes(x=ydline, y=data_2016_pass), color='orange', size=1.3)
