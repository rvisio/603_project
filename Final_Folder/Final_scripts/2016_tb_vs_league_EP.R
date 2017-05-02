


library(dplyr)
library(ggplot2)


#########################################


##this just makes a simple line graph comparing EP from 2011 to 2016

##TODO

## legend
## add title/format axis



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
#df <- subset(df, Down == 1)

by_ydline <- summarize(group_by(df, ABS.Yardline), raw_ep = mean(nextscore), count = n())

ep_model <- loess(raw_ep ~ ABS.Yardline, by_ydline, weights = count, span = .6)

by_ydline$smooth_ep <- predict(ep_model, by_ydline)

#ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_ep), color='red', size=1.3)

#################
tb <- read.csv("C:/Users/MZeidan/Documents/QA/OR/big_data/tom_brady_elite.csv", header = TRUE)

tb <- subset(tb, Quarter == 1 | Quarter == 3)

tb <- subset(tb, Score.Diff >= -10 & Score.Diff <= 10)

by_ydline_tb <- summarize(group_by(tb, ABS.Yardline), raw_ep_tb = mean(nextscore), count = n())

#view results of avg by yard line
plot(by_ydline_tb$ABS.Yardline, by_ydline_tb$raw_ep_tb)
plot(by_ydline_tb$ABS.Yardline, by_ydline_tb$count)

#create a LOWESS non-parametric model of expected points
ep_model_tb <- loess(raw_ep_tb ~ ABS.Yardline, by_ydline_tb, weights = count, span = .4)

#output LOWESS results to a vector
by_ydline_tb$smooth_ep_tb <- predict(ep_model_tb, by_ydline_tb)

#view result
#plot(by_ydline$ydline, by_ydline$smooth_ep)

#ggplot example
#ggplot(by_ydline, aes(x=ydline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ydline, y=smooth_ep), color='red', size=1.3)

#2016
runs <- df

data_2016 <- df

#2011
passes <- first_downs

data_2011 <- first_downs

#2011/2016
run_by_yardline <- summarize(group_by(data_2016, ABS.Yardline), raw_ep_run = mean(nextscore), count_run = n())
pass_by_yardline <- summarize(group_by(data_2011, ABS.Yardline), raw_ep_pass = mean(nextscore), count_pass = n())

run_model <- loess(raw_ep_run ~ ABS.Yardline, run_by_yardline, weights = count_run, span = .5)
pass_model <- loess(raw_ep_pass ~ ABS.Yardline, pass_by_yardline, weights = count_pass, span = .5)

#output LOWESS results to a vector
by_ydline$smooth_run <- predict(run_model, run_by_yardline)
by_ydline$smooth_pass <- predict(pass_model, pass_by_yardline)



ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep_tb)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_run), color='red', size=1.3) + 
  geom_line(aes(x=ABS.Yardline, y=smooth_pass), color='green', size=1.3)



ggplot(by_ydline_tb, aes(x=ABS.Yardline, y=raw_ep_tb)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_ep_tb), color='red', size=1.3) + 
  geom_line(aes(x=ABS.Yardline, y=smooth_pass), color='green', size=1.3)




EP_TB_point <- geom_point(data = by_ydline_tb, aes(x=ABS.Yardline, y=raw_ep_tb,color="blue"))
EP_League_point <- geom_point(data = by_ydline, aes(x=ABS.Yardline, y=raw_ep,color="red"))
EP_TB_Line <- geom_line(data=by_ydline_tb, aes(x=ABS.Yardline, y=smooth_ep_tb,color="EP_Line_2011 "), size=1.3)
EP_League_Line <- geom_line(data=by_ydline, aes(x=ABS.Yardline, y=smooth_ep,color="EP_Line_2016 "), size=1.3)

Negative_EP <- data.frame( x = c(-Inf, Inf), y = 0, Negative_EP = factor(0) )
kickoff_line <- data.frame( x = 80 ,y= c(-Inf, Inf), kickoff_line = factor(0) )
kickoff_cutoff <- geom_line(aes( x, y, linetype = kickoff_line ), kickoff_line, size=0.8)
cutoff_line <- geom_line(aes( x, y, linetype = Negative_EP ), Negative_EP, size=0.8)

EP_TB_Plot <- ggplot() + EP_TB_point + EP_League_point + EP_TB_Line + EP_League_Line + kickoff_cutoff + cutoff_line

EP_TB_Plot + ggtitle("Tom Brady vs the League")+theme(plot.title = element_text(hjust = 0.5)) +labs(x="Yard Line", y="Expected Points")


EP_Points_2016 <- geom_point(data = by_ydline_2016, aes(x=ABS.Yardline, y=raw_ep_2016,color="EP2016"))
EP_Line_2011 <- geom_line(data=by_ydline, aes(x=ydline, y=smooth_ep,color="EP_Line_2011 "), size=1.3)
EP_Line_2016 <- geom_line(data=by_ydline_2016, aes(x=ABS.Yardline, y=smooth_ep_2016,color="EP_Line_2016 "), size=1.3)
Negative_EP <- data.frame( x = c(-Inf, Inf), y = 0, Negative_EP = factor(0) )
kickoff_line <- data.frame( x = 80 ,y= c(-Inf, Inf), kickoff_line = factor(0) )
kickoff_cutoff <- geom_line(aes( x, y, linetype = kickoff_line ), kickoff_line, size=0.8)
cutoff_line <- geom_line(aes( x, y, linetype = Negative_EP ), Negative_EP, size=0.8)
title <-ggtitle("2016-2011 NFL Expected Points Comparison")+theme(plot.title = element_text(hjust = 0.5)) 
labels <- labs(x="Yard Line", y="Expected Points")














