
library(dplyr)
library(ggplot2)




#####

# this is a loess regression showing EP for the 2016 data set of pass vs run_by_yardline
# note that passing is in green and running is in red



#####

#####TODO

## add a legend
## add a title
## format axis to be more readable
## note in the title that this is 2016 data 
## change axis names 
## change line colors


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


#where yardline is not 100

first_downs <- df
#first_downs <- subset(first_downs, Quarter == 1)
#first_downs <- subset(first_downs, Down == 1)


hist(first_downs$nextscore)


first_downs <- subset(first_downs, scorediff >= -10 & scorediff <= 10)
by_ydline <- summarize(group_by(first_downs, ABS.Yardline), raw_ep = mean(nextscore), count = n())
#view results of avg by yard line
plot(by_ydline$ydline, by_ydline$raw_ep)
plot(by_ydline$ydline, by_ydline$count)
#create a LOWESS non-parametric model of expected points
ep_model <- loess(raw_ep ~ ABS.Yardline, by_ydline, weights = count, 
                  span = .4)
#output LOWESS results to a vector
by_ydline$smooth_ep <- predict(ep_model, by_ydline)

#view result
plot(by_ydline$ydline, by_ydline$smooth_ep)

#ggplot example
ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep)) + geom_point(color='blue') +
  geom_line(aes(x=ABS.Yardline, y=smooth_ep), color='red', size=1.3)

runs <- subset(first_downs, run_or_pass == 'run')
passes <- subset(first_downs, run_or_pass == 'pass')

run_by_yardline <- summarize(group_by(runs, ABS.Yardline), raw_ep_run = mean(nextscore), count_run = n())
pass_by_yardline <- summarize(group_by(passes, ABS.Yardline), raw_ep_pass = mean(nextscore), count_pass = n())

run_model <- loess(raw_ep_run ~ ABS.Yardline, run_by_yardline, weights = count_run, span = .5)
pass_model <- loess(raw_ep_pass ~ ABS.Yardline, pass_by_yardline, weights = count_pass, span = .5)

#output LOWESS results to a vector
by_ydline$smooth_run <- predict(run_model, run_by_yardline)
by_ydline$smooth_pass <- predict(pass_model, pass_by_yardline)

ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_run), color='red', size=1.3) + 
  geom_line(aes(x=ABS.Yardline, y=smooth_pass), color='green', size=1.3)


cutoff <- data.frame( x = c(-Inf, Inf), y = 0, cutoff = factor(0) )

Negative_EP <- data.frame( x = c(-Inf, Inf), y = 0, Negative_EP = factor(0) )
kickoff_cutoff <- geom_line(aes( x, y, linetype = kickoff_line ), kickoff_line, size=0.8)


ggplot() + geom_point(data = by_ydline, aes(x=ABS.Yardline, y=raw_ep,color="Points") ) + 
  geom_line(data=by_ydline, aes(x=ABS.Yardline, y=smooth_run,color="Run"), size=1.3) +
  geom_line(data=by_ydline, aes(x=ABS.Yardline, y=smooth_pass, color='Pass'), size=1.3) +
  scale_fill_manual(name="ez pz lgnd", values=c(points="blue", run="red", pass = "green")) + 
  ggtitle("2016 NFL Expected Points - Run/Pass")+theme(plot.title = element_text(hjust = 0.5))  + labs(x="Yard Line", y="Expected Points") +geom_line(aes( x, y, linetype = Negative_EP ), Negative_EP, size=1.3)+ kickoff_cutoff



