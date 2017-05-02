

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


df <- read.csv("C:/Users/MZeidan/Documents/QA/OR/big_data/tom_brady_elite.csv", header = TRUE)
df <- subset(df, TomFlag  = 1)

#df <- read.csv("C:/Users/MZeidan/Documents/QA/OR/big_data/tom_brady_elite.csv", header = TRUE)
#df <- subset(df, TomFlag  != 0)



df <- subset(df, timeout.flag !=  1)
df <- subset(df, intercepted.flag  !=  1)
df <- subset(df, fumble.flag  !=  1)
df <- subset(df, challenge.flag  !=  1)
df <- subset(df, kick_flag  !=  1)
df <- subset(df, punt_flag  !=  1)
df <- subset(df, YardLine  !=  100)
df <- subset(df, nextscore  <  8)
df <- subset(df, nextscore  >  -8)
##df <- subset(df, Quarter == 1 | Quarter == 3)


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
#plot(by_ydline$ydline, by_ydline$smooth_ep)
Negative_EP <- data.frame( x = c(-Inf, Inf), y = 0, Negative_EP = factor(0) )
kickoff_line <- data.frame( x = 80 ,y= c(-Inf, Inf), kickoff_line = factor(0) )
kickoff_cutoff <- geom_line(aes( x, y, linetype = kickoff_line ), kickoff_line, size=0.8)
cutoff_line <- geom_line(aes( x, y, linetype = Negative_EP ), Negative_EP, size=0.8)
title <-ggtitle("2016-2011 NFL Expected Points Comparison")+theme(plot.title = element_text(hjust = 0.5)) 
labels <- labs(x="Yard Line", y="Expected Points")


TB_EP_points <-  geom_point(data = by_ydline, aes(x=ABS.Yardline, y=raw_ep,color="EP2011"))
TB_EP_line <- geom_line(data=by_ydline, aes(x=ABS.Yardline, y=smooth_ep,color="EP_Line_2011 "), size=1.3)

ggplot() + TB_EP_points +TB_EP_line+ cutoff_line +ggtitle("2016 Tom Brady EP")+theme(plot.title = element_text(hjust = 0.5))

