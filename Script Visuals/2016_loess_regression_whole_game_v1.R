library(dplyr)
library(ggplot2)


df <- read.csv("C:/Users/MZeidan/Documents/QA/OR/big_data/algo_v2_values_chenedit.csv", header = TRUE)


#this is just to check the data types of the file being loaded in. 
#sapply(df, class)

#first and 3rd quarter data only for now
#df <- subset(df, Quarter == 1 | Quarter == 3)

df <- subset(df, timeout.flag !=  1)
df <- subset(df, intercepted.flag  !=  1)
df <- subset(df, fumble.flag  !=  1)
df <- subset(df, challenge.flag  !=  1)
df <- subset(df, kick_flag  !=  1)
df <- subset(df, punt_flag  !=  1)
df <- subset(df, YardLine  !=  100)
df <- subset(df, nextscore  <  8)
df <- subset(df, nextscore  >  -8)


#regular loess regression
by_ydline <- summarize(group_by(df, ABS.Yardline), raw_ep = mean(nextscore), count = n())

ep_model <- loess(raw_ep ~ ABS.Yardline, by_ydline, weights = count, span = .6)

by_ydline$smooth_ep <- predict(ep_model, by_ydline)

ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_ep), color='red', size=1.3)
