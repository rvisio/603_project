


library(dplyr)
library(ggplot2)


df <- read.csv("C:/Users/MZeidan/Documents/QA/OR/big_data/algo_v1.csv", header = TRUE)



#this is just to check the data types of the file being loaded in. 
#sapply(df, class)


#first and 3rd quarter data only for now
df <- subset(df, Quarter == 1 | Quarter == 3)

#first down data
df <- subset(df, Down == 1)

#score difference cant be too high
df <- subset(df, Score.Diff < 11)

#timeouts in the data mess stuff up, same with intercept
#TODO need to add fumble flag, challenge flag
df <- subset(df, timeout.flag !=  1)
df <- subset(df, intercepted.flag  !=  1)

#regular loess regression
by_ydline <- summarize(group_by(df, ABS.Yardline), raw_ep = mean(nextscore), count = n())

ep_model <- loess(raw_ep ~ ABS.Yardline, by_ydline, weights = count, span = .5)

by_ydline$smooth_ep <- predict(ep_model, by_ydline)

ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_ep), color='red', size=1.3)




