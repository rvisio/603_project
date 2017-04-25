library(dplyr)
library(ggplot2)

df <- read.csv("~/Desktop/Rob_Data_Clean.csv", header = TRUE)

hist(df$nextscore)

df <- subset(df, Quarter == 1 | Quarter == 3)

by_ydline <- summarize(group_by(df, ABS.Yardline), raw_ep = mean(nextscore), count = n())

ep_model <- loess(raw_ep ~ ABS.Yardline, by_ydline, weights = count, span = .4)

by_ydline$smooth_ep <- predict(ep_model, by_ydline)

ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_ep), color='red', size=1.3)
