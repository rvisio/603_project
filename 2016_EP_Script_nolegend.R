



library(dplyr)
library(ggplot2)


df <- read.csv("/Users/robjarvis/603_project//big_data/algo_v2_values_chenedit.csv", header = TRUE)


#this is just to check the data types of the file being loaded in. 
#sapply(df, class)

#first and 3rd quarter data only for now
df <- subset(df, Quarter == 1 | Quarter == 3)

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

Negative_EP <- data.frame( x = c(-Inf, Inf), y = 0, Negative_EP = factor(0) )
kickoff_line <- data.frame( x = 80 ,y= c(-Inf, Inf), kickoff_line = factor(0) )
kickoff_cutoff <- geom_line(aes( x, y, linetype = kickoff_line ), kickoff_line, size=0.8)
cutoff_line <- geom_line(aes( x, y, linetype = Negative_EP ), Negative_EP, size=1.3)

ggplot(by_ydline, aes(x=ABS.Yardline, y=raw_ep)) + geom_point(color='blue') + geom_line(aes(x=ABS.Yardline, y=smooth_ep), color='red', size=1.3)+cutoff_line


scale_fill_discrete(name = "New Legend Title")


  legend_title <- "New Title"
  
  
  p <- ggplot() + geom_point(data = by_ydline, aes(x=ABS.Yardline, y=raw_ep,color="Points") ) + 
    geom_line(data=by_ydline, aes(x=ABS.Yardline, y=smooth_ep,color="EarnedPoints"), size=1.3)
  
  p <- p +  scale_fill_manual(
    name   = legend_title,
    breaks = c('upper', 'lower'), # <<< corresponds to fill aesthetic labels
    values = c(lightGreen, lightRed),
    labels = c('Over', 'Under'))
  
  p <- p + labs(colour = "donut dude bro!!!")
p
  p <- p + guides(fill=guide_legend(title="New Legend Title"))

  p <- p +  scale_fill_manual(
      name   = legend_title,
      breaks = c('upper', 'lower'), # <<< corresponds to fill aesthetic labels
      values = c(lightGreen, lightRed),
      labels = c('Over', 'Under'))
  

  p <- p +ggtitle("2016 NFL Expected Points")+ theme(plot.title = element_text(hjust = 0.5)) + labs(x="Yard Line", y="Expected Points",linetype='custom title') +cutoff_line +kickoff_cutoff

  p



