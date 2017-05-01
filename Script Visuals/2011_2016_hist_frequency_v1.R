

library(dplyr)
library(ggplot2)

############################################################################################################



########### theme



hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  
  #  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.10,"cm"),
  panel.spacing.y = unit(0.05,"cm"),
  
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)


###################












#load data into data frame
first_downs <- read.csv('first down data.csv')



data_2011 <- read.csv('first down data.csv')
data_2011 <- subset(data_2011, down == 1)


Q1 <- subset(data_2011, quarter == 1)
Q2 <- subset(data_2011, quarter == 2)
Q3 <- subset(data_2011, quarter == 3)
Q4 <- subset(data_2011, quarter == 4 )

Q1_Score_Freq <-Q1$nextscore
Q2_Score_Freq <-Q2$nextscore
Q3_Score_Freq <-Q3$nextscore
Q4_Score_Freq <-Q4$nextscore

p1 <- hist(Q1_Score_Freq)
p2 <- hist(Q2_Score_Freq)
p3 <- hist(Q3_Score_Freq)
p4 <- hist(Q4_Score_Freq)

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot( p1, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="") # first histogram
title(main = "Q1 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p2, col="red", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "Q2 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p3, col="green", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "Q3 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p4, col="blue", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "Q4 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
mtext("2011 NFL Scoring Data", outer = TRUE, cex = 1.5)



########## 2016 ##########

df <- read.csv("C:/Users/MZeidan/Documents/QA/OR/big_data/algo_v2_values_chenedit.csv", header = TRUE)


df <- subset(df, timeout.flag !=  1)
df <- subset(df, intercepted.flag  !=  1)
df <- subset(df, fumble.flag  !=  1)
df <- subset(df, challenge.flag  !=  1)
df <- subset(df, kick_flag  !=  1)
df <- subset(df, punt_flag  !=  1)
df <- subset(df, YardLine  !=  100)
df <- subset(df, nextscore  <  9)
df <- subset(df, nextscore  >  -9)


Q1 <- subset(df, Quarter == 1)
Q2 <- subset(df, Quarter == 2)
Q3 <- subset(df, Quarter == 3)
Q4 <- subset(df, Quarter == 4 )

Q1_Score_Freq <-Q1$nextscore
Q2_Score_Freq <-Q2$nextscore
Q3_Score_Freq <-Q3$nextscore
Q4_Score_Freq <-Q4$nextscore

p1_2016 <- hist(Q1_Score_Freq)
p2_2016 <- hist(Q2_Score_Freq)
p3_2016 <- hist(Q3_Score_Freq)
p4_2016 <- hist(Q4_Score_Freq)

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot( p1_2016, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "Q1 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p2_2016, col="red", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "Q2 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p3_2016, col="green", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "Q3 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p4_2016, col="blue", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "Q4 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
mtext("2016 NFL Scoring Data", outer = TRUE, cex = 1.5)



############## comparing quarters

par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot( p1, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2011-Q1 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p1_2016, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2016-Q1 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
mtext("2016 NFL Scoring Data", outer = TRUE, cex = 1.5)



par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot( p2, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2011-Q2 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p2_2016, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2016-Q2 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
mtext("2016 NFL Scoring Data", outer = TRUE, cex = 1.5)


par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot( p3, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2011-Q3 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p3_2016, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2016-Q3 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
mtext("2016 NFL Scoring Data", outer = TRUE, cex = 1.5)


par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot( p4, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2011-Q4 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p4_2016, col="cyan", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
title(main = "2016-Q4 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
mtext("2016 NFL Scoring Data", outer = TRUE, cex = 1.5)



