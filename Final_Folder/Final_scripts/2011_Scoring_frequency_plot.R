
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

p1 <- hist(Q1_Score_Freq, border="blue", col="green")
p2 <- hist(Q2_Score_Freq, border="blue", col="green")
p3 <- hist(Q3_Score_Freq, border="blue", col="green")
p4 <- hist(Q4_Score_Freq, border="blue", col="green")

par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot( p1,col="cyan",xlim=c(-10,10), main="",xlab ="", ylab="") # first histogram
axis(side = 2, lwd = 2,lwd.ticks=3)
title(main = "Q1 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p2, col="red", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
axis(side = 2, lwd = 2)
title(main = "Q2 Scoring Frequency",xlab="Points Scored", ylab="Frequency")

plot( p3, col="green", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
axis(side = 2, lwd = 2)
title(main = "Q3 Scoring Frequency",xlab="Points Scored", ylab="Frequency")
plot( p4, col="blue", xlim=c(-10,10), main="",xlab ="", ylab="")  # first histogram
axis(side = 2, lwd = 2)
title(main = "Q4 Scoring Frequency",xlab="Points Scored", ylab="Frequency")

mtext("2011 NFL Scoring Frequency", outer = TRUE, cex = 1.5)