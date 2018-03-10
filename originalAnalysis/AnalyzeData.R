#Load libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(boot)
library(checkpoint)

# Uncomment line below if code doesn't work
# checkpoint("2014-12-12")

# Load data
data<-read.csv("JaraEttinger_Tenenbaum_Schulz_Results.csv") %>% tbl_df

#EXPERIMENT 1---------------------------------------------------------

Exp1<-filter(data,Experiment=="Experiment 1")
# Look at the status of participants in Experiment 1
table(Exp1$Status)
# Warmupfail=12: 12 toddlers were recruited but not included because they did not complete the warmup task.
# 24 toddlers tested: 19 (Data) + 2 (excluded due to family interference) + 3 (excluded due to position bias)

# Get statistics for Participants section
TestedChildren<-filter(Exp1,Status!="WarmupFail")
mean(TestedChildren$DaysOld)/30.4368 # 21.58 months old
min(TestedChildren$DaysOld)/30.4368 # 17.12 months old
max(TestedChildren$DaysOld)/30.4368 # 28.52 months old
sd(TestedChildren$DaysOld) # 96.63 days

# Final results
TestedChildren %>% filter(Status=="Data") %>% select(Choice) %>% table
# 15 chose the competent agent, 1 chose the incompetent agent, and 3 didn't make a choice.

#EXPERIMENT 2---------------------------------------------------------

Exp2<-filter(data,Experiment=="Experiment 2")
# Look at the status of participants in Experiment 2
table(Exp2$Status)
# Warmupfail=5 participants
# 17 two year-olds tested

# Get statistics for Participants section
TestedChildren<-filter(Exp2,Status!="WarmupFail")
mean(TestedChildren$DaysOld)/365.25 # 2.64 years old
min(TestedChildren$DaysOld)/365.25 # 2.258 years old
max(TestedChildren$DaysOld)/365.25 # 2.962 years old
sd(TestedChildren$DaysOld) # 83.74 days

# Final results
TestedChildren %>% filter(Status=="Data") %>% select(Choice) %>% table
# 11 chose the competent agent, 5 chose the incompetent agent, and 1 didn't make a choice.

#EXPERIMENT 3---------------------------------------------------------

Exp3<-filter(data,Experiment=="Experiment 3")
# Look at the status of participants in Experiment 3
table(Exp3$Status)
# Warmupfail=13 participants
# 66 two year-olds tested

# Get statistics for Participants section
TestedChildren<-filter(Exp3,Status!="WarmupFail")
mean(TestedChildren$DaysOld)/365.25 # 2.48 years old
# NOTE: Because we're using 365.25 as the average length of a year,
# the youngest participant appears to be 1.998 years-old. This child
# was tested on their birthday and was thus 2.00 years old.
min(TestedChildren$DaysOld)/365.25 # 1.998 years old for child tested on their birthday
max(TestedChildren$DaysOld)/365.25 # 2.978 years old
sd(TestedChildren$DaysOld) # 114.29 days days

# Final results by condition
TestedChildren %>% filter(Status=="Data") %>% plyr::ddply(c("Condition"),function(x){return(table(x$Choice))})
# 10 children did not make a choice: 3 in the control condition, five in the nicer condition, and two in the play condition.
# Control condition: 11/16 chose the competent agent
# Nicer condition: 5/16 chose the competent agent
# Play condition: 13/16 chose the competent agent

# confidence intervals -----------------

ProportionFunction<-function(data,indices){return(sum(data[indices])/length(data[indices]))}

# Experiment 1
exp1<-c(rep(1,15),rep(0,1))
resultsexp1 <- boot(data=exp1, statistic=ProportionFunction, R=500000)
boot.ci(resultsexp1,type="basic") # 87.50%-100.00%

# Experiment 2
exp2<-c(rep(1,11),rep(0,5))
resultsexp2 <- boot(data=exp2, statistic=ProportionFunction, R=500000)
boot.ci(resultsexp2,type="basic") # 50.00%-93.75%

# Experiment 3 Play condition
exp3p<-c(rep(1,13),rep(0,3))
resultsexp3p <- boot(data=exp3p, statistic=ProportionFunction, R=500000)
boot.ci(resultsexp3p,type="basic") # 62.50%-100%

# Experiment 3 Nicer condition
exp3n<-c(rep(1,5),rep(0,11))
resultsexp3n <- boot(data=exp3n, statistic=ProportionFunction, R=500000)
boot.ci(resultsexp3n,type="basic") # 6.25%-50.00%

# Experiment 3 control condition
exp3c<-c(rep(1,11),rep(0,5))
resultsexp3c <- boot(data=exp3c, statistic=ProportionFunction, R=500000)
boot.ci(resultsexp3c,type="basic") # 50.00%-93.75%

# Compare Play and Nicer conditions in Experiment 3 ------------------------------

# Fisher's exact test
Exp3ChoiceSwitch <-
  matrix(c(13, 5, 3, 11),
         nrow = 2,
         dimnames =
           list(c("Play", "Nicer"),
                c("Competent", "Incompetent")))
Exp3ChoiceSwitch
fisher.test(Exp3ChoiceSwitch)

# Bootstrap the difference
Exp3NP<-data.frame(
  choice=c(rep(1,13),rep(0,3),rep(1,5),rep(0,11)),
  condition=c(rep("Play",16),rep("Nicer",16))
)

IncreasePerc<-function(x,id){
  dat<-x[id,]
  d1<-filter(dat,condition=="Play")
  d2<-filter(dat,condition=="Nicer")
  res<-(sum(d1$choice)/nrow(d1))-((sum(d2$choice)/nrow(d2)))
  return(res)
}
NicerDiff<-boot(Exp3NP,statistic=IncreasePerc, R=500000)
boot.ci(NicerDiff,type="basic") # 21.05%-81.67%

# Compare Nicer and Control conditions in Experiment 3 ---------------------------

# Fisher's exact test
Exp3NiceBias <-
  matrix(c(5, 11, 11, 5),
         nrow = 2,
         dimnames =
           list(c("Nicer", "Control"),
                c("Competent", "Incompetent")))
Exp3NiceBias
fisher.test(Exp3NiceBias)

# Bootstrap the difference
Exp3NB<-data.frame(
  choice=c(rep(1,11),rep(0,5),rep(1,5),rep(0,11)),
  condition=c(rep("Baseline",16),rep("Nicer",16))
)

IncreasePerc<-function(x,id){
  dat<-x[id,]
  d1<-filter(dat,condition=="Baseline")
  d2<-filter(dat,condition=="Nicer")
  res<-(sum(d1$choice)/nrow(d1))-((sum(d2$choice)/nrow(d2)))
  return(res)
}
set.seed(59283) # so we can replicate the exact numbers
NicerDiff<-boot(Exp3NB,statistic=IncreasePerc, R=500000)
boot.ci(NicerDiff,type="basic") # 6.17%-71.03%

# Plots -----------------------------------------------------------

Summarydat<-data %>% filter(Status=="Data",Choice!="None") %>%
  dplyr::group_by(Experiment,Condition) %>%
  dplyr::summarise(Total=n(),Competent=sum(Choice=="Competent")*100/Total,Incompetent=100-Competent) %>%
  select(-Total) %>%
  gather(Answer,Percentage,Competent,Incompetent) %>%
  arrange(Experiment,Condition,Answer)

#Add confidence intervals
Summarydat$BootLower=NA
Summarydat$BootUpper=NA

# From code above:
Summarydat<-arrange(Summarydat,Experiment,Condition)
Summarydat$BootUpper=c(100.00,NA,93.75,NA,93.75,NA,50.00,NA,100.00,NA)
Summarydat$BootLower=c(087.50,NA,50.00,NA,50.00,NA,06.26,NA,062.50,NA)

Summarydat$Condition<-as.character(Summarydat$Condition)
Summarydat[which(Summarydat$Experiment=="Experiment 1"),]$Condition=c("Exp 1","Exp 1")
Summarydat[which(Summarydat$Experiment=="Experiment 2"),]$Condition=c("Exp 2","Exp 2")

Summarydat$order=c(1,1,2,2,5,5,4,4,3,3)

ggplot(Summarydat,aes(x=order,y=Percentage,fill=Answer))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=BootLower,ymax=BootUpper),width=0.2)+
  scale_x_continuous("Experiment",breaks=c(1:5),labels=c("Exp 1","Exp 2","Play","Nicer","Control"))+
  ggtitle("Results from all experiments")
