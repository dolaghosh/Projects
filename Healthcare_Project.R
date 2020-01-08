setwd("C:/Users/aghosh/Downloads/dola simplilearn/R  doc/Project/Projects for Submission/Healthcare/Healthcare")
#To record the patient statistics, the agency wants to find the age category of people who
#frequent the hospital
hops<-read.csv('HospitalCosts.csv')
hist(hops$AGE)
summary(as.factor(hops$AGE))
#the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure
pl<-ggplot(hops,aes(x=AGE,y=TOTCHG)) + geom_point(color='blue',alpha=0.5)
print(pl)
aggregate(TOTCHG~AGE,FUN = sum,data=hops)
y<-aggregate(TOTCHG~AGE,FUN = sum,data=hops)
max(y)
# the Grp which has max hospitalization
aggregate(TOTCHG~APRDRG,FUN=sum,data=hops)
x<-aggregate(TOTCHG~APRDRG,FUN=sum,data=hops)
max(x)
# Wanted to find out the RACE of the people who mostly frequent the hospital
head(hops)
hist(hops$RACE)
#the agency needs to analyze if the race of the patient is related to the hospitalization costs.
x<-hops$RACE
y<-hops$TOTCHG
plot(x,y)
lm.out <- lm(y ~ x )
lm.out
abline(lm.out)
summary(lm.out)
#the agency has to analyze the severity of the hospital costs by age and gender
results1.frame<-lm(TOTCHG ~ AGE+FEMALE,hops)
results1.frame
summary(results1.frame)
#to find if the length of stay can be predicted from age, gender, and race.
results2.frame<-lm(LOS ~ AGE+FEMALE+RACE,hops)
results2.frame
summary(results2.frame)
#the agency wants to find the variable that mainly affects the hospital costs.
#Corplot and Corrgram for the analysis of complete dataset
any(is.na(hops))
num.cols <- sapply(hops, is.numeric)
cor.data <- cor(hops[,num.cols])
mydata.clean<-na.omit(hops)
nrow(mydata.clean)
cor.data<-cor(mydata.clean)
cor.data
corrplot(cor.data,method='color')
corrgram(hops,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
#regression for analysis of TOTCHG and how it is related to the other variables  
results3.frame<-lm(TOTCHG ~ AGE+FEMALE+RACE+LOS+APRDRG,hops)
results3.frame
summary(results3.frame)
# HAS the Total cost anything to do with the RACE : MALRACTICE :using ANOVA
hops <- na.omit(hops)
model <- aov(TOTCHG ~ RACE, data = hops)
model
