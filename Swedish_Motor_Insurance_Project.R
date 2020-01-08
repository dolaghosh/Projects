insurance<-read.csv("SwedishMotorInsurance.csv")
names(insurance)
head(insurance)
summary(insurance)
library (caTools)
modelpayment<-lm(Payment~Insured+Claims,insurance)
summary(modelpayment)
# sample the input data with 70% for training and 30% for testing
sample <- sample.split(insurance$Payment,SplitRatio=0.70)
train_data <- subset(insurance,sample==TRUE)#split of the data using subset command 
test_data <- subset(insurance,sample==FALSE)
model<-lm(Payment~.,data=train_data)
summary(model)
#prediction on  on testing data set 
predtest<-predict(model,test_data)
head(predtest)
predtest
head(predtest)
# atach it with the dataframe
predtest1<- data.frame(predtest)
#to bind the predicted data set with test data set by cbind function
final_data<- cbind(test_data,predtest1)
write.csv(final_data,"Motor_insurance_output.csv")
# Data Visualization of the model
cor.data<-cor(model)
cor.data
corrplot(cor.data,method='color')
corrgram(insurance,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

#aggregate(Claims~Zone,FUN = sum,data=insurance)
# Aggregate function to find highest "Insured" amount, "Claims" and "Payment" based on "Zone"
# Looking at the table we find out "Zone 4:Rural areas in southern Sweden" has the highest Insured Claims   Payment" 
aggZone = aggregate(insurance,
                by = list(insurance$Zone),
                FUN = sum)
aggZone
# Aggregate function to find out 2: 1000-15000 based on "Kilometres"
# Looking at the table we find out "Kilometres 2: 1000-15000 km" has the highest Insured Claims   Payment" 
aggKilo = aggregate(insurance,
                by = list(insurance$Kilometres),
                FUN = sum)
aggKilo
# Aggregate function to find out 2: 1000-15000 based on "Kilometres"
# Looking at the table we find out "Bonus 7: No of policy years" has the highest Insured Claims   Payment" 
aggBonus = aggregate(insurance,
                by = list(insurance$Bonus),
                FUN = sum)
aggBonus

# Claims Model 
sample1 <- sample.split(insurance$Payment,SplitRatio=0.70)
train_data1 <- subset(insurance,sample1==TRUE)#split of the data using subset command 
test_data1 <- subset(insurance,sample1==FALSE)
model2<-lm(Claims~.,insurance)
model2
summary(model2)
claims.frame<-data.frame(model2)
predclaims<-predict(claims.frame,test_data1)
head(predclaims)
predclaims
head(predclaims)
#to bind the predicted data set with test data set by cbind function
final_data_claims<- cbind(test_data1,predclaims)

