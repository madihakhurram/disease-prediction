setwd("D:/MS Thesis/ThesisData/Collected Data Set")

data <- read.csv("MasterData.csv", stringsAsFactors = FALSE)
head(data)

# Since there are only 8 observations in time
data$Time <- NULL

# Convert into date objects from string dates
data$Date <- as.Date(data$Date, "%d-%b-%y")

# Since all are male, therefore this variable is not needed
data$Gender <- NULL

hist(data$Age)
summary(data$Height)
summary(data$Weight)
summary(data$SystolicBP)

data <- data[!is.na(data$SystolicBP) & !is.na(data$Temperature),]
summary(data$SystolicBP)

# Useless variable
data$Stats <- NULL

file <- data$ECGLink
ecgdata <- read.csv(file, stringsAsFactors = FALSE)
head(data)

#calculation of BMI

#1= Very Severely Under Weight
#2= Severe Under weight  
#3=Under weight
#4=Normal
#5=Over Weight
#6=Moderate
#7=Severely obese
#8=Very obese
height1<-data$Height
Weight1<-data$Weight
height1
mheight<- height1/100
data$BMI<-0
BMI<-Weight1/mheight
barplot(BMI)
BMI
data$Obese<-NULL
data$obese<-sapply(data$BMI, function(x) { if(x<15)"Very Severely Under Weight" else if (x>=15 && X<=16)"Severe Under weight " 
    else if(x>=16.5 & x<18.5)"Under weight" else if (x>=18.5 & x<25)"Normal" else if (x>=25 && x<30)"Over weight" else if (x>=30 && x<35)"Moderate" 
    else if (x>= 35 & x<40)"Severely Obese" else "Very Obese"})
dist<-table (data$obese)
barplot(table(data$obese))
data$obese

output1<-data[,c("BMI","obese")]
write.csv(output1,"obese.csv",sep=",", row.names=FALSE, quote = FALSE)


#Calculation for Blood pressure


data$bpdata<-NULL
colnames(data)
data$bpdata<-sapply(data$SystolicBP,data$DiastolicBP,function(x,y){if(x<120 && y<80)"Normal"
    else if ((x>=120 && X<=139)&& (y>=80 && y<=89))"Pre-hypertension"
    else if ((x>=140 && X<=159)&& (y>=90 && y<=99))"Hypertension-I"
    else if (x>=160 && y>=100)"Hypertension-II"
    else "wrong BP"})
dist<-table (data$bpdata)
barplot(table(data$bpdata))

output2<-data[,c("SystolicBP","DiastolicBP", "bpdata")]
write.csv(output2,"bp.csv",sep=",", row.names=FALSE, quote = FALSE)


#for HR

data$HR<-NULL
data$HR<-sapply (data$Age, function(x){if (x>=18 && x<=25){
    sapply(data$HeartRate, function (y){if (y>=49 && y<=55)"Athlete" 
        else if (y>=56 && y<=61)"Excellent"
        else if (y>=62 && y<=65)"Good"
        else if (y>=66 && y<=69)"Above Average"
        else if (y>=70 && y<=73)"Average"
        else if (y>=74 && y<=81)"Below Average"
        else if (y>=82)"Poor"})}
    else if(x>=26 && x<=35){
        sapply(data$HeartRate, function (y){if (y>=49 && y<=55)"Athlete" 
            else if (y>=56 && y<=61)"Excellent"
            else if (y>=62 && y<=65)"Good"
            else if (y>=66 && y<=69)"Above Average"
            else if (y>=70 && y<=73)"Average"
            else if (y>=74 && y<=81)"Below Average"
            else if (y>=82)"Poor"})}
    
    else if (x>=36 && x<=45){
        sapply(data$HeartRate, function (y){if (y>=50 && y<=56)"Athlete" 
            else if (y>=57 && y<=62)"Excellent"
            else if (y>=63 && y<=66)"Good"
            else if (y>=67 && y<=70)"Above Average"
            else if (y>=71 && y<=75)"Average"
            else if (y>=76 && y<=82)"Below Average"
            else if (y>=83)"Poor"})}
    else if (x>=46 && x<=55){
        sapply(data$HeartRate, function (y){if (y>=50 && y<=57)"Athlete" 
            else if (y>=58 && y<=63)"Excellent"
            else if (y>=64 && y<=67)"Good"
            else if (y>=68 && y<=71)"Above Average"
            else if (y>=72 && y<=76)"Average"
            else if (y>=77 && y<=83)"Below Average"
            else if (y>=84)"Poor"})}
    else if (x>=56 && x<=65){
        sapply(data$HeartRate, function (y){if (y>=51 && y<=56)"Athlete" 
            else if (y>=57 && y<=61)"Excellent"
            else if (y>=62 && y<=67)"Good"
            else if (y>=68 && y<=71)"Above Average"
            else if (y>=72 && y<=75)"Average"
            else if (y>=76 && y<=81)"Below Average"
            else if (y>=82)"Poor"})}
    else if(x>=66){
        sapply(data$HeartRate, function (y){if (y>=50 && y<=55)"Athlete" 
            else if (y>=56 && y<=61)"Excellent"
            else if (y>=62 && y<=65)"Good"
            else if (y>=66 && y<=69)"Above Average"
            else if (y>=70 && y<=73)"Average"
            else if (y>=74 && y<=79)"Below Average"
            else if (y>=80)"Poor"})}
})
dist<-table (data$HR)
barplot(table(data$HR))
output3<-data[,c("HeartRate","HR")]
write.csv(output3,"pluseRate.csv",sep=",", row.names=FALSE, quote = FALSE)
