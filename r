library(rpart)
setwd("D:/MS Thesis/ThesisData/Collected Data Set")


data <- read.csv("MasterData1.csv", stringsAsFactors = FALSE)
head(data)

# Since there are only 8 observations in time
data$Time <- NULL
# Convert into date objects from string dates
data$Date <- as.Date(data$Date, "%d-%b-%y")
# Since all are male, therefore this variable is not needed
data$Gender <- NULL

hist(data$Age)
plot(table(data$Age))
summary(data$Age)
summary(data$Height)
summary(data$Weight)
summary(data$SystolicBP)

data <- data[!is.na(data$SystolicBP) & !is.na(data$Temperature),]
summary(data$SystolicBP)


# Useless variable
data$Stats <- NULL

file <- data$ECGLink
ecgdata <- read.csv(file, stringsAsFactors = FALSE)
head(ecgdata)



data$BMI <- data$Weight / (data$Height / 100)
data$BMI
hist(data$BMI)

lookup <- function(value) {
  category <- "OBESE"
  if (value < 15)
    category <- "very-severely-UW"
  else if (value >= 15 && value <=16)
    category <- "SEVERE_UW"
  else if (value >=16.5 && value < 18.5)
    category <- "under-weight"
  else if (value >=18.5 && value < 25)
    category <- "NORMAL"
  else if (value >=25 && value < 30)
    category <- "Over-weight"
  else if (value >=30 && value < 35)
    category <- "MODERATE-1"
  else if (value>= 35 && value <40 )
    category <- "Severely-obese-2"
  else if (value >= 40)
    category<-"very-Obese-3"
  category
}

data$WeightCategory <- sapply(data$BMI, lookup)
plot(table(data$WeightCategory))
table(data$WeightCategory)
data$WeightCategory
output1 <- data[,c("BMI","WeightCategory")]
write.csv(output1, "obese.csv", sep=",", row.names=FALSE, quote = FALSE)


# Calculation for Blood pressure

data$BP<-NULL

for(i in 1:row(data))
{
  current.age<-data$Age[i]
  if (current.age>=15 && current.age<30 )
  {
    if((data$SystolicBP>=120 && data$systolic<=123) && (data$DiastolicBP>=80 && data$DiastolicBP<=84))
    {
      data$BP<-"NORMAL"
    }
    else if((data$SystolicBP>123 && data$DiastolicBP>84)|| (data$SystolicBP>123 || data$DiastolicBP>84))
    {
      data$BP<-"Hypertension"
    }
    else if((data$SystolicBP<120 && data$DiastolicBP<80) || (data$SystolicBP<120 || data$DiastolicBP<80 ))
    {
      data$BP<-"Hypotension"
    }
  }
  else if (current.age>=30 && current.age<40 )
  {
    if((data$SystolicBP>=123 && data$systolic<=130) && (data$DiastolicBP>=84 && data$DiastolicBP<=85))
    {
      data$BP<-"NORMAL"
    }
    else if((data$SystolicBP>130 && data$DiastolicBP>85)|| (data$SystolicBP>130 || data$DiastolicBP>85))
    {
      data$BP<-"Hypertension"
    }
    else if((data$SystolicBP<123 && data$DiastolicBP<84) || (data$SystolicBP<123 || data$DiastolicBP<84 ))
    {
      data$BP<-"Hypotension"
    }
  }
  else if (current.age>=40 )
  {
    if((data$SystolicBP>=130 && data$systolic<=140) && (data$DiastolicBP>=85 && data$DiastolicBP<=90))
    {
      data$BP<-"NORMAL"
    }
    else if((data$SystolicBP>140 && data$DiastolicBP>90)|| (data$SystolicBP>140 || data$DiastolicBP>90))
    {
      data$BP<-"Hypertension"
    }
    else if((data$SystolicBP<130 && data$DiastolicBP<85) || (data$SystolicBP<130 || data$DiastolicBP<85 ))
    {
      data$BP<-"Hypotension"
    }
  }
}
#Why data$BP shows "Hypotension" for records
data$BP  
# Multi-variable apply
#data$BPCategory <- mapply(bp_lookup, data$SystolicBP, data$DiastolicBP)
plot(table(data$BP))
table(data$BP)
#plot(table(data$BPCategory))
#table(data$BPCategory)
#data$BPCategory
output2 <- data[,c("Age","SystolicBP","DiastolicBP","BP")]
write.csv(output2, "BP.csv", sep=",", row.names=FALSE, quote = FALSE)


# For Heart Rate


data$HR<-NULL

for(i in 1:row(data))
{
  current.age<-data$Age[i]
      if (current.age>=18 && current.age<=25)
        {
        data$HR<-sapply(data$HeartRate, function (y)
          {
            if (y>=49 && y<=55)"Athlete" 
            else if (y>=56 && y<=61)"Excellent"
            else if (y>=62 && y<=65)"Good"
            else if (y>=66 && y<=69)"Above Average"
            else if (y>=70 && y<=73)"Average"
            else if (y>=74 && y<=81)"Below Average"
            else if (y>=82)"Poor"})}
      else if(current.age>=26 && current.age<=35)
        {
        data$HR<-sapply(data$HeartRate, function (y)
          {
            if (y>=49 && y<=55)"Athlete" 
            else if (y>=56 && y<=61)"Excellent"
            else if (y>=62 && y<=65)"Good"
            else if (y>=66 && y<=69)"Above Average"
            else if (y>=70 && y<=73)"Average"
            else if (y>=74 && y<=81)"Below Average"
            else if (y>=82)"Poor"})}
      else if (current.age>=36 && current.age<=45)
        {
        data$HR<-sapply(data$HeartRate, function (y)
          {
           if (y>=50 && y<=56)"Athlete" 
          else if (y>=57 && y<=62)"Excellent"
          else if (y>=63 && y<=66)"Good"
          else if (y>=67 && y<=70)"Above Average"
          else if (y>=71 && y<=75)"Average"
          else if (y>=76 && y<=82)"Below Average"
          else if (y>=83)"Poor"})}
      else if (current.age>=46 && current.age<=55)
        {
        data$HR<-sapply(data$HeartRate, function (y)
          {
            if (y>=50 && y<=57)"Athlete" 
            else if (y>=58 && y<=63)"Excellent"
            else if (y>=64 && y<=67)"Good"
            else if (y>=68 && y<=71)"Above Average"
            else if (y>=72 && y<=76)"Average"
            else if (y>=77 && y<=83)"Below Average"
            else if (y>=84)"Poor"})}
      else if (current.age>=56 && current.age<=65)
        {
        data$HR<-sapply(data$HeartRate, function (y)
          {
            if (y>=51 && y<=56)"Athlete" 
            else if (y>=57 && y<=61)"Excellent"
            else if (y>=62 && y<=67)"Good"
            else if (y>=68 && y<=71)"Above Average"
            else if (y>=72 && y<=75)"Average"
            else if (y>=76 && y<=81)"Below Average"
            else if (y>=82)"Poor"})}
      else if(current.age>=66)
        {
        data$HR<-sapply(data$HeartRate, function (y)
          {
            if (y>=50 && y<=55)"Athlete" 
            else if (y>=56 && y<=61)"Excellent"
            else if (y>=62 && y<=65)"Good"
            else if (y>=66 && y<=69)"Above Average"
            else if (y>=70 && y<=73)"Average"
            else if (y>=74 && y<=79)"Below Average"
            else if (y>=80)"Poor"})}
}
unlist(data$HR)

plot(table(unlist(data$HR)))
output3<-data[,c("HeartRate","HR")]
#error msg, while running the write.csv
write.csv(output3,"pluseRate.csv",sep=",", row.names=FALSE, quote = FALSE)

#Event Driven Queries 

is_diabetic<-data$Diabetes=='Yes'
is_diabetic
dist<-table(is_diabetic)
barplot(is_diabetic)
dist <- table(data$Diabetes)
barplot(table(data$Diabetes))

hist(data$Age)
is_fourty<-(data$Age==40)

hist(is_fourty)

#mtcars[mtcars$gear == 4,]
diab<-(data$Age>=40 && data$Diabetes=='yes')
diab
dist<-table(diab)
barplot(diab)
