#setwd("D:/MS Thesis/ThesisData/Collected Data Set")

data <- read.csv("MasterData1.csv", stringsAsFactors = FALSE)
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
head(ecgdata)

data$BMI <- data$Weight / (data$Height / 100)
data$BMI
hist(data$BMI)

lookup <- function(value) {
    category <- "OBESE"
    if (value < 15)
        category <- "CRITICAL_UW"
    else if (value < 16.5)
        category <- "SEVERE_UW"
    else if (value < 18.5)
        category <- "UW"
    else if (value < 15)
        category <- "NORMAL"
    else if (value < 30)
        category <- "OW"
    else if (value < 35)
        category <- "MODERATE_OW"
    category
}

data$WeightCategory <- sapply(data$BMI, lookup)
plot(table(data$WeightCategory))


output1 <- data[,c("BMI","obese")]
write.csv(output1, "obese.csv", sep=",", row.names=FALSE, quote = FALSE)


# Calculation for Blood pressure
bp_lookup <- function(x, y) {
    category <- "INVALID"
    if (x < 120 & y < 80)
        category <- "NORMAL"
    else if ((x >= 120 & x < 140) & (y >= 80 & y < 90))
        category <- "PREHYP"
    else if ((x >= 140 & x < 160) & (y >= 90 & y < 100))
        category <- "HYP1"
    else if ((x >= 160) & (y >= 100))
        category <- "HYP2"
    category
}

# Multi-variable apply
data$BPCategory <- mapply(bp_lookup, data$SystolicBP, data$DiastolicBP)
plot(table(data$BPCategory))


# For Health Rank
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
