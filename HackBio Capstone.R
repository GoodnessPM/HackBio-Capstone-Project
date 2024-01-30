     #Getting and Importing Data
url <- "https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/nhanes.csv" #url link
#reading  the data from url into a variable
Data <- read.table(url,header = TRUE,sep = ",")
head(Data) #sample of data
#getting the summary statistics of the data
summary(Data)




#Data Cleaning
#Dealing with the null values
library(tidyverse)#import library
#checked out the columns with null set

Data_tb = tibble(Data)

#Diabetes and DiabetesAge Column
fd_tb <- Data_tb[
  complete.cases(Data_tb$Diabetes), #delete the nulls from diabetes column
] 
fd_tb %>% count(
  Diabetes, DiabetesAge) #check if the nullset in DiabetesAge aligns with No of Diabetes column

fd_tb$DiabetesAge[(fd_tb$Diabetes == "No")] <- 0 #equating those with no record of diabetes with zero diabetes years
fd_tb <- fd_tb[complete.cases(fd_tb$DiabetesAge),] #delete the remaining nulls from DiabetesAge column

cat("Null value in Diabetes Column:", sum(is.na(fd_tb$Diabetes)), "\n")
cat ("null value in DiabetesAge Column:", sum(is.na(fd_tb$DiabetesAge)), "\n")

#npregnanies Column
fd_tb$nPregnancies[is.na(fd_tb$nPregnancies)] <- 0 #equating null to having zero pregnancies.

#BMI, PULSE, BPSys, BPDia, Testerone
col_to_del = c("BMI", "Pulse", "BPSys", 'BPDia', "Testosterone", 'HDLChol','TotChol') #delete null col from BMI, Pulse, BPSy s, BPDia, Testosterone, HDLchol and TotChol
filteredata_tb <- subset(fd_tb, complete.cases(fd_tb[, col_to_del]))

#nBabies          
nBabies_NA <- subset(filteredata_tb, is.na(nBabies))
filteredata_tb %>% count(nPregnancies, nBabies) #getting the summary data of nPrgnancies and nBabies.

#filling null values in nBabies where pregnancy is 0 with 0.
filteredata_tb <- filteredata_tb %>% mutate( nBabies_filled = ifelse(nPregnancies ==0 & is.na(nBabies),0, nBabies)) 
filt_tb <- filteredata_tb %>% group_by( nPregnancies) %>% mutate(
  nBabies_filled = ifelse(is.na(nBabies_filled), round(mean(nBabies_filled, na.rm = TRUE), digits = 0), nBabies_filled)) #filling with the mean nBabies for each group of Pregnancies
filteredata_tb <- filteredata_tb %>% mutate(
  nBabies_filled = filt_tb$nBabies_filled) 


#Alcoholday, AlcoholYear, Income, HomeRooms, SmokingStatus
filteredata_tb$AlcoholYear[is.na(filteredata_tb$AlcoholYear)] <- 0
filteredata_tb$AlcoholDay[is.na(filteredata_tb$AlcoholDay)] <- 0
filteredata_tb$Income[is.na(filteredata_tb$Income)] <- 0
filteredata_tb$HomeRooms[is.na(filteredata_tb$HomeRooms)] <-0
filteredata_tb$SmokingStatus[is.na(filteredata_tb$SmokingStatus)] <- "Never"



#Data Analysis
dataplot1 <- select(filteredata_tb,Weight, BMI, Age)
#computing weight in pounds
dataplot1 <- dataplot1 %>% mutate(Weightsp = Weight * 2.2)

#plotting the histograms
plots <- list()

for (col_name in names(dataplot1)) {
  # Use .. notation to dynamically refer to the column in aes()
  plot <- ggplot(dataplot1, aes_string(x = col_name)) +
    geom_histogram(fill = "lightblue", col = 'black') +
    labs(title = paste("Histogram of", col_name))
  
  plots[[col_name]] <- plot
}

# Arrange and print the plots
gridExtra::grid.arrange(grobs = plots, ncol = 2) 


round(mean(filteredata_tb$Pulse)) #mean pulse of the population

paste("The range of diastolic blood pressure of all participant is", 
      min(filteredata_tb$BPDia), "-", max(filteredata_tb$BPDia))

paste("The standard deviation and variance of the partcipants income  are", 
      round(sd(filteredata_tb$Income)), "and", round(var(filteredata_tb$Income)),
      "respectively")

ggplot(filteredata_tb, aes(x = Height,y = Weight,
                           color = Gender))+ geom_point() + labs(
                             title = "Scatter Plot of Height and Weight",
                             x = "Height (cm)", y = "Weight (kg)")

#colour by diabetes
ggplot(filteredata_tb, aes(x = Height, y = Weight, 
                           color = Diabetes)) + geom_point() + labs(
                             title = "Scatter Plot of Height and Weight",
                             x = "Height (cm)", y = "Weight (kg)")

#Colour by Smoking Status
ggplot(filteredata_tb, aes(x = Height, y = Weight, 
                           color = SmokingStatus)) + geom_point() + labs(
                             title = "Scatter Plot of Height and Weight",                   
                             x = "Height (cm)", y = "Weight (kg)")

#statistical testing
print(t.test(Age ~ Gender, data= filteredata_tb))
print(t.test(BMI ~ Diabetes, data= filteredata_tb))
print(t.test(AlcoholYear~RelationshipStatus, data= filteredata_tb))

