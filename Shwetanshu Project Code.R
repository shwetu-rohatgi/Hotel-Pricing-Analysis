# Hotel Room Pricing In Indian Market
# NAME: Shwetanshu Rohatgi
# EMAIL: shwetanshu.rohatgi@gmail.com
# COLLEGE : Maharaja Surajmal Institute of Technology.

##setting the directory and assigning a variabel to the data frame
setwd("D:/IIML DSA/Final Project")

#Reading the dataset and creating a data frame
hotel.df<-read.csv(paste("Cities42.csv",sep = ""))

#Viewing the data
View(hotel.df)

#Removing the repeated date by gsub command

hotel.df$Date<-gsub("18-Dec-16", "Dec 18 2016", hotel.df$Date)
hotel.df$Date<-gsub("21-Dec-16", "Dec 21 2016", hotel.df$Date)
hotel.df$Date<-gsub("24-Dec-16", "Dec 24 2016", hotel.df$Date)
hotel.df$Date<-gsub("25-Dec-16", "Dec 25 2016", hotel.df$Date)
hotel.df$Date<-gsub("28-Dec-16", "Dec 28 2016", hotel.df$Date)
hotel.df$Date<-gsub("31-Dec-16", "Dec 31 2016", hotel.df$Date)
hotel.df$Date<-gsub("4-Jan-17", "Jan 04 2017", hotel.df$Date)
hotel.df$Date<-gsub("4-Jan-16", "Jan 04 2017", hotel.df$Date)
hotel.df$Date<-gsub("8-Jan-16", "Jan 08 2017", hotel.df$Date)
hotel.df$Date<-gsub("8-Jan-17", "Jan 08 2017", hotel.df$Date)
hotel.df$Date<-gsub("Jan 4 2017", "Jan 04 2017", hotel.df$Date)
hotel.df$Date<-gsub("Jan 8 2017", "Jan 08 2017", hotel.df$Date)

#Checking the dates

table(hotel.df$Date)

#Changing dates to factors for labelling 

hotel.df$Date<-factor(hotel.df$Date)
is.factor(hotel.df$Date)

#Checking the labelling
levels(hotel.df$Date)

#Analyzing the summary of the data and describing the variables

library(psych)
describe(hotel.df)

summary(hotel.df)

#Taking Y = RoomRent, identifying the most relevent predictor variables by boruta and correlation

#Using Boruta
library(Boruta)
hotel1.df<-na.omit(hotel.df)
xvar<-Boruta(RoomRent~CityName + Population + IsMetroCity + IsTouristDestination + IsWeekend +
               IsNewYearEve + Date + HotelName + StarRating + Airport + HotelAddress + HotelPincode
             + HotelDescription + FreeWifi + FreeBreakfast + HasSwimmingPool, data= hotel1.df)
print(xvar)

xvar<-Boruta(RoomRent~.-X - CityName - HotelAddress - HotelDescription, data= hotel1.df, doTrace = 2)
print(xvar)
##From Boruta we got the idea to atleast reject three attributes i.e Date, IsNewYearEve, IsWeekend

#Corrgram

library(corrgram)

corrgram(hotel.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of Hotel  data")
  ##through corrgram HasSwimming, StarRating, HotelCapital are very well correlated to RoomRent
  ##so we can take them as predictors

##Visualizing data for Y as Room rent and X1,X2,X3 as HasSwimmingPool, StarRating and HotelCapacity respectively

  #Table for HasSwimmingPool
    table(hotel.df$HasSwimmingPool)
    Swim<-table(hotel.df$HasSwimmingPool)
    barplot(Swim,main="Barrplot of Hotel Swimming Pool")
    
  #Table for StarRating
    table(hotel.df$StarRating)
    starRating<-table(hotel.df$StarRating)
    barplot(starRating,main = "Barrplot for Star Rating")
  
  #BoxPlot for HotelCapacity
    boxplot(hotel.df$HotelCapacity, main="Boxplot for Hotel Capacity",horizontal = TRUE)

#Scatterplot pair wise for predictor variable
    
    library(car)
    #StarRating Vs RoomRent
    
    scatterplot(hotel.df$StarRating,hotel.df$RoomRent,main="RoomRent of Hotels  with StarRating",ylab = "RoomRent in INR", xlab="Star rating out of 5",cex=1.1)
    
    #RoomRent Vs HotelCapacity
    
    scatterplot(hotel.df$RoomRent,hotel.df$HotelCapacity,main="RoomRent of Hotels  with Hotel capacity",ylab = "Hotel Capacity in rooms", xlab="RoomRent in INR",cex=1.1)
    
    #RoomRent Vs HasSwimmingPool
    
    plot(jitter(hotel.df$RoomRent),jitter(hotel.df$HasSwimmingPool),main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent",cex=1.1)
    library(lattice)
    bwplot(HasSwimmingPool~RoomRent, data = hotel.df,main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent" )
    
    #Scatterplot matrix
    
    scatterplotMatrix(
      hotel.df[
        ,c("RoomRent","HasSwimmingPool","StarRating", "HotelCapacity")], 
      spread=FALSE, smoother.args=list(lty=2),
      main="Scatter Plot Matrix", diagonal = "histogram")
        
    
    #Corrgram of Y, x1, x2, x3
    
    library(corrgram)
    
    xyz<-data.frame(hotel.df$RoomRent, hotel.df$HasSwimmingPool, hotel.df$HotelCapacity, hotel.df$StarRating)
    corrgram(xyz, order=TRUE, lower.panel=panel.shade,
             upper.panel=panel.pie, text.panel=panel.txt,
             main="Corrgram of Hotel Prices In India")
    
    #Variance-Covariance Matrix for Y, x1, x2, x3

    x<-hotel.df[,c("HasSwimmingPool","StarRating", "HotelCapacity")]
    y<-hotel.df[,c("RoomRent")]
    cor(x,y)
    cov(x,y)
    var(x,y)