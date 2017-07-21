# Hotel Room Pricing In The Indian Market
# NAME: Shwetanshu Rohatgi
# EMAIL: shwetanshu.rohatgi@gmail.com
# COLLEGE : Maharaja Surajmal Institute of technology, New Delhi

##setting the directory and assigning a variabel to the data frame
setwd("F:/My Projects/IIM L project/Capstone project")

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

#Taking Y = RoomRent, identifying the most relevent predictor variables by  correlation corrgram


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
    #Forming a variable which is having RoomRent less than 1 lakh because the outliers effect the average
   RoomRent1.df <-hotel.df[which(hotel.df$RoomRent<100000),]

   #Comparing other factors and their pattern using other trends with roomrent
   
   #Analyzing IsWeekeng effect on RoomRent
   table(hotel.df$IsWeekend)
   
   table1<-table(hotel.df$IsWeekend)
   barplot(table1, main="Distribution of Weekend", xlab="Not weekend(0)         Weekend(1)", col="orange")
   
   #Effect of Isweekend on RoomRent
   iw= aggregate(RoomRent ~ IsWeekend, data=hotel.df,mean)
   iw
   
   boxplot(RoomRent~IsWeekend,data=hotel.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)
   #Without extreme outliers 
   boxplot(RoomRent~IsWeekend,data=RoomRent1.df, main="Room rent vs. IsWeekend", ylab="Not weekend(0)  weekend(1)", xlab="Room Rent in rupees ", col=c("red","blue"),horizontal=TRUE)
   
   #Comapring RoomRent on different dates
   table(hotel.df$Date)
   
   library(lattice)
   histogram(~Date, data = hotel.df, main="Distribution of Dates", xlab = "Differnt of Dates", col="Blue")
   
   
   #Effect of different dates on RoomRent
   
   d = aggregate(RoomRent ~ Date, data = hotel.df,mean)
   d
   
   scatterplot(d$Date,d$RoomRent, main="Scatterplot between Date and RoomRent", xlab="Date", ylab = "Room Rent in Rupees")
   
   
   boxplot(RoomRent~Date,data=hotel.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))
   ##Without extreme outliers
   boxplot(RoomRent~Date,data=RoomRent1.df, main="Room rent vs. Date", xlab="Different Dates", ylab="Room Rent in rupees ", col=c("red","blue","green","yellow"))
   
   #Analyzing IsMetroCity effect on RoomRent
   table(hotel.df$IsMetroCity)
   
   table1<-table(hotel.df$IsMetroCity)
   barplot(table1, main="Distribution of IsMetroCity", xlab="Not a Metro city(0)         Is a Metro City(1)", col="blue")
   
   #Effect of IsMetroCity on RoomRent
   imc = aggregate(RoomRent ~ IsMetroCity, data = hotel.df, mean)
   imc
   
   boxplot(RoomRent~IsMetroCity,data=hotel.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   ##Without extreme outliers
   boxplot(RoomRent~IsMetroCity,data=RoomRent1.df, main="Room rent vs. IsMetroCity", ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   
   
   #Analyzing IsTouristDestination effect on RoomRent
   table(hotel.df$IsTouristDestination)
   
   table1<-table(hotel.df$IsTouristDestination)
   barplot(table1, main="Distribution of IsToursitDestination", xlab="Not a Tourist Destination(0)         Is a Tourist Destination(1)", col="yellow")
   
   #Effect of IsTouristDestination on RoomRent
   itd = aggregate(RoomRent ~ IsTouristDestination, data = hotel.df, mean)
   itd
   
   boxplot(RoomRent~IsTouristDestination,data=hotel.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   ##Without extreme outliers
   boxplot(RoomRent~IsTouristDestination,data=RoomRent1.df, main="Room rent vs. IsTouristDestination", ylab="IsTouristDestination(1) or not(0)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   
   #Analyzing FreeWifi Vs RoomRent
   table(hotel.df$FreeWifi)
   fw<-table(hotel.df$FreeWifi)
   barplot(fw, main="Borplot of FreeWifi",xlab= "FreeWifi" ,col="red")
   
   #Effect of FreeWifi on RoomRent
   fw = aggregate(RoomRent ~ FreeWifi, data = hotel.df, mean)
   fw
   
   ##With extreme outliers of roomrent
   boxplot(RoomRent~FreeWifi,data=hotel.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   ##Without extreme outliers of roomrent
   boxplot(RoomRent~FreeWifi,data=RoomRent1.df, main="Room rent vs. FreeWifi", ylab="Free Wifi available(1)", xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)
   
   #Analyzing FreeBreakfast Vs RoomRent
   table(hotel.df$FreeWifi)
   fw<-table(hotel.df$FreeBreakfast)
   barplot(fw, main="Borplot of FreeBreakfast",xlab= "FreeWifi" ,col="red")
   
   
   #Effect of FreeBreakfast on RoomRent
   fb = aggregate(RoomRent ~ FreeBreakfast, data =hotel.df, mean)
   fb1  = aggregate(RoomRent ~ FreeBreakfast, data =RoomRent1.df, mean)
   ##Aggregate are affected by outliers a lot in the case of FreeBreakfast on RoomRent
   fb
   fb1
   
   ##With extreme outliers of roomrent
   boxplot(RoomRent~FreeBreakfast,data=hotel.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)
   ##Without extreme outliers of roomrent
   boxplot(RoomRent~FreeBreakfast,data=RoomRent1.df, main="Room rent vs. FreeBreakfast", ylab="Free Breakfast available(1)", xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)
   
   
   #Analyzing Airport distance from hotel effects in what way on RoomRent
   summary(hotel.df$Airport)
   
   boxplot(hotel.df$Airport, main="Boxplot of Airport",xlab= "Distance of airport from hotel(Km)" ,col="green",horizontal = TRUE)
   
   #Effect of Airport distance on RoomRent
   
   scatterplot(hotel.df$Airport,hotel.df$RoomRent, main="Room rent vs. Airport distance", xlab="Airport distance(km)", ylab="Room Rent in rupees ",cex=1.1)
   
##Hypothesis
   
   #1.Average RoomRent in hotels having swimming pool is more than that which don't have.
   t.test(RoomRent~HasSwimmingPool,data = hotel.df, alternative="less")
   
   #2.Average RoomRent in hotels with high star rating is high as compared to one which has less star rating.
   t.test(hotel.df$RoomRent,hotel.df$StarRating)
   
   #3.Average RoomRent in hotels providing Free Breakfast is more than that which don't provide.
   t.test(RoomRent~FreeBreakfast, data = hotel.df, alternative="less")
   
   #4.Average RoomRent in metro cities hotels is more than that of non metro cities.
   t.test(RoomRent~IsMetroCity, data = hotel.df, alternative="less")
   
   #5.Average RoomRent in hotels having more hotel capacity is more compared to one with less capacity.
   t.test(hotel.df$RoomRent,hotel.df$HotelCapacity)
   
#Generating a multiple linear regression model for RoomRent
   #1.
   fit1<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity-1, data = hotel.df)
   summary(fit1)
   
   #Coefficents of the model
   fit1$coefficients
   
   #Fitted residuals and values  are checked and the deviation was around 1000 , because of 
   #large data points it's not suitable to show those in the output file.
   
###.  Model1:    salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity
#   b0 = -1(assumption),  b1 =  1396.874562, b2=3719.6943, b3= -7.659814
#  Model:    salary = -1 + 1396.874562*StarRating + 3719.6943*HasSwimmingPool -7.659814*HotelCapacity
   
   
   #2.
   fit2<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+IsWeekend+IsTouristDestination-1, data = hotel.df)
   summary(fit2)
   
   #Coefficents of the model
   fit2$coefficients
  
    #Fitted residuals and values  are checked and the deviation was around 1000 , because of 
   #large data points it's not suitable to show those in the output file.
   
   
   ###.  Model1:    salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity +b4*IsWeekend(0) + b5*IsWeekend(1) + b6*IsTouristDestination
   #   b0 = -1(assumption),  b1 =  3635.819, b2=2285.132, b3= -13.965, b4=-8396.67457, b5=-8325.09152,b6=1878.94395
   #  Model:    salary = -1 + 3635.819*StarRating + 2285.132*HasSwimmingPool -13.965*HotelCapacity
   # -8396.67457*IsWeekend(0) - 8325.09152*IsWeekend(1) + 1878.94395*IsTouristDestination 
   
   
   #3.
   fit3<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport-1, data = hotel.df)
   summary(fit3)
   
   #Coefficents of the model
   fit3$coefficients
   
   #Fitted residuals and values  are checked and the deviation was around 1000 , because of 
   #large data points it's not suitable to show those in the output file.
   
   ###.  Model1:    salary = b0 + b1*StarRating + b2*HasSwimmingPool+ b3*HotelCapacity +b4*Airport + b5*Date
   #   b0 = -1(assumption),  b1 =  1248.426988 , b2=3903.736921, b3= -6.743354, b4= 18.869726
   #  Model:    salary = -1 + 1248.426988*StarRating + 3903.736921*HasSwimmingPool -6.743354*HotelCapacity  + 18.869726*Aiport
   
   