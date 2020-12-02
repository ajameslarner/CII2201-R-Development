library(dplyr)
library(ggplot2)

#import data sets

AMZN_PP <- read.csv(file = 'AMZN 2020-2015/AMZN-PP.csv', header = TRUE)
AAL_PP <- read.csv(file = 'AAL 2020-2015/AAL-PP.csv', header = TRUE)
MAR_PP <- read.csv(file = 'MAR 2020-2015/MAR-PP.csv', header = TRUE)
NFLX_PP <- read.csv(file = 'NFLX 2020-2015/NFLX-PP.csv', header = TRUE)
TSLA_PP <- read.csv(file = 'TSLA 2020-2015/TSLA-PP.csv', header = TRUE)

#Calculate Avg of two inputs (custom function)
xy_avg <- function(x, y){
  add <- x+y
  avrg <- add/2
  return(avrg)
}

#Calculate daily average of each day between high and low stock prices
AMZN_avg <- xy_avg(AMZN_PP$High, AMZN_PP$Low)
AAL_avg <- xy_avg(AAL_PP$High, AAL_PP$Low)
MAR_avg <- xy_avg(MAR_PP$High, MAR_PP$Low)
NFLX_avg <- xy_avg(NFLX_PP$High, NFLX_PP$Low)
TSLA_avg <- xy_avg(TSLA_PP$High, TSLA_PP$Low)

#Data sets contain the same date scope so date is only needed from one
date <- AMZN_PP$Date

#Create new date frame for analysis + add formatting
ALL_STOCKS <- data.frame(matrix(ncol = 3))
colnames(ALL_STOCKS) <- c("Date", "AvgValue", "Company")
ALL_STOCKS$Date <- as.POSIXct(ALL_STOCKS$Date)
ALL_STOCKS$AvgValue <- as.numeric(ALL_STOCKS$AvgValue)
ALL_STOCKS$Company <- as.character(ALL_STOCKS$Company)
ALL_STOCKS <- ALL_STOCKS[-c(1),]
str(ALL_STOCKS)


#Create & format data frames from each set of date prior to merging
AMZN_stocks <- data.frame(date, AMZN_avg)
AAL_stocks <- data.frame(date, AAL_avg)
MAR_stocks <- data.frame(date, MAR_avg)
NFLX_stocks <- data.frame(date, NFLX_avg)
TSLA_stocks <- data.frame(date, TSLA_avg)
#Date format needs changing to a relevant date format 
#I'm choosing POSIXlt - a common date/time format
#Using the strptime function, I can appoint which characters within the string
#represent the relative month, day or year - function is case sensitive

?strptime

date_convert <- function(column, date){
  x <- strptime(column, format = date)
  return(x)
}

AMZN_stocks$date <- strptime(AMZN_stocks$date, format = "%m/%d/%Y")
AAL_stocks$date <- strptime(AAL_stocks$date, format = "%m/%d/%Y")
MAR_stocks$date <- strptime(MAR_stocks$date, format = "%m/%d/%Y")
NFLX_stocks$date <- strptime(NFLX_stocks$date, format = "%m/%d/%Y")
TSLA_stocks$date <- strptime(TSLA_stocks$date, format = "%m/%d/%Y")

#Custom function to filter data by the desired date inputted
date_filter <- function(data, column, date){
  x <- data[which(column > date),]
  return(x)
}

AMZN_YTD <- date_filter(AMZN_stocks, AMZN_stocks$date, "2019-01-01")
AAL_YTD <- date_filter(AAL_stocks, AMZN_stocks$date, "2019-01-01")
MAR_YTD <- date_filter(MAR_stocks, AMZN_stocks$date, "2019-01-01")
NFLX_YTD <- date_filter(NFLX_stocks, AMZN_stocks$date, "2019-01-01")
TSLA_YTD <- date_filter(TSLA_stocks, AMZN_stocks$date, "2019-01-01")

#Add new column with company code data
AMZN_YTD$Company <- "AMZN"
AAL_YTD$Company <- "AAL"
MAR_YTD$Company <- "MAR"
NFLX_YTD$Company <- "NFLX"
TSLA_YTD$Company <- "TSLA"

#Rename columns
colnames(AMZN_YTD) <- c("Date", "AvgValue", "Company")
colnames(AAL_YTD) <- c("Date", "AvgValue", "Company")
colnames(MAR_YTD) <- c("Date", "AvgValue", "Company")
colnames(NFLX_YTD) <- c("Date", "AvgValue", "Company")
colnames(TSLA_YTD) <- c("Date", "AvgValue", "Company")

#Merge data
ALL_STOCKS <- rbind(ALL_STOCKS, AMZN_YTD)
ALL_STOCKS <- rbind(ALL_STOCKS, AAL_YTD)
ALL_STOCKS <- rbind(ALL_STOCKS, MAR_YTD)
ALL_STOCKS <- rbind(ALL_STOCKS, NFLX_YTD)
ALL_STOCKS <- rbind(ALL_STOCKS, TSLA_YTD)

#Verify date is still in POSIXct format.
ALL_STOCKS$Date <- as.POSIXct(ALL_STOCKS$Date)

#One dimensional plot showing average value of stock prices from Jan 2019 to Nov 2020
a <- ggplot(ALL_STOCKS, aes(x=Date, y=AvgValue, color=Company)) + geom_line() 
#Y axis label rename
a <- a + ylab("Daily Avg Value") + xlab("")
#Modifying the text displayed on the right of each individual graph
a <- a + theme(strip.text.y=element_text(size=18, face='italic'))
#Adding the title to the graph
a <- a + ggtitle("The COVID-19 'Affect' On Company Stock Prices")
#To center the title
a <- a + theme(plot.title=element_text(size=18, hjust=0.5))
a

#Import 2-dimensional data
OWID_covid <- read.csv(file = 'owid-covid-data.csv', header = TRUE)
#Extract data required
OWID_SIMPLE <- data.frame(OWID_covid$date, OWID_covid$total_cases, OWID_covid$location)

#Data frame needs formatting before I can merge.
str(OWID_SIMPLE)

#Rename column names for easier merging
colnames(OWID_SIMPLE) <- c("Date","Covid-19 Cases","Location")
#Convert the date column to the correct format
OWID_SIMPLE$Date <- as.POSIXct(OWID_SIMPLE$Date)

#Filter by Location to extract "World" data
OWID_WORLD <- OWID_SIMPLE[which(OWID_SIMPLE$Location == "World"),]

#Now I can merge the data with no problems
STOCKS_VS_OWID <- merge(x=ALL_STOCKS,y=OWID_WORLD, by="Date")

#Two dimensional plot showing average value of stock prices from Jan 2019 to Nov 2020, while showing the growth of Covid Cases
b <- ggplot(STOCKS_VS_OWID, aes(x=Date, y=AvgValue, color=Company, size=`Covid-19 Cases`)) + geom_line() 
#Y axis label rename
b <- b + ylab("Daily Avg Value")
#split graph by company into different grids, y-axis relative to AvgValue data
b <- b + facet_grid(Company~., scales="free_y")
#Modifying the text displayed on the right of each individual graph
b <- b + theme(strip.text.y=element_text(size=12, face='italic'))
#Adding the title to the graph
b <- b + ggtitle("The COVID-19 'effect' On Company Stock Prices")
#To center the title
b <- b + theme(plot.title=element_text(size=18, hjust=0.5))
#Change the size legend text to output "M" as the million units
b <- b + scale_size_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6,))
b

#Extract data for Year on Year histogram plot
AMZN_YOY <- AMZN_stocks[which(AMZN_stocks$date == "2019-05-01" | AMZN_stocks$date == "2020-05-01"),]
AAL_YOY <- AAL_stocks[which(AAL_stocks$date == "2019-05-01" | AAL_stocks$date == "2020-05-01"),]
MAR_YOY <- MAR_stocks[which(MAR_stocks$date == "2019-05-01" | MAR_stocks$date == "2020-05-01"),]
NFLX_YOY <- NFLX_stocks[which(NFLX_stocks$date == "2019-05-01" | NFLX_stocks$date == "2020-05-01"),]
TSLA_YOY <- TSLA_stocks[which(TSLA_stocks$date == "2019-05-01" | TSLA_stocks$date == "2020-05-01"),]

#Add company column
AMZN_YOY$Company <- "AMZN"
AAL_YOY$Company <- "AAL"
MAR_YOY$Company <- "MAR"
NFLX_YOY$Company <- "NFLX"
TSLA_YOY$Company <- "TSLA"

#Rename columns
colnames(AMZN_YOY) <- c("Date", "AvgValue", "Company")
colnames(AAL_YOY) <- c("Date", "AvgValue", "Company")
colnames(MAR_YOY) <- c("Date", "AvgValue", "Company")
colnames(NFLX_YOY) <- c("Date", "AvgValue", "Company")
colnames(TSLA_YOY) <- c("Date", "AvgValue", "Company")

#Create year on year df
YOY_STOCKS <- data.frame(matrix(ncol = 3))
colnames(YOY_STOCKS) <- c("Date", "AvgValue", "Company")
YOY_STOCKS$Date <- as.POSIXct(YOY_STOCKS$Date)
YOY_STOCKS$AvgValue <- as.numeric(YOY_STOCKS$AvgValue)
YOY_STOCKS$Company <- as.character(YOY_STOCKS$Company)
YOY_STOCKS <- YOY_STOCKS[-c(1),]
str(YOY_STOCKS)

#Bind rows to the new df
YOY_STOCKS <- rbind(YOY_STOCKS, AMZN_YOY)
YOY_STOCKS <- rbind(YOY_STOCKS, AAL_YOY)
YOY_STOCKS <- rbind(YOY_STOCKS, MAR_YOY)
YOY_STOCKS <- rbind(YOY_STOCKS, NFLX_YOY)
YOY_STOCKS <- rbind(YOY_STOCKS, TSLA_YOY)

#Confirm date is in correct format
YOY_STOCKS$Date <- as.character(YOY_STOCKS$Date)
str(YOY_STOCKS)

#Round the avg value to 2 decimal points
YOY_STOCKS$AvgValue <- round(YOY_STOCKS$AvgValue,2)

#1-Dimensional plot of year on year
c <- ggplot(YOY_STOCKS, aes(x=Company, y=AvgValue, label=AvgValue)) + geom_text(size=5, vjust=-1)
c <- c + geom_bar(stat = "identity", aes(fill=Company))
c <- c + facet_wrap(Date~., scales="free_x")
c <- c + ylim(0, 3500)
c <- c + theme(strip.text.y=element_text(size=12, face='italic'))
c <- c + ggtitle("The COVID-19 'effect' On Company Stock Prices (Year on Year)")
c <- c + theme(plot.title=element_text(size=18, hjust=0.5))
c <- c + ylab("Daily Avg Value") + xlab("")
c

