# set working directory------------------------------------------------------------
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
setwd(PROJHOME)
getwd()

# functions------------------------------------------------------------
# function to caculate daily weight per month
date_func <- function(l, date_var) {
  ifelse(((month(date_var)==(l-1) & day(date_var)<16))|((month(date_var)==(l+1) & day(date_var)>15))|month(date_var)>(l+1)|month(date_var)<(l-1),0,
         ifelse(month(date_var)==l, 1-abs(day(date_var)-15)/as.numeric(days_in_month(l)), 
                ifelse(month(date_var)<l, abs(day(date_var)-15)/as.numeric(days_in_month(l-1)),
                       abs(day(date_var)-15)/as.numeric(days_in_month(l)))))
}

# function to identify odd and even numbers
is.odd <- function(x) x %% 2 != 0 


# library
library(lubridate)

# read data------------------------------------------------------------
micro_felix <- read.table("output/micro_felix.csv", sep = ";", header = T)
air_temp <- read.table("output/air_temperature.csv", sep = ";", header = T)
solar <- read.table("output/solar.csv", sep = ";", header = T)
wind <- read.table("output/wind.csv", sep = ";", header = T)
precipitation <- read.table("output/precipitation.csv", sep = ";", header = T)
# read file with coordinates of the sampling sites
coordinates <- read.table (file = "data/coordinates.csv",
                           row.names=1, header=TRUE, sep=";", fill=T)

air_temp <- merge(air_temp, coordinates, by = "ID")
solar <- merge(solar, coordinates, by = "ID")
wind <- merge(wind, coordinates, by = "ID")
precipitation <- merge(precipitation, coordinates, by = "ID")

# extract data for the modelling------------------------------------------------------------
air_temp_new <- data.frame(site = air_temp$site,
                           biotype = air_temp$biotype,
                           date = air_temp$date,
                           temperature_DWD = air_temp$TT_TU,
                           temperature_DWD_before = air_temp$before,
                           humidity_DWD = air_temp$RF_TU)

solar_new <- data.frame(site = solar$site,
                        biotype = solar$biotype,
                        date = solar$date,
                        radiation = as.numeric(solar$FG_LBERG))

wind_new <- data.frame(site = wind$site,
                       biotype = wind$biotype,
                       date = wind$date,
                       wind = wind$F)

precipitation_new <- data.frame(site = precipitation$site,
                                biotype = precipitation$biotype,
                                date = precipitation$date,
                                precipitation = precipitation$R1)

# merge data------------------------------------------------------------
merge_dat1 <- merge(micro_felix, air_temp_new, by = c("date","site", "biotype"))
merge_dat2 <- merge(merge_dat1, wind_new, by = c("date","site", "biotype"))
merge_dat3 <- merge(merge_dat2, solar_new, by = c("date","site", "biotype"))
merge_dat4 <- merge(merge_dat3, precipitation_new, by = c("date","site", "biotype"))

# remove unrealistic data and reference logger------------------------------------------------------------
merge_sub1 <- subset(merge_dat4, temperature_DWD > -200)
merge_sub2 <- subset(merge_sub1, !(biotype %in% "R"))

# built hour variable
merge_sub2$hour <- hour(merge_sub2$date)

# built month variable
merge_sub2$month <- month(merge_sub2$date)

# caculate daily weight per month
# values before and after period with temp values = 1 ????
merge_sub2$month4 <- date_func(4, merge_sub2$date)
merge_sub2$month5 <- date_func(5, merge_sub2$date)
merge_sub2$month6 <- date_func(6, merge_sub2$date)
merge_sub2$month7 <- date_func(7, merge_sub2$date)
merge_sub2$month8 <- date_func(8, merge_sub2$date)
merge_sub2$month9 <- date_func(9, merge_sub2$date)
merge_sub2$month10 <- date_func(10, merge_sub2$date)
merge_sub2$month11 <- date_func(11, merge_sub2$date)

# remove rows with missing values
merge_sub3 <- subset(merge_sub2, !(radiation  == -999))
merge_sub4 <- subset(merge_sub3, !(wind  == -999))
merge_sub5 <- subset(merge_sub4, !(humidity_DWD  == -999))

write.table(merge_sub5, "merge_sub5.csv", sep = ";", row.names = F)
merge_sub5 <- read.table("merge_sub5.csv", sep = ";", header = T)

# model
m1 <- lm(temperature~temperature_DWD+temperature_DWD_before+radiation+
           wind+
           month4+month5+month6+month7+month8+month9+month10+month11+
           as.factor(hour)+
           (radiation:wind)+
           (precipitation:humidity_DWD)+
           (radiation:as.factor(month))+
           (wind:as.factor(height))+
           (radiation:as.factor(height)),
         data = merge_sub5)

# model output
summary(m1)

# R2
summary(m1)[8]

# plot observed vs. predicted values
plot(merge_sub5$temperature, predict(m1))

# correlations coefficient
cor.test(merge_sub5$temperature, predict(m1))

# plot residuals
plot(residuals(m1))


# validation------------------------------------------------------------
# subset odd and even calendar weeks
merge_sub5$week_number <- week(merge_sub5$date)
merge_sub5$odd <- is.odd(merge_sub5$week_number)

# built training and test dataset
merge_sub2_even <- subset(merge_sub5, odd == F)
merge_sub2_odd <- subset(merge_sub5, odd == T)

# training model
m1_even <- lm(temperature~temperature_DWD+temperature_DWD_before+radiation+
                wind+
                month4+month5+month6+month7+month8+month9+month10+month11+
                as.factor(hour)+
                (radiation:wind)+
                (precipitation:humidity_DWD)+
                (radiation:as.factor(month))+
                (wind:as.factor(height))+
                (radiation:as.factor(height)),
              data = merge_sub2_even)

# explained variance, R2
summary(lm(merge_sub2_odd$temperature~predict(m1_even, merge_sub2_odd)))[8]


# cluster------------------------------------------------------------
# built training and test dataset
merge_sub2_cluster2 <- subset(merge_sub5, cluster == "2")

# training model
m1_cluster2 <- lm(temperature~temperature_DWD+temperature_DWD_before+radiation+
                    wind+
                    month4+month5+month6+month7+month8+month9+month10+month11+
                    as.factor(hour)+
                    (radiation:wind)+
                    (precipitation:humidity_DWD)+
                    (radiation:as.factor(month))+
                    (wind:as.factor(height))+
                    (radiation:as.factor(height)),
                  data = merge_sub2_cluster2)

# explained variance, R2
summary(m1_cluster2)[8]
