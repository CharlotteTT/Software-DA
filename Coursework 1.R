library(tidyverse)
library(lubridate)
library(dplyr)
BikeSeoul <- read_csv("BikeSeoul.csv") ## read the data from working directory
names(BikeSeoul)
BikeSeoul
summary(BikeSeoul)
BikeSeoul <- BikeSeoul %>% 
  select( -`Visibility (10m)`,
          -`Dew point temperature(C)`,
          -`Solar Radiation (MJ/m2)`,
          -`Rainfall(mm)`,
          -`Snowfall (cm)`) %>% ## remove the columns from the data sets
  filter(`Rented Bike Count` != 0) %>% ## filter out rows with no bike has rented
  select(- `Functioning Day`) %>% ## remove the functioning day column
  rename(Count = `Rented Bike Count`, Temperature = `Temperature(C)`,
         Humidity = `Humidity(%)`, WindSpeed = `Wind speed (m/s)`, Season = `Seasons`) %>% ## rename
  mutate(Date = as.Date(parse_date_time(Date, "dmy"))) %>% ## Convert date to a date object
  mutate(FullDate = make_datetime(year = year(Date), month = month(Date), day = day(Date), hour = Hour))%>%
  mutate(Holiday = fct_recode(Holiday, "No" = "No Holiday", "Yes" = "Holiday")) %>% ## add a new object
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))) ## sort the season variable by order
BikeSeoul
names(BikeSeoul)
summary(BikeSeoul)

BikeWashingtonDC <- read.csv("BikeWashingtonDC.csv")
names(BikeWashingtonDC)
summary(BikeWashingtonDC)
BikeWashingtonDC <- BikeWashingtonDC %>%
  select(-`instant`,
         -`yr`,
         -`mnth`,
         -`weekday`,
         -`workingday`,
         -`weathersit`,
         -`atemp`,
         -`casual`,
         -`registered`) %>%
  rename(Date = `dteday`, Count = `cnt`, Hour =`hr`,  Temperature = `temp`,
         Humidity = `hum`, WindSpeed = `windspeed`, Season = `season`, Holiday =`holiday`) %>%
  mutate(Humidity = as.numeric(Humidity * 100)) %>% ## convert the humidity in percentage
  mutate(Temperature = Temperature*47 -8) %>%
  mutate(WindSpeed = WindSpeed*335/18) %>%
  mutate(Season = as.character(Season)) %>%
  mutate(Season = fct_recode(Season, "Winter" = "1", "Spring" = "2", "Summer" = "3", "Autumn" = "4")) %>% ##change the factor level
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))) %>%
  mutate(Holiday = as.character(Holiday)) %>%
  mutate(Holiday = fct_recode(Holiday, "Yes" = "1", "No" = "0")) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(FullDate = make_datetime(year = year(Date), month = month(Date), day = day(Date), hour = Hour))
  
summary(BikeWashingtonDC)
names(BikeWashingtonDC)

Scater_Seoul <- BikeSeoul %>% ## create a scatter plot
  ggplot(aes(x = Date, y = Temperature)) +
  geom_point() +
  stat_smooth()+
  xlab("Month of The Year") + ## title the x ais label
  ylab("Temperature (in Celsius)") +
  ggtitle("Temperature Change in Seoul (12/2017-11/2018)") ## title the plot
Scater_Seoul

Scater_Washington <- BikeWashingtonDC %>%
  ggplot(aes(x = Date, y = Temperature)) +
  geom_point() +
  stat_smooth()+ ## fit a smooth line
  xlab("Month of 2011 - 2012") + 
  ylab("Temperature (in Celsius)") +
  ggtitle("Temperature Change in Washington (01/2011-12/2012)")
Scater_Washington

Seoul_Tempreture <- BikeSeoul %>%
  select(`Date`, `Temperature`) %>%
  ggplot(mapping = aes(x = cut(Date, breaks="month"), y = Temperature))+ ## cut the x axis by month
  geom_boxplot() +
  xlab("Month Of The Year") + 
  ylab("Temperature (in Celsius)") +
  ggtitle("Temperature Change in Seoul (12/2017-11/2018) ")
Seoul_Tempreture 

Washington_Tempreture <- BikeWashingtonDC %>%
  select(`Date`, `Temperature`) %>%
  ggplot(mapping = aes(x = cut(Date, breaks="month"), y = Temperature))+
  geom_boxplot() +
  xlab("Month Of The Year") + 
  ylab("Temperature (in Celsius)") +
  ggtitle("Temperature Change in Washington (01/2011-12/20122) ")
Washington_Tempreture

Average_Rent_Seoul <- BikeSeoul %>%
  ggplot(aes(x = Season, y = Count, color = Season))+
  geom_boxplot() +
  stat_summary(fun.y = "mean") +
  xlab("Seasons of The Year") + 
  ylab("Number of The Total Rented Bike (per hour)") +
  ggtitle("Seasons vs Quantity of The Bike Rented in Seoul(12/2017-11/2018) ")
Average_Rent_Seoul 

Average_Rent_Washington <- BikeWashingtonDC %>%
  ggplot(aes(x = Season, y = Count, color = Season))+
  geom_boxplot() +
  stat_summary(fun.y = "mean")+
  xlab("Seasons of Year 2011 to 2012 ") + 
  ylab("Number of The Total Rented Bike (per hour)") +
  ggtitle("Seasons vs Quantity of The Bike Rented in Washington(01/2011-12/2012)")
Average_Rent_Washington

Holiday_Rent_Seoul <- BikeSeoul %>%
  ggplot(aes(x = Holiday, y = Count, color = Holiday)) +
  geom_boxplot() +
  coord_flip() + ## flip the x coordinate and y coordinate
  stat_summary(fun.y = "mean")+
  xlab("Holiday") + 
  ylab("The Total Number of Rented Bike (per dour)") +
  ggtitle(" Holiday vs Quantity of The Bike Rented in Seoul (12/2017-11/2018)")
Holiday_Rent_Seoul

Holiday_Rent_Washington <- BikeWashingtonDC %>%
  ggplot(aes(x = Holiday, y = Count, color = Holiday)) +
  geom_boxplot() +
  coord_flip() +
  stat_summary(fun.y = "mean")+
  xlab("Holiday") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Holiday vs Quantity of The Bike Rented in Wahsington (01/2011-12/2012)")
Holiday_Rent_Washington

Time_Seoul <- BikeSeoul %>%
  group_by(Hour) %>%
  ggplot(aes(x = Hour, y = Count, color = Season))+
  geom_point() +
  xlab("Time (in hour)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Time vs Quantity of The Bike Rented in Seoul (12/2017-11/2018)")
Time_Seoul

Time_Washington <- BikeWashingtonDC %>%
  group_by(Hour) %>%
  ggplot(aes(x = Hour, y = Count, color = Season))+
  geom_point() +
  xlab("Time (in hour)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Time vs Quantity of The Bike Rented in Wahsington (01/2011-12/2012)")
Time_Washington

TempA_Souel <- BikeSeoul %>%
  group_by(Temperature) %>%
  mutate(Tmean_Rent = mean(Count)) %>% ## calculate the mean of total rented bike under the same temperature
  ggplot(aes(x = Temperature, y = Tmean_Rent, color = Temperature)) +
  geom_point() +
  stat_smooth()+
  xlab("Temperature (in Celsius)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Temperature vs Quantity of The Bike Rented in Souel")
TempA_Souel

TempA_Wash <- BikeWashingtonDC %>%
  group_by(Temperature) %>%
  mutate(Tmean_Rent = mean(Count)) %>%
  ggplot(aes(x = Temperature, y = Tmean_Rent, color = Temperature)) +
  geom_point() +
  stat_smooth()+
  xlab("Temperature (in Celsius)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Temperature vs Quantity of The Bike Rented in Wahsington ")
TempA_Wash

WindA_Souel <- BikeSeoul %>%
  group_by(WindSpeed) %>%
  mutate(Wmean_Rent = mean(Count)) %>%
  ggplot(aes(x = WindSpeed , y = Wmean_Rent, color = WindSpeed)) +
  geom_point() +
  stat_smooth()+
  xlab("Wind Speed (m/s)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Wind Speed vs Quantity of The Bike Rented in Souel")
WindA_Souel

WindA_Wash <- BikeWashingtonDC %>%
  group_by(WindSpeed) %>%
  mutate(Wmean_Rent = mean(Count)) %>%
  ggplot(aes(x = WindSpeed , y = Wmean_Rent, color = WindSpeed)) +
  geom_point() +
  stat_smooth()+
  xlab("Wind Speed (m/s)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Wind Speed vs Quantity of The Bike Rented in Wahsington ")
WindA_Wash

HumiA_Souel <- BikeSeoul %>%
  group_by(Humidity) %>%
  mutate(Hmean_Rent = mean(Count)) %>%
  ggplot(aes(x = Humidity, y = Hmean_Rent, color = Humidity)) +
  geom_point() +
  stat_smooth()+
  xlab("Humidity (%)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Humidity vs Quantity of The Bike Rented in Souel")
HumiA_Souel

HumiA_Wash <- BikeWashingtonDC %>% 
  group_by(Humidity) %>%
  mutate(Hmean_Rent = mean(Count)) %>%
  ggplot(aes(x = Humidity, y = Hmean_Rent, color = Humidity)) +
  geom_point() +
  stat_smooth()+
  xlab("Humidity (%)") + 
  ylab("The Total Number of Rented Bike (per hour)") +
  ggtitle(" Humidity vs Quantity of The Bike Rented in Wahsington")
HumiA_Wash

lm_seoul <- lm(log(Count) ~ Temperature + Humidity + WindSpeed + Season, data = BikeSeoul )
summary(lm_seoul) ## fit a linear model

lm_washington <- lm(log(Count) ~ Temperature + Humidity + WindSpeed + Season, data = BikeWashingtonDC )
summary(lm_washington)

confint(lm_seoul, level = 0.97) ## compute the confidence interval with level 97%
confint(lm_washington, level = 0.97)

SeoulPred <- data.frame(WindSpeed = 0.5, 
                     Humidity = 20,
                     Temperature = 0, 
                     Season = "Winter")
predict(lm_seoul, newdata = SeoulPred, interval = "prediction", level = 0.90) ## prediction interval with level 90%

WashPred<- data.frame(WindSpeed = 0.5, 
                        Humidity = 20,
                        Temperature = 0, 
                        Season = "Winter")
predict(lm_washington, newdata = WashPred, interval = "prediction", level = 0.90)

