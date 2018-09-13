

###### 1. Data Loading ---- ######

require(stringr)
require(dplyr)
require(ggplot2)


uber <- read.csv("Uber Request Data.csv", stringsAsFactors = F)

str(uber)
View(uber)



###### 2. Data Cleaning ---- ######

# Finding NA values column-wise

find_na <- data.frame(is.na(uber))
sapply(find_na, sum)

# Driver.id and Drop.timesstamp are the only columns containing NA's.

# Driver.id containing 2650 NA's (39%)

sum(uber$Status[which(is.na(uber$Driver.id))] == "Cancelled")
sum(uber$Status[which(is.na(uber$Driver.id))] == "Trip Completed")
sum(uber$Status[which(is.na(uber$Driver.id))] == "No Cars Available")

# All the NA's in Driver.id are for requests which got "No Cars Available".
# When no cars are not available, Driver.id cannot be available too.
# So these NA's are reasonable.

# Drop.timesstamp containing 3914 NA's (58%)

sum(uber$Status[which(is.na(uber$Drop.timestamp))] == "Cancelled")
sum(uber$Status[which(is.na(uber$Drop.timestamp))] == "Trip Completed")
sum(uber$Status[which(is.na(uber$Drop.timestamp))] == "No Cars Available")

# All the NA's in Drop.timesstamp are for requests either "cancelled" or "No Cars Available".
# For requests that did not get a ride, there cannot be drop times too.
# So these NA's are also reasonable.

# Let's keep them as they are for now.


# Request.timestamp and Drop.timestamp columns are not in proper format

str_count(uber[, 5:6], pattern = "/")
str_count(uber[, 5:6], pattern = "-")

# Two types of separators are used heavily in dates ("/" and "-"). 
# Converting them to one type i.e. "-"

uber$Request.timestamp <-  str_replace_all(uber$Request.timestamp, pattern = "/", "-")
uber$Drop.timestamp <- str_replace_all(uber$Drop.timestamp, pattern = "/", "-")


# Seconds values are missing at several indices in both these columns
# For the sake of proper formatting, assuming seconds = "00" where they are missing.

uber$correction1 <- 
  str_detect(uber$Request.timestamp, pattern = "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")

uber$Request.timestamp[which(uber$correction1 == F)] <- 
  paste(uber$Request.timestamp[which(uber$correction1 == F)], "00", sep = ":")

uber$correction2 <- 
  str_detect(uber$Drop.timestamp, pattern = "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")

uber$Drop.timestamp[which(uber$correction2 == F)] <- 
  paste(uber$Drop.timestamp[which(uber$correction2 == F)], "00", sep = ":")


# Coverting Request.timestamp and Drop.timestamp columns into proper format

uber$Request.timestamp <- 
  as.POSIXct(uber$Request.timestamp, tz = "", format = "%d-%m-%Y %H:%M:%S")

uber$Drop.timestamp <- 
  as.POSIXct(uber$Drop.timestamp, tz = "", format = "%d-%m-%Y %H:%M:%S")

# Deriving new metrics which may be useful for analysis and plotting.

uber$request.date <- as.Date(uber$Request.timestamp, tz = "")
uber$request.hrs <- as.numeric(format(uber$Request.timestamp, "%H"))

uber$travel.time <- as.numeric(uber$Drop.timestamp - uber$Request.timestamp)


# Dropping useless columns
uber <- uber[, -c(7,8)]

str(uber)
# The dataset is ready for plotting and analysis



###### 3. Plotting and Analysis ---- ######

# The pressing problem for Uber, like any business, is revenue loss.
# For Uber, revenue loss has 2 forms:

# 1. Drivers cancelling requests of customers
# 2. Unavailablity of cabs at pickup points


# Plotting no. of requests against request dates
date_plot <- ggplot(uber, aes(x = request.date)) + 
             ggtitle("Date-wise Plot") + 
             labs(x = "Request Date", y = "No. of requests")

# Choosing bar graph geometry
# because we want to plot request dates as category on x-axis
# and no. of requests as quantity on y-axis

# Also, fractions of quantity as per sub-category is visibly pleasing
# in form of stacked bars

date_plot + geom_bar()
# Almost consistent pattern of no. of requests everyday.

date_plot + geom_bar(aes(fill = Status))
# Almost consistent pattern of status of requests everyday.

date_plot + geom_bar(aes(fill = Pickup.point))
# Almost consistent pattern of requests from city and airport everyday.


# Plotting travel time using boxplot as we need to see the spread of travel time - 

# against request dates
ggplot(uber, aes(x = as.factor(request.date), y = travel.time)) + geom_boxplot()

# against request hours
ggplot(uber, aes(x = as.factor(request.hrs), y = travel.time)) + geom_boxplot()

mean(uber$travel.time, na.rm = T)
median(uber$travel.time, na.rm = T)
range(uber$travel.time, na.rm = T)

# Travel time varies but within a reasonable range for all dates and hours.

# No specific pattern so far.


# Plotting no. of requests against request hours
hour_plot <- ggplot(uber, aes(x = request.hrs)) + 
             ggtitle("Hour-wise Plot") + 
             labs(x = "Request Hour", y = "No. of requests")

# Choosing bar graph geometry
# because we want to plot request hours as category on x-axis
# and no. of requests as quantity on y-axis

# Also, fractions of quantity as per sub-category is visibly pleasing
# in form of stacked bars

hour_plot + geom_bar()
hour_plot + geom_bar(aes(fill = ..count..))

# We see that bars for hours 5 to 9 and for hours 17 to 21 rise tremendously than others.
# Here, time of requests is slotted into hours i.e. 
# Time from 09:00:00 to 09:59:59 is considered Hour "9" and 21:00:00 to 21:59:59 is considered Hour "21".

# It means maximum requests are made in time slot 5 AM to 9:59:59 AM (Practically 10 AM)
# and 5 PM to 9:59:59 PM (Practically 10 PM).
# Let's call them morning peak slot and evening peak slot respectively.

# Making subsets for these slots
morning_peak_slot <- filter(uber, (request.hrs>=5 & request.hrs<10))
evening_peak_slot <- filter(uber, (request.hrs>=17 & request.hrs<22))

# We also see that minimum requests are made in slot 00-04 hours.

hour_plot + geom_bar(aes(fill = ..count..)) +  facet_wrap(~request.date)

# We see that this pattern exists for all the dates.
# So, it is true for any typical day


count(morning_peak_slot) / count(uber) * 100
# Approx. 31% of total requests come in morning peak slot

count(evening_peak_slot) / count(uber) * 100
# Approx. 35% of total requests come in evening peak slot

# Collectively, these slots get 66% of total requests.


# Plotting fraction of requests' status against request hours
hour_plot + geom_bar(aes(fill = Status))

hour_plot + geom_bar(aes(fill = Status), position = "fill")

# Big fraction of cancelled requests in morning peak slot 
# Big fraction of requests facing unavailablity of cars in 00-04 hours and evening peak slot  

# We already know that slot 00-04 hours has lowest demand.
# Also, being past midnight hours,unavailability here can be understood.
# So neglecting this slot for now and going ahead with high-demand slots only

hour_plot + geom_bar(aes(fill = Status)) +  facet_wrap(~request.date)

# We see that this pattern exists for all the dates.
# So, it is true for any typical day


count(morning_peak_slot[which(morning_peak_slot$Status == "Cancelled"),])/count(morning_peak_slot)*100
# In morning peak slot, 40% of requests are cancelled.

count(evening_peak_slot[which(evening_peak_slot$Status == "No Cars Available"),])/count(evening_peak_slot)*100
# In evening peak slot, 60% of requests face unavailability.

# So these 2 slots are critical (high-demand and low-supply).


# Plotting no. of requests against request hours as per Pickup points
# Using geom_step + geom_point geometry because -
# We want to see trend of no. of requests from both pickup points against timeline
# (hours of a day).

# Using geom_step instead of geom_line because
# We want to notice steep rises and falls clearly.

hour_plot + geom_step(aes(col = Pickup.point), stat = "count", size = 1) + 
            geom_point(aes(col = Pickup.point), stat = "count", size = 2)

# For pickup point City, we see that -

# 1. No. of requests rises drastically between  5 AM to 10 AM i.e. morning peak slot
# 2. Comparably very low no. of requests in evening peak slot
# 3. This suggests that there is high no. of flights departure near to morning peak slot
# 4. This suggests that there is low no. of flights departure near to evening peak slot


# For pickup point at Airport, we see that -
# 1. No. of requests rises drastically between 5 PM to 10 PM i.e. evening peak slot
# 2. Comparably very low no. of requests in morning peak slot
# 3. This suggests that there is high no. of flights arrival near to evening peak slot
# 4. This suggests that there is low no. of flights arrival near to morning peak slot

hour_plot + geom_step(aes(col = Pickup.point), stat = "count", size = 1) + 
            geom_point(aes(col = Pickup.point), stat = "count", size = 2) + 
            facet_wrap(~request.date)

# We see that this pattern exists for all the dates.
# So, it is true for any typical day.

count(morning_peak_slot[which(morning_peak_slot$Pickup.point == "City"),]) / count(morning_peak_slot) * 100
# Approx. 80% requests in morning peak slot come from City

count(evening_peak_slot[which(evening_peak_slot$Pickup.point == "Airport"),]) / count(evening_peak_slot) * 100
# Approx. 77% requests in 5 PM - 10 PM come from airport


# For a consolidated view, plotting no. of requests against request hours,
# displaying status and Pickup.point on same graph 

hour_plot + geom_bar(aes(fill = Status), alpha = 0.5) +
            geom_step(aes(col = Pickup.point), stat = "count", size = 1) +
            geom_point(aes(col = Pickup.point), stat = "count", size = 2) +
            scale_color_brewer(palette = "Set1")

# Each line represent no. of requests against request hours for each pickup point.
# Stacked Bars in background represent fraction of each status in total no. of requests
# against respective request hours.



###### 4. Write-up of Analysis ---- ######

# There are two high-demand time slots in a typical day,
# 1. 5 AM - 10 AM (morning peak slot)
# 2. 5 PM - 10 PM (evening peak slot)


# In morning peak slot,
# 80% demand comes for city to airport and 20% for airport to city.
# It implies high no. of flights departure, but low no. of flight arrival.
# It is likely, that drivers who go to airport don't get customers for return journey easily.
# Rather than going to airport and waiting for long, they prefer to cancel the request.
# That is why, 40% of total requests in this slot face cancellation.


# In evening peak slot,
# 77% demand comes for airport to city and 23% for city to airport.
# It implies high no. of flights arrival, but low no. of flight departure.
# It is likely, that due to low no. of flight departure in this slot,
# enough cars don't reach from city to airport to suffice the demand from airport to city.
# That is why, 60% of total requests in this slot face unavailability.


# Another reason, which is not directly evident, may be that,
# Both the slots contain peak office hours.
# Generally office timings start around 9 AM - 10 AM and end around 5 PM - 6 PM.
# Lot of people travel to and fro their offices in vicinity of their office timings.
# Both of these office timings lie between our critical slots.
# We already saw that travel time between City and Airport averages to around 1 hour.
# So rather than spending 1 hour to go to airport, and 1 hour to return,
# drivers may be preferring to profit more by serving requests from office commuters within the city
# which are abundant in peak office hours and consume lesser time to complete.



#####################################################################.