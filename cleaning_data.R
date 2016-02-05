library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(lubridate)

# load slave trade database data
library(foreign)
st <- foreign::read.spss("tastdb-exp-2010.sav")
str(st)
labels <- attr(st, "label.table")
data <- data.frame(st)


dat <- read.csv("tastdb-2010.csv", stringsAsFactors = FALSE)
country_codes <- read.csv("country_codes.csv")
names(country_codes)[2] <- 'national'
dat <- full_join(dat, country_codes)
dat <- dat %>% select(-national)

# turn date variables into actual dates 
dat$Date_dep %<>% parse_date_time("%m/%d/%y")
dat$Date_buy %<>% parse_date_time("%m/%d/%y")
dat$Date_leftAfr %<>% parse_date_time("%m/%d/%y")
dat$Date_land1 %<>% parse_date_time("%m/%d/%y")
dat$Date_depam %<>% parse_date_time("%m/%d/%y")
dat$Date_end %<>% parse_date_time("%m/%d/%y")
# number of trips
length(unique(dat$voyageid))
# number of unique ships
length(unique(dat$shipname))

# mean # of trips per ship
length(unique(dat$voyageid))/length(unique(dat$shipname))
summary(data.frame(table(dat$shipname)))
ships <- data.frame(table(dat$shipname))
ships %>% filter(Freq <=100) %>% select(Freq)
# ships that made <=100 & >100 journeys
ships100 <- ships %>% filter(Freq <=100)
ships101 <- ships %>% filter(Freq >100) 
qplot(ships100$Freq,  binwidth = 1)

# ncar 13, ncar15, ncar17
# number of slaves at departure: TSLAVESD
# number of slaves at first destination: SLAARRIV 

summary(dat$tslavesd) # 26,734 nas
summary(dat$slaarriv) # 16,757 nas

qplot(dat$tslavesd, binwidth=20)
qplot(dat$slaarriv, binwidth = 20)
qplot(data = dat, x = tslavesd, y = slaarriv, color = year(Date_dep)) +
  stat_function(fun = 'identity') + facet_wrap(~Country)
# DATEDEPC Year that voyage began

# look at # slaves leaving over time

qplot(data = dat, x = Date_dep, y = tslavesd, group = Country, 
      color = Country, geom = 'line') 

# what the ports are 

unique(as.character(data$embport))

data$embport <- tolower(as.character(data$embport))
data$portdep <- tolower(as.character(data$portdep))

table(match(ports$PORT_NAME, data$embport))
927 2567    3  463  290 2303  303 5055 2303    5    5

unique(match(unique(data$portdep), ports$PORT_NAME))

data$embport[927]
which(ports$PORT_NAME == "calabar")
which(data$embport == "calabar")

data$embport[c(3,5,290,303,463,927,2303,2567, 5055)]

matching_ports <- ports[na.omit(unique(match(unique(data$portdep), ports$PORT_NAME))),]

qplot(data = matching_ports, x= LONGITUDE, y = LATITUDE)


data_ports_coords <- data[na.omit(unique(match(ports$PORT_NAME, data$portdep))), ]
nrow(data_ports_coords)

sum(table(data$portdep))


# data with no port of departure 
data_no_portdep <- data[which(is.na(data$portdep)),]
head(data_no_portdep[which(data_no_portdep$fate == "Voyage completed as intended"),])

length(unique(as.character(data$majselpt)))

data_yes_portdep <- data[-which(is.na(data$portdep)),]

length(unique(data_yes_portdep$portdep))
table(data_yes_portdep$portdep)
