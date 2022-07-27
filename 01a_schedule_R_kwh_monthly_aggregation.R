
library(lubridate); library(dplyr)

# load formatted smart meter data (w/o HECO data merged)
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Residential/00_smartMeterData.R")
#z <- readRDS('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Residential/00_smartMeterData_noHeco.rds')

# aggregate data by account and month
z$year <- year(z$dt)
z$month <- month(z$dt)
monthly_consumption <- aggregate(z$KW_NET, list(z$ID, z$year, z$month),
                                 sum, na.rm = TRUE)

# format and sort data
colnames(monthly_consumption) <- c('ID', 'year', 'month', 'net_kwh')
z_rates_and_pv <- z[!duplicated(z$ID),  c('ID', 'RATE', 'PV')]
monthly_consumption <- merge(monthly_consumption, z_rates_and_pv, 'ID')
monthly_consumption <- monthly_consumption[order(monthly_consumption$ID,
                                                 monthly_consumption$year,
                                                 monthly_consumption$month), ]

# merge census tract with aggregated data
dat_censusTract <- z[!duplicated(z$ID), c('ID', 'CENSUS_TRACT')]
monthly_consumption <- left_join(monthly_consumption,
                                 dat_censusTract,
                                 'ID')


# save monthly aggregate data
saveRDS(monthly_consumption, file = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Residential/01a_schedule_R_kwh_monthly_aggregation.rds")
