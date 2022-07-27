
library(data.table); library(lubridate)

# load data
load("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Residential/00_smartMeterData.R")

# keep only schedule R customers (residential)
z <- z[z$RATE == 'R',]

# add billing YearMonth to data
z$billing_YearMonth <- z$dt %m-% months(3)
z$billing_year <- year(z$billing_YearMonth)
z$billing_month <- month(z$billing_YearMonth)




##### calculate bills #####

# calculate total amount of MC (net value) by billing month for each customer
monthly_cost <- aggregate(z$netValue, list(z$ID, z$billing_year, z$billing_month), sum, na.rm = TRUE)
colnames(monthly_cost) <- c('ID', 'billing_year', 'billing_month', 'netValue_monthlyDollars')

# calculate net monthly kWh
monthly_kwh <- aggregate(z$KW_NET, list(z$ID, z$billing_year, z$billing_month), sum, na.rm = TRUE)
colnames(monthly_kwh) <- c('ID', 'billing_year', 'billing_month', 'net_kwh')

# merge monthly cost and monthly data
monthly_consumption <- merge(monthly_cost, monthly_kwh, c('ID', 'billing_year', 'billing_month'))

# add PV and rate schedule
id_chars <- z[!duplicated(z$ID), c('ID', 'PV', 'RATE')]
monthly_consumption <- merge(monthly_consumption, id_chars, 'ID')

rm(monthly_cost, monthly_kwh)

# add census tract
dat_censusTract <- z[!duplicated(z$ID), c('ID', 'CENSUS_TRACT')]
monthly_consumption <- left_join(monthly_consumption,
                                 dat_censusTract,
                                 'ID')
rm(dat_censusTract)

saveRDS(monthly_consumption, file = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Residential/01c_schedule_R_bill_calculations_marginalCharge.rds")
