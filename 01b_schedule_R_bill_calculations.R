
library(readxl); library(ggplot2); library(lubridate)




##### load and format data #####

# load residential bill calculation function 
source("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Code/Functions/bill_calculator_scheduleR.R")

# test Jan 2020 500 kWh example from HECO: attachment 7 from https://www.hawaiianelectric.com/documents/billing_and_payment/rates/energy_cost_adjustment_filings/oahu/2020/oahu_ecrc_2020_01.pdf
bill_calculator_scheduleR(customer_charge_dollars = 11.50,
                          demand_response_adjustment_clause_cents = -0.0019,
                          dsm_adjustment_cents = 0.0,
                          ecrf_cents = 13.629,
                          green_infrastructure_fee_dollars = 1.25,
                          nonfuel_fuel_energy_block1_charge_cents = 10.6812,
                          nonfuel_fuel_energy_block2_charge_cents = 11.8347,
                          nonfuel_fuel_energy_block3_charge_cents = 13.7121,
                          nonfuel_fuel_energy_block1_qtyKwh = 350,
                          nonfuel_fuel_energy_block2_qtyKwh = 1200,
                          pbf_surcharge_cents = 0.7437,
                          purchase_power_adjustment_cents = 2.3027,
                          rba_rate_adjustment_cents = 0.9376,
                          kwh = 500)

# load schedule R (residential) rate data
rate_data_names <- list.files('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_r',
                              pattern = '.xlsx')
rate_data <- lapply(rate_data_names,
                    function(x) as.data.frame(read_xlsx(paste0('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Rate_Data/schedule_r/',
                                                               x))))
rate_data_names <- sapply(rate_data_names, function(s) substr(s, 1, nchar(s)-5))
names(rate_data) <- rate_data_names
rm(rate_data_names)

# load billing data and keep only schedule R customers
monthly_consumption <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/01a_schedule_R_kwh_monthly_aggregation.rds")
monthly_consumption <- monthly_consumption[monthly_consumption$RATE == 'R',]
monthly_consumption$YearMonth <- as.Date(paste(monthly_consumption$year,
                                               monthly_consumption$month, '15',
                                               sep = '-'))




##### apply running (month-to-month) credits to PV customers as appropriate #####

# PV customers can accumulate credits to use throughout the year. Any remaining at the end of the year are forfeited.
# assume start of "year" is in April
monthly_consumption$billing_YearMonth <- monthly_consumption$YearMonth %m-% months(3)
monthly_consumption$billing_month <-  month(monthly_consumption$billing_YearMonth)
monthly_consumption$billing_year <- year(monthly_consumption$billing_YearMonth)


# take out PV customers, order by customer and date
monthly_consumption_PV <- monthly_consumption[monthly_consumption$PV == 1,]
monthly_consumption_PV <- monthly_consumption_PV[order(monthly_consumption_PV$ID,
                                                       monthly_consumption_PV$billing_YearMonth),]

# split by customer ID and billing year
monthly_consumption_PV_split <- split(monthly_consumption_PV, f = monthly_consumption_PV$ID)
monthly_consumption_PV_split <- lapply(monthly_consumption_PV_split, function(l) split(l, f = l$billing_year))

# get monthly credit (if applicable) carried over from previous month(s) from the same year
for(i in 1:length(monthly_consumption_PV_split)){
  for(j in 1:length(monthly_consumption_PV_split[[i]])){
    
    # get billing data for customer i in year j
    x <- monthly_consumption_PV_split[[i]][[j]]
    
    # within year j, get YTD kWh credits
    x$net_kwh_wCredit <- NA
    for(m in 1:nrow(x)){
      
      # first month of billing year can't have credits carried over
      if(m == 1){ x$net_kwh_wCredit[[m]] <- x$net_kwh[[m]] } else {
        
        # otherwise, any NEGATIVE net usage can be added to the credit
        x$net_kwh_wCredit[[m]] <- x$net_kwh_wCredit[[m-1]] + x$net_kwh[[m]]
        x$net_kwh_wCredit[[m]] <- ifelse(x$net_kwh_wCredit[[m]] > x$net_kwh[[m]],
                                         x$net_kwh[[m]],
                                         x$net_kwh_wCredit[[m]])  # if credit IS NOT negative (i.e. billed kwh would be larger than kwh actually used), bill only for kwh used
      }
    }
    
    # calculate final kWh billed (can't be less than 0)
    x$kwh_billed <- ifelse(x$net_kwh_wCredit < 0, 0, x$net_kwh_wCredit)
    
    monthly_consumption_PV_split[[i]][[j]] <- x; rm(x)
  
  }
}

rm(i, j, m)

# merge data back together
monthly_consumption_PV_split <- lapply(monthly_consumption_PV_split,
                                       function(df) do.call("rbind", df))
monthly_consumption_PV <- do.call('rbind', monthly_consumption_PV_split)

monthly_consumption <- monthly_consumption[monthly_consumption$PV == 0,]
monthly_consumption$net_kwh_wCredit <- monthly_consumption$net_kwh
monthly_consumption$kwh_billed <- ifelse(monthly_consumption$net_kwh < 0, 0,
                                         monthly_consumption$net_kwh)
monthly_consumption <- rbind(monthly_consumption, monthly_consumption_PV)
rm(monthly_consumption_PV, monthly_consumption_PV_split)




##### calculate final monthly bills #####

# calculate monthly bill
monthly_consumption$bill_dollars_tariff <- NA
for(i in 1:nrow(monthly_consumption)){
  monthly_consumption[i, 'bill_dollars_tariff'] <-
    bill_calculator_scheduleR(customer_charge_dollars =
                                rate_data$customer_charge[rate_data$customer_charge$year == monthly_consumption$year[[i]] &
                                                            rate_data$customer_charge$month == monthly_consumption$month[[i]],
                                                          'dollars_per_month'],
                              demand_response_adjustment_clause_cents =
                                rate_data$demand_response_adjustment_clause[rate_data$demand_response_adjustment_clause$year == monthly_consumption$year[[i]] &
                                                                              rate_data$demand_response_adjustment_clause$month == monthly_consumption$month[[i]],
                                                                            'cents_per_kwh'],
                              dsm_adjustment_cents =
                                rate_data$dsm_adjustment[rate_data$dsm_adjustment$year == monthly_consumption$year[[i]] &
                                                           rate_data$dsm_adjustment$month == monthly_consumption$month[[i]],
                                                         'cents_per_kwh'],
                              ecrf_cents =
                                rate_data$ecrf[rate_data$ecrf$year == monthly_consumption$year[[i]] &
                                                 rate_data$ecrf$month == monthly_consumption$month[[i]],
                                               'cents_per_kwh'],
                              green_infrastructure_fee_dollars =
                                rate_data$green_infrastructure_fee[rate_data$green_infrastructure_fee$year == monthly_consumption$year[[i]] &
                                                                     rate_data$green_infrastructure_fee$month == monthly_consumption$month[[i]],
                                                                   'dollars_per_month'],
                              nonfuel_fuel_energy_block1_charge_cents =
                                rate_data$nonfuel_fuel_energy_charge[rate_data$nonfuel_fuel_energy_charge$year == monthly_consumption$year[[i]] &
                                                                       rate_data$nonfuel_fuel_energy_charge$month == monthly_consumption$month[[i]],
                                                                     'cents_per_kwh_first350'],
                              nonfuel_fuel_energy_block2_charge_cents =
                                rate_data$nonfuel_fuel_energy_charge[rate_data$nonfuel_fuel_energy_charge$year == monthly_consumption$year[[i]] &
                                                                       rate_data$nonfuel_fuel_energy_charge$month == monthly_consumption$month[[i]],
                                                                     'cents_per_kwh_next850'],
                              nonfuel_fuel_energy_block3_charge_cents =
                                rate_data$nonfuel_fuel_energy_charge[rate_data$nonfuel_fuel_energy_charge$year == monthly_consumption$year[[i]] &
                                                                       rate_data$nonfuel_fuel_energy_charge$month == monthly_consumption$month[[i]],
                                                                     'cents_per_kwh_over1200'],
                              nonfuel_fuel_energy_block2_qtyKwh = 1200,
                              pbf_surcharge_cents =
                                rate_data$pbf_surcharge[rate_data$pbf_surcharge$year == monthly_consumption$year[[i]] &
                                                          rate_data$pbf_surcharge$month == monthly_consumption$month[[i]],
                                                        'cents_per_kwh'],
                              purchase_power_adjustment_cents =
                                rate_data$purchase_power_adjustment[rate_data$purchase_power_adjustment$year == monthly_consumption$year[[i]] &
                                                                      rate_data$purchase_power_adjustment$month == monthly_consumption$month[[i]],
                                                                    'cents_per_kwh'],
                              rba_rate_adjustment_cents =
                                rate_data$rba_rate_adjustment[rate_data$rba_rate_adjustment$year == monthly_consumption$year[[i]] &
                                                                rate_data$rba_rate_adjustment$month == monthly_consumption$month[[i]],
                                                              'cents_per_kwh'],
                              kwh = monthly_consumption$kwh_billed[[i]])
}

rm(i)



##### summary data #####

monthly_consumption$`PV status` <- ifelse(monthly_consumption$PV == 1, 'PV', 'No PV')

# get median consumption and bill by household and year
monthly_consumption_median <- aggregate(monthly_consumption[c("kwh_billed", "bill_dollars_tariff")],
                                        list(monthly_consumption$ID), median)
colnames(monthly_consumption_median) <- c('ID', 'kwh_billed_median', 'bill_dollars_tariff_median')
pv_status <- monthly_consumption[!duplicated(monthly_consumption$ID), c('ID', 'PV status')]
monthly_consumption_median <- merge(monthly_consumption_median, pv_status, 'ID')
rm(pv_status)

# summarize/plot estimated bills
aggregate(monthly_consumption_median$kwh_billed_median,
          list(monthly_consumption_median$`PV status`), summary)
aggregate(monthly_consumption_median$bill_dollars,
          list(monthly_consumption_median$`PV status`), summary)


# save data
saveRDS(monthly_consumption, file = "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/01c_schedule_R_bill_calculations_currentBlock.rds")
