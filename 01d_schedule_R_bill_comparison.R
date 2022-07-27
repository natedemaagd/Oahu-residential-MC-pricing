
library(ggplot2)

# load data
monthly_data_currentBlock <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/01c_schedule_R_bill_calculations_currentBlock.rds")
monthly_data_mcPricing <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/01c_schedule_R_bill_calculations_marginalCost.rds")
monthly_data_currentBlock$net_kwh <- NULL  # get rid of one of the net_kwh for merging (both data.frames have this already, but MC has the negative ones we need)

# merge data
monthly_data_currentBlock$RATE <- monthly_data_currentBlock$PV <- NULL
monthly_data <- merge(monthly_data_mcPricing, monthly_data_currentBlock,
                      by = c('ID', 'billing_year', 'billing_month'))
colnames(monthly_data)[colnames(monthly_data) == 'netValue_monthlyDollars'] <- 'bill_dollars_MC_NoFixedCost'

# total revenue for each type
total_revenue_currentBlock <- sum(monthly_data$bill_dollars_tariff)
total_revenue_mcPricing <- sum(monthly_data$bill_dollars_MC_NoFixedCost)

# what fixed price needs to be added to the marginal cost structure to earn the same revenue as the block structure?
fixed_monthly_cost <-
  (total_revenue_currentBlock - total_revenue_mcPricing)/nrow(monthly_data)
#fixed_monthly_cost*nrow(monthly_data) + sum(monthly_data$bill_dollars_MC_NoFixedCost) == sum(monthly_data$bill_dollars_tariff)  # test equality of revenues

# add fixed cost to MC pricing to get total bill
monthly_data$bill_dollars_MC_wFixedCost <-
  monthly_data$bill_dollars_MC_NoFixedCost + fixed_monthly_cost

# add consumption deciles (based on median consumption by customer)
consumer_median_consumption <-
  aggregate(monthly_data$net_kwh, list(monthly_data$ID), median, na.rm = TRUE)
colnames(consumer_median_consumption) <- c('ID', 'median_monthly_kwh')
consumer_median_consumption <-
  within(consumer_median_consumption,
         consumption_decile <- as.integer(cut(median_monthly_kwh,
                                              quantile(median_monthly_kwh,
                                                       probs=0:10/10),
                                              include.lowest=TRUE)))
monthly_data <- merge(monthly_data, consumer_median_consumption, 'ID')
rm(consumer_median_consumption, monthly_data_currentBlock, monthly_data_mcPricing)

# add consumption deciles, separated by PV and non-PV
monthly_data_PV <- monthly_data[monthly_data$PV == 1,]
consumer_median_consumption_PV <- aggregate(monthly_data_PV$net_kwh,
                                            list(monthly_data_PV$ID),
                                            median, na.rm = TRUE)
colnames(consumer_median_consumption_PV) <- c('ID', 'median_monthly_kwh')
consumer_median_consumption_PV <- within(consumer_median_consumption_PV,
                                         consumption_decile_byPV <-
                                           as.integer(cut(median_monthly_kwh,
                                                          quantile(median_monthly_kwh,
                                                                   probs=0:10/10),
                                                          include.lowest=TRUE)))
monthly_data_PV <- merge(monthly_data_PV, consumer_median_consumption_PV, 'ID')
rm(consumer_median_consumption_PV)

monthly_data_nonPV <- monthly_data[monthly_data$PV == 0,]
consumer_median_consumption_nonPV <- aggregate(monthly_data_nonPV$net_kwh,
                                               list(monthly_data_nonPV$ID),
                                               median, na.rm = TRUE)
colnames(consumer_median_consumption_nonPV) <- c('ID', 'median_monthly_kwh')
consumer_median_consumption_nonPV <- within(consumer_median_consumption_nonPV,
                                            consumption_decile_byPV <-
                                              as.integer(cut(median_monthly_kwh,
                                                             quantile(median_monthly_kwh,
                                                                      probs=0:10/10),
                                                             include.lowest=TRUE)))
monthly_data_nonPV <- merge(monthly_data_nonPV,
                            consumer_median_consumption_nonPV,
                            'ID')
rm(consumer_median_consumption_nonPV)

monthly_data <- rbind(monthly_data_nonPV, monthly_data_PV)
monthly_data$median_monthly_kwh.x <- monthly_data$median_monthly_kwh.y <- NULL
rm(monthly_data_nonPV, monthly_data_PV, total_revenue_currentBlock, total_revenue_mcPricing)




##### analyze differences in bills - table #####

# get summary data by customer ID, split by PV
monthly_data$`PV status` <- ifelse(monthly_data$PV == 1, 'PV', 'No PV')
monthly_data_median_kwh_billed <- aggregate(monthly_data$kwh_billed,
                                            list(monthly_data$ID, monthly_data$`PV status`),
                                            median)
colnames(monthly_data_median_kwh_billed) <- c('ID', 'PV status', 'kwh_billed_median')

monthly_data_median_kwh_net <- aggregate(monthly_data$net_kwh,
                                         list(monthly_data$ID, monthly_data$`PV status`),
                                         median)
colnames(monthly_data_median_kwh_net) <- c('ID', 'PV status', 'kwh_net_median')

monthly_data_median_bill_tariff <- aggregate(monthly_data$bill_dollars_tariff,
                                             list(monthly_data$ID, monthly_data$`PV status`),
                                             median)
colnames(monthly_data_median_bill_tariff) <- c('ID', 'PV status', 'bill_dollars_tariff_median')

monthly_data_median_bill_MC <- aggregate(monthly_data$bill_dollars_MC_wFixedCost,
                                         list(monthly_data$ID, monthly_data$`PV status`),
                                         median)
colnames(monthly_data_median_bill_MC) <- c('ID', 'PV status', 'bill_dollars_MC_wFixedCost_median')

# merge all data
monthly_data_median <- merge(subset(monthly_data_median_kwh_billed,
                                    select = -c(`PV status`)),
                             monthly_data_median_bill_tariff, 'ID')
monthly_data_median <- merge(subset(monthly_data_median,
                                    select = -c(`PV status`, bill_dollars_tariff_median)),
                             monthly_data_median_kwh_net, 'ID')
monthly_data_median <- merge(subset(monthly_data_median,
                                    select = -c(`PV status`)),
                             monthly_data_median_bill_tariff, 'ID')
monthly_data_median <- merge(subset(monthly_data_median,
                                    select = -c(`PV status`)),
                             monthly_data_median_bill_MC, 'ID')
rm(monthly_data_median_bill_MC, monthly_data_median_bill_tariff,
   monthly_data_median_kwh_billed, monthly_data_median_kwh_net)

# merge deciles to median data
consumption_deciles <- monthly_data[!duplicated(monthly_data$ID),
                                    c('ID', 'consumption_decile', 'consumption_decile_byPV')]
monthly_data_median <- merge(monthly_data_median, consumption_deciles, 'ID')
rm(consumption_deciles)

# calculate difference in bills
monthly_data$billMC_minus_billBlock <-
  monthly_data$bill_dollars_MC_wFixedCost - monthly_data$bill_dollars_tariff
monthly_data_median$billMC_minus_billBlock_median <-
  monthly_data_median$bill_dollars_MC_wFixedCost_median -
  monthly_data_median$bill_dollars_tariff_median

# summary stats: difference between PV and non-PV: mc bill / block bill
aggregate(monthly_data_median$kwh_billed_median,
          list(monthly_data_median$`PV status`), summary)
aggregate(monthly_data_median$bill_dollars_tariff_median,
          list(monthly_data_median$`PV status`), summary)
aggregate(monthly_data_median$bill_dollars_MC_wFixedCost_median,
          list(monthly_data_median$`PV status`), summary)

aggregate(monthly_data$kwh_billed,
          list(monthly_data$`PV status`), summary)
aggregate(monthly_data$bill_dollars_tariff,
          list(monthly_data$`PV status`), summary)
aggregate(monthly_data$bill_dollars_MC_wFixedCost,
          list(monthly_data$`PV status`), summary)




##### analyze differences in bills - plots #####

# plots - median monthly consumption between PV and non-PV
ggplot(monthly_data_median) + geom_histogram(aes(kwh_net_median,
                                                 color = `PV status`,
                                                 fill = `PV status`),
                                             alpha = 0.2) +
  xlim(-1000, 3500) +
  labs(x = 'Median net monthly kWh', y = 'Number of households')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_net_monthly_kwh_histogram.png',
       height = 6, width = 8)

ggplot(monthly_data_median) + geom_density(aes(kwh_net_median,
                                               color = `PV status`,
                                               fill = `PV status`),
                                           alpha = 0.2) +
  xlim(-1000, 3500) +
  labs(x = 'Median net monthly kWh', y = 'Proportion of households')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_net_monthly_kwh_density.png',
       height = 6, width = 8)


# plots - median monthly bills between PV and non-PV, current block tariff
ggplot(monthly_data_median) + geom_histogram(aes(bill_dollars_tariff_median,
                                                 color = `PV status`,
                                                 fill = `PV status`),
                                             alpha = 0.2) +
  xlim(-100, 1000) +
  labs(x = 'Median net monthly bill ($)', y = 'Number of households')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_net_monthly_bill_BlockTariff_histogram.png',
       height = 6, width = 8)

ggplot(monthly_data_median) + geom_density(aes(bill_dollars_tariff_median,
                                               color = `PV status`,
                                               fill = `PV status`),
                                           alpha = 0.2) +
  xlim(-100, 1000) +
  labs(x = 'Median net monthly bill ($)', y = 'Proportion of households')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_net_monthly_bill_BlockTariff_density.png',
       height = 6, width = 8)


# plots - median monthly bills between PV and non-PV, MC pricing
ggplot(monthly_data_median) + geom_histogram(aes(bill_dollars_MC_wFixedCost_median,
                                                 color = `PV status`,
                                                 fill = `PV status`),
                                             alpha = 0.2) +
  xlim(-100, 500) +
  labs(x = 'Median net monthly bill ($)', y = 'Number of households')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_net_monthly_bill_McPricing_histogram.png',
       height = 6, width = 8)

ggplot(monthly_data_median) + geom_density(aes(bill_dollars_MC_wFixedCost_median,
                                               color = `PV status`,
                                               fill = `PV status`),
                                           alpha = 0.2) +
  xlim(-100, 500) +
  labs(x = 'Median net monthly bill ($)', y = 'Proportion of households')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_net_monthly_bill_McPricing_density.png',
       height = 6, width = 8)


# plots - difference between median bills (MC pricing - block tariff pricing), between PV and non-PV
ggplot(monthly_data_median) +
  geom_histogram(aes(bill_dollars_MC_wFixedCost_median - bill_dollars_tariff_median,
                     color = `PV status`, fill = `PV status`), alpha = 0.2) +
  xlim(-500,250) +
  labs(x = 'Difference in monthly bill ($, MC pricing - Block pricing)',
       y = 'Number of households') +
  geom_vline(xintercept = 0, linetype = 'longdash')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_priceDifference_histogram.png',
       height = 6, width = 8)

ggplot(monthly_data_median) +
  geom_density(aes(bill_dollars_MC_wFixedCost_median - bill_dollars_tariff_median,
                   color = `PV status`, fill = `PV status`), alpha = 0.2) +
  xlim(-500,250) +
  labs(x = 'Difference in monthly bill ($, MC pricing - Block pricing)', y = 'Proportion of households') +
  geom_vline(xintercept = 0, linetype = 'longdash')
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_priceDifference_density.png',
       height = 6, width = 8)


# plot - difference in bill between top and bottom 20%
plotdat <- monthly_data_median[monthly_data_median$consumption_decile_byPV %in% c(1,2,9,10),]
plotdat$top_bottom <-
  ifelse(plotdat$consumption_decile_byPV %in% c(1,2), 'Bottom 20%', 'Top 20%')

ggplot(plotdat) +
  geom_histogram(aes(bill_dollars_MC_wFixedCost_median - bill_dollars_tariff_median,
                     color = `PV status`, fill = `PV status`), alpha = 0.2) +
  xlim(-400, 100) +
  labs(x = 'Difference in monthly bill ($, MC pricing - Block pricing)', y = 'Number of households') +
  geom_vline(xintercept = 0, linetype = 'longdash') +
  facet_grid(rows = vars(top_bottom))
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_priceDifference_TopBottomConsumers_histogram.png',
       height = 6, width = 8)

ggplot(plotdat) +
  geom_density(aes(bill_dollars_MC_wFixedCost_median - bill_dollars_tariff_median,
                   color = `PV status`, fill = `PV status`), alpha = 0.2) +
  xlim(-400, 100) +
  labs(x = 'Difference in monthly bill ($, MC pricing - Block pricing)', y = 'Proportion of households') +
  geom_vline(xintercept = 0, linetype = 'longdash') +
  facet_grid(rows = vars(top_bottom))
ggsave('D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/01d_median_priceDifference_TopBottomConsumers_density.png',
       height = 6, width = 8)


