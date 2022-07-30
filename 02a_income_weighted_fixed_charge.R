
# This script creates weights for households based on the median income of their census tract.
# The weights are then used to adjust the fixed charge used in marginal cost pricing. Instead of
# a fixed charge that is equal between households, the fixed charge can be calculated using
# these weights.

library(xlsx); library(tidyverse); library(ggplot2); library(scales)
library(ggpubr); library(grid); library(gridExtra)




##### load data #####

# load census tract income data
dat_income <- read.csv("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/census/ACSST5Y2020.S1901_data_with_overlays_2022-07-27T115637.csv")

# load HECO schedule R data
dat_meterCounts <-
  read.xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Aggregated_Profiles_ csv_ and_parquet/Full_Meter_Population_Counts_AMI_2021_for_UH_V20220719_V1.xlsx",
            sheetIndex = 1)

# load previous MC pricing data with constant fixed charge across all households
dat_bills <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/Residential/01d residential mc pricing.rds")




##### data formatting #####

# format income data
dat_income <- dat_income[c('Geographic.Area.Name',
                           'Estimate..Households..Median.income..dollars.')]
colnames(dat_income) <- c('CT_CODE', 'medianHouseholdIncome2020Dollars')
dat_income$medianHouseholdIncome2020Dollars <-
  as.numeric(dat_income$medianHouseholdIncome2020Dollars)
dat_income$CT_CODE <- gsub("[^[:digit:]. ]", "", dat_income$CT_CODE)
dat_income$CT_CODE <- gsub(" ", "", dat_income$CT_CODE)

# merge census data, keep only residential (schedule R) observations
dat_census <- merge(dat_income, dat_meterCounts, 'CT_CODE')
dat_census <- dat_census[dat_census$BILL_RATE_SUMMARY == 'R',]
rm(dat_income, dat_meterCounts)




##### adjust monthly charges according to income #####

# merge income data to billing data
dat_census <- rename(dat_census, CENSUS_TRACT = CT_CODE)
dat_census$CENSUS_TRACT<- dat_census$CENSUS_TRACT %>%
  as.numeric()
dat_bills <- left_join(dat_bills,
                       dat_census[c('CENSUS_TRACT', 'medianHouseholdIncome2020Dollars')],
                       'CENSUS_TRACT')
rm(dat_census)

# if income is missing, assign mean income
dat_bills$medianHouseholdIncome2020Dollars[is.na(dat_bills$medianHouseholdIncome2020Dollars)] <-
  mean(dat_bills$medianHouseholdIncome2020Dollars, na.rm = TRUE)

# create income weights
dat_bills$income_weight <-
  dat_bills$medianHouseholdIncome2020Dollars /
  sum(dat_bills$medianHouseholdIncome2020Dollars, na.rm = TRUE)

# get total sum of fixed charge
fixedCharge <- with(dat_bills,
                    sum(bill_dollars_MC_wFixedCharge - bill_dollars_MC_NoFixedCharge))

# redistribute fixed charge according to income weight
dat_bills$monthlyFixedCharge_incomeWeighted <-
  fixedCharge * dat_bills$income_weight

# create final bill under MC pricing with income-weighted fixed charge
dat_bills$bill_dollars_MC_wIncomeWtdFixedCharge <-
  dat_bills$bill_dollars_MC_NoFixedCharge + dat_bills$monthlyFixedCharge_incomeWeighted

rm(fixedCharge)




##### plot differences in bills between PV and non-PV, income-based fixed charge #####

lineColors <- rev(hue_pal()(2))

# PV variable for plotting
dat_bills$`Has PV?` <- ifelse(dat_bills$PV == 1, 'Yes', 'No')
dat_bills$`Has PV?` <- factor(dat_bills$`Has PV?`, levels = c('Yes', 'No'))

# plot monthly income-based fixed charge
plotdat <- dat_bills[!duplicated(dat_bills$ID),]
bill_currentTariff <- textGrob(round(unique(plotdat$monthlyFixedCharge), 2),
                               gp = gpar(col = 'black', fontface="bold"))
bill_McPv0 <- textGrob(round(mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 0]), 2),
                       gp = gpar(col = lineColors[[1]], fontface="bold"))
bill_McPv1 <- textGrob(round(mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 1]), 2),
                       gp = gpar(col = lineColors[[2]], fontface="bold"))
ggplot(data = plotdat,
       aes(monthlyFixedCharge_incomeWeighted, fill = `Has PV?`)) +
  geom_histogram(alpha = 0.2, bins = 10) +
  geom_vline(xintercept = unique(plotdat$monthlyFixedCharge),
             linetype = 'longdash', size = 0.8) +
  geom_vline(xintercept = c(mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 0]),
                            mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 1])),
             linetype = 'longdash', size = 0.8, color = lineColors) +
  labs(x = 'Income-weighted fixed charge ($/month)',
       y = 'Number of households') +
  annotation_custom(bill_currentTariff,
                    xmin = unique(plotdat$monthlyFixedCharge) + 2,
                    xmax = unique(plotdat$monthlyFixedCharge) + 2,
                    ymin = -190, ymax = -190) +
  annotation_custom(bill_McPv0,
                    xmin = mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 0]) - 4,
                    xmax = mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 0]) - 4,
                    ymin = -190, ymax = -190) +
  annotation_custom(bill_McPv1,
                    xmin = mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 1]) + 4,
                    xmax = mean(dat_bills$monthlyFixedCharge_incomeWeighted[dat_bills$PV == 1]) + 4,
                    ymin = -190, ymax = -190) +
  coord_cartesian(clip = "off") +
  theme(text = element_text(size = 13))
ggsave(filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/Residential/02a income-weighted fixed charges.png',
       dpi = 300, height = 4, width = 9)
rm(plotdat, bill_currentTariff, bill_McPv0, bill_McPv1)

# plot monthly bills - original block pricing
labels0 <- data.frame(x = c(mean(dat_bills$bill_dollars_tariff[dat_bills$PV == 0]) + 35,
                            mean(dat_bills$bill_dollars_tariff[dat_bills$PV == 1]) - 33),
                      y = c(0.014, 0.014),
                      label = paste0('$',
                                     format(round(c(mean(dat_bills$bill_dollars_tariff[dat_bills$PV == 0]),
                                                    mean(dat_bills$bill_dollars_tariff[dat_bills$PV == 1])),
                                                  2),
                                            nsmall = 2)))
p0 <- ggplot() +
  geom_density(data = dat_bills, aes(bill_dollars_tariff,
                              fill = `Has PV?`),
               alpha = 0.3) +
  scale_x_continuous(limits = c(0, 400)) +
  geom_vline(xintercept = c(mean(dat_bills$bill_dollars_tariff[dat_bills$PV == 0]),
                            mean(dat_bills$bill_dollars_tariff[dat_bills$PV == 1])),
             linetype = 'longdash', size = 0.8, color = lineColors) +
  geom_label(data = labels0, aes(x = x, y = y, label = label),
             color = lineColors, alpha = 0.9) +
  labs(x = 'Bill under current tariff ($/month)',
       y = 'Household bill density') +
  theme(text = element_text(size = 13))

# plot monthly bills - MC pricing and single monthly fixed charge
labels1 <- data.frame(x = c(mean(dat_bills$bill_dollars_MC_wFixedCharge[dat_bills$PV == 0]) + 35,
                            mean(dat_bills$bill_dollars_MC_wFixedCharge[dat_bills$PV == 1]) - 35),
                      y = c(0.014, 0.002),
                      label = paste0('$',
                                     format(round(c(mean(dat_bills$bill_dollars_MC_wFixedCharge[dat_bills$PV == 0]),
                                                    mean(dat_bills$bill_dollars_MC_wFixedCharge[dat_bills$PV == 1])),
                                                  2),
                                            nsmall = 2)))
p1 <- ggplot() +
  geom_density(data = dat_bills, aes(bill_dollars_MC_wFixedCharge,
                                     fill = `Has PV?`), alpha = 0.3) +
  scale_x_continuous(limits = c(0, 400)) +
  geom_vline(xintercept = c(mean(dat_bills$bill_dollars_MC_wFixedCharge[dat_bills$PV == 0]),
                            mean(dat_bills$bill_dollars_MC_wFixedCharge[dat_bills$PV == 1])),
             linetype = 'longdash', size = 0.8, color = lineColors) +
  geom_label(data = labels1, aes(x = x, y = y, label = label),
             color = lineColors, alpha = 0.9) +
  labs(x = 'Bill under MC pricing ($/month)',
       y = 'Household bill density') +
  theme(text = element_text(size = 13))

# plot monthly bills - MC pricing and income-weighted monthly fixed charge
labels2 <- data.frame(x = c(mean(dat_bills$bill_dollars_MC_wIncomeWtdFixedCharge[dat_bills$PV == 0]) + 35,
                            mean(dat_bills$bill_dollars_MC_wIncomeWtdFixedCharge[dat_bills$PV == 1]) - 35),
                      y = c(0.010, 0.001),
                      label = paste0('$',
                                     format(round(c(mean(dat_bills$bill_dollars_MC_wIncomeWtdFixedCharge[dat_bills$PV == 0]),
                                                    mean(dat_bills$bill_dollars_MC_wIncomeWtdFixedCharge[dat_bills$PV == 1])),
                                                  2),
                                            nsmall = 2)))
p2 <- ggplot() +
  geom_density(data = dat_bills, aes(bill_dollars_MC_wIncomeWtdFixedCharge,
                                     fill = `Has PV?`), alpha = 0.3) +
  scale_x_continuous(limits = c(0, 400)) +
  geom_vline(xintercept = c(mean(dat_bills$bill_dollars_MC_wIncomeWtdFixedCharge[dat_bills$PV == 0]),
                            mean(dat_bills$bill_dollars_MC_wIncomeWtdFixedCharge[dat_bills$PV == 1])),
             linetype = 'longdash', size = 0.8, color = lineColors) +
  geom_label(data = labels2, aes(x = x, y = y, label = label),
             color = lineColors, alpha = 0.9) +
  labs(x = 'Bill under MC pricing and income-weighted fixed charge ($/month)',
       y = 'Household bill density') +
  theme(text = element_text(size = 13))

# combine plots
g <- ggarrange(p0, p1, p2,
               ncol = 1, common.legend = TRUE, legend = 'bottom')
plot(g)
ggsave(g, filename = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Tables and figures/Figures/Residential/02a bill comparison - current tariff - MC pricing - MC pricing income-wtd fixed charge.png',
       dpi = 300, height = 7.5, width = 6)

rm(g, p0, p1, p2, labels0, labels1, labels2)
dat_bills$`Has PV?` <- NULL
