# HECO smart meter data (15 minute interval, February 2020 - )
# Some notes:
#  - some missing data, either NA or no observation reported for an ID
#  - missingness typically occurs in chunks of 96 observations (whole days)
library(data.table)
library(lessR)
library(lubridate)

# Adjust path to appropriate directory.
setwd("D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/HECO/Anonymous_Profiles_csv")
file.list = list.files()[-1]
n = length(file.list)

# Aggregate data to hourly and merge with FERC data
amonth = function(fname) {
  x = fread( fname ); colnames(x)[1] <- "ID"
  x$dt = force_tz(x$INTERVAL_START_DATETIME, "US/Hawaii")
  y = year(x$dt)
  m = month(x$dt)
  d = day(x$dt)
  h = hour(x$dt)
  x$ymdh = ((y*100+m)*100+d)*100+h
  x$hour = hour(x$dt)
  x$day = weekdays(x$dt)
  z = x[,.(KW_NET = mean(KW_NET), KW_DEL=mean(KW_DEL), KW_REC = mean(KW_REC),
           RATE = first(RATE_SUMMARY), PV = first(PV_INTERVAL), 
           CENSUS_TRACT = first(CENSUS_TRACT), dt = first(dt), 
           day = first(day), hour=first(hour)), .(ID, ymdh)]
   z
}

z = amonth(file.list[1])
for ( i in 2:n ) z = rbind(z, amonth(file.list[i]) )

# FERC data: extract aggregate demand and marginal cost (system lambda)

# Adjust path to appropriate directory. The ferc714 folder can be downloaded from
#   https://www.ferc.gov/sites/default/files/2021-06/Form-714-csv-files-June-2021.zip
ferc <- "D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Raw/ferc714/"
load <- fread(paste0(ferc,"Part 3 Schedule 2 - Planning Area Hourly Demand.csv"))
heco <- load[respondent_id==178]
rm(load)
mc <- fread(paste0(ferc,"Part 2 Schedule 6 - Balancing Authority Hourly System Lambda.csv"))
mcHeco <- mc[mc$respondent_id==178,]
rm(mc)

# Reshape data

heco2 = melt(heco[,1:31], measure.vars = to("hour",24), value.name="load")
heco2$hour = as.numeric(substr(heco2$variable, 5,6)) 
heco2$date_time = as.POSIXct(heco2$plan_date, format="%m/%d/%Y", tz="HST")
heco2$date_time = heco2$date_time + hours(heco2$hour) 
heco = heco2[,c("date_time","load")]

mcHeco2 = melt(mcHeco[,1:31], measure.vars = to("hour",24), value.name="mc")
mcHeco2$hour = as.numeric(substr(mcHeco2$variable, 5,6)) 
mcHeco2$date_time = as.POSIXct(mcHeco2$lambda_date, format="%m/%d/%Y", tz="HST")
mcHeco2$date_time = mcHeco2$date_time + hours(mcHeco2$hour) 
mcHeco = mcHeco2[,c("date_time","mc")]

heco = merge(heco, mcHeco, by="date_time")

# correct an apparent inconsistency in FERC data pre/post 1/1/2013
heco$date_time[years(heco$date_time<=2012)] <- heco$date_time[years(heco$date_time<=2012)] + hours(1)

# assign missing values to zeros
heco$load[heco$load==0] <- NA
heco$mc[heco$mc==0] <- NA

# FERC 714 data appears to report instantaneous values for system lambda (mc) and demand at each hour. 
#   -The FERC instructions are clearer with regard to system lambda than demand
#   -Assuming both value reported in the same manner, it is best to interpolate between hours
#     to obtain a match for average values over the duration of each hour. 

n   <- nrow(heco)
l0  <- heco$load[1:(n-1)]
l1  <- heco$load[2:n]
mc0 <- heco$mc[1:(n-1)]
mc1 <- heco$mc[2:n]
heco$load = c( (l0+l1)/2, l1[(n-1)] )
heco$mc = c( (mc0+mc1)/2, mc1[(n-1)] )

#saveRDS(z, file = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/00_smartMeterData_noHeco.rds')
#z = merge(heco, z, by.x="date_time", by.y="dt")
z = merge(z, heco, by.x = 'dt', by.y = 'date_time')

#  Find total cost and value using MC
z$KW_REC[is.na(z$KW_REC)] <- 0
z[, costDEL := mc*KW_DEL/1000,]
z[, valREC  := mc*KW_REC/1000,]
z[, netValue := valREC + costDEL]
net.value.by.customer = tapply(z$netValue, z$ID, sum, na.rm=TRUE)

pv.id <- unique(z$ID[z$PV==1])
R.id <- unique(z$ID[z$RATE=="R"])
G.id <- unique(z$ID[z$RATE=="G"])
J.id <- unique(z$ID[z$RATE=="J"])
nv.id <- as.numeric(names(net.value.by.customer)) 

net.value.by.customer.pv = net.value.by.customer[nv.id %in% pv.id]
net.value.by.customer.nonpv = net.value.by.customer[ nv.id %in% R.id & !(nv.id %in% pv.id)]

# Compare value delivered vs value received for PV customers
z.pv = z[z$ID %in% pv.id]
sum(z.pv$valREC)/sum(z.pv$KW_REC)
sum(z.pv$costDEL, na.rm=T)/sum(z.pv$KW_DEL, na.rm=T)


# plot
del.by.hour = tapply(z.pv$KW_DEL, z.pv$hour, mean, na.rm=T)
rec.by.hour = tapply(z.pv$KW_REC, z.pv$hour, mean, na.rm=T)
mc.by.hour = tapply(z.pv$mc, z.pv$hour, mean, na.rm=T)

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot( 1:24, del.by.hour, type="l", lwd=2, bty="u",
      xlab="Hour of day", ylab="kWh", col="red",
      ylim = range( c(del.by.hour, -rec.by.hour)),
      )
lines( 1:24, -rec.by.hour, lwd=2, col="darkgreen")
mtext("Average system marginal cost in relation to energy", side=3, line=2)
mtext("delivered and received to and from PV customers", side=3, line=1)
text(9,2, "kWh Received", col="darkgreen")
text(13,0.55, "kWh Delivered", col="red")
par(new = TRUE)
plot( 1:24, mc.by.hour, type="l", lwd=2, col="blue", 
       axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(mc.by.hour)), col="blue")
mtext("$/MWh", side=4, line=2, col="blue")
text(4,93, "Marginal Cost", col="blue")


# Any PV customers in G or J classes? None reported here.
# Consider time of use relative to MC

# install.packages("tidycensus")
library(tidyverse)
library(tidycensus)
library(viridis)
census_api_key( key = "5d0dbe99ab82cd592b819a8c8b2776453af7c3e6", install=TRUE, overwrite = TRUE)

hiinc <- get_acs(geography = "tract", variables = "B19013_001",
                state = "HI", county = "Honolulu", geometry = TRUE)
hiinc <- hiinc[-135,] # remove strange geometry far from the island
ggplot(hiinc, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")

ct1 = sub(".*Census Tract ", "", hiinc$NAME)
ct2 = sub(", Honolulu.*", "", ct1)
ct = as.numeric(ct2)
hiinc$census_tract = ct


# save data
save.image(file = 'D:/OneDrive - hawaii.edu/Documents/Projects/HECO/Data/Output/00_smartMeterData.R')
