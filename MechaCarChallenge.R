# Deliverable 1

# Libraries
library(dplyr)
library(ggplot2)

# import csv MechaCar_mpg.csv
MechaCars <- read.csv("Resources/MechaCar_mpg.csv", check.names = F,stringsAsFactors = F)
colnames(MechaCars)

# Linear regression on each variables
# p-value and r squared
# Summary
vehicle_l <- summary(lm(mpg ~ vehicle_length,MechaCars))
vehicle_w <- summary(lm(mpg ~ vehicle_weight,MechaCars))
spoiler_a <- summary(lm(mpg ~ spoiler_angle,MechaCars))
ground_c <- summary(lm(mpg ~ ground_clearance,MechaCars))
AWD <- summary(lm(mpg ~ AWD,MechaCars))

# Ordered information
mpg_vl_rp <- c(vehicle_l$r.squared, vehicle_l$coefficients[,4])
mpg_vw_rp <- c(vehicle_w$r.squared, vehicle_w$coefficients[,4])
mpg_va_rp <- c(spoiler_a$r.squared, spoiler_a$coefficients[,4])
mpg_vc_rp <- c(ground_c$r.squared, ground_c$coefficients[,4])
mpg_AWD_rp <- c(AWD$r.squared, AWD$coefficients[,4])

#DataFrame for r-square and p-value 
MechaCars_rp<- rbind(mpg_vl_rp, mpg_vw_rp, mpg_va_rp, mpg_vc_rp, mpg_AWD_rp)

# Dataframe changed names
colnames(MechaCars_rp)[1] <- 'R-squared'
colnames(MechaCars_rp)[3] <- 'P-Value'
row.names(MechaCars_rp) <- colnames(MechaCars[-6])

# Deliverable 2
SuspCoil <- read.csv("Resources/Suspension_Coil.csv", check.names = F,stringsAsFactors = F)
colnames(SuspCoil)

# Total summary
Total_summary <- SuspCoil %>% summarize(Mean=mean(PSI),
                                        Median=median(PSI),
                                        Variance=var(PSI),
                                        SD_Engine=sd(PSI), .groups = 'keep')

# Lot summary
lot_summary <- SuspCoil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),
                                                                      Median=median(PSI),
                                                                      Variance=var(PSI),
                                                                      SD_Engine=sd(PSI), .groups = 'keep')
Total_summary
lot_summary

View(Total_summary)
View(lot_summary)

# Deliverable 3
# t test
t.test(SuspCoil$PSI,mu=1500)

# PSI
t.test(subset(SuspCoil, Manufacturing_Lot=="Lot1")[,3],mu=1500)
t.test(subset(SuspCoil, Manufacturing_Lot=="Lot2")[,3],mu=1500)
t.test(subset(SuspCoil, Manufacturing_Lot=="Lot3")[,3],mu=1500)
