# Import library
library(dplyr)

# Deliverable 1
# Comparing mpg with other vehicles stats

# Read csv file
MechaCar_mpg <- read.csv('Resources/MechaCar_mpg.csv')

# Multi-linear regression model, using lm()
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +
     ground_clearance + AWD, data=MechaCar_mpg)

# Summary function to determine the p-value and r-squared
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +
     ground_clearance + AWD, data=MechaCar_mpg))

# Deliverable 2
# Summary tables

# Import and read csv file as a table
SuspensionCoil <- read.csv('Resources/Suspension_Coil.csv')

# Script that creates a total summary dataframe of psi column
total_summary <- SuspensionCoil %>% summarize(Mean = mean(PSI),
                           Median = median(PSI),
                           Variance = var(PSI),
                           SD = sd(PSI))


# Script that creates a lot summary grouping by manufacturing lot
lot_summary <- SuspensionCoil %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean = mean(PSI),
            Median = median(PSI),
            Variance = var(PSI),
            SD = sd(PSI), .groups= 'keep')

# Deliverable 3
# T-test on suspension coils to determine lot difference from mean of 1,500 psi
t.test(SuspensionCoil$PSI, mu=1500)


# 3 more scripts using t.test and its subset argument to determine
# PSI for each manufacturing lot is statistically different from the population
# mean of 1,500 psi
t.test(subset(SuspensionCoil, Manufacturing_Lot == "Lot1")$PSI, mu = 1500)

t.test(subset(SuspensionCoil, Manufacturing_Lot == "Lot2")$PSI, mu = 1500)

t.test(subset(SuspensionCoil, Manufacturing_Lot == "Lot3")$PSI, mu = 1500)

# Deliverable 4

# Study design: MechaCar vs Competition
