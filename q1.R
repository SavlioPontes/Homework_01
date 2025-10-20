emissions <- scan(); # Allows you to read all the grouped data
15.8 22.7 26.8 19.1 18.5 14.4 8.3 25.9 26.4 9.8 21.9 10.5
17.3 6.2 18.0 22.9 24.6 19.4 12.3 15.9 20.1 17.0 22.3 27.5
23.9 17.5 11.0 20.4 16.2 20.8 20.9 21.4 18.0 24.3 11.8 17.9
18.7 12.8 15.5 19.2 13.9 28.6 19.4 21.6 13.5 24.6 20.0 24.1
9.0 17.6 25.7 20.1 13.2 23.7 10.7 19.0 14.5 18.1 31.8 28.5
22.7 15.2 23.0 29.6 11.2 14.7 20.5 26.6 13.3 18.1 24.8 26.1
7.7 22.5 19.3 19.4 16.7 16.9 23.5 18.4

# --- ITEM 1 --- #

# Central tendency measures:
mean_emissions <- mean(emissions); # Calculates the mean of emissions
median_emissions <- median(emissions); # Calculates the median of emissions
# Function for mode:
get_mode <- function(x){
  ux <- unique(x); # Vector containing each unique element of the original vector
  count <- tabulate(match(x,ux)); # Counts how many times each element occurs
  if(all(count == 1)){
    return(NA); # If no elements are repeated, there is no mode
  }else{
    return(ux[which.max(count)]); # If there are repetitions, get the most frequent element
  }
}
mode_emissions <- get_mode(emissions); # Calculates the mode of emissions

# Dispersion measures:
amplitude_emissions <- diff(range(emissions));
# Calculates the range of emissions (difference between the largest and the smallest values)

variance_emissions <- var(emissions); # Calculates the variance of emissions
stDev_emissions <- sqrt(variance_emissions); # Calculates the standard deviation of emissions
coefVar_emissions <- (stDev_emissions/mean_emissions) * 100;
# Calculates the coefficient of variation of emissions

# --- ITEM 2 --- #

par(mfrow = c(1,2)); # Divides the plot area into two columns

# Creates our histogram
hist(emissions,
     freq = FALSE,
     xlab = "Emissions Values",
     ylab = "Density",
     main = "Emissions Histogram", # Title adjusted
     col = "skyblue",
     border = "black")


# Creates our box plot
boxplot(emissions,
        main = "Emissions Boxplot", # Title adjusted
        col = "skyblue",
        border = "black")

par(mfrow = c(1, 1)); # Returns the plot area configuration to normal

# --- ITEM 3 --- #

q1_emissions = quantile(emissions, 0.25); # Calculates Quartile 1
q2_emissions = quantile(emissions, 0.5); # Calculates Quartile 2
q3_emissions = quantile(emissions, 0.75); # Calculates Quartile 3
IQR_emissions = IQR(emissions); # Calculates the IQR of emissions

# --- ITEM 4 --- #
days_exceeded <- sum(emissions > 25); # Count of days exceeding the limit
total_days <- length(emissions); # Total number of emissions records
exceed_proportion <- days_exceeded/total_days; # Proportion of exceeded days