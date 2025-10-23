# File read
setwd("C:\\Users\\savli\\Documents\\GitHub\\Homework_01");
dataBikes <- read.csv("HW1_bike_sharing.csv", header = TRUE);

# ---- Item 1 ----#
#função para obter o número de linhas (=número de observações)
observationNumber <- nrow(dataBikes);
#função para obter o 1º elemento do vetor com os dias das observações
startDate <- dataBikes$dteday[1];
#função para obter o último elemento do vetor com os dias das observações
endDate <- dataBikes$dteday[observationNumber];

#Display das informações obtidas
cat("Número de observações:")
print(observationNumber)

cat("Data de início da amostra:")
print(startDate)

cat("Data de fim da amostra:") 
print(endDate)

# ---- Item 2 ----#

# Measures of Central Tendency
tempMean <- mean(dataBikes$temp);
tempMedian <- median(dataBikes$temp);
tempQuantile <- quantile(dataBikes$temp,
                         probs = c(0.25, 0.5, 0.75))

casualMean <- mean(dataBikes$casual);
casualMedian <- median(dataBikes$casual);
casualQuantile <- quantile(dataBikes$casual,
                           probs = c(0.25, 0.5, 0.75));

registeredMean <- mean(dataBikes$registered);
registeredMedian <- median(dataBikes$registered);
registeredQuantile <- quantile(dataBikes$registered,
                               probs = c(0.25, 0.5, 0.75));

# Table with the measures
centralTendTable <- data.frame(
  variable = c("temp", "casual", "registered"),
  Mean = c(tempMean, casualMean, registeredMean),
  Median = c(tempMedian, casualMedian, registeredMedian),
  Q1 = c(tempQuantile[1], casualQuantile[1], registeredQuantile[1]),
  Q3 = c(tempQuantile[3], casualQuantile[3], registeredQuantile[3])
)
cat("Statistical Summary of Numeric Variables\n") 
print(centralTendTable)

# ---- Item 3 ----#
# Replace the numbers with their names of representation
dataBikes$season <- factor(dataBikes$season,
                           levels = c(1, 2, 3, 4),
                           labels = c("Spring", "Summer",
                                      "Autumn", "Winter")); 
dataBikes$weathersit <- factor(dataBikes$weathersit,
                               levels = c(1, 2, 3, 4),
                               labels = c("Clear skies", "Cloudy",
                                          "Light rain", "Heavy rain"));

freqSeason <- table(dataBikes$season); # Shows each frequency of seasons
maxSeason <- names(freqSeason)[which.max(freqSeason)];
# Returns the most frequent season
freqWeathersit <- table(dataBikes$weathersit);
# Shows each frequency of weather situation 
maxWeathersit <- names(freqWeathersit)[which.max(freqWeathersit)];
# Returns the most frequent weather situation

par(mfrow = c(1,2))
barplot(freqSeason,
        col = "skyblue",
        main = "Number of Records by Season", 
        ylab = "Count",                       
        xlab = "Season",                     
        ylim = c(0, 240)
)

barplot(freqWeathersit,
        col = "lightgreen",
        main = "Number of Records by Weather",
        ylab = "Count",                        
        xlab = "Weather",                       
        ylim = c(0,550)
);
par(mfrow = c(1,1))

# ---- Item 4 ----#
dataBikes$totalUsers <- dataBikes$casual + dataBikes$registered;
# Calculates the number of total users per day
dataBikes$tempReal <- dataBikes$temp * 41;
# Converts the normalized temperature to its real value

par(mfrow = c(2,1))

plot(dataBikes$X, dataBikes$totalUsers, type = "l",
     col = "brown", lwd = 2,
     main = "Total User Count Over Time", 
     xlab = "Days (Index)", ylab = "Number of Users" 
);

plot(dataBikes$X, dataBikes$temp, type = "l",
     col = "red", lwd = 2,
     main = "Temperature Trend Over Time", 
     xlab = "Days (Index)", ylab = "Temperature"
);