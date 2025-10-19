  #File read
  setwd("C:\\Users\\savli\\Documents\\GitHub\\Homework_01");
  dataBikes <- read.csv("HW1_bike_sharing.csv",header = TRUE);
  
  #---- Item 1 ----#
  observationNumber <- nrow(dataBikes);
  startDate <- dataBikes$dteday[1];
  endDate <- dataBikes$dteday[observationNumber];
  
  #---- Item 2 ----#
  
  # measures of central tendency
  tempMean <- mean(dataBikes$temp);
  tempMedian <- median(dataBikes$temp);
  tempQuantile <- quantile(dataBikes$temp, probs = c(0.25, 0.5, 0.75))
  
  casualMean <- mean(dataBikes$casual);
  casualMedian <- median(dataBikes$casual);
  casualQuantile <- quantile(dataBikes$casual, probs = c(0.25, 0.5, 0.75));
  
  registeredMean <- mean(dataBikes$registered);
  registeredMedian <- median(dataBikes$registered);
  registeredQuantile <- quantile(dataBikes$registered,probs = c(0.25, 0.5, 0.75));
  
  centralTendTable <- data.frame(
    variable = c("temp", "casual", "registered"),
    Mean = c(tempMean, casualMean, registeredMean),
    Median = c(tempMedian, casualMedian, registeredMedian),
    Q1 = c(tempQuantile[1], casualQuantile[1], registeredQuantile[1]),
    Q3 = c(tempQuantile[3], casualQuantile[3], registeredQuantile[3])
  )
  cat("Estatistical Resume of Numeric Variables\n")
  print(centralTendTable)
  
  #---- Item 3 ----#
  
  dataBikes$season <- factor(dataBikes$season,
                             levels = c(1, 2, 3, 4),
                             labels = c("Spring", "Summer", "autumn", "Winter"));
  dataBikes$weathersit <- factor(dataBikes$weathersit,
                             levels = c(1, 2, 3, 4),
                             labels = c("Clear skies", "Cloudy", "Light rain", "Heavy rain"));
  
  freqSeason <- table(dataBikes$season);
  maxSeason <- names(freqSeason)[which.max(freqSeason)];
  freqWeathersit <- table(dataBikes$weathersit);
  maxWeathersit <- names(freqWeathersit)[which.max(freqWeathersit)];
  par(mfrow = c(1,2))  
  par(mfrow = c(1,1))  
  
  barplot(freqSeason,
          col = "skyblue",
          main = "Número de registros por estação",
          ylab = "Contagem",
          xlab = "Estação",
          ylim = c(0, 240)
  )
  
  barplot(freqWeathersit,
          col = "lightgreen",
          main = "Número de registros por Clima",
          ylab = "Contagem",
          xlab = "Clima",
          ylim = c(0,550)
  )
  
  #---- Item 4 ----#
  
   
  
  
  
