# Load packages
require(ggplot2)
require(corrplot)
require(tidyverse)
require(caret)

# Read in data
setwd("E:/AirfoilSelfNoise/scripts")
airfoil_self_noise <- read.delim("E:/AirfoilSelfNoise/data/airfoil_self_noise.dat", sep = "", 
                                header = FALSE)
# Rename columns
names(airfoil_self_noise) <- c("FREQ", "AOA", "CL", "VEL", "SSDT", "SSPL")

# Exploratory Data Analysis
class(airfoil_self_noise)

summary(airfoil_self_noise)

str(airfoil_self_noise)

# Data Preprocessing
airfoil_self_noise$FREQ <- as.numeric(as.integer(airfoil_self_noise$FREQ))

head(airfoil_self_noise)

dim(airfoil_self_noise)
map(airfoil_self_noise, class)
map(airfoil_self_noise, ~sum(is.na(.)))

airfoil_self_noise %>% ggplot(aes(FREQ, SSPL)) +
  geom_point(color= "blue", alpha = 0.3) +
  ggtitle("Frequency vs Scaled Sound Pressure Level") +
  xlab("Frequency") +
  ylab("Scaled Sound Pressure Level") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

airfoil_self_noise %>% ggplot(aes(AOA, SSPL)) +
  geom_point(color= "darkgreen", alpha = 0.3) +
  ggtitle(" Angle of Attack vs Scaled Sound Pressure Level") +
  xlab("Angle of Attack") +
  ylab("Scaled Sound Pressure Level") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

airfoil_self_noise %>% ggplot(aes(CL, SSPL)) +
  geom_point(color= "red", alpha = 0.3) +
  ggtitle("Chord Length vs Scaled Sound Pressure Level") +
  xlab("Chord Length") +
  ylab("Scaled Sound Pressure Level") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

airfoil_self_noise %>% ggplot(aes(VEL, SSPL)) +
  geom_point(color= "magenta", alpha = 0.3) +
  ggtitle("Free-Stream Velocity vs Scaled Sound Pressure Level") +
  xlab("Free-Stream Velocity") +
  ylab("Scaled Sound Pressure Level") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

airfoil_self_noise %>% ggplot(aes(SSDT, SSPL)) +
  geom_point(color= "black", alpha = 0.3) +
  ggtitle("Suction Side Displacement Thickness vs Scaled Sound Pressure Level") +
  xlab("Suction Side Displacement Thickness") +
  ylab("Scaled Sound Pressure Level") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

correlations = cor(airfoil_self_noise)
corrplot(correlations, method = "color")
set.seed(100)
inTrain <- createDataPartition(y = airfoil_self_noise$SSPL, 
                               p = 0.8, list = FALSE)
# subset epi data to training
training <- airfoil_self_noise[inTrain,]
# subset the rest to test
testing <- airfoil_self_noise[-inTrain,]

# Size ratio of training and test dataset
message("As shown below, the training set is about 80%  and the test set is about 20% of the original data")

rbind("Training set" = nrow(training)/nrow(airfoil_self_noise),
      "Testing set" = nrow(testing)/nrow(airfoil_self_noise)) %>% 
  round(2)*100

my_lm = train(training[,1:4], training[,5],
              method = "lm",
              preProc = c("center", "scale")
)
message("Linear Regression: Model performance on \n the training set")
my_lm$results[c("RMSE","Rsquared")] %>%
  round(2)
summary(my_lm)

pred = predict(my_lm, testing[, 1:4])
SSE = sum((testing[,5] -pred)^2)    # sum of squared errors
SST = sum((testing[,5] - mean(training[,5]))^2) # total sum of squares, remember to use training data here
R_square = 1 - SSE/SST
message('R_squared on the test data:')
round(R_square, 2)
SSE = sum((testing[,5] - pred)^2)
RMSE = sqrt(SSE/length(pred))
message("Root mean square error on the test data: ")
round(RMSE, 2)

my_data = as.data.frame(cbind(predicted = pred,
                              observed = testing$SSPL))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') +
  ggtitle("Linear Regression: Prediction vs Test Data") +
  xlab("Predicited Scaled Sound Pressure Level") +
  ylab("Observed Scaled Sound Pressure Level") +
  theme(plot.title = element_text(color="darkgreen",size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))
