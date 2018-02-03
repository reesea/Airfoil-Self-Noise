# Load packages
require(ggplot2)
require(corrplot)
require(tidyverse)
require(caret)

# Read in data
setwd("E:/AirfoilSelfNoise")
airfoil_self_noise <- read.delim("data/airfoil_self_noise.dat", sep = "", 
                                 header = FALSE)
# Exploratory Data Analysis
class(airfoil_self_noise)
