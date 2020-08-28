# Libraries
library(dplyr)
library(EnvStats) # For epdfPlot
library(ggplot2)

# Main assumption is that there are no tv's sold with identical criteria. These were assumed to be
# duplicates and removed so as to retain the independence assumption necessary for modelling.

setwd("C:/Users/Charlie/Documents/MyProjects/scrapping")

# ridge/asso regression works best in situations when the least squares has a high variances (potentially this case)
# probably should include an interaction term between brand and condition

data.old <- read.csv("gumtreeTV.csv", stringsAsFactors = FALSE)   # database of all previous sites
data.old <- data.old[, -1] # removes the X columns


data.new <- read.csv("newGumtreeTV.csv", stringsAsFactors = FALSE)# new sites from python
data.new <- data.new[, -1] # removes x column


data.new$Price <- as.numeric(gsub('[$,]', '', data.new$Price)) # changes price from a string to numeric
data.old$Price <- as.numeric(gsub('[$,]', '', data.old$Price)) # changes price from a string to numeric

data <- full_join(data.new, data.old) # Combines new data into old dataset


data[data==""]<-NA # sets all blanks to NA
data <- data[!duplicated(data[,-1]),]  # removes any duplicates rows (except for URL since there are multiple entries with diff URL's)

write.csv(data, file = "gumtreeTV.csv") # Writes combined data to a new csv

# TV's with complete data
data.complete <- data[complete.cases(data[, -11]), ]

# Exploratory Analysis

# New
epdfPlot(data[which(data$Condition == "New"),"Price"], discrete=FALSE)
# Used
epdfPlot(data[which(data$Condition == "Used"),"Price"], discrete=FALSE)


# Plots the relationship between different variables
# Wouldnt work in a for loop for some reason??
#
# Nothing particularly interesting was shown
setwd("C:/Users/Charlie/Documents/MyProjects/scrapping/Scatters")
data.var <- colnames(data)
i = 1
while (i < length(data)) {
  title <- paste0("Plot of ", data.var[i], " vs Price")
  print(title)
  p1 <- ggplot(data = data, aes(y = Price, x = get(data.var[i]), color = factor(Condition))) +
    geom_point() +
    guides(col = guide_legend("Condition"))
  ggsave(paste0("Plot of ", data.var[i], " vs Price.png"), plot = p1, device = "png")
  while (!is.null(dev.list()))  dev.off()
  i = i + 1
}


# perform a linear regression on the tv's with complete data


dataLM <- lm(Price ~ Condition + Brand + Display.Technology + Screen.Size + Max..Resolution, data = data.complete)
# Below is attempt at doing dynamically (didnt work)
# dataLM <- lm(Price ~ sum(get(data.var[6:10])), data = data)

summary(dataLM)

# Then get the average price of each product and put into a dataframe
# If can, construct 95% CI intervals for each product and highlight the ones that are outside this interval
predict(dataLM, data = data.complete)
dataLM.predict <- predict(dataLM, data = data.complete, interval = 'prediction')

data.complete$`Point Estimate` <- dataLM.predict[,1]
data.complete$`Lwr Estimate` <- dataLM.predict[,2]
data.complete$`Upr Estimate` <- dataLM.predict[,3]

data.complete$`Chpr than avg` <- 0
data.complete$`Chpr than lwr estimate` <- 0

# This loops checks to see if cheaper than average
for (i in 1:length(data.complete$Price)) {
  if(data.complete$Price[i] < data.complete$`Point Estimate`[i]) {
    data.complete$`Chpr than avg`[i] <- 1
  }
}

# checks to see if cheaper than lower CI
for(i in 1:length(data.complete$Price)) {
  if(data.complete$Price[i] < data.complete$`Lwr Estimate`[i]) {
    data.complete$`Chpr than lwr estimate`[i] = 1
  }
}

# Calculates proportion of the average
data.complete <- data.complete %>%
  mutate("% of Avg" = Price/`Point Estimate`)

