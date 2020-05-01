
# Libraries
library(tidyverse)
library(ggbiplot)

LifeExpectancy <- read.csv("Data/WHO Life Expectancy Data.csv")
LifeExpectancy <- LifeExpectancy %>%
  select(-c("Country","Year","Status","PercentageExpenditure"))

# Normalise the Data  
normalise <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}
normalised.le <- as.data.frame(lapply(LifeExpectancy, normalise)) # I'll confirm if this normalisation also works -> le.normalised <- scale(LifeExpectancy)
sum(is.na(normalised.le))

# Create a PCA Model to Observe Diagnostic Results
normalised.le.2 <- data.frame(t(na.omit(t(normalised.le))))
le.pca <- prcomp(normalised.le.2, scale. = FALSE, center = TRUE)

# Diagnostic Results
summary(le.pca)



