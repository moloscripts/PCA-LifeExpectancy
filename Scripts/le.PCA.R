# Libraries
library(tidyverse)
library(factoextra)
library(ggbiplot)
library(ggfortify)
library(RColorBrewer)
library(plotly)

# Data
LifeExpectancy <- LifeExpectancy %>%
  filter(Status == "Developing")%>%
  select(-c("Country","Year","Status", "LifeExpectancy")) %>%
  na.omit()
sum(is.na(LifeExpectancy))

# PCA Model
le.norm <- scale(LifeExpectancy)
le.pca <- prcomp(le.norm, scale. = FALSE, center = TRUE)
le.pca$x
summary(le.pca)
print(le.pca)
get_eig(le.pca)

# Various Biplots
fviz_pca_biplot(le.pca)
fviz_pca(le.pca)

fviz_pca_var(le.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

### Using ggfortify
autoplot(le.pca, loadings = TRUE, 
         loadings.labels = TRUE, data = LiffeExpectancy, colour='LifeExpectancy', labels=TRUE) + theme_light() + 
  labs(title = "Biplot with response column values as a legend")

autoplot(pca, loadings = TRUE, loadings.label = TRUE,
         data = iris, colour = 'Species') + scale_color_brewer(palette = "YIOrRd")

# Scree plot
fviz_screeplot(le.pca, addlabels=TRUE,ylim = c(0, 50))

