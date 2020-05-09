# Ref: https://www.kaggle.com/pavansanagapati/a-simple-tutorial-on-exploratory-data-analysis

# theme_excel_new
# theme_minimal()
# EDA of Life Expenctancy Data
library(easypackages)
libraries("tidyverse", "skimr","psych", "inspectdf", "dlookr", "naniar","visdat", "purrr", "ggthemes","reshape2","GGally")
le.raw <- read.csv("Data/WHO Life Expectancy Data.csv")

# Light Theme
theme_set(theme_light())

# Summary Stats using dlokkr
dim(le.raw)
glimpse(le.raw)
le.raw$Year <- as.factor(le.raw$Year)
summary(le.raw)
describe(le.raw)
# Numeric Only
le.raw.numeric <- le.raw %>%
  select(-Country, -Year, -Status)

# dlookr for data diagnosis
diagnose(le.raw)

le.raw %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

diagnose_numeric(le.raw)

# Diagnose Oultiers
le.raw %>%
  diagnose_outlier() %>%
  arrange(desc(outliers_cnt))

# Box and Histogram plots with & without Outliers
le.raw.numeric %>%
  plot_outlier(
    diagnose_outlier(le.raw.numeric) %>%
      filter(outliers_ratio > 9) %>%
      select(variables) %>%
      unlist())

# Using inspectDF
inspect_types(le.raw)
inspect_na(le.raw)
inspect_num(le.raw)

# Missingness uing Naniar
gg_miss_upset(le.raw)
gg_miss_var(le.raw, facet = Year) + coord_flip()
vis_miss(le.raw)
le.raw %>%
  group_by(Year) %>%
  miss_var_summary()

# EDA
# Univariate Analysis

# Quick plot Histogram of the target Variable - Life Expectancy
qplot(le.raw$LifeExpectancy, geom = "histogram", binwidth=2,
      main = "Histogram of Life Expectancy", xlab = "Life Expectancy",
      fill=I("#006400"), alpha=I(.6), xlim = c(35,90))

# Histogram of Life Expectancy across using ggplot
ggplot(data = le.raw, aes(LifeExpectancy)) + geom_histogram(breaks=seq(35, 90, by=2),
                                                            fill="#006400", alpha=.6) +
  labs(title = "Histogram of Life Expectancy Across years", x="Age", y="count") + geom_vline(aes(xintercept=mean(LifeExpectancy, na.rm = T)),
                                                                                color="red", linetype="dashed", size=1) + facet_wrap(~Year)

# Histogram of all Numeric Variables in the DF
le.raw %>%
  # select(mpg:drat) %>% 
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~key, scales = "free") + geom_histogram() +
  scale_fill_brewer(palette = "Set1")

le.raw$Year <- as.factor(le.raw$Year)

# Boxplot of the target Variable - Life Expectancy
ggplot(le.raw, aes(Year, LifeExpectancy)) + 
  geom_boxplot() + geom_jitter(width = 0.2)  + theme_fivethirtyeight()

ggplot(le.raw, aes(Year, LifeExpectancy)) +
  geom_boxplot(aes(colour=Status)) + theme_hc()

# Bar plot showing the count of both Developed and Developing countries
ggplot(le.raw, aes(Status, LifeExpectancy)) +
  geom_bar(stat = "identity") + theme_hc()

ggplot(le.raw, aes(Year, LifeExpectancy, fill=Status)) +
  geom_bar(stat = "identity") + theme_hc()

# Correlation
numeric.le.raw <-  le.raw %>%
  select(-Country, -Year, -Status)
  # drop_na()
ggcorr(numeric.le.raw, label = TRUE, label_round = 2, label_alpha = TRUE)

# Scatter Plots - Affirmation of Correlation

## Adult Mortality vs Life Expectancy
ggplot(le.raw, aes(AdultMortality, LifeExpectancy, colour=Status, size=Population)) +
  geom_jitter(alpha=0.4) +
  labs(title = "Relationship Between Adult Mortality and Life Expectancy") + xlab("Adult Mortlality") + ylab("Life Expectancy")

## Number of Years in Schooling vs Life Expectancy
ggplot(le.raw, aes(Schooling, LifeExpectancy, colour=Status)) + 
  geom_point() + 
  # geom_density2d() +
  geom_smooth(method = "auto", se=TRUE, fullrange=FALSE, level=0.95, linetype="dashed") +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Relationship Between Number of Schooling (Years) and Life Expectancy") + xlab("Schooling (Years)") + ylab("Life Expectancy")

## Income Composition  of Resources vs Life Expectancy
ggplot(le.raw, aes(IncomeComposition_of_Resources,LifeExpectancy, color=Status)) +
  geom_point()

## Thinnes and BMI
ggplot(le.raw, aes(thinness5.9Years, BMI)) +
  geom_point(colour="#1E90FF", alpha=0.7)


