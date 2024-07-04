
library(tidyverse)
library(skimr)
library(GGally)
library(patchwork)
library(moderndive)

#A. Read the data
college <- read.csv("/Users/admin/Downloads/College.csv")

#B. Use the View() function
View(college)

#C.i. A numerical summary of quantitative attributes
p = summary(college)
print(p)

#C.ii. Scatterplot Matrix of the first ten columns of the quantitative data.
college$Private <- as.factor(college$Private)
pairs(college[,2:11])


Scatter_matrix <- ggpairs(college, columns = c(2:11),
                          title = "Scatterplot Matrix for College Dataset")
Scatter_matrix

#C.iii. Boxplots of Outstate versus Private
ggplot(college, aes(x= Private, y = Outstate)) +
  geom_boxplot() +
  labs(x = "Private Indicator", y = "Out of State Tuition",
       title = "Out of state tuition by private indicator") 

#C.iv.Creating a new qualitative variable, Elite, by binning the Top10perc attribute.

college$Elite <- cut(college$Top10perc, breaks=c(0, 50, 100),
                     include.lowest = T,
                     right = F)

#C.v. Using the summary() function to identify the number of elite universities.
summary(college$Elite)

#Producing boxplots of Outstate versus Elite
ggplot(data= college, mapping= aes(x= Elite, y = Outstate)) +
  geom_boxplot() +
  labs(x = "Elite Schools", y ="Out of State Tuition",
       title = "Out of state tuition by elite school status")

#C.vi.Various histograms for quantitative variables

p1 <- ggplot(data= college, mapping= aes(x= Enroll))+
  geom_histogram(binwidth = 300, color = "black") 

p2 <- ggplot(data = college, mapping=aes(x= Books)) +
  geom_histogram(binwidth = 100, fill= "blue", color= "black")

p3 <- ggplot(data= college, mapping=aes(x= Room.Board)) + 
  geom_histogram(binwidth = 500, fill= "yellow", color= "grey")

p4 <-ggplot(data= college, mapping=aes(x= PhD)) +
  geom_histogram(binwidth = 10, fill= "white", color= "grey")

p1 + p2 + p3 + p4

#C.vii. Exploratory Data Analysis

#Reviewing the raw data
glimpse(college)

#Display a random sample of 5 rows
college %>% sample_n(size = 5)



#Computing summary statistics
#The summary statistics show that there are 0 missing values. PhD is skewed left with values peaking on the right side, while S.F. Ratio and Top10per are both skewed right meaning values peak on the left side.
#The statistics demonstrate that the mean percentage of faculty having a PhD is 72.7%, with a moderate spread of 16.4 (the standard deviation).
#They show that the Student/Faculty ratio has a mean of 14.1 students per faculty member, with a standard deviation of 3.98.
#The data also shows that the mean percentage of new students from the top 10% of high school class is 27.6 with a larger spread standard deviation of 17.6.
college %>%
  select(PhD,S.F.Ratio, Top10perc) %>%
  skim()

#Correlation Matrix
#There is a degree of collinearity between Expend and PhD explanatory variables as seen by the 0.4313352 correlation coefficient.
college %>%
  select(Top10perc, PhD, Expend) %>%
  cor()

#Correlation Coefficient
college %>%
  get_correlation(formula = PhD ~ Top10perc)

#There is a positive correlation between percentage of faculty with Ph.D's and new students from the top 10%.
p1 <- ggplot(data= college, mapping=aes(x= PhD, y = Top10perc)) +
  geom_point() +
  labs(x= "Percentage of faculty with Ph.D.'s", y = "New students from the top 10%",
       title = "Relationship between top 10% students and percentage of faculty with Ph.D.'s") +
  geom_smooth(method = "lm", se = FALSE)

#Correlation Coefficient
college %>%
  get_correlation(formula = S.F.Ratio ~ Top10perc)

#There is a slightly negative correlation between student/faculty ratio and new students from the top 10%.
p2 <- ggplot(data= college, mapping=aes(x= S.F.Ratio, y = Top10perc)) +
  geom_point() +
  labs(x= "Student/Faculty Ratio", y = "New students from the top 10%",
       title = "Relationship between top 10% students and and student/faculty ratio") +
  geom_smooth(method = "lm", se = FALSE)


#Correlation Coefficient
college %>%
  get_correlation(formula = S.F.Ratio ~ Top10perc)

#There is a positive correlation between instructional expenditure per student and new students from the top 10%.
p3 <- ggplot(data= college, mapping=aes(x= Expend, y = Top10perc)) +
  geom_point() +
  labs(x= "Instructional expenditure per student", y = "New students from the top 10%",
       title = "Relationship between top 10% students and instructional expenditure per student") +
  geom_smooth(method = "lm", se = FALSE)

p1 + p2 + p3

#Fit regression model
college_model <- lm(Top10perc ~ Expend + Private, data = college)

#Get regression table
get_regression_table(college_model)

#Multiple Regression Model
#Some trends from this model are that only private schools have funding greater than $20,000 per student and that there is a mix of new students from the top 10% in both private and public schools, althought it appears to be a greater number in private schools.
ggplot(college, mapping = aes(x = Expend, y = Top10perc, color = Private)) +
  geom_point() +
  labs(x = "Instructional expenditure per student", y = "New students from the top 10%", 
       color = "Private School", title = "Multiple Regression Model")+
  geom_smooth(method = "lm", se = FALSE)

# 2a.
auto <- read.csv("/Users/admin/Downloads/Auto.csv")
auto <- na.omit(auto)
glimpse(auto)

#The following variables are quantitative: mpg, cylinders, displacement, horsepower, weight and acceleration.

#The following variables are qualitative: year, origin and name.

# 2b.Ranges for each quantitative predictor

#Converting the qualitative variables to factors.
auto$year <- as.factor(auto$year)
auto$origin <- as.factor(auto$origin)
auto$name <- as.factor(auto$name)

#Converting the int and double variables to numeric
auto$mpg <- as.numeric(auto$mpg)
auto$cylinders <- as.numeric(auto$cylinders)
auto$displacement <- as.numeric(auto$displacement)
auto$horsepower <- as.numeric(auto$horsepower)
auto$weight <- as.numeric(auto$weight)
auto$acceleration <- as.numeric(auto$acceleration)

#Finding the ranges for each quantitative predictor
range(auto$mpg)
range(auto$cylinders)
range(auto$displacement)
range(auto$horsepower)
range(auto$weight)
range(auto$acceleration)

# 2c. Mean and standard deviation for each quantitative predictor
auto %>%
  select(mpg, cylinders, displacement, horsepower, weight, acceleration) %>%
  skim()

# mpg: mean = 23.7, sd = 7.87
# cylinders: mean = 5.44, sd = 1.71
# displacement: mean = 192, sd= 105
#horsepower: mean = 104, sd = 39
#weight: mean = 2955, sd = 848
#acceleration: mean = 15.5, sd = 2.76

# 2d.Calculations with 10 records removed
# The removal of 10 records did not cause the calculations to change substantialy. In fact, the first time I completed the removal I did not see any changes in any of the variables. When I changed the records that were removed a second time, I finally saw one range change, but it only changed the minimum by 1.5.
removed <- 5:15
auto_less10 <- auto[-removed, ]

range(auto_less10$mpg)
range(auto_less10$cylinders)
range(auto_less10$displacement)
range(auto_less10$horsepower)
range(auto_less10$weight)
range(auto_less10$acceleration)

# 2e.Predicting gas mileage on the basis of other variables

# Correlation 
# The correlation between cylinders and mpg is a strong correlation at -0.776896.
cor(auto$cylinders, auto$mpg)

#Setting the seed for reproducibility
set.seed(1)

#Sampling the dataset
row.number <- sample(1:nrow(auto), 0.8 * nrow(auto))

#Splitting the dataset into train and test sets
train <- auto[row.number, ]
test <- auto[-row.number, ]

dim_train <- dim(train)
dim_test <- dim(test)
dim_train
dim_test

#Fit the linear model
auto_model <- lm(mpg ~ cylinders, data = train)
summary(auto_model)

#Make predictions on the test data
predictions <- predict(auto_model, newdata = test)

#Calculating Mean Squared Error and R-squared
mse <- mean((test$mpg - predictions)^2)
mse

rss <- sum((test$mpg - predictions)^2)
tss <- sum((test$mpg - mean(test$mpg)) ^2)
rsquared <- 1 - (rss/tss)
rsquared

#The rsquared value is a little over halfway close to 1, so it isn't the strongest fit. 

#Scatterplot of cylinders and mpg
ggplot(auto, mapping = aes(x = cylinders, y = mpg)) +
  geom_point() +
  labs(x = "Cylinders", y = "mpg", title = "Cylinders ~ mpg") +
  geom_smooth(method = "lm", se = FALSE)

# Correlation 
# The correlation between weight and mpg is a strong correlation at -0.8324161.
cor(auto$weight, auto$mpg)

# Scatterplot of weight and mpg
ggplot(auto, mapping = aes(x = weight, y = mpg)) +
  geom_point() +
  labs(x = "Weight", y = "mpg", title = "Weight ~ mpg") +
  geom_smooth(method = "lm", se = FALSE)

# The analysis suggests that there is a correlation between more cylinders and a smaller mile per gallon and a higher weight and a smaller mile per gallon.

# 3a Loading the Boston dataset
# The dataset contains 485 rows and 14 columns. The rows represent a single record in the datset whereas the columns represent the variables of the datset.
boston <- read.csv("/Users/admin/Downloads/boston.csv")
glimpse(boston)

# 3b. Pairwise scatterpolots of the predictor columns
str(boston)

# Converting ints to numeric values
boston$chas <- as.numeric(boston$chas)
boston$rad <- as.numeric(boston$rad)
boston$tax <- as.numeric(boston$tax)
pairs(boston)

# The findings indicate that there are some strong and weak correlations between some of the variables.

# 3c. Associations with per capita crime rate
correlations <- cor(boston)
correlations_with_crim <- correlations[, "crim"]
correlations_with_crim

# Some positive correlation between crime rate and taxes with a 0.5800564 correlation.
cor(boston$crim, boston$tax)

ggplot(boston, mapping=aes(x= tax, y = crim)) +
  geom_point()+
  labs(x = "Taxes", y = "Crime rate", title = "Taxes ~ Crime Rate") +
  geom_smooth(model = "lm", se = FALSE)

# Some positive correlation between crime rate and accessibility to radial highways with a 0.6228416 correlation.
cor(boston$crim, boston$rad)

ggplot(boston, mapping=aes(x= rad, y = crim)) +
  geom_point()+
  labs(x = "Accessibility to highways", y = "Crime rate", title = " Accessibility to highways ~ Crime rate") +
  geom_smooth(model = "lm", se = FALSE)

# Very weak negative correlation between crime rate and average number of rooms with a -0.2252109 correlation.
cor(boston$crim, boston$rm)

ggplot(boston, mapping=aes(x= rm, y = crim)) +
  geom_point()+
  labs(x = "Average number of rooms", y = "Crime rate", title = "Average number of rooms ~ Crime rate") +
  geom_smooth(model = "lm", se = FALSE)

### 3d. Crime rates and tax rates of Boston suburbs
summary(boston$crim)
# The average crime rate is 3.76, but some suburbs have much higher crime rates as evident by the maximum crime rate of 88.97620.

summary(boston$tax)
# The tax rates vary greatly, with a minimum tax rate of $187 per $10,000 but some suburbs have up to $711 per $10,000.

summary(boston$ptratio)
# The pupil teacher ratios have some variation with a minimum of 12.60 pupils to teachers up to 22 pupils per teachers in some suburbs.

### 3e. There are 35 suburbs in Boston that bound the Charles River.
boston %>%
  filter(chas == 1) %>%
  summarize(count = n())

### 3f. The median pupil-teacher ratio among the towns in the dataset is 19.1.
median(boston$ptratio)



### 3g. The lowest value of median owner occupied homes is $5,000 and it belongs to the suburbs 399 and 406. 

# The value of the other predictors compare to overall range:
# Suburb 399: crime rate,  percent lower status of the population, proportion of non-retail business acres per town, tax rate are over average, proportion of residential land zone for lots over 25,000 sq. ft. is at minimum and nixtric oxides concentration are below average.
# Suburb 406: crime rate, percent lower status of the population,proportion of non-retail business acres per town, tax rate are over average, proportion of residential land zone for lots over 25,000 sq. ft. is at minimum and nixtric oxides concentration are below average.
# Both suburbs have nearly identical numbers for the predictors.

summary(boston$medv)

boston %>%
  group_by(X) %>%
  filter(medv == 5)

boston %>%
  filter(X == 399 | X == 406)

summary(boston)
  

### 3h. There are 64 suburbs that average more than 7 rooms per dwelling and there are 13 suburbs that average more than 8 rooms per dwelling.
# The suburbs that average more than 8 rooms have an average crime rate of 0.71879, a range of taxes from $224 - $666 per $10,000 and an average distance of 3.430 to five Boston employment centers.
boston %>%
  filter(rm > 7) %>%
  summarize(count = n())

boston %>%
  filter(rm > 8) %>%
  summarize(count = n())

eight_rooms <- boston %>%
  filter(rm > 8)

summary(eight_rooms)
