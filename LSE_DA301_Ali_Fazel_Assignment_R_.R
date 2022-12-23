## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
## performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales_data <- read.csv(file.choose(), header=TRUE)

# Print the data frame.
View(sales_data)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_data2 <- subset(sales_data, select=-c(Ranking, Year, Genre, Publisher))

# View the data frame.
view(sales_data2)

# View the descriptive statistics.
summary(sales_data2)

# Explore the dataset
dim(sales_data2)
as_tibble(sales_data2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(NA_Sales, EU_Sales, data=sales_data2)
qplot(NA_Sales, Global_Sales, data=sales_data2)
qplot(EU_Sales, Global_Sales, data=sales_data2)

## 2b) Histograms
# Create histograms.
plot(hist(sales_data2$Global_Sales))
plot(hist(sales_data2$NA_Sales))
plot(hist(sales_data2$EU_Sales))

## 2c) Boxplots
# Create boxplots.
qplot(Platform, Global_Sales, data=sales_data2, geom='boxplot')
qplot(Platform, NA_Sales, data=sales_data2, geom='boxplot')
qplot(Platform, EU_Sales, data=sales_data2, geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
## The scatter plot shows us that there is a positive correlation between 
## European Sales and North American sales.
## When plotting the histograms, it became clear that across the board, sales 
## are skewed to the right with the vast majority of global sales falling into 
## bins in the £0-10 million. 
## Games sold on DS, GB, and NES platforms appear to be the best performing
## sales globally. 

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
view(sales_data2)

# View data frame head 
head(sales_data2)

# Search for missing values in a data set.
sum(is.na(sales_data2))

# Determine structure of the dataset
as_tibble(sales_data2)

# Check output: Determine the min, max, and mean values.
# Firstly, for North American Sales 
min(sales_data2$NA_Sales)
max(sales_data2$NA_Sales)
mean(sales_data$NA_Sales)

# Next, for European sales
min(sales_data2$EU_Sales)
max(sales_data2$EU_Sales)
mean(sales_data$EU_Sales)

# Finally, for global sales
min(sales_data2$Global_Sales)
max(sales_data2$Global_Sales)
mean(sales_data$Global_Sales)

# View the descriptive statistics.
summary(sales_data2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.

# Group data based on Product and determine the sum per Product.
sales_by_product <- sales_data2 %>% group_by(Product) %>% 
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            Global_Sales = sum(Global_Sales))

# View the data frame.
sales_by_product <- arrange(sales_by_product, desc(Global_Sales))
view(sales_by_product)

# Explore the data frame.
dim(sales_by_product)
str(sales_by_product)
summary(sales_by_product)

# Create new data frame grouping sales by platform for later use
sales_by_platform <- sales_data2 %>% group_by(Platform) %>% 
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            Global_Sales = sum(Global_Sales))

# View the data frame.
sales_by_platform <- arrange(sales_by_platform, desc(Global_Sales))
view(sales_by_platform)

# Explore the data frame
head(sales_by_platform)
dim(sales_by_platform)
str(sales_by_platform)
summary(sales_by_platform)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

# NA Sales va EU Sales
ggplot(data = sales_by_product,
       mapping = aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(color='red',
             alpha=0.5,
             size=1.5) +
  labs(title = "Relationship between North American & European Sales",
       subtitle = "Each point represents single product", 
       caption = "Source: Turtle Games",
       x = "North American Sales (£/million)",
       y = "European Sales (£/million)") +
  theme_bw()

# NA Sales vs Global Sales
ggplot(data = sales_by_product,
       mapping = aes(x = NA_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=0.5,
             size=1.5) +
  labs(title = "Relationship between North American & Global Sales",
       subtitle = "Each point represents single product", 
       caption = "Source: Turtle Games",
       x = "North American Sales (£/million)",
       y = "Global Sales (£/million)") +
  theme_bw()

# EU Sales vs Global Sales
ggplot(data = sales_by_product,
       mapping = aes(x = EU_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=0.5,
             size=1.5) +
  labs(title = "Relationship between European Sales & Global Sales",
       subtitle = "Each point represents single product", 
       caption = "Source: Turtle Games",
       x = "European Sales (£/million)",
       y = "Global Sales (£/million)") +
  theme_bw()

# Create histograms.

# Global sales data
ggplot(data = sales_by_product, 
       mapping = aes(x = Global_Sales)) +
  geom_histogram(color = 'black',
                 alpha = 0.5,
                 binwidth = 10 ) +
  labs(title = "Global Sales",
       caption = "Source: Turtle Games",
       x = "Global Sales (£/millions)",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 75, 10)) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  theme_bw()

# NA sales data
ggplot(data = sales_by_product, 
       mapping = aes(x=NA_Sales)) +
  geom_histogram(color = 'black',
                 alpha=0.5,
                 binwidth = 5) +
  labs(title = "Noth American Sales",
       caption = "Source: Turtle Games",
       x = "North American Sales (£/millions)",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_y_continuous(breaks = seq(0, 110, 20)) +
  theme_bw() 

# EU sales data
ggplot(data = sales_by_product, 
       mapping = aes(x=EU_Sales)) +
  geom_histogram(color = 'black',
                 alpha=0.5,
                 binwidth = 5) +
  labs(title = "European Sales",
       caption = "Source: Turtle Games",
       x = "European Sales (£/millions)",
       y = "Count") +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_bw()

# Create boxplots.

# Global sales
ggplot(data = sales_by_product, 
       mapping = aes(x=Global_Sales)) +
  geom_boxplot(color = 'red') +
  labs(title = "Distribution of Global Sales",
       caption = "Source: Turtle Games",
       x = "Global Sales (£/millions)",
       y = "All Products") +
  scale_x_continuous(breaks = seq(0, 70, 2)) +
  theme_classic()

# North American sales
ggplot(data = sales_by_product, 
       mapping = aes(x=NA_Sales)) +
  geom_boxplot(color = 'red') +
  labs(title = "Distribution of North American Sales",
       caption = "Source: Turtle Games",
       x = "North American Sales (£/millions)",
       y = "All Products") +
  scale_x_continuous(breaks = seq(0, 36, 3)) +
  theme_classic()

# European sales
ggplot(data = sales_by_product, 
       mapping = aes(x=EU_Sales)) +
  geom_boxplot(color = 'red') +
  labs(title = "Distribution of European Sales",
       caption = "Source: Turtle Games",
       x = "Euroean Sales (£/millions)",
       y = "All Products") +
  scale_x_continuous(breaks = seq(0, 25, 2)) +
  theme_classic()

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_data2$NA_Sales)
qqnorm(sales_data2$EU_Sales)
qqnorm(sales_data2$Global_Sales)

qqline(sales_data2$NA_Sales)
qqline(sales_data2$EU_Sales)
qqline(sales_data2$Global_Sales)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_by_product$NA_Sales)
shapiro.test(sales_by_product$EU_Sales)
shapiro.test(sales_by_product$Global_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_by_product$NA_Sales) 
kurtosis(sales_by_product$NA_Sales)
skewness(sales_by_product$EU_Sales) 
kurtosis(sales_by_product$EU_Sales)
skewness(sales_by_product$Global_Sales) 
kurtosis(sales_by_product$Global_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(sales_by_product)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

## Plot scatter plots to explore correlations in sales data 

# NA Sales vs EU Sales with trend line
ggplot(data = sales_by_product,
       mapping = aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(color='red',
             alpha=0.5,
             size=1.5) +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, col = 'black')+
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  labs(title = "Relationship between North American & European Sales",
       subtitle = "Each point represents single product", 
       caption = "Source: Turtle Games",
       x = "North American Sales (£/million)",
       y = "European Sales (£/million)") +
  theme_bw()

# NA Sales vs Global Sales with trend line 
ggplot(data = sales_by_product,
       mapping = aes(x = NA_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=0.5,
             size=1.5) +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, col = 'black')+
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  labs(title = "Relationship between North American & Global Sales",
       subtitle = "Each point represents single product", 
       caption = "Source: Turtle Games",
       x = "North American Sales (£/million)",
       y = "Global Sales (£/million)") +
  theme_bw()

# EU Sales vs Global Sales with trend line
ggplot(data = sales_by_product,
       mapping = aes(x = EU_Sales, y = Global_Sales)) +
  geom_point(color='red',
             alpha=0.5,
             size=1.5) +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, col = 'black')+
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  labs(title = "Relationship between European Sales & Global Sales",
       subtitle = "Each point represents single product", 
       caption = "Source: Turtle Games",
       x = "European Sales (£/million)",
       y = "Global Sales (£/million)") +
  theme_bw()


## Plot barplots exploring sales by platform 

# Global sales by platform 
ggplot(data = sales_by_platform,
       mapping = aes(x = Platform, y= Global_Sales)) +
  geom_bar(stat = "identity", fill= 'navy blue') +
  labs(title = "Global Sales by Platform",
       caption = "Source: Turtle Games",
       x = "Platform",
       y = "Global Sales (£/million)") +
  theme_bw()

# European sales by platform 
ggplot(data = sales_by_platform,
       mapping = aes(x = Platform, y= EU_Sales)) +
  geom_bar(stat = "identity", fill= 'navy blue') +
  labs(title = "European Sales by Platform",
       caption = "Source: Turtle Games",
       x = "Platform",
       y = "European Sales (£/million)") +
  theme_bw()

# North American sales by platform
ggplot(data = sales_by_platform,
       mapping = aes(x = Platform, y= NA_Sales)) +
  geom_bar(stat = "identity", fill= 'navy blue') +
  labs(title = "North American Sales by Platform",
       caption = "Source: Turtle Games",
       x = "Platform",
       y = "North American Sales (£/million)") +
  theme_bw()

## Melt the data set to compare EU sales directly with NA sales
install.packages('reshape2')
library(reshape2)

# Create new dataset without Global Sales figures
sales_compare <- sales_data2 %>% group_by(Platform) %>% 
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales))

# Melt it so that you have one variable column 
sales_compare.long<-melt(sales_compare)
View(sales_compare.long)

# Plot a bar plot directly comparing sales in EU vs NA. 
ggplot(sales_compare.long,aes(Platform,value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  labs(title = "Sales by Platform in North America and Europe",
       caption = "Source: Turtle Games",
       x = "Platform",
       y = "Sales (£/million)", 
       varibale = "Region",
       fill = "Sales Region")+
  theme_bw()

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# North American sales and European sales show a strong positive correlation of 
# 0.706, suggesting that if a game is popular in Europe, it is likely to also 
# be popular in North America. We saw even stronger correlations with North 
# American sales and global sales (0.935) as well as European sales and 
# global sales (0.878). 

# Globally, Wii is most popular with over £300 million in sales, X360 in second 
# place with just over £250 million and PS3 with over £200 million. 

# Whilst Wii and X360 are the most popular, both platforms are far more revenue 
# generating in North America than in Europe. 

# Also noteworthy is that there are only two platforms which generate greater 
# revenues in Europe than in North America: PS3 and PS4. 


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
head(sales_by_product)
dim(sales_by_product)

# Determine a summary of the data frame.
summary(sales_by_product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns

cor(sales_by_product)

## Linear regression model one: EU_Sales and NA_Sales
plot(sales_by_product$NA_Sales, sales_by_product$EU_Sales)

# Create a model with only one x variable.
model1 <- lm(EU_Sales~NA_Sales,
             data=sales_by_product)

# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1) 

# View residuals on a plot.
plot(model1$residuals)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# Plot the relationship with base R graphics.
plot(sales_by_product$NA_Sales, sales_by_product$EU_Sales)
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1))

# Create a log transformation to see if we can achieve a better fit on the same 
# data - comparing EU_Sales with NA_Sales

# Complete a log transformation for both columns with dplyr's mutate() function.
sales_by_product <- mutate(sales_by_product, 
                           logNA_Sales=log(NA_Sales))
sales_by_product <- mutate(sales_by_product, 
                           logEU_Sales=log(EU_Sales))

# View new object with new variable.
head(sales_by_product)

#Replace NaN & Inf in logEU_Sales column with NA so we can run regression
sales_by_product[is.na(sales_by_product) | sales_by_product=="-Inf"] = NA

# Create a new model using logEU_Sales 
model2 <- lm(logEU_Sales ~ NA_Sales, 
             data = sales_by_product)

# View full regression table.
summary(model2)

# Plot the relationship between NA_Sales and logEU_Sales.
plot(sales_by_product$NA_Sales, sales_by_product$logEU_Sales)

# Add a line-of-best fit to existing plot.
abline(coefficients(model2))

# The r-squared is lower when we use the logs, so it is a worse fit - we will 
# use the initial model1

## Linear regression model two: Global_Sales and EU_Sales
plot(sales_by_product$EU_Sales, sales_by_product$Global_Sales)

# Create a model with only one x variable.
model3 <- lm(Global_Sales~EU_Sales,
             data=sales_by_product)

# View the model.
model3

# View more outputs for the model - the full regression table.
summary(model3)

# View residuals on a plot.
plot(model3$residuals)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# Plot the relationship with base R graphics.
plot(sales_by_product$EU_Sales, sales_by_product$Global_Sales)
coefficients(model3)

# Add line-of-best-fit.
abline(coefficients(model3))

## Linear regression model three: Global_Sales and NA_Sales
plot(sales_by_product$NA_Sales, sales_by_product$Global_Sales)

# Create a model with only one x variable.
model4 <- lm(Global_Sales~NA_Sales,
             data=sales_by_product)

# View the model.
model4

# View more outputs for the model - the full regression table.
summary(model4)

# View residuals on a plot.
plot(model4$residuals)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# Plot the relationship with base R graphics.
plot(sales_by_product$NA_Sales, sales_by_product$Global_Sales)
coefficients(model4)

# Add line-of-best-fit.
abline(coefficients(model4))

###############################################################################

# 3. Create a multiple linear regression model

## Original dataset
# Select only numeric columns from the original data frame.
sales_mlr <- subset(sales_data, select = c(Product, NA_Sales, EU_Sales, 
                                           Global_Sales))

# Explore the data set.
summary(sales_mlr)
dim(sales_mlr)
str(sales_mlr)

# Determine correlation between variables.
cor(sales_mlr)

# Import the psych package.
library(psych)

# Use the corPlot() function.
corPlot(sales_mlr, cex=2)

# Multiple linear regression model.
modela = lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales_mlr)

# Print the summary statistics.
summary(modela)


## Grouped dataset
# Select only numeric columns from the grouped data frame.
sales_mlr2 <- subset(sales_by_product, select = c(Product, NA_Sales, EU_Sales, 
                                                  Global_Sales))

# Explore the data set.
summary(sales_mlr2)
dim(sales_mlr2)
str(sales_mlr2)

# Determine correlation between variables.
cor(sales_mlr2)

# Use the corPlot() function.
corPlot(sales_mlr2, cex=2)

# Multiple linear regression model.
modelb = lm(Global_Sales ~ EU_Sales + NA_Sales, data=sales_mlr2)

# Print the summary statistics.
summary(modelb)


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)

# Create a new data frame with the test data 
sales_test <- data.frame(cbind(NA_Sales, EU_Sales))

str(sales_test)

# Create a new object and specify the predict function with modelb (aggregated).
predictTest = predict(modelb, newdata=sales_test,
                      interval='confidence')

predictTest

# Determine the observed Global_Sales for the values above
sales_mlr[sales_mlr$NA_Sales == 34.02,] 
sales_mlr[sales_mlr$NA_Sales == 3.93,]
sales_mlr[sales_mlr$NA_Sales == 2.73,]
sales_mlr[sales_mlr$NA_Sales == 2.26,]
sales_mlr[sales_mlr$NA_Sales == 22.08,]

# Create a data frame to compare observed vs predicted sales for mod
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
predicted_Global_Sales <- c(68.06, 7.35, 4.91, 4.76, 26.63) 
observed_Global_Sales <- c(67.85, 6.04, 4.32, 3.53, 23.21)

outputs <- data.frame(cbind(NA_Sales, EU_Sales, predicted_Global_Sales,
                            observed_Global_Sales))

# View the data frame
outputs

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The predicted Global_Sales are quite close to the observed values. The model’s 
# R-squared value of 0.9668 informs us that 96.68% of the variation in global 
# sales can be explained by NA_Sales and EU_Sales. The Adjusted R-squared of 
# 0.9664 is also high suggesting we can confidently trust the model to make 
# accurate enough predictions to inform stakeholders. 


###############################################################################
###############################################################################




