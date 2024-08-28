
housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

#scatterplot for housing price against living area
plot(housing$sqft_living, housing$price,
    main = "Scatterplot of Price against Living Area",
    xlab = "Living Area", ylab = "Price",
    col = "red",
    pch = 19)

#scatterplot for housing price against home age
plot(housing$age, housing$price,
    main = "Scatterplot of Price against Home Age",
    xlab = "Home Age", ylab = "Price",
    col = "red",
    pch = 19)

#select price, sqft_living, and age as variables to subset the data
myvars <-c("price", "sqft_living","age")
housing_subset <-housing[myvars]

#print correlation matrix
print("Correlation Matrix")
round(cor(housing_subset, method = 'pearson'),4)


# multiple regression model using price as the response variable and sqft_living, sqft_above, age, bathrooms, and view as predictors
myvars <-c("price", "sqft_living", "sqft_above", "age", "bathrooms", "view")
housing_subset <-housing[myvars]

model1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data = housing_subset)
summary(model1)

# predicted values
print("Model 1 Fitted Values")
fitted_values <-fitted.values(model1)
head(fitted_values, 10)

write.csv(fitted_values,'model1_fitted_values.csv')


#residuals
print("Model 1 Residuals")
residuals = residuals(model1)
head(residuals, 10)

write.csv(residuals,'model1_residuals.csv')

#residuals against fitted values for model1
plot(fitted_values, residuals,
    main = "Model 1 Residuals against Fitted Values",
    xlab = "Fitted Values", ylab = "Residuals",
    col = "red",
    pch = 19)

#q-q plot for model1
qqnorm(residuals, pch = 19, col = "red")
qqline(residuals, col = "blue", lwd = 2)

newdata1 <- data.frame("sqft_living"= 2150, "sqft_above" = 1050, "age" = 15, "bathrooms" = 3, "view" = '0')

# prediction interval for first new set of values
print("Data 1 Prediction Interval")
round(predict(model1, newdata1, interval = "predict", level = .90),4)

# confidence interval for first new set of values
print("Data 1 Confidence Interval")
round(predict(model1, newdata1, interval = "confidence", level = .90),4)

newdata2 <- data.frame("sqft_living"= 4250, "sqft_above" = 2100, "age" = 5, "bathrooms" = 5, "view" = '2')

# prediction interval for second new set of values
print("Data 2 Prediction Interval")
round(predict(model1, newdata2, interval = "predict", level = .90),4)

# confidence interval for second new set of values
print("Data 2 Confidence Interval")
round(predict(model1, newdata2, interval = "confidence", level = .90),4)

#scatterplot for housing price against Average School Rating
plot(housing$school_rating, housing$price,
    main = "Scatterplot of Price against Average School Rating",
    xlab = "Average School Rating", ylab = "Price",
    col = "red",
    pch = 19)

#scatterplot for housing price against crime rate per 100,000
plot(housing$crime, housing$price,
    main = "Scatterplot of Price against Average Crime Rate",
    xlab = "Average Crime Rate per 100,000", ylab = "Price",
    col = "red",
    pch = 19)

# complete second order regression model using price as the response variable and school rating and average crime rate as predictors
myvars2 <-c("price", "school_rating", "crime")
housing_subset2 <-housing[myvars2]

model2 <-lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2)+I(crime^2), data = housing_subset2)
summary(model2)

# predicted values
print("Model 2 Fitted Values")
fitted_values2 <-fitted.values(model2)
head(fitted_values2, 10)

write.csv(fitted_values2,'model2_fitted_values.csv')


#residuals
print("Model 2 Residuals for")
residuals2 = residuals(model2)
head(residuals2, 10)

write.csv(residuals2,'model2_residuals.csv')

#residuals against fitted values for model1
plot(fitted_values2, residuals2,
    main = "Model 2 Residuals against Fitted Values",
    xlab = "Fitted Values", ylab = "Residuals",
    col = "red",
    pch = 19)

#q-q plot for model1
qqnorm(residuals2, pch = 19, col = "red")
qqline(residuals2, col = "blue", lwd = 2)

M2newdata1 <- data.frame("school_rating"= 9.80, "crime" = 81.02)

# prediction interval for first new set of values
print("Model 2 Data 1 Prediction Interval")
round(predict(model2, M2newdata1, interval = "predict", level = .90),4)

# confidence interval for first new set of values
print("Model 2 Data 1 Confidence Interval")
round(predict(model2, M2newdata1, interval = "confidence", level = .90),4)

M2newdata2 <- data.frame("school_rating"= 4.28, "crime" = 215.50)

# prediction interval for second new set of values
print("Model 2 Data 2 Prediction Interval")
round(predict(model2, M2newdata2, interval = "predict", level = .90),4)

# confidence interval for second new set of values
print("Model 2 Data 2 Confidence Interval")
round(predict(model2, M2newdata2, interval = "confidence", level = .90),4)

# complete first order regression model using price as the response variable and school rating and average crime rate as predictors
myvars2 <-c("price", "school_rating", "crime")
housing_subset2 <-housing[myvars2]

model2_reduced <-lm(price ~ school_rating + crime + school_rating:crime, data = housing_subset2)
summary(model2_reduced)

#Nested F-test between model2 and model3
anova(model2, model2_reduced)
