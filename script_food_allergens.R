# Install and load packages
# install.packages(c('caTools', 'caret', 'vip','randomForest'))
library(caTools)
library(caret)
library(randomForest)
library(vip)
# Import the dataset
food <- read.csv("food_ingredients_and_allergens.csv")

# Have a look at the dataframe
head(food)
colnames(food)
str(food)

# Handle duplicate rows
food <- distinct(food)

# Encode the Prediction columm
food$Prediction = factor(food$Prediction,
                         levels = c('Contains', 'Does not contain'),
                         labels = c(0,1))

# Now add factors for variables that are factors
food$Allergens <- as.factor(food$Allergens)
food$Food.Product <- as.factor(food$Food.Product)
food$Main.Ingredient <- as.factor(food$Main.Ingredient)
food$Fat.Oil <- as.factor(food$Fat.Oil)
food$Seasoning <- as.factor(food$Seasoning)
food$Sweetener <- as.factor(food$Sweetener)
str(food)

# Handle missing values
colSums(is.na(food))
food <- na.omit(food)

# Remove unnecessary columns
food <- food %>%
  select("Allergens", "Sweetener", "Fat.Oil", "Prediction")

# Prepare to run the random tree
# Split the dataset
set.seed(123)
split <- sample.split(food$Prediction, SplitRatio = 0.8)
training_set <- subset(food, split == TRUE)
test_set <- subset(food, split == FALSE)

# Build the random tree
classifier <- randomForest(Prediction ~ ., data=training_set, ntree=500, proximity=TRUE)
classifier

# Predict on the test set
pred <- predict(classifier, newdata = test_set[-4])
pred

# Create the confusion matrix
cm <- confusionMatrix(pred, test_set$Prediction)
cm

## The Accuracy of the model is 90%

# Create a variable importance plot
var_importance <- vip::vip(classifier, num_features = 10)
print(var_importance)