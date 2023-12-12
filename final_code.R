#load data into data
data <- read.csv("~/Documents/GitHub/Sharktank-Analysis-Using-R/Dataset 3 — Shark tank pitches.csv")
attach(data)

summary(data)

#columns
names(data)
# Histogram for a numerical variables: episode,askedFor, exchangeForStake,valuation,season
hist(data$episode, breaks=20, xlab="Episode")
hist(data$askedFor, breaks=20, xlab="Asked For", main="Histogram of Asked For")
hist(data$exchangeForStake, breaks=20, xlab="Exchange For Stake", main="Histogram of Exchange For Stake")
hist(data$valuation, breaks=20, xlab="Valuation")
hist(data$season, breaks=20, xlab="Season")

#log transformation of askedFor
data$log_askedFor <- log(data$askedFor)
hist(data$log_askedFor, breaks=20, xlab="Log Asked For", main="Histogram of Log Asked For")

data$log_exchangeForStake <- log(data$exchangeForStake)
hist(data$log_exchangeForStake, breaks=20, xlab="Log Exchange For Stake", main="Histogram of Log Exchange For Stake")

data$log_valuation <- log(data$valuation)
hist(data$log_valuation, breaks=20, xlab="Log Valuation")
# Boxplot in ggplot for a numerical variables: episode,askedFor, exchangeForStake,valuation,season
library(ggplot2)
ggplot(data, aes(x=episode)) + geom_boxplot()
ggplot(data, aes(x=askedFor)) + geom_boxplot()
ggplot(data, aes(x=exchangeForStake)) + geom_boxplot()
ggplot(data, aes(x=valuation)) + geom_boxplot()
ggplot(data, aes(x=season)) + geom_boxplot()

#count of different values in a categorical variable: deal, category, location, shark1, shark2, shark3, shark4, shark5, Multiple.Entreprenuers
table(data$deal)
table(data$category)
table(data$shark1)
table(data$shark2)
table(data$shark3)
table(data$shark4)
table(data$shark5)
table(data$Multiple.Entreprenuers)

#ploting deal true and false
ggplot(data, aes(x=deal)) + geom_bar()
#ploting category verticle x axis
ggplot(data, aes(x=category)) + geom_bar() + coord_flip()


#creating a new column Country = last two letters of the location
data$Country <- substr(data$location, nchar(data$location)-1, nchar(data$location))
table(data$Country)


# Function to assign broad categories
assign_broad_category <- function(category) {
  if (category %in% c("Men and Women's Apparel", "Men's Accessories", "Women's Accessories", "Women's Apparel", 
                      "Men and Women's Shoes", "Women's Shoes", "Undergarments and Basics", "Men and Women's Accessories",
                      "Fitness Apparel and Accessories")) {
    return("Apparel and Accessories")
  } else if (category %in% c("Baby and Child Care", "Baby and Children's Apparel and Accessories", 
                             "Baby and Children's Bedding", "Baby and Children's Entertainment", "Baby and Children's Food")) {
    return("Baby and Children Products")
  } else if (category %in% c("Health and Well-Being", "Homeopathic Remedies", "Personal Care and Cosmetics", 
                             "Fitness Equipment", "Fitness Programs")) {
    return("Health and Wellness")
  } else if (category %in% c("Gardening", "Home Accessories", "Home Improvement", "Home Security Solutions", 
                             "Storage and Cleaning Products")) {
    return("Home and Garden")
  } else if (category %in% c("Alcoholic Beverages", "Non-Alcoholic Beverages", "Specialty Food", 
                             "Kitchen Tools")) {
    return("Food and Beverages")
  } else if (category %in% c("Entertainment", "Education", "Music", "Party Supplies")) {
    return("Entertainment and Education")
  } else if (category %in% c("Electronics", "Mobile Apps", "Productivity Tools", "Online Services")) {
    return("Technology and Electronics")
  } else if (category %in% c("Cycling", "Golf Products", "Outdoor Recreation")) {
    return("Sports and Recreation")
  } else {
    return("Pets and Novelties")
  }
}

# Apply function to the dataset
data$broad_category <- sapply(data$category, assign_broad_category)

table(data$broad_category) 

table(data$deal, data$broad_category)
#ploting broad category
ggplot(data, aes(x=broad_category)) + geom_bar() + coord_flip()

# Text length features
data$desc_word_count <- sapply(strsplit(data$description, " "), length)
data$description_char_count <- nchar(data$description)


#sentiment score of the description
install.packages("syuzhet")
library("syuzhet")
#data$sentiment <- get_nrc_sentiment(data$description)
sentiment_results <- get_nrc_sentiment(data$description)

# Combine the sentiment results with the original dataframe
data <- cbind(data, sentiment_results)

#correlation matrix
library(corrplot)

# Select only numeric columns from the dataframe
numeric_data <- data[sapply(data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

names(data)


#datatype of each column
sapply(data, class)

data$deal <- as.integer(data$deal)
data$Multiple.Entreprenuers <- as.integer(data$Multiple.Entreprenuers)




# List of six popular sharks
popular_sharks <- c("Barbara Corcoran", "Lori Greiner", "Kevin O'Leary", "Robert Herjavec", "Daymond John", "Mark Cuban")


# Add new columns for sharks
for (shark in popular_sharks) {
  data[[shark]] <- integer(nrow(data))
}
data$Other <- integer(nrow(data))


# List of six popular sharks
popular_sharks <- c("Barbara Corcoran", "Lori Greiner", "Kevin O'Leary", "Robert Herjavec", "Daymond John", "Mark Cuban")

# Add new columns for sharks
for (shark in popular_sharks) {
  data[[shark]] <- integer(nrow(data))
}
data$Other <- integer(nrow(data))

# Update function to return modified row
update_shark_columns <- function(row) {
  sharks_present <- unlist(row[c("shark1", "shark2", "shark3", "shark4", "shark5")])
  
  for (shark in popular_sharks) {
    row[[shark]] <- as.integer(shark %in% sharks_present)
  }
  
  row[["Other"]] <- as.integer(sum(row[popular_sharks]) == 0)
  return(row)
}

# Apply the function to each row and update the dataframe
for (i in 1:nrow(data)) {
  data[i, ] <- update_shark_columns(data[i, ])
}

#drop the columns shark1, shark2, shark3, shark4, shark5 and other
data$shark1 <- NULL
data$shark2 <- NULL
data$shark3 <- NULL
data$shark4 <- NULL
data$shark5 <- NULL
data$Other <- NULL

#rename kevin o leary to kevin oleary
names(data)[names(data) == "Kevin O'Leary"] <- "Kevin_OLeary"
#removing space with _ in column names
names(data) <- gsub(" ", "_", names(data))


#
names(data)

#making a new dataset data2 to remove the columns that are not required
#making a copy
data2 <- data

#removing the columns that are not required
data2$location <- NULL
data2$description <- NULL
data2$entrepreneurs <- NULL
data2$website <- NULL
data2$episode.season <- NULL
data2$title <- NULL
data2$category <- NULL


###country pre processing
# Step 1: Identify the top 5 countries
top_countries <- names(sort(table(data2$Country), decreasing = TRUE)[1:5])

# Step 2: Map abbreviations to full names (modify this as per your country names)
full_names <- c("CA" = "California", "NY" = "New York", "TX" = "Texas", "FL" = "Florida", "IL" = "Illinois")

# Step 3: Replace all other country names with 'Others'
data2$Country <- ifelse(data2$Country %in% top_countries, data2$Country, "Others")

# Step 4: Update to full names for the top 5 countries
data2$Country <- ifelse(data2$Country %in% names(full_names), full_names[data2$Country], data2$Country)

# Check the updated data
table(data2$Country)

###dumifying the categorical variables
# Install the fastDummies package
install.packages("fastDummies")

# Load the fastDummies package
library(fastDummies)

# Now use the dummy_cols function
data2 <- dummy_cols(data2, select_columns = c("Country", "broad_category"), remove_first_dummy = TRUE)

#making final data by dropping categorical columns
data3 <- data2
data3$Country <- NULL
data3$broad_category <- NULL
names(data3) <- gsub(" ", "_", names(data3))




# Calculate the correlation matrix
cor_matrix <- cor(data3, use = "complete.obs")





#unique number of vlues in each column
sapply(data3, function(x) length(unique(x)))

#dropping kevin oleary column as it is constant
data3$Kevin_OLeary <- NULL

datawithlog=data3

#removing collinear variables: description_char_count, positive, negative, askedFor,valuation, joy, exchangedForStake
data3$description_char_count <- NULL
data3$positive <- NULL
data3$negative <- NULL
data3$askedFor <- NULL
data3$valuation <- NULL
data3$joy <- NULL
data3$exchangedForStake <- NULL




##Generalized boosted models (GBM)

#train test split
set.seed(1)
train_index <- sample(1:nrow(data3), 0.7 * nrow(data3))
train <- data3[train_index, ]
test <- data3[-train_index, ]

table(train$deal)
table(test$deal)
library(gbm)
set.seed(1) ##get same results

boosted=gbm(deal~., data=train, distribution="bernoulli", n.trees=1000, interaction.depth=4, shrinkage=0.01, cv.folds=5, verbose=TRUE)
summary(boosted)


# Predict using the boosted model
#using 0.5 as threshold
predicted_probabilities <- predict(boosted, newdata = test, n.trees = 1000, type = "response")
predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Actual labels
actual_labels <- test$deal

# Calculate accuracy
accuracy <- mean(predicted_labels == actual_labels)
print(paste("Accuracy:", accuracy))

# Generate and print confusion matrix
confusion_matrix <- table(Predicted = predicted_labels, Actual = actual_labels)
print(confusion_matrix)

##########################################################################
#Random forest

library(randomForest)

# Convert 'deal' to a factor for classification
train$deal <- as.factor(train$deal)
test$deal <- as.factor(test$deal)

# Train the random forest model
rf_model <- randomForest(deal ~ ., data = train, ntree = 1000)

# Predict using the random forest model
predicted_labels <- predict(rf_model, newdata = test)

# Actual labels
actual_labels <- test$deal

# Calculate accuracy
accuracy <- mean(predicted_labels == actual_labels)
print(paste("Accuracy:", accuracy))

# Generate and print confusion matrix
confusion_matrix <- table(Predicted = predicted_labels, Actual = actual_labels)
print(confusion_matrix)


##########################################################################
#logistic regression

# Fit a logistic regression model
logistic_model <- glm(deal ~ ., data = train, family = "binomial")

# Predict using the logistic regression model
predicted_probabilities <- predict(logistic_model, newdata = test, type = "response")
predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Actual labels
actual_labels <- test$deal

# Calculate accuracy
accuracy <- mean(predicted_labels == actual_labels)
print(paste("Accuracy:", accuracy))

# Generate and print confusion matrix
confusion_matrix <- table(Predicted = predicted_labels, Actual = actual_labels)
print(confusion_matrix)


##########################################################################
#Linear Discriminant Analysis
install.packages("MASS")
library(MASS)
mylda=lda(deal~., data=train)
mylda


# Predict using the LDA model
predicted_labels <- predict(mylda, newdata = test)$class

# Actual labels
actual_labels <- test$deal

# Calculate accuracy
accuracy <- mean(predicted_labels == actual_labels)
print(paste("Accuracy:", accuracy))

# Generate and print confusion matrix
confusion_matrix <- table(Predicted = predicted_labels, Actual = actual_labels)
print(confusion_matrix)

##########################################################################

#Principal Component Analysis
library(GGally)
library(ggfortify)


#considering columns with high influence: valuation, description_char_count,episode,askedFor,exchangeForStake,
# positive, trust, broad_category_Pets_and_Novelties, fear, surprise, Country_Others, Multiple.Entreprenuers
# broad_category_Health_and_Wellness, Lori_Greiner, Country_New_York

names(data3)
data4 <- data3[, c("log_valuation", "deal","log_askedFor", "log_exchangeForStake", "surprise", "Country_Texas", "Multiple.Entreprenuers", "broad_category_Health_and_Wellness", "Mark_Cuban", "Country_New_York")]


# Perform PCA
pca <- prcomp(data4, scale = TRUE)


# Plotting PCA
autoplot(pca, data = data4, loadings = TRUE, loadings.label = TRUE)

# Plotting PCA with color condition based on 'deal'
autoplot(pca, data = data4, loadings = TRUE, loadings.label = TRUE, colour = 'deal')



#Clustering using K-means
# valuation vs exchangeForStake
datak=data3[,c("log_exchangeForStake","log_valuation")]

ggplot(datak, aes(x = exchangeForStake, y=askedFor)) +geom_point()

# Scale the data
datak_scaled <- scale(datak)
datak_scaled = datak

ggplot(datak_scaled, aes(x = exchangeForStake, y=askedFor)) +geom_point()

# Find optimal number of clusters using the elbow method
wss <- c()
for (i in 1:10) {
  wss[i] <- sum(kmeans(datak_scaled, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Fit the k-means model
set.seed(1)
km.3 = kmeans(datak_scaled, 3)

# Plot the clusters

datak$cluster=as.factor(km.3$cluster)
ggplot(datak, aes(x = exchangeForStake, y=askedFor, color=cluster)) +geom_point()

km.3

##########################################################################
#Linear Discriminant Analysis on limited predictors
mylda2=lda(deal~log_exchangeForStake+log_valuation, data=train)
mylda2

# Predict using the LDA model
predicted_labels <- predict(mylda2, newdata = test)$class

# Actual labels
actual_labels <- test$deal

# Calculate accuracy
accuracy <- mean(predicted_labels == actual_labels)
print(paste("Accuracy:", accuracy))

# Generate and print confusion matrix
confusion_matrix <- table(Predicted = predicted_labels, Actual = actual_labels)
print(confusion_matrix)
#lda on cluster means
predict(mylda2, data.frame(log_exchangeForStake=2.045556, log_valuation=15.85741))
predict(mylda2, data.frame(log_exchangeForStake=2.543888, log_valuation=14.22859))
predict(mylda2, data.frame(log_exchangeForStake=3.068338, log_valuation=12.78116))

#antilog of 2.045556, 15.85741
exp(2.045556)
exp(15.85741)

#antilog of 2.543888, 14.22859
exp(2.543888)
exp(14.22859)

#antilog of 3.068338, 12.78116
exp(3.068338)
exp(12.78116)



