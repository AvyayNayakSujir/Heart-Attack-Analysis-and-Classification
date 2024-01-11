#-----------------------------------------------------------LOADING LIBRARIES-----------------------------------------------
library(ggplot2)
library(dplyr)

# Load the data set
data <- read.csv("/heart.csv") #---------------------------------change path                                    

#--------------------------------------------------------INFORMATION ON THE DATASET------------------------------------------

# printing the number of rows and columns in the data set
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

# Checking if the values of an attribute are discrete or continuous
for (i in names(data)) {
  unique_values <- unique(data[[i]])
  
  if (length(unique_values) < 5) {
    cat("Attribute '", i, "' has discrete values.\n")
  } else {
    cat("Attribute '", i, "' has continuous values.\n")
  }
}

#--------------------------------------------------------------DATA CLEANING-------------------------------------------------

# Create a clean copy for analysis
semi_clean_data <- data

# checking for missing values
missing_values <- sum(is.na(semi_clean_data))
cat("Total missing values in the dataset:", missing_values, "\n")

cat("The first few rows before data cleaning are:\n")
print(semi_clean_data)


#replacing 1 and 0 with yes and no for easier manipulation in the analysis
semi_clean_data<-semi_clean_data%>% mutate(EIA=
                                             ifelse(EIA=="Yes",1,EIA))
semi_clean_data<-semi_clean_data%>% mutate(EIA=
                                             ifelse(EIA=="No",0,EIA))
print(semi_clean_data)

#------------------------------------------------------------------------SAVING semi_clean_data as a CSV FILE
# Specify the path and filename for the new CSV file
new_csv_path <- "/semi_clean_data.csv"   #-------------------------------change path

# Save the semi_clean_data DataFrame to the new CSV file
write.csv(semi_clean_data, file = new_csv_path, row.names = FALSE)

# Print a message indicating the successful save
cat("semi clean data has been saved to:", new_csv_path, "\n")

#load the data set
clean_data <- read.csv("/semi_clean_data.csv")       #---------------------------change path

# removing the rows with the number of missing values being greater than 3. Otherwise, filling in the missing values
for (i in 1:nrow(clean_data)) {
  if (sum(is.na(clean_data[i, ])) > 3) {
    clean_data <- clean_data[-i, ]  # Omitting row
  } else {
    for (col in names(clean_data)) {
      if (is.na(clean_data[i, col])) {                                       
        mean_val <- mean(clean_data[[col]], na.rm = TRUE)
        clean_data[i, col] <- round(mean_val)  
      }
    }
  }
}

cat("Number of rows after handling missing values:", nrow(clean_data), "\n")
cat("Number of columns after handling missing values:", ncol(clean_data), "\n")

cat("The data frame after data cleaning :\n")
print(clean_data)

#------------------------------------------------------------------------SAVING clean_data as a CSV FILE

# Specify the path and filename for the new CSV file
new_csv_path <- "/clean_data.csv"      #-----------------------------------change path

# Save the clean_data DataFrame to the new CSV file
write.csv(clean_data, file = new_csv_path, row.names = FALSE)

# Print a message indicating the successful save
cat("Clean data has been saved to:", new_csv_path, "\n")

#-------------------------------------------------------------DATA ANALYSIS--------------------------------------------------

# getting the summary of the data set
cat("Summary of the data:\n")
print(summary(clean_data))

# plotting bar plots between 'output' and all other attributes
for (attribute in names(clean_data)) {
  if (attribute != "output") {
    plot_title <- paste("Bar plot of '", attribute, "' by 'output'")
    
    p <- ggplot(clean_data, aes(x = factor(clean_data[[attribute]]), fill = factor(clean_data$output))) +
      geom_bar(position = "dodge", stat = "count") +
      labs(title = plot_title, x = attribute, y = "Count", fill = "Output") +
      theme_minimal()
    
    print(p)  
  }
}

# Specify the attributes for which you want to plot pie charts
attributes_to_plot <- c("gender", "CPT", "FBST", "RECG", "EIA")

# Loop through each attribute and plot a pie chart
for (attribute in attributes_to_plot) {
  # Count the occurrences of each unique value in the attribute
  attribute_counts <- table(clean_data[[attribute]])
  
  # Calculate percentages
  percentages <- prop.table(attribute_counts) * 100
  
  # Plot a pie chart with percentages
  pie(attribute_counts, main = paste("Pie Chart for ", attribute), col = rainbow(length(attribute_counts)),
      labels = paste(names(attribute_counts), "\n", sprintf("%.1f%%", percentages)))
  
  # Add a legend
  legend("topright", names(attribute_counts), fill = rainbow(length(attribute_counts)), cex = 0.8)
}


#----------------------------------------CLASSIFICATION USING NAVIE BAYES ALGORITHM------------------------------------------

# Define the conditions for the test case 
#test_case <- data.frame(age=63,gender=1,CPT=3,RBP=145,cholesterol=233,FBST=1,RECG=0,MHRA=150,EIA=0,PreviousPeak=2.3) #------------has a sure probablity of 1 as it is a unique case in the dataset
test_case <- data.frame(age=64,FBST=1)

# Define the prior probabilities P(C1) and P(C0)
P_C1 <- sum(clean_data$output == 1) / nrow(clean_data)
P_C0 <- sum(clean_data$output == 0) / nrow(clean_data)

# Function to calculate conditional probability 
calculate_conditional_probability <- function(attribute, value, output) {
  count_condition <- 0
  count_total <- 0
  
  for (i in 1:nrow(clean_data)) {
    if (clean_data[[attribute]][i] == value) {
      count_total <- count_total + 1
      if (clean_data$output[i] == output) {
        count_condition <- count_condition + 1
      }
    }
  }
  
  # Check if count_total is zero
  if (count_total == 0) {
    cat("count total is 0")
    return(0)  # or any other suitable handling for this case
  }
  
  result <- count_condition / count_total  #--------n(E|output) / n(E)
  
  return(result)
}


# Function to calculate posterior probability for the specified test case
calculate_posterior_probability <- function(test_case, output) {
  posterior_prob <- 1
  for (attribute in names(test_case)){
    posterior_prob <- posterior_prob * calculate_conditional_probability(attribute, test_case[[attribute]], output)
  }
  
  return(posterior_prob)
}


# Calculate posterior probabilities for both classes 
posterior_prob_C1 <- calculate_posterior_probability(test_case, 1) * P_C1
posterior_prob_C0 <- calculate_posterior_probability(test_case, 0) * P_C0

normalization_factor <- posterior_prob_C1 + posterior_prob_C0
posterior_prob_C1_normalized <- posterior_prob_C1 / normalization_factor
posterior_prob_C0_normalized <- posterior_prob_C0 / normalization_factor


# Print the results
cat("Normalized Posterior Probability of having a heart attack:", posterior_prob_C1_normalized, "\n")
cat("Normalized Posterior Probability of not having a heart attack:", posterior_prob_C0_normalized, "\n")
