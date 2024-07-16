# Load necessary libraries
if (!requireNamespace("hbsae", quietly = TRUE)) {
  install.packages("hbsae")
}
library(hbsae)
library(dplyr)

# Load your data
puma_data <- read.csv('maine_puma_edu_binary.csv')
tract_data <- read.csv('maine_tract_binary.csv')

# Define covariates and outcome variable
covariates <- c('X65_69', 'X70_74', 'X75_79', 'X80_84', 'X85', 'male', 'female', 'other', 'hispanic', 'black')
education_levels <- c('newedu09', 'newedu12', 'newedu14', 'newedu19')

# Prepare PUMA-level data
puma_data <- puma_data %>%
  mutate(area = as.factor(puma)) %>%
  select(area, all_of(covariates), all_of(education_levels))

# Prepare Tract-level data
tract_data <- tract_data %>%
  mutate(area = as.factor(puma)) %>%
  select(area, tracta, all_of(covariates))

# Fit the model for each education level
models <- list()
for (edu_level in education_levels) {
  formula <- as.formula(paste(edu_level, "~", paste(covariates, collapse = " + ")))
  
  # Try fitting the model and catch errors
  tryCatch({
    models[[edu_level]] <- fSAE(formula, puma_data, area = "area")
    print(paste("Model fitting succeeded for", edu_level))
    print(summary(models[[edu_level]]))  # Print summary of the model for diagnostics
  }, error = function(e) {
    print(paste("Error in model fitting for", edu_level, ":", e$message))
    models[[edu_level]] <- NULL
  })
}

# Generate predictions for each successfully fitted education level
# Ensure models are fitted correctly and generate predictions for each row in tract_data
# Generate predictions for each successfully fitted education level
predictions <- list()
for (edu_level in education_levels) {
  if (!is.null(models[[edu_level]])) {
    # Extract beta coefficients
    coef <- models[[edu_level]]$beta
    print(paste("Beta coefficients for", edu_level, ":", coef))  # Diagnostic print
    
    # Ensure coefficients are numeric and check their length
    coef <- as.numeric(coef)
    print(paste("Length of coefficients for", edu_level, ":", length(coef)))  # Diagnostic print
    
    # Create design matrix for tract_data without intercept
    X_new <- model.matrix(as.formula(paste("~", paste(covariates, collapse = " + "), "- 1")), data = tract_data)
    print(paste("Number of columns in design matrix for", edu_level, ":", ncol(X_new)))  # Diagnostic print
    
    # Ensure dimensions match
    if (ncol(X_new) == length(coef)) {
      # Generate predictions
      predictions[[edu_level]] <- as.vector(X_new %*% coef)
      print(head(predictions[[edu_level]]))  # Print first few predictions for diagnostics
    } else {
      print(paste("Non-conformable arguments for", edu_level))
      predictions[[edu_level]] <- rep(NA, nrow(tract_data))  # Handle non-conformable case
    }
  } else {
    predictions[[edu_level]] <- rep(NA, nrow(tract_data))  # Handle cases where the model failed
  }
}


# Combine predictions into a single dataframe at the tracta level
tract_predictions <- tract_data %>%
  select(tracta) %>%
  distinct() %>%
  mutate(
    newedu09 = ifelse(length(predictions[['newedu09']]) == nrow(tract_data), 
                      aggregate(predictions[['newedu09']], by=list(tracta=tract_data$tracta), FUN=mean)$x, 
                      NA),
    newedu12 = ifelse(length(predictions[['newedu12']]) == nrow(tract_data), 
                      aggregate(predictions[['newedu12']], by=list(tracta=tract_data$tracta), FUN=mean)$x, 
                      NA),
    newedu14 = ifelse(length(predictions[['newedu14']]) == nrow(tract_data), 
                      aggregate(predictions[['newedu14']], by=list(tracta=tract_data$tracta), FUN=mean)$x, 
                      NA),
    newedu16 = NA,  # Set to NA due to lack of variability
    newedu19 = ifelse(length(predictions[['newedu19']]) == nrow(tract_data), 
                      aggregate(predictions[['newedu19']], by=list(tracta=tract_data$tracta), FUN=mean)$x, 
                      NA)
  )

# Display the first few rows of the predictions for diagnostics
print(head(tract_predictions))


# Display the first few rows of the predictions for diagnostics
print(head(tract_predictions))


