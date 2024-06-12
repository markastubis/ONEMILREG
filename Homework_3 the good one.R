library(readxl)
library(dplyr)
library(broom)
library(knitr)

# loeading the data
setwd("C:/Users/Admin/Documents/R projects")
data <- read_excel("millions.xls", sheet=2)

# clearly defining the dependent and fixed variables
dep_var <- "gamma"  # dependent variable
fixed_vars <- c("X2", "X3", "X16")  #fixed variables

# filtering data to only keep non missing observations 
filtered_data <- data %>%
  filter(!is.na(!!sym(dep_var)) & !is.na(X2) & !is.na(X3) & !is.na(X16))

# printing summary statistics
summary_stats <- summary(filtered_data[, c(dep_var, fixed_vars)])
print(summary_stats)

summary(filtered_data$X1, filtered_data$X2,filtered_data$X3,filtered_data$X16)
# running the fixed regression and summarizing it
fixed_regression <- lm(as.formula(paste(dep_var, "~", paste(fixed_vars, collapse=" + "))), data = filtered_data)
fixed_summary <- summary(fixed_regression)

# printing results
print(fixed_summary)

# defining the vector of combinations for regression loops
# my first instinct was to create a function that would randomize it 
# so there would be less manual labor but then you wouldnt be able to replicate it :)
vector_for_regression_loop <- list(
  c("X42", "X23", "X53", "X39"),
  c("X56", "X32", "X5", "X4"),
  c("X33", "X29", "X59", "X42"),
  c("X38", "X60", "X31", "X50"),
  c("X34", "X41", "X57", "X51"),
  c("X23", "X52", "X53", "X57"),
  c("X39", "X41", "X56", "X34"),
  c("X32", "X43", "X5", "X31"),
  c("X4", "X60", "X33", "X29"),
  c("X59", "X7", "X38", "X39"),
  c("X4", "X7", "X43", "X52"),
  c("X56", "X50", "X6", "X32"),
  c("X33", "X6", "X42", "X51")
)
 
# creating an empty list for now to store the regression results in the future p value < 0.01 significant
results <- list()

# running the regression loop with all of our variables and adding an index 
# so it would be clear from which regression we got what results
for (i in seq_along(vector_for_regression_loop)) {
  combo <- vector_for_regression_loop[[i]]
  formula_str <- paste(dep_var, "~", paste(c(fixed_vars, combo), collapse=" + "))
  formula <- as.formula(formula_str)
  regression <- lm(formula, data = filtered_data)
  tidy_regression <- tidy(regression)
  tidy_regression$regression_index <- i  
  results[[i]] <- tidy_regression
}

# combining all of the regression dfs into a one nice big one
all_results <- bind_rows(results)

# creating a new column variable that would catch a variable name
all_results$variable <- gsub(".*\\b(X\\d+).*", "\\1", all_results$term)

# filtering out the intercept ( the task does not ask for it)
all_results <- all_results[all_results$term != "(Intercept)",]

all_results <- all_results %>%
  select(regression_index, variable, estimate, std.error, statistic, p.value)

# now sorting the data by variable
all_results <- all_results %>%
  arrange(variable, regression_index)

# reseting the row names
rownames(all_results) <- NULL

# displaying the results (i use knitr because it puts it into a nice table:))
knitr::kable(all_results, format = "simple", caption = "Sorted Regression Results by Variable")