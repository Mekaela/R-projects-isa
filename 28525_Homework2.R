getwd()

# add file to working directory
setwd("")

library(readxl) # for reading Excel files
library(openxlsx) # for writing to Excel files
library("ggplot2") # for plotting

data <- read_excel("./Homework2_data.xlsx", sheet = "Juvenil_h_age_independent")
data

# Task one: Create an excel file that has two separated sheets referring one to the fitting data set
# and another one to the evaluation/validation data set. Use each sheet in the next tasks.
# split the data into two parts: one with the 10% of the rows and another with the remaining 90%
set.seed(123) # for reproducibility
sample_size <- floor(0.9 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = sample_size)
train_sample <- data[train_indices, ]
test_sample <- data[-train_indices, ]
train_sample
test_sample

# save the test and train samples to new sheets in the Excel file
wb = loadWorkbook("././28525_Homework2_data.xlsx")
addWorksheet(wb, "Juv_h_age_ind_90_train")
writeData(wb, "Juv_h_age_ind_90_train", train_sample)
addWorksheet(wb, "Juv_h_age_ind_10_test")
writeData(wb, "Juv_h_age_ind_10_test", test_sample)
saveWorkbook(wb, "././28525_Homework2_data.xlsx", overwrite = TRUE)

# visualisation of the relationship between h1 and h2
ggplot(data, aes(x = h1, y = h2, colour = int)) +
  geom_point() + 
  theme_bw()

yearly <- (data$h2-data$h1)/data$int

ggplot(data, aes(x = h1, y = yearly, colour = Cod_Par)) +
  geom_point() + 
  theme_bw()

# Question two: using the age independent Richards function, fit the data set using A=12 for:
# A: None of the parameters are expressed as a function of any other variable

# Richards function
richards_age_ind <- function(int, A, k, h1, m) {
  A * (1 - exp(-k * int) * ( 1 -(h1/A)^(1-m)))^(1 / (1-m))
}

# find the average growth rate: (h2-h1)/int
avg_growth_rate <- (train_sample$h2 - train_sample$h1) / train_sample$int
avg_growth_rate.mean <- mean(avg_growth_rate, na.rm = TRUE)
avg_growth_rate.mean

# Fit the model to the data
fit_no_dependency <- nlsLM(h2 ~ richards_age_ind(int, A, k, h1, m), 
                             data = train_sample,
                             start = list(A = 12, k = avg_growth_rate.mean, m = 1),
                             control = list(maxiter = 100, warnOnly = TRUE))

# Check convergence
if (!fit_no_dependency$convInfo$isConv) {
  warning("Model failed to converge. Adjust starting values or model structure.")
}

summary(fit_no_dependency)


# now we can fit this to the test data
test_predictions_a <- predict(fit_no_dependency, newdata = test_sample)
# Plot of actual vs. predicted (test set)
ggplot(data = test_sample, aes(x = h2, y = test_predictions_a)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual h2", y = "Predicted h2", title = "Test Set Performance") +
  theme_bw()

# Residual diagnostics
plot(nlsResiduals(fit_no_dependency)) 

# Calculate the residuals
test_residuals_a <- test_sample$h2 - test_predictions_a
# Plot the residuals
ggplot() +
  geom_point(aes(x = test_predictions_a, y = test_residuals_a)) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predicted h2", y = "Residuals") +
  theme_bw()

shapiro.test(test_residuals_a)

# Training residuals
train_residuals_a <- resid(fit_no_dependency)

# RMSE (test set)
rmse_test <- sqrt(mean(test_residuals^2, na.rm = TRUE))
cat("Test RMSE:", rmse_test, "\n")

# R² (training set)
ssr_train <- sum(train_residuals^2)
sst_train <- sum((train_sample$h2 - mean(train_sample$h2))^2)
sst_test <- sum((test_sample$h2 - mean(test_sample$h2))^2)
r2_train <- 1 - (ssr_train / sst_train)
cat("Training R²:", r2_train, "\n")

ssr_test_a <- sum(test_residuals_a^2)
r2_test_a <- 1 - (ssr_test_a / sst_test)
cat("Test R²:", r2_test_a, "\n")

#The model efficiency
# Calculate the residual sum of squares
sse_a <- sum((test_residuals_a)^2)
# Calculate the model efficiency
model_efficiency_a <- 1 - (sse_a / sst_test)
cat("Model Efficiency:", model_efficiency_a, "\n")

# precision is the mean of the absolute values of the residuals
precision_a <- mean(abs(test_residuals_a))
precision_a

# bias is mean value of the residuals
bias_a <- mean(test_residuals_a)
bias_a


# -----------------------------------------------------------------------
# B: k parameter is dependent of the number of trees per hectare (N)
#richards function changes to multiply k by N^theta. theta adds a weight, negative
# or positive, to the number of trees per hectare, to indicate the competition effect..
richards_density_dependent_B <- function(int, A, k0, theta, h1, m, N) {
  k <- k0 * N^theta  # Density-dependent growth rate
  A * (1 - exp(-k * int) * (1 - (h1/A)^(1 - m)))^(1 / (1 - m))
}

# the start values come from the values in the previous fit, except theta, 
# which is set to -0.5 to indicate competition between trees
start_vals_b <- list(
  A = 9.98, 
  k0 = 0.096,
  theta = -0.5,
  m = 0.86 
)

fit_n <- nlsLM(
  h2 ~ richards_density_dependent_B(int, A, k0, theta, h1, m, N),
  data = train_sample,
  start = start_vals_b,
  control = list(maxiter = 500)
)

# Check convergence
if (!fit_n$convInfo$isConv) {
  warning("Model failed to converge. Adjust starting values or model structure.")
}

summary(fit_n)

# Plot k(N) across observed densities
N_range <- seq(min(train_sample$N), max(train_sample$N), length=100)
k_vals <- coef(fit_n)["k0"] * N_range^coef(fit_n)["theta"]

plot(N_range, k_vals, type="l", xlab="Trees/ha", ylab="k(N)", 
     main="Density-Dependent Growth Rate")
points(train_sample$N, coef(fit_n)["k0"] * train_sample$N^coef(fit_n)["theta"])

# now we can fit this to the test data
test_predictions_b <- predict(fit_n, newdata = test_sample)

# And graph it. The steps are now the same as in the previous model
ggplot(data = test_sample, aes(x = h2, y = test_predictions_B)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual h2", y = "Predicted h2", title = "Test Set Performance") +
  theme_bw()

#the graph appears to be the same

# Residual diagnostics
plot(nlsResiduals(fit_n)) 

# Calculate the residuals
test_residuals_b <- test_sample$h2 - test_predictions_b
# Plot the residuals
ggplot() +
  geom_point(aes(x = test_predictions_B, y = test_residuals)) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predicted h2", y = "Residuals") +
  theme_bw()

# Training residuals
train_residuals_b <- resid(fit_n)

# RMSE (test set)
rmse_test <- sqrt(mean(test_residuals_b^2, na.rm = TRUE))
cat("Test RMSE:", rmse_test, "\n")

# R² (training set)
ssr_train <- sum(train_residuals_b^2)
sst_train <- sum((train_sample$h2 - mean(train_sample$h2))^2)
r2_train <- 1 - (ssr_train / sst_train)
cat("Training R²:", r2_train, "\n")

# h2 standard deviation of test data
h2_sd_test <- sd(test_sample$h2, na.rm = TRUE)
cat("Test h2 SD:", h2_sd_test, "\n")

ssr_test_b <- sum(test_residuals_b^2)
r2_test_b <- 1 - (ssr_test_b / sst_test)
cat("Test R²:", r2_test_b, "\n")

shapiro.test(test_residuals_b)

#The model efficiency
# Calculate the residual sum of squares
sse_b <- sum((test_residuals_b)^2)
# Calculate the model efficiency
model_efficiency_b <- 1 - (sse_b / sst_test)
cat("Model Efficiency:", model_efficiency_b, "\n")

# precision is the mean of the absolute values of the residuals
precision_b <- mean(abs(test_residuals_b))
precision_b

# bias is mean value of the residuals
bias_b <- mean(test_residuals_b)
bias_b



# C:  k parameter is dependent of Precipitation (Prec)
richards_precip_dependent_C <- function(int, A, k0, theta, h1, m, Prec) {
  k <- k0 * Prec^theta  # Density-dependent growth rate
  A * (1 - exp(-k * int) * (1 - (h1/A)^(1 - m)))^(1 / (1 - m))
}


start_vals <- list(
  A = 9.87, 
  k0 = 0.15,
  theta = 0,
  m = 0.86 
)

fit_precip <- nlsLM(
  h2 ~ richards_precip_dependent_C(int, A, k0, theta, h1, m, Prec),
  data = train_sample,
  start = start_vals,
  control = list(maxiter = 500)
)

summary(fit_precip)

# Plot k(Prec) across observed precipitation values
Prec_range <- seq(min(train_sample$Prec), max(train_sample$Prec), length=100)
k_vals <- coef(fit_precip)["k0"] * Prec_range^coef(fit_precip)["theta"]

plot(Prec_range, k_vals, type="l", xlab="Precipitation (mm/year)", 
     ylab="k(Prec)", main="Precipitation-Dependent Growth Rate")
points(train_sample$Prec, coef(fit_precip)["k0"] * train_sample$Prec^coef(fit_precip)["theta"])

# now we can fit this to the test data
test_predictions_c <- predict(fit_precip, newdata = test_sample)

# And graph it. The steps are now the same as in the previous model
ggplot(data = test_sample, aes(x = h2, y = test_predictions_c)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual h2", y = "Predicted h2", title = "Test Set Performance") +
  theme_bw()

# Residual diagnostics
plot(nlsResiduals(fit_precip)) 

# Calculate the residuals
test_residuals_c <- test_sample$h2 - test_predictions_c

# Training residuals
train_residuals_c <- resid(fit_precip)

# RMSE (test set)
rmse_test_c <- sqrt(mean(test_residuals_c^2, na.rm = TRUE))
cat("Test RMSE:", rmse_test_c, "\n")

# R² (training set)
ssr_train_c <- sum(train_residuals_c^2)
# sst train already calculated abovw
r2_train_c <- 1 - (ssr_train_c / sst_train)
cat("Training R²:", r2_train_c, "\n")

ssr_test_c <- sum(test_residuals_c^2)
r2_test_c <- 1 - (ssr_test_c / sst_test)
cat("Test R²:", r2_test_c, "\n")

#The model efficiency
# Calculate the residual sum of squares
sse_c <- sum((test_residuals_c)^2)
# Calculate the model efficiency
model_efficiency_c <- 1 - (sse_c / sst_test)
cat("Model Efficiency:", model_efficiency_c, "\n")

shapiro.test(test_residuals_c)

# precision is the mean of the absolute values of the residuals
precision_c <- mean(abs(test_residuals_c))
precision_c

# bias is mean value of the residuals
bias_c <- mean(test_residuals_c)
bias_c



# D:  k parameter is dependent of the number of trees per hectare (N) and Precipitation (Prec)
richards_density_precip_D <- function(int, A, k0, theta, gamma, h1, m, N, Prec) {
  k <- k0 * N^theta * Prec^gamma  # Density- and precipitation-dependent growth rate
  A * (1 - exp(-k * int) * (1 - (h1/A)^(1 - m)))^(1 / (1 - m))
}
start_vals <- list(
  A = 9.87, 
  k0 = 0.15,
  theta = 0.5,
  gamma = -0.5,
  m = 0.86 
)

fit_combined <- nlsLM(
  h2 ~ richards_density_precip_D(int, A, k0, theta, gamma, h1, m, N, Prec),
  data = train_sample,
  start = start_vals,
  control = list(maxiter = 500)
)
summary(fit_combined)


# Plot k(N, Prec) surface
library(plotly)
N_seq <- seq(min(train_sample$N), max(train_sample$N), length=50)
Prec_seq <- seq(min(train_sample$Prec), max(train_sample$Prec), length=50)
k_matrix <- outer(N_seq, Prec_seq, function(N, Prec) 
  coef(fit_combined)["k0"] * N^coef(fit_combined)["theta"] * Prec^coef(fit_combined)["gamma"])

plot_ly(x = N_seq, y = Prec_seq, z = k_matrix) %>%
  add_surface() %>%
  layout(scene = list(
    xaxis = list(title = "Trees/ha (N)"),
    yaxis = list(title = "Precipitation (Prec)"),
    zaxis = list(title = "k(N, Prec)")
  ))

# now we can fit this to the test data
test_predictions_d <- predict(fit_combined, newdata = test_sample)

# And graph it. The steps are now the same as in the previous model
ggplot(data = test_sample, aes(x = h2, y = test_predictions_d)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual h2", y = "Predicted h2", title = "Test Set Performance") +
  theme_bw()

# Residual diagnostics
plot(nlsResiduals(fit_combined))

# Calculate the residuals
test_residuals_d <- test_sample$h2 - test_predictions_d

# Training residuals
train_residuals_d <- resid(fit_precip)

# RMSE (test set)
rmse_test_d <- sqrt(mean(test_residuals_d^2, na.rm = TRUE))
cat("Test RMSE:", rmse_test_d, "\n")

# R² (training set)
ssr_train_d <- sum(train_residuals_d^2)
# sst train already calculated abovw
r2_train_d <- 1 - (ssr_train_d / sst_train)
cat("Training R²:", r2_train_d, "\n")

ssr_test_d <- sum(test_residuals_d^2)
r2_test_d <- 1 - (ssr_test_d / sst_test)
cat("Test R²:", r2_test_d, "\n")

shapiro.test(test_residuals_d)

#The model efficiency
# Calculate the residual sum of squares
sse_d <- sum((test_residuals_d)^2)
# Calculate the model efficiency
model_efficiency_d <- 1 - (sse_d / sst_test)
cat("Model Efficiency:", model_efficiency_d, "\n")

# precision is the mean of the absolute values of the residuals
precision_d <- mean(abs(test_residuals_d))
precision_d

# bias is mean value of the residuals
bias_d <- mean(test_residuals_d)
bias_d



# Compare with simpler models (e.g., k dependent on N only)
AIC(fit_combined, model_density_B, fit_precip, model_richards_age_ind_LM)
