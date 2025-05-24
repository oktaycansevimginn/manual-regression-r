
# OKTAY CAN SEVIMGIN 
# Mail : oktaycansevimgin2022@gmail.com

if (!require(knitr)) install.packages("knitr")
library(knitr)

# 1. Dosya yolunu ayarla
setwd("C:/Users/Oktay Can/Desktop/Data")
data <- read.table("MultRegData.txt", header = TRUE)

Y <- data$Y
X_df <- data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")]

transpose_matrix <- function(matrix_in) {
  rows <- nrow(matrix_in)
  cols <- ncol(matrix_in)
  matrix_out <- matrix(0, nrow = cols, ncol = rows)
  for (i in 1:rows) {
    for (j in 1:cols) {
      matrix_out[j, i] <- matrix_in[i, j]
    }
  }
  return(matrix_out)
}

multiply_matrices <- function(matrix_a, matrix_b) {
  rows_a <- nrow(matrix_a)
  cols_a <- ncol(matrix_a)
  rows_b <- nrow(matrix_b)
  cols_b <- ncol(matrix_b)
  if (cols_a != rows_b) {
    stop("Uyumsuz matris boyutları!")
  }
  matrix_out <- matrix(0, nrow = rows_a, ncol = cols_b)
  for (i in 1:rows_a) {
    for (j in 1:cols_b) {
      for (k in 1:cols_a) {
        matrix_out[i, j] <- matrix_out[i, j] + matrix_a[i, k] * matrix_b[k, j]
      }
    }
  }
  return(matrix_out)
}

inverse_matrix <- function(matrix_in) {
  n <- nrow(matrix_in)
  if (n != ncol(matrix_in)) {
    stop("Girdi kare matris olmalı!")
  }
  augmented_matrix <- cbind(matrix_in, diag(n))
  for (i in 1:n) {
    pivot <- augmented_matrix[i, i]
    if (abs(pivot) < 1e-9) {
      stop("Matrisin tersi alınamaz (sıfır pivot)!")
    }
    augmented_matrix[i, ] <- augmented_matrix[i, ] / pivot
    for (j in 1:n) {
      if (i != j) {
        factor <- augmented_matrix[j, i]
        augmented_matrix[j, ] <- augmented_matrix[j, ] - factor * augmented_matrix[i, ]
      }
    }
  }
  return(augmented_matrix[, (n + 1):(2 * n)])
}

calculate_mean <- function(vector_in) {
  return(sum(vector_in) / length(vector_in))
}

calculate_sum <- function(vector_in) {
  total <- 0
  for (val in vector_in) {
    total <- total + val
  }
  return(total)
}

my_regression <- function(Y, X) {
  X_with_intercept <- cbind(1, as.matrix(X))  
  
  XT <- transpose_matrix(X_with_intercept)
  XTX <- multiply_matrices(XT, X_with_intercept)
  XTX_inv <- inverse_matrix(XTX)
  XTy <- multiply_matrices(XT, as.matrix(Y))
  beta_hat <- multiply_matrices(XTX_inv, XTy)
  
  Y_hat <- multiply_matrices(X_with_intercept, beta_hat)
  residuals <- Y - Y_hat
  
  Y_mean <- calculate_mean(Y)
  TSS <- calculate_sum((Y - Y_mean)^2)
  RMSS <- calculate_sum((Y_hat - Y_mean)^2)
  RSS <- calculate_sum(residuals^2)
  R_squared <- 1 - RSS / TSS
  
  return(list(
    beta_hat = beta_hat,
    Y_hat = Y_hat,
    residuals = residuals,
    TSS = TSS,
    RMSS = RMSS,
    RSS = RSS,
    R_squared = R_squared
  ))
}

model_selection <- function(Y, X_df) {
  vars <- colnames(X_df)
  all_models <- list()
  
  for (k in 1:length(vars)) {
    combs <- combn(vars, k, simplify = FALSE)
    for (comb in combs) {
      X_sub <- as.matrix(X_df[, comb, drop = FALSE])
      reg_result <- my_regression(Y, X_sub)
      all_models <- append(all_models, list(list(
        Variables = paste(comb, collapse = " "),
        Num_Variables = length(comb),
        TSS = reg_result$TSS,
        RMSS = reg_result$RMSS,
        RSS = reg_result$RSS,
        R_squared = reg_result$R_squared
      )))
    }
  }
  
  results_df <- data.frame(
    Num_Variables = sapply(all_models, function(m) m$Num_Variables),
    Variables = sapply(all_models, function(m) m$Variables),
    TSS = sapply(all_models, function(m) m$TSS),
    RMSS = sapply(all_models, function(m) m$RMSS),
    RSS = sapply(all_models, function(m) m$RSS),
    R_squared = sapply(all_models, function(m) m$R_squared),
    stringsAsFactors = FALSE
  )
  
  results_df <- do.call(rbind, lapply(split(results_df, results_df$Num_Variables), function(df) {
    df[order(df$R_squared, decreasing = TRUE), ]
  }))
  
  results_df$Model <- seq_len(nrow(results_df))
  

  results_df <- results_df[, c("Model", "Num_Variables", "Variables", "TSS", "RMSS", "RSS", "R_squared")]
  results_df$TSS <- round(results_df$TSS, 2)
  results_df$RMSS <- round(results_df$RMSS, 2)
  results_df$RSS <- round(results_df$RSS, 2)
  results_df$R_squared <- round(results_df$R_squared, 4)
  

  colnames(results_df) <- c("Model", "Number of Variables", "Variable (X) Name", "TSS", "RMSS", "RSS", "R-Square")
  
 
  print(kable(results_df, format = "markdown", align = "c", row.names = FALSE))
  

}

model_selection(Y, X_df)


