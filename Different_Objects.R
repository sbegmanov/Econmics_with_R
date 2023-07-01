# Vectors
Price <- c(10, 3, 15)
Quantity <- c(25, 3, 20)
Expenditure <- Price * Quantity

# Matrices
Matrix_PQE <- matrix(data = cbind(Price, Quantity, Expenditure), ncol = 3)
Matrix_PQE[1, ]   # first row
Matrix_PQE[, 2]   # second column
Matrix_PQE[1, 2]  # first row, second column

# Data frames
Exp_data <- data.frame(Price, Quantity, Expenditure)
print(Exp_data)
Exp_data[, 2] # second column
Exp_data$Quantity

# Lists
Expenditure_list <- list(Price, Quantity, Expenditure)
print(Expenditure_list)
Expenditure_list[[2]] # second element


