# Question:
# The joint probability density of two random variables X and Y is
# f(x, y) = 2(2x + 3y)/5; 0 ≤ x, y ≤ 1, 0; elsewhere

# (i) Check if it is a joint density function or not?
f <- function(x, y) {
  return((2 * (2 * x + 3 * y)) / 5)
}

joint_density_check <- integral2(f, 0, 1, 0, 1)
cat("The result of the joint density check is:", joint_density_check, "\n")
cat("Since the result is close to 1, it confirms that f(x, y) is a joint density function.\n\n")

# (ii) Find marginal distribution g(x) at x = 1.
fx <- function(y) {
  return(f(1, y))
}

marginal_distribution_x <- integral(fx, 0, 1)
cat("The marginal distribution g(x) at x = 1 is:", marginal_distribution_x, "\n\n")

# (iii) Find the marginal distribution h(y) at y = 0.
fy <- function(x) {
  return(f(x, 0))
}

marginal_distribution_y <- integral(fy, 0, 1)
cat("The marginal distribution h(y) at y = 0 is:", marginal_distribution_y, "\n\n")

# (iv) Find the expected value of g(x, y) = xy.
fxy <- function(x, y) {
  return(x * y * f(x, y))
}

expected_value <- integral2(fxy, 0, 1, 0, 1)
cat("The expected value of g(x, y) = xy is:", expected_value, "\n")


-----------2---------------

# Create a matrix for joint mass function
joint_mass_function <- matrix(0, nrow = 4, ncol = 3)

# Fill the matrix with the given formula
for (x in 0:3) {
  for (y in 0:2) {
    joint_mass_function[x + 1, y + 1] <- (x + y) / 30
  }
}

# Display the joint mass function
print("Joint Mass Function:")
print(joint_mass_function)




# Check if the sum of probabilities equals 1
is_joint_mass_function <- sum(joint_mass_function) == 1

# Display the result
cat("Is it a joint mass function? ", ifelse(is_joint_mass_function, "Yes", "No"), "\n")


# Marginal distribution for g(x)
marginal_gx <- apply(joint_mass_function, 1, sum)

# Display the marginal distribution for g(x)
print("Marginal distribution g(x):")
print(marginal_gx)



# Marginal distribution for h(y)
marginal_hy <- apply(joint_mass_function, 2, sum)

# Display the marginal distribution for h(y)
print("Marginal distribution h(y):")
print(marginal_hy)

# Conditional probability P(X = 0 | Y = 1)
conditional_prob <- joint_mass_function[1, 2] / marginal_hy[2]

# Display the result
cat("Conditional probability at x = 0 given y = 1:", conditional_prob, "\n")


# Define the values of x and y
x_values <- 0:3
y_values <- 0:2

# Calculate E(x), E(y), E(xy), Var(x), Var(y), Cov(x, y), and correlation coefficient
ex <- sum(x_values * marginal_gx)
ey <- sum(y_values * marginal_hy)
exy <- sum(outer(x_values, y_values) * joint_mass_function)
var_x <- sum((x_values - ex)^2 * marginal_gx)
var_y <- sum((y_values - ey)^2 * marginal_hy)
cov_xy <- exy - ex * ey
correlation_coefficient <- cov_xy / (sqrt(var_x) * sqrt(var_y))

# Display the results
cat("E(x):", ex, "\n")
cat("E(y):", ey, "\n")
cat("E(xy):", exy, "\n")
cat("Var(x):", var_x, "\n")
cat("Var(y):", var_y, "\n")
cat("Cov(x, y):", cov_xy, "\n")
cat("Correlation coefficient:", correlation_coefficient, "\n")

