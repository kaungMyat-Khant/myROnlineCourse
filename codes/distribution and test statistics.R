number <- 23.214
df <- 2

# Generate data for the chi-square distribution
x <- seq(0, 30, length.out = 1000)
y <- dchisq(x, df)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Plot the chi-square distribution
ggplot(data, aes(x, y)) +
  geom_line(color = "blue") +
  geom_area(data = subset(data, x >= number), aes(y = y), fill = "red", alpha = 0.5) +
  geom_vline(xintercept = number, linetype = "dashed", color = "black") +
  ggtitle("Chi-Square Distribution with df = 2") +
  xlab("Chi-Square Value") +
  ylab("Density") +
  annotate("text", x = number, y = 0.01, label = paste("Number =", number), vjust = -1) +
  theme_minimal()
