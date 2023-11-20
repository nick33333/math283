
n_X = 100
p_X = 0.4
x_values = 0:n_X
pdf_X = dbinom(x_values,
               size = n_X,
               prob = p_X)
plot(x_values, pdf_X, type = "o", pch = 20, col = "red", xlab = "Values", ylab = "Probability",
     main = "Probability Density Functions of X and Y")

n_Y = 100
p_Y = 0.3
y_values = 0:n_Y
pdf_Y = dbinom(y_values,
               size = n_Y,
               prob = p_Y)
points(y_values, pdf_Y, type = "o", pch = 22, col = "blue")
legend("topright", legend = c("X ~ Bin(100, 0.4)", "Y ~ Bin(100, 0.3)"), col = c("red", "blue"), pch = c(20, 22))

# Cumulative distribution function (cdf) of X
cdf_X = pbinom(x_values, size = n_X, prob = p_X)
plot(x_values, cdf_X, type = "o", pch = 20, col = "red", xlab = "Values", ylab = "Cumulative Probability",
     main = "Cumulative Distribution Function (CDF) of X ~ Bin(100, 0.4)")







mu = 35
var = 12
sd = sqrt(12)

z1 = (33-mu)/sd
z2 = (38-mu)/sd

a = pnorm(z1) # lower bound
b = pnorm(z2) # upper bound

px33 = a
c_px38 = 1-b
p33x38 = b - a
