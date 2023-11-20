library(latex2exp)
library(ggplot2)


# H401

# a
# Plot PDF of X_max
h401_pdf = function(x, n, k) {
  fx = (x/k)^n
  fx_1 = ((x-1)/k)^n
  return(fx - fx_1)
}

xmax = seq(from=1, to=4, length.out = 4)
k=4

n1 = h401_pdf(x=xmax, n=1, k=k)
n2 = h401_pdf(x=xmax, n=2, k=k)
n3 = h401_pdf(x=xmax, n=3, k=k)
# png(file="h401_c_pmf.png", width = 800, height = 600)
plot(xmax, n1, xlim=c(1,4),
     ylim = c(0,0.6), col = "red", pch = 16,
     ylab = "Probability", xlab = TeX("$X_{max}$"),
     main=TeX("H-401 (c) PDF of $X_{max}$ for k = 4"))
points(xmax, n2, col = "blue", pch = 16,)
points(xmax, n3, col = 'green', pch = 16,)
legend("topleft", legend = c("n=1", "n=2", "n=3"),
       col = c("red", "blue", "green"), pch = 16,
       title = "Values of n")
# Plot CDF of X_max
# png(file="h401_c_cdf.png", width = 800, height = 600)


x = seq(from=1, to=4, length.out=4)
fx = h401_pdf(x=x, n=3, k=4)
Fx = cumsum(fx)
n = length(Fx)
plot(x = NA, y = NA, pch = NA, 
     xlim = c(1, max(x)), 
     ylim = c(0, 1),
     xlab = TeX("$X_{max$}"),
     ylab = "Probability",
     main = TeX("H-401 (c) CDF of $X_{max}$ for k=4, n=3"))
points(x = x, y = Fx, pch=19)
points(x = x+1, y = Fx, pch=1)
for(i in 1:(n)) {
  points(x=x[i+0:1], y=Fx[c(i,i)], type="l")
}



# h401_cdf = function(x, n, k) {
#   return ((x/k)^n)
# }
# n3k4_cdf = h401_cdf(x=xmax, n=3, k=4)
# 
# plot(xmax, n3k4_cdf, xlim=c(1,4),
#      type='s',
#      ylim = c(0,1), col = "black", pch = 16,
#      ylab = "Probability", xlab = TeX("$X_{max}$"),
#      main=TeX("H-401 (c) CDF of $X_{max}$ for k=4, n=3"))
# # dev.off()


# H402
p = 0.25
y = 10
N = 100000
1-(1-p^y)^((1-p)*N)



ymax = log10(1-0.9^(1/700000)) / log10(0.3)



# c
p = seq(from = 0, to = 1, length.out = 100)
a = 0.1
N = 1000000
y_max = log10(1-(1-a)^(1/(N*(1-p)))) / log10(p)

par(mfrow = c(1, 2))


plot(p, y_max, type='l', col='blue', lwd=3,
     main=TeX('$y_{max}$ vs p'),
     ylab=TeX('$y_{max}$'),
     )



plot(p, log10(y_max), type='l', col='blue', lwd=3,
     main=TeX('$log10(y_{max})$ vs p'),
     ylab=TeX('$log10(y_{max})$'),
)

mtext("H-402 (c) a=0.1, N=1000000", side = 3, line = - 1.5, outer = TRUE, cex=1.2)


# d
N = seq(from = 0, to = 1000000, length.out = 1000000)
# y = seq(from = 0, to = 100, length.out = 100)
a = 0.1
p=0.3
# y_val = 1-(1-p^y)^((1-p)*N)
y_max = log10(1-(1-a)^(1/(N*(1-p)))) / log10(p)

par(mfrow = c(1, 2))
plot(N,  y_max, type='l', col='blue', lwd=3,
     main=TeX('$y_{max}$ vs N'),
     ylab=TeX('$y_{max}$')
)

plot(log10(N), y_max, type='l', col='blue', lwd=3,
     main=TeX('$y_{max}$ vs log10(N)'),
     ylab=TeX('$y_{max}$'),
     xlim = c(1,6),
)
mtext("H-402 (d) a=0.1, p=0.3", side = 3, line = - 1.5, outer = TRUE, cex=1.2)


# e
a = seq(from = 0, to = 0.999, length.out = 100)
# y = seq(from = 0, to = 100, length.out = 100)
N=1000000
p=0.3
# y_val = 1-(1-p^y)^((1-p)*N)
y_max = log10(1-(1-a)^(1/(N*(1-p)))) / log10(p)

par(mfrow = c(1, 2))
plot(a, y_max, type='l', col='blue', lwd=3,
     main=TeX('$y_{max}$ vs a'),
     ylab=TeX('$y_{max}$')
)
plot(log10(a), y_max, type='l', col='blue', lwd=3,
     main=TeX('$y_{max}$ vs log10(a)'),
     ylab=TeX('$y_{max}$')
)
mtext("H-402 (e) N=1000000, p=0.3", side = 3, line = - 1.5, outer = TRUE, cex=1.2)

# plot(a, log10(y_max), type='l', col='blue', lwd=3,
#      main=TeX('log10($y_{max}$) vs a'),
#      ylab=TeX('log10($y_{max}$)')
# )
# mtext("H-402 (e) N=1000000, p=0.3", side = 3, line = - 1.5, outer = TRUE, cex=1.2)


# log(a)

log(ymax)

x = (sqrt(21) * qnorm(0.1)) + 30
x
pnorm((25-30)/sqrt(21))



# d

x <- seq(25,100,by = 1)

# Create the binomial distribution.
y <- dbinom(x,100, 0.2)
y
sum(y)


# H404

# a 
data = c(1.7, 1.6, 1.5, 2.3, 1.8)

mean(data) - ((qt(1-0.025, 4) * sd(data)) / sqrt(5))

mean(data) + ((qt(1-0.025, 4) * sd(data)) / sqrt(5))

pnorm(1.507, mean = mean(data), sd = sd(data))
pnorm( 2.052991, mean = mean(data), sd = sd(data))


