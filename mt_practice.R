# Two-sided z-test for means
library(BSDA)

# H602b (i)
n = 9
a = 0.1

sample_data = seq(19, 30, by=1)
test = wilcox.test(sample_data, mu=25, exact = FALSE)
sample_data
mean(sample_data)
# > sample_data
# [1] 19 20 21 22 23 24 25 26 27 28 29 30
# > mean(sample_data)
# [1] 24.5
test$statistic
test$p.value
# > test$statistic
# V 
# 27.5 
# > test$p.value
# [1] 0.6558454
ztest = z.test(sample_data, mu=25, alternative='two.sided', sigma.x=12, conf.level=a)
ztest$statistic
ztest$p.value
# > ztest$statistic
# z 
# -0.1443376 
# > ztest$p.value
# [1] 0.8852339



# H602b (ii)
n = 9
a = 0.1

sample_data = seq(37, 50, by=1)
sample_data
mean(sample_data)
# > sample_data
# [1] 37 38 39 40 41 42 43 44 45 46 47 48 49 50
# > mean(sample_data)
# [1] 43.5
test = wilcox.test(sample_data, mu=25, exact = FALSE)
test$statistic
test$p.value
# > test$statistic
# V 
# 105 
# > test$p.value
# [1] 0.001097051
ztest = z.test(sample_data, mu=25, alternative='two.sided', sigma.x=12, conf.level=a)
ztest$statistic
ztest$p.value
# > ztest$statistic
# z 
# 5.768388 
# > ztest$p.value
# [1] 8.003315e-09



# H602b (iii)
n = 9
a = 0.1

sample_data = seq(25, 34, by=1)
sample_data
mean(sample_data)
# > sample_data
# [1] 25 26 27 28 29 30 31 32 33 34
# > mean(sample_data)
# [1] 29.5
test = wilcox.test(sample_data, mu=25, exact = FALSE)
test$statistic
test$p.value
# > test$statistic
# V 
# 45 
# > test$p.value
# [1] 0.009151689

ztest = z.test(sample_data, mu=25, alternative='two.sided', sigma.x=12, conf.level=a)
ztest$statistic
ztest$p.value
# > ztest$statistic
# z 
# 1.185854 
# > ztest$p.value
# [1] 0.2356799


# H602b (iv)
n = 9
a = 0.1

sample_data = seq(5, 8, by=1)
sample_data
mean(sample_data)
# > sample_data
# [1] 5 6 7 8
# > mean(sample_data)
# [1] 6.5
test = wilcox.test(sample_data, mu=25, exact = FALSE)
test$statistic
test$p.value
# > test = wilcox.test(sample_data, mu=25, exact = FALSE)
# > test$statistic
# V 
# 0 
# > test$p.value
# [1] 0.1003482
ztest = z.test(sample_data, mu=25, alternative='two.sided', sigma.x=12, conf.level=a)
ztest$statistic
ztest$p.value
# > ztest$statistic
# z 
# -3.083333 
# > ztest$p.value
# [1] 0.002046957

library(latex2exp)

a = 0.05
n = 9
sigma = 12
mu0 = 25
mu1 = seq(0, 50, by=0.5)
samples = 400

beta_z_t_w = function(a, n, sigma, mu0, mu1, samples, test_type) {
  power = c()
  for (mui in mu1){
    type_2_errs = 0
    for (samp in 1:samples){
      sample = rnorm(n, mui, sigma)
      
      if (mui) {
        if (test_type == "z") {
          ztest = z.test(sample, mu=mu0, alternative='two.sided', sigma.x=sigma, conf.level=a)
          if (ztest$p.value > a) {
            type_2_errs = type_2_errs + 1
          }
        }
        if (test_type == "t") {
          ttest = t.test(sample, mu=mu0, alternative='two.sided')
          if (ttest$p.value > a) {
            type_2_errs = type_2_errs + 1
          }
        }
        if (test_type == "w") {
          wtest = wilcox.test(sample, mu=mu0, exact=FALSE)
          if (wtest$p.value > a) {
            type_2_errs = type_2_errs + 1
          }
        }
      }

    }
    type_2_err_rate = type_2_errs/samples
    power = c(power, 1- type_2_errs)
    
  }

  return (power)
}


z = beta_z_t_w(a, n, sigma, mu0, mu1, samples, test_type='z')
t = beta_z_t_w(a, n, sigma, mu0, mu1, samples, test_type='t')
w = beta_z_t_w(a, n, sigma, mu0, mu1, samples, test_type='w')

plot(mu1, z, type = 'l', col="red", ylab="Power (1-B)",
     xlab=TeX(sprintf(r'($\mu_{1}$)')),
     main="H-603 (a) Power Curve Comparison")
lines(mu1, t, col="green")
lines(mu1,w, col="blue")
legend("bottomright", legend = c('Z-test', "T-test", "Wilcoxon Test"), col = c("red", "green", "blue"), lty=1)

