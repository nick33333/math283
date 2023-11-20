library(readr)

# df = readr::read_tsv('D:/School_Wasteland/MATH 283/hw7/hw7data.txt', col_names=FALSE)
df = read.table('D:/School_Wasteland/MATH 283/hw7/hw7data.txt', header=FALSE, sep='\t', row.names = 1)


# x_ij = log(red/green) for spot i on jth BRCA1 array
# x_ij <0 control expressed more
# x_ij >0 tumor expressed more
# x_ij = 0 equally expressed


spotIDs = rownames(df)
befores = df[,1:9]
afters = df[,10:18]

# Preparation
# (a) 
d_js = befores - afters
t_test_t_vals = c()
t_test_p_vals = c()
ct = 1
for (i in spotIDs) {
  idx = as.character(i)
  t_test = t.test(unlist(befores[idx,]), unlist(afters[idx,]), paired=TRUE)
  t_test_t_vals[ct] = as.numeric(t_test$statistic)
  t_test_p_vals[ct] = as.numeric(t_test$p.value)
  ct = ct + 1
}
t_test_df = data.frame(spotID = spotIDs, p_val = t_test_p_vals, t_stat = t_test_t_vals)
t_test_df = t_test_df[order(t_test_df$p_val), ]

# (b)
wilcoxon_tests_w_vals = c()
wilcoxon_tests_p_vals = c()
ct = 1
for (i in spotIDs) {
  idx = as.character(i)
  wilcoxon_test = wilcox.test(unlist(befores[idx,]), unlist(afters[idx,]), paired=TRUE, exact=FALSE)
  wilcoxon_tests_w_vals[ct] = as.numeric(wilcoxon_test$statistic)
  wilcoxon_tests_p_vals[ct] = as.numeric(wilcoxon_test$p.value)
  ct = ct + 1
}
wilcoxon_test_df = data.frame(spotID = spotIDs, p_val = wilcoxon_tests_p_vals, wilcoxon_stat = wilcoxon_tests_w_vals)
wilcoxon_test_df = wilcoxon_test_df[order(wilcoxon_test_df$p_val), ]

# H-701
# (a)
t_test_df[1:10,]
#(b)
wilcoxon_test_df[1:10,]

# H-702
a = 0.05
# (i) for t-test
# t-test results are regarded as truths
truth_reject_h0 = t_test_df[t_test_df$p_val < 0.05, ]$spotID
truth_accept_h0 = t_test_df[which(t_test_df$p_val >= 0.05), ]$spotID
# Wilcoxon test results are regarded as predictions
preds_reject_h0 = wilcoxon_test_df[wilcoxon_test_df$p_val < 0.05, ]$spotID
preds_accept_h0 = wilcoxon_test_df[wilcoxon_test_df$p_val >= 0.05, ]$spotID

true_negatives = length(intersect(truth_accept_h0, preds_accept_h0))
true_positives = length(intersect(truth_reject_h0, preds_reject_h0))
false_positives= length(intersect(truth_accept_h0, preds_reject_h0))
false_negatives = length(intersect(truth_reject_h0, preds_accept_h0))

# Create a confusion matrix
conf_matrix <- matrix(c(true_negatives, false_negatives, true_positives),
                      nrow = 2, byrow = TRUE,
                      dimnames = list(Predicted = c("Accept H0", "Reject H0"),
                                      Actual = c("Accept H0", "Reject H0")))

# Print the confusion matrix
print(conf_matrix)

# (ii) 
t1_error_rate = false_positives/true_negatives
t2_error_rate = false_negatives/true_positives
print(paste("Type I Error Rate (False Positive Rate):", t1_error_rate))
print(paste("Type II Error Rate (False Negative Rate):", t2_error_rate))


# (iv)
# estimated_fps = a * dim(befores)[1]
# FDR = estimated_fps/sum(wilcoxon_test_df$p_val < a)
PPV = ((true_positives) / (true_positives + false_positives))
FDR = 1 - PPV

# > FDR
# [1] 0.07395498

# H-703

# (a)
hist(t_test_df$t_stat, breaks = 20, xlab='t-Test Statistic', main='t-Test Statistic Distribution')

# (b)
hist(wilcoxon_test_df$wilcoxon_stat, breaks = 20, xlab='Wilcoxon Test Statistic',  main='Wilcoxon Test Statistic Distribution')

# H-704

# (a)
hist(t_test_df$p_val, breaks = 20, xlab='P-Value', main='P-Value Distribution for t-Tests')

# (b)
hist(wilcoxon_test_df$p_val, breaks = 20, xlab='P-Value',  main='P-Value Distribution Wilcoxon Tests')

# H-705
library(ggplot2)
x = t_test_df[spotIDs,]$p_val
y = wilcoxon_test_df[spotIDs,]$p_val
data = data.frame(x=x, y=y)
ggplot(data = data.frame(x, y), aes(x, y)) +
  geom_point(alpha = 0.2) +
  labs(title = "Paired t-Test VS Wilcoxon Test P-Values",
       x = 'Paired t-Test P-Value',
       y = 'Wilcoxon Test P-Value')

