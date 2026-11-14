# Lab 2: Six-group results analysis (COSC 423 results file)
# Author: Bright Joses
# Groups = final letter grade (A-F), the only six-level grouping in the file.
# Outcome 1 = total_score (final mark /100). Note grade is derived from the
# total, so this comparison is partly circular; Outcome 2 = attendance %
# (att_pct) is not used to assign grades and is the cleaner comparison.
# Run from the folder containing COSC_423_Results.csv

suppressPackageStartupMessages(library(ggplot2))
set.seed(42)

# ---- 1. Import and inspect -------------------------------------------------
col_names <- c("id", paste0("att_", 1:20), "att_total", "att_marks", "att_pct",
               "gender", "cgpa_range", "att_score", paste0("q", 1:6), "quiz",
               "a1", "a2", "assignments", "mid_sem", "ca", "python_lab", "r_lab",
               "exam_raw", "exam_30", "main_exam", "total_score", "grade",
               "pass_fail")

raw <- read.csv("COSC_423_Results.csv", skip = 3, header = FALSE,
                col.names = col_names, stringsAsFactors = FALSE)
results <- raw[grepl("^CSA", raw$id), ]
results$group <- factor(results$grade, levels = c("A", "B", "C", "D", "E", "F"))
results$gender <- factor(results$gender)

n_obs  <- nrow(results)
n_vars <- ncol(raw)   # original columns, before adding the factor `group`

# ---- 2. Missingness --------------------------------------------------------
missing_by_col <- colSums(is.na(results) | results == "")
n_missing_cells <- sum(missing_by_col)
pct_missing <- 100 * n_missing_cells / (n_obs * n_vars)

# ---- 3. Data-quality checks (malformed CGPA ranges) ------------------------
rng <- do.call(rbind, strsplit(results$cgpa_range, " - "))
lo <- as.numeric(rng[, 1]); hi <- as.numeric(rng[, 2])
bad_cgpa <- results[hi > 5 | hi < lo, c("id", "cgpa_range")]

# ---- 4. Outliers (1.5 x IQR) -----------------------------------------------
flag_outliers <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE)
  i <- q[2] - q[1]
  x < q[1] - 1.5 * i | x > q[2] + 1.5 * i
}
metrics <- c("total_score", "att_pct", "ca", "main_exam")
outlier_counts <- sapply(metrics, function(m) sum(flag_outliers(results[[m]])))
results$out_total <- flag_outliers(results$total_score)
results$out_att   <- flag_outliers(results$att_pct)

# ---- 5. Group comparison: total score --------------------------------------
fit_t   <- aov(total_score ~ group, data = results)
anova_t <- summary(fit_t)[[1]]
tukey_t <- TukeyHSD(fit_t)$group
welch_t <- oneway.test(total_score ~ group, data = results, var.equal = FALSE)
kw_t    <- kruskal.test(total_score ~ group, data = results)
n_sig_t <- sum(tukey_t[, "p adj"] < 0.05)

# sensitivity: without outliers
fit_t2   <- aov(total_score ~ group, data = results[!results$out_total, ])
anova_t2 <- summary(fit_t2)[[1]]
tukey_t2 <- TukeyHSD(fit_t2)$group

# ---- 6. Group comparison: attendance % (not used to assign grades) ---------
fit_a   <- aov(att_pct ~ group, data = results)
anova_a <- summary(fit_a)[[1]]
tukey_a <- TukeyHSD(fit_a)$group
welch_a <- oneway.test(att_pct ~ group, data = results, var.equal = FALSE)
kw_a    <- kruskal.test(att_pct ~ group, data = results)
n_sig_a <- sum(tukey_a[, "p adj"] < 0.05)

# Equal-variance check (Levene-style: ANOVA on absolute deviations from median)
dev_t <- abs(results$total_score - ave(results$total_score, results$group, FUN = median))
levene_t <- summary(aov(dev_t ~ results$group))[[1]]

# ---- 7. Summary table ------------------------------------------------------
group_summary <- do.call(rbind, lapply(split(results, results$group), function(d) {
  data.frame(group = d$group[1], n = nrow(d),
             score_mean = round(mean(d$total_score), 1),
             score_sd = round(sd(d$total_score), 1),
             score_median = round(median(d$total_score), 1),
             attendance_mean_pct = round(mean(d$att_pct), 1),
             attendance_sd_pct = round(sd(d$att_pct), 1))
}))
rownames(group_summary) <- NULL
write.csv(group_summary, "group_summary.csv", row.names = FALSE)

# ---- 8. Figures ------------------------------------------------------------
fig_score <- ggplot(results, aes(x = group, y = total_score, fill = group)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.45, size = 1.2) +
  theme_minimal() +
  labs(title = "Total score by grade group", x = "Grade group",
       y = "Total score (out of 100)",
       caption = "Points show individual students") +
  theme(legend.position = "none")

fig_att <- ggplot(results, aes(x = group, y = att_pct, fill = group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.45, size = 1.2) +
  theme_minimal() +
  labs(title = "Attendance by grade group", x = "Grade group",
       y = "Attendance (%)", caption = "Points show individual students") +
  theme(legend.position = "none")

ggsave("figure_score_by_group.png", fig_score, width = 7, height = 4.5, dpi = 150)
ggsave("figure_attendance_by_group.png", fig_att, width = 7, height = 4.5, dpi = 150)
