
library(tidyverse)
library(readxl)
library(MASS)    
library(purrr)   
library(broom)
library(caret)
library(gt)
library(binom)      
library(flextable) 


ci95 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n <= 1) return(c(M = mean(x), LCL = NA_real_, UCL = NA_real_))
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  tcrit <- qt(0.975, df = n - 1)
  c(M = m, LCL = m - tcrit * se, UCL = m + tcrit * se)
}

data_for_gr<-score_train_rand
FII_act<- FII_Result_rand

#data_for_gr<-score_test_rand
#FII_act<- FII_Result_rand

#data_for_gr<-score_train_dyn
#FII_act<- FII_Result_dyn

#data_for_gr<-score_test_dyn
#FII_act<- FII_Result_dyn
#00000000000000000000000000000000000000000000000000000000000
summary <- data_for_gr %>%
  filter(!is.na(score), 
         !is.na(DaysBfOutcome), 
         !is.na(Outcome)) %>%
  group_by(Outcome, DaysBfOutcome) %>%
  summarise(
    M   = ci95(score)[["M"]],
    LCL = ci95(score)[["LCL"]],
    UCL = ci95(score)[["UCL"]],
    .groups = "drop"
  )

summary <- summary %>%
  mutate(Outcome = factor(Outcome, 
                          levels = c(0, 1), 
                          labels = c("Discharge", "Death")))

ggplot(summary %>% filter(DaysBfOutcome <= 20), 
       aes(x = DaysBfOutcome,
           y = M,
           color = Outcome )) +
  geom_line() +
  geom_segment(aes(xend = DaysBfOutcome, 
                   y = LCL, 
                   yend = UCL),
               alpha = 0.5) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = c("Discharge" = "#00BFC4", 
                                "Death" = "#F8766B")) +
  scale_x_reverse(breaks = seq(20, 0, 
                               by = -2)) +
  labs(
    x = "Day before outcome",
    y = "Average score, 95% CI",
    title = "Score Trajectory Before Outcome",
    color = "Outcome"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom") 


#000000000000000000000000000000000000000000

max_or_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

score_9_max <- data_for_gr %>%
  group_by(id, Outcome) %>%
  summarise(score = max_or_na(score), .groups = "drop") %>%
  tidyr::drop_na(score)

# Logistic regression: Outcome ~ score
fit <- glm(Outcome ~ score, 
           data = score_9_max, 
           family = binomial())
sum<-sum(FII_act$fii_score)
# Score from 1 to 36
newdat <- tibble(score = 1:sum)

pred_link <- predict(fit, newdata = newdat, type = "link", se.fit = TRUE)
z <- qnorm(0.975)

eta      <- pred_link$fit
se_eta   <- pred_link$se.fit
eta_lcl  <- eta - z * se_eta
eta_ucl  <- eta + z * se_eta

outcome_pr  <- plogis(eta)
ci_low_pr   <- plogis(eta_lcl)
ci_up_pr    <- plogis(eta_ucl)

scores <- newdat %>%
  mutate(
    outcome_pr = outcome_pr,
    ci_low_pr  = ci_low_pr,
    ci_up_pr   = ci_up_pr,
    odd        = outcome_pr / (1 - outcome_pr),
    ci_low_odd = ci_low_pr  / (1 - ci_low_pr),
    ci_up_odd  = ci_up_pr   / (1 - ci_up_pr) 
  )

ggplot(scores, aes(x = score, y = odd, group = 1)) +
  geom_errorbar(aes(ymin = ci_low_odd, ymax = ci_up_odd),
                width = 0.2, color = "#F8766B") +
  geom_point(color = "#F8766B") +
  geom_line(color = "#F8766B") +
  scale_x_continuous(breaks = seq(0, 40, 
                                  by = 2)) +
  scale_y_log10(
    limits = c(0.001, 200),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c("1/1000", "1/100", "1/10", "1", "10", "100")
  ) +
  labs(
    x = "Maximum score",
    y = "Odds + 95% CI"
  ) +
  theme_minimal()

#000000000000000000000000000000000000000000000000000

data <- data.frame(
  `Score range` = c("< 2", "[2, 4)", 
                    "[4, 7)", 
                    "[7, 10)", " >= 10"),
  `Expected death/discharge odds` = c("< 1:100", 
                                      "1:100 - 1:25", 
                                      "1:25 - 1:5", 
                                      "1:5 - 1:1", 
                                      ">1:1"),
  `Risk grade` = c("Very Low", "Low", 
                   "Average", 
                   "High", "Very High"),
  check.names = FALSE
)


data %>%
  gt() %>%
  tab_header(
    title = "Risk Grades by Score Range",
    subtitle = "Based on expected death/discharge odds"
  ) %>%
  data_color(
    columns = "Risk grade",
    colors = scales::col_factor(
      palette = c("#2E8B57", "#3CB371", "#FFD700", "#FF8C00", "#B22222"),
      levels = c("Very Low", "Low", "Average", "High", "Very High")
    )
  ) %>%
  fmt_markdown(columns = everything())

#0000000000000000000000000000000000000000000000000

selected_scores <- scores %>%
  mutate(score = as.numeric(score)) %>%   
  filter(score %in% c(4, 8, 14, 20)) %>%
  arrange(score)

ggplot(selected_scores, aes(x = score, y = odd, group = 1)) +
  geom_errorbar(aes(ymin = ci_low_odd, ymax = ci_up_odd),
                width = 0.2, color = "#F8766B") +
  geom_point(color = "#F8766B") +
  geom_line(color = "#F8766B") +
  scale_y_log10(
    breaks = c(0.5, 1, 2),
    labels = c("1/2", "1", "2/1")
  ) +
  scale_x_continuous(breaks = c(4, 8, 14, 20)) +
  labs(
    x = "Score grades",
    y = "Odds, 95% CI"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_line(linetype = "dashed", linewidth = 0.3)
  )

#00000000000000000000000000000000000000000000000000

score_max <- data_for_gr %>%
  group_by(id, Outcome) %>%
  summarise(score = max(score, na.rm = TRUE))


freq_table <- score_max %>%
  group_by(score) %>%
  summarise(
    deaths = sum(Outcome == 1),
    survived = sum(Outcome == 0),
    N = deaths + survived,
    freq_dead = (100 * deaths / N),
    freq_surv = (100 * survived / N),
    .groups = "drop"
  ) %>%
  arrange(score)


freq_table <- score_max %>%
  group_by(score) %>%
  summarise(
    deaths = sum(Outcome == 1),
    survived = sum(Outcome == 0),
    .groups = "drop"
  ) %>%
  mutate(
    death_share = deaths / (deaths + survived)
  )

ggplot(freq_table, aes(x = score, y = death_share)) +
  geom_line(color = "#F8766B", size = 1) +
  geom_point(color = "#F8766B") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Max score",
    y = "Rate of deaths (%)"
  ) +
  theme_minimal()


work <- freq_table %>%
  arrange(desc(score)) %>%
  mutate(
    TP = cumsum(deaths),
    FP = cumsum(survived)
  )

# Total
tot_dead <- sum(work$deaths, na.rm = TRUE)
tot_surv <- sum(work$survived, na.rm = TRUE)

# TN and FN
work <- work %>%
  mutate(
    FN = tot_dead - TP,
    TN = tot_surv - FP
  )

# 95%CI (asymptotic)
work <- work[!is.na(work$deaths), ]
se_ci <- binom.confint(work$TP, 
                       work$TP + work$FN, 
                       methods = "asymptotic")
sp_ci <- binom.confint(work$TN, 
                       work$TN + work$FP, 
                       methods = "asymptotic")

Sens_Sp <- work %>%
  transmute(
    score,
    TP, FP, TN, FN,
    Sensitivity = 100 * TP / (TP + FN),
    Specificity = 100 * TN / (TN + FP),
    LCL_Se = 100 * se_ci$lower,
    UCL_Se = 100 * se_ci$upper,
    LCL_Sp = 100 * sp_ci$lower,
    UCL_Sp = 100 * sp_ci$upper
  ) %>%
  arrange(score)  


Sens_Sp %>% flextable::flextable() %>% flextable::theme_box()

Sens_long <- Sens_Sp %>%
  tidyr::pivot_longer(
    cols = c(Sensitivity, Specificity),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    LCL = ifelse(Metric == "Sensitivity", LCL_Se, LCL_Sp),
    UCL = ifelse(Metric == "Sensitivity", UCL_Se, UCL_Sp)
  )

# Graph
ggplot(Sens_long, aes(x = score, y = Value, color = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = Metric), alpha = 0.2, color = NA) +
  scale_y_continuous(limits = c(0, 100), name = "Percent (%)") +
  scale_x_continuous(breaks = seq(min(Sens_long$score), max(Sens_long$score), 2)) +
  labs(
    x = "Max Score",
    title = "Sensitivity and Specificity vs Max Score"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())