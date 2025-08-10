#All

#Prepare main data


#View data and variables

#View data and variables
#data_wide <- read.csv("C:/Users/Andrey Myslivets/Downloads/data_wide(1).csv")
library(dplyr)

drops <- c()

test <-data_wide$Lymphocytes*data_wide$WBC/100

data_wide["Lymphocytes"]<-data_wide$Lymphocytes*data_wide$WBC/100

for (var in 10:ncol(data_wide)){
  val <- 100-round(sum(is.na(data_wide[var]))/nrow(data_wide)*100,  digits = 2)
  cat(colnames(data_wide[var]), val)
  
  if (val <31) {
    drops <-  append(drops, colnames(data_wide[var]))
  }
  
  
  print("")
}
#clean <- data_wide[, !(names(data_wide) %in% drops)]
clean <- data_wide[!is.na(data_wide$id), ]


result <- clean %>% 
  group_by(id) %>% 
  summarize(days_max = max(DaysBfOutcome)) %>% 
  filter(days_max <= 20)


df_filtered <- data_wide %>% inner_join(result, by = "id")


full_data <- df_filtered[order(clean$id, - clean$DaysBfOutcome), ]
#full_data["Neg"] <- -full_data$DaysBfOutcome

library(MASS)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%


full_data_10 <- full_data[which(full_data$DaysBfOutcome < 11), ]

full_data_10["Neg"] <- -full_data_10$DaysBfOutcome


#Calculate Threshold and FII functions


positive_t <- function(data, i, name){
  
  
  c<-cutpointr(data, !!colnames(data[i]), Outcome, 
               method = maximize_metric, metric = sum_sens_spec,
               posdirection = "<=", pos_class = 0)
  
  best_sens <- c$sensitivity
  best_spec <- c$specificity
  if(best_sens != 1 & best_spec != 1){ 
    best_score <- log(best_sens/(best_sens-1) * best_spec/(best_spec-1))
  }
  # plan 1/
  best_t <- c$optimal_cutpoint
  
  ss<- data.frame(list(type = "gt", name = name, best_threshold = best_t, sensitivity = best_sens, specificity = best_spec, fii_score = best_score))
  return(ss)
}


negative_t <- function(data, i, name){
  
  c<-cutpointr(data, !!colnames(data[i]), Outcome, 
               method = maximize_metric, metric = sum_sens_spec,
               posdirection = ">=", pos_class = 0)
  
  best_sens <- c$sensitivity
  best_spec <- c$specificity
  if(best_sens != 1 & best_spec != 1){ 
    best_score <- log(best_sens/(best_sens-1) * best_spec/(best_spec-1))
  }
  best_t <- c$optimal_cutpoint
  
  ss<- data.frame(list(type = "lt", name = name, best_threshold = best_t, sensitivity = best_sens, specificity = best_spec, fii_score = best_score))
  return(ss)
}

#Create dataset for random val

#install.packages("caret")
library(caret)
#data_wide <- read_csv("Data/Public Health Hackathon 2025/data_wide_no_imputation.csv")

old_score_df <- full_data[!is.na(data_wide$id), ]
set.seed(42)

# Уникальные id
unique_ids <- unique(old_score_df$id)

# Индексы id для train
train_idx <- createDataPartition(unique_ids[!is.na(unique_ids)], p = 0.7, list = FALSE)

# Сами id для train и test
train_ids <- unique_ids[train_idx]
test_ids  <- setdiff(unique_ids, train_ids)


train_data_ids <- unique_ids[train_idx]
test_data_ids  <- unique_ids[-train_idx]

# Фильтрация датафрейма
train_data <- old_score_df %>% filter(id %in% train_data_ids)
test_data  <- old_score_df %>% filter(id %in% test_data_ids)


length(unique(train_data$id))

length(unique(test_data$id))

#Create dataset for random val

data_time <- full_data %>%
  group_by(id) %>%
  slice(1) %>%
  arrange(StartDate) %>% 
  ungroup()

n_time<-round(length(data_time$StartDate)*0.7, digits = 0)
range_date = as.Date(data_time$StartDate[n=n_time])

time_train<-full_data[full_data$StartDate<=range_date, ]
time_test<-full_data[full_data$StartDate>range_date, ]

#Train data and create scores


train_fun <- function(data){
  train <- data
  train_10 <- train[which(train$DaysBfOutcome < 11), ]
  train_10["Neg"] <- -train_10$DaysBfOutcome
  
  FII = data.frame()
  f_pt_day = data.frame()
  s = c()
  median_day <- c()
  
  
  for(i in 8:(ncol(train_10)-3)){
    
    non_nan_full <- train[!is.na(train[i]), ]
    score_data <-data.frame()
    f_pt_day <- data.frame()
    first_pt <- data.frame()
    first_pt_day <- data.frame()
    resp_name <- names(train_10 )[i] 
    
    
    mod <- as.formula(paste(resp_name, "~ Neg + Outcome + Neg*Outcome"))
    robust <- rlm(mod, data = train_10 )
    
    sign = sign(robust$coefficients["Neg:Outcome"])
    
    s<-append(s, c(names(train_10[i]), robust$coefficients["Neg:Outcome"]))
    
    
    f=c()
    
    if (sign(robust$coefficients["Neg:Outcome"])==-1){
      
      non_nan <- train_10[!is.na(train_10[i]), ]
      df_sorted <- non_nan %>% arrange(id, non_nan[i])
      score_data <- df_sorted %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup()
      
      q1 <- quantile(score_data[i], 0.05, na.rm = TRUE)
      q3 <- quantile(score_data[i], 0.95, na.rm = TRUE)
      
      score_filtered <- score_data[score_data[i] > q1 & score_data[i] < q3, ]
      
      f<-negative_t(score_filtered, i, names(score_data[i]))
      th <- f$best_t
      first_pt <-non_nan_full[which(non_nan_full[i] <= th & non_nan_full$Outcome == 1), ]
      first_pt_day <-first_pt[which(first_pt$DaysBfOutcome == max(first_pt$DaysBfOutcome)), ]
      f_pt_day <- rbind(f_pt_day, first_pt_day )
    }
    
    if (sign(robust$coefficients["Neg:Outcome"])==1){
      
      non_nan <- train_10[!is.na(train_10[i]), ]
      df_sorted <- non_nan %>% arrange(id, desc(non_nan[i]))
      score_data <- df_sorted %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup()
      
      q1 <- quantile(score_data[i], 0.05, na.rm = TRUE)
      q3 <- quantile(score_data[i], 0.95, na.rm = TRUE)
      
      score_filtered <- score_data[score_data[i] > q1 & score_data[i] < q3, ]
      
      f<-positive_t(score_filtered, i, names(score_data[i]))
      th <- f$best_t
      first_pt <-non_nan_full[which(non_nan_full[i] >= th & non_nan_full$Outcome == 1), ]
      first_pt_day <-first_pt[which(first_pt$DaysBfOutcome == max(first_pt$DaysBfOutcome)), ]
      f_pt_day <- rbind(f_pt_day, first_pt_day )
    }
    
    median_day <- append(median_day, c(median(f_pt_day$DaysBfOutcome)))
    
    FII <- rbind(FII, f)
  }
  
  FII["median_day"] <- median_day 
  
  ggplot(FII, aes(x= median_day, y= fii_score, colour="green", label=name))+
    geom_point() +geom_text(hjust=0, vjust=0)
  
  
  return(FII)
}

fii_random <- train_fun(train_data)
fii_time <- train_fun(time_train)
fii_all <- train_fun(full_data)

# Final score for random val

FII_Result_rand<-fii_random[which(!is.na(fii_random$fii_score) & fii_random$fii_score>0.8), ]
FII_Result_rand$fii_score <- round(FII_Result_rand$fii_score, digits=0)


rules <- data.frame(
  var = c(FII_Result_rand$name),
  threshold = c(FII_Result_rand$best_threshold),
  direction = c(FII_Result_rand$type),
  points = c(FII_Result_rand$fii_score),
  stringsAsFactors = FALSE
)

score_train_rand <- {
  
  per_rule <- Map(function(varname, thr, dir, pts) {
    v <- train_data[[varname]]
    ok <- if (dir == "gt") (!is.na(v) & v > thr) else (!is.na(v) & v < thr)
    ifelse(ok, pts, 0)
  },
  varname = rules$var,
  thr = rules$threshold,
  dir = rules$direction,
  pts = rules$points)
  
  score <- Reduce("+", per_rule)
  tibble(train_data, score = score)
}

score_test_rand <- {
  
  per_rule <- Map(function(varname, thr, dir, pts) {
    v <- test_data[[varname]]
    ok <- if (dir == "gt") (!is.na(v) & v > thr) else (!is.na(v) & v < thr)
    ifelse(ok, pts, 0)
  },
  varname = rules$var,
  thr = rules$threshold,
  dir = rules$direction,
  pts = rules$points)
  
  score <- Reduce("+", per_rule)
  tibble(test_data, score = score)
}


# Final score for dynamic val

FII_Result_dyn<-fii_time[which(!is.na(fii_time$fii_score) & fii_time$fii_score>0.8), ]
FII_Result_dyn$fii_score <- round(FII_Result_dyn$fii_score, digits=0)


rules <- data.frame(
  var = c(FII_Result_dyn$name),
  threshold = c(FII_Result_dyn$best_threshold),
  direction = c(FII_Result_dyn$type),
  points = c(FII_Result_dyn$fii_score),
  stringsAsFactors = FALSE
)

score_train_dyn <- {
  
  per_rule <- Map(function(varname, thr, dir, pts) {
    v <- train_data[[varname]]
    ok <- if (dir == "gt") (!is.na(v) & v > thr) else (!is.na(v) & v < thr)
    ifelse(ok, pts, 0)
  },
  varname = rules$var,
  thr = rules$threshold,
  dir = rules$direction,
  pts = rules$points)
  
  score <- Reduce("+", per_rule)
  tibble(train_data, score = score)
}

score_test_dyn <- {
  
  per_rule <- Map(function(varname, thr, dir, pts) {
    v <- test_data[[varname]]
    ok <- if (dir == "gt") (!is.na(v) & v > thr) else (!is.na(v) & v < thr)
    ifelse(ok, pts, 0)
  },
  varname = rules$var,
  thr = rules$threshold,
  dir = rules$direction,
  pts = rules$points)
  
  score <- Reduce("+", per_rule)
  tibble(test_data, score = score)
}
