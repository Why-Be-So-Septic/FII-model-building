# ===============================
# CIF (Train vs Test) — Survival-only & Death-only
# Show Gray's test: stat and p on plots
# Drops values above p90
# Saves two PNGs
# ===============================

pkgs <- c("data.table","dplyr","stringr","cmprsk")
new  <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(new)) install.packages(new, repos="https://cloud.r-project.org")
library(data.table); library(dplyr); library(stringr); library(cmprsk)

# --- Settings
train_path <- "train_data.csv"
test_path  <- "test_data.csv"
quantile_cutoff <- 0.9  # drop everything above this quantile

# --- Load
stopifnot(file.exists(train_path), file.exists(test_path))
train_raw <- fread(train_path)
test_raw  <- fread(test_path)

# --- Prepare per id
prep_by_id <- function(dt){
  names(dt) <- str_trim(names(dt))
  dt$DaysBfOutcome <- suppressWarnings(as.numeric(dt$DaysBfOutcome))
  dt$Outcome       <- suppressWarnings(as.integer(dt$Outcome))
  dt %>%
    filter(!is.na(id), !is.na(DaysBfOutcome), !is.na(Outcome)) %>%
    group_by(id) %>%
    summarise(
      DaysBfOutcome = max(DaysBfOutcome, na.rm = TRUE),
      Outcome       = { x <- Outcome[!is.na(Outcome)]
                        if (length(x)==0) NA_integer_ else dplyr::first(x) },
      .groups = "drop"
    )
}
train_days <- prep_by_id(train_raw)
test_days  <- prep_by_id(test_raw)

# --- Drop values above common p90
p90 <- quantile(c(train_days$DaysBfOutcome, test_days$DaysBfOutcome),
                quantile_cutoff, na.rm=TRUE)
train_days <- train_days %>% filter(DaysBfOutcome <= p90)
test_days  <- test_days  %>% filter(DaysBfOutcome <= p90)
cat(sprintf("Using p90 cutoff = %.1f days (dropped values above this)\n", as.numeric(p90)))

# --- Competing risks setup: 1=Death, 2=Survival/Discharge
encode_status <- function(x) ifelse(x==1L, 1L, 2L)
all_c <- rbind(
  data.frame(ftime=train_days$DaysBfOutcome, fstatus=encode_status(train_days$Outcome), group="Train"),
  data.frame(ftime=test_days$DaysBfOutcome,  fstatus=encode_status(test_days$Outcome),  group="Test")
)
all_c$group <- factor(all_c$group, levels=c("Train","Test"))

# --- CIF + Gray's test
ci <- cuminc(ftime = all_c$ftime, fstatus = all_c$fstatus, group = all_c$group)
gray <- ci$Tests

# Helpers to extract stat and p for a given event (1 or 2)
get_idx <- function(tests, ev){
  rn <- rownames(tests); if (is.null(rn)) return(integer(0))
  which(rn == as.character(ev) | grepl(paste0("^", ev, "(\\b|\\s)"), rn))
}
get_p    <- function(tests, ev){ idx <- get_idx(tests, ev); if (!length(idx)) NA_real_ else suppressWarnings(as.numeric(tests[idx[1], "pv"])) }
get_stat <- function(tests, ev){ idx <- get_idx(tests, ev); if (!length(idx)) NA_real_ else suppressWarnings(as.numeric(tests[idx[1], "stat"])) }
fmt_p    <- function(p)   if (is.na(p)) "NA" else if (p < 1e-3) "< 0.001" else sprintf("%.3f", p)
fmt_stat <- function(s)   if (is.na(s)) "NA" else sprintf("%.2f", s)

stat_death <- get_stat(gray, 1); p_death <- get_p(gray, 1)
stat_disc  <- get_stat(gray, 2); p_disc  <- get_p(gray, 2)

# --- Safely get a single curve (event, group) from cuminc
get_curve <- function(ci_obj, event_code, group_label){
  valid <- names(ci_obj)[vapply(ci_obj, function(x) is.list(x) && all(c("time","est") %in% names(x)), logical(1))]
  hits  <- valid[grepl(paste0(" ", event_code, "$"), valid) & grepl(group_label, valid)]
  if (!length(hits)) return(NULL)
  comp <- ci_obj[[hits[1]]]
  if (!length(comp$time)) return(NULL)
  list(time=comp$time, est=comp$est)
}

# --- Manual step-plot with exactly two lines (Train/Test)
plot_two_groups_event <- function(curve_train, curve_test, title_main, subtitle, file_name,
                                  col_train="#1f77b4", col_test="#d62728"){
  png(file_name, width=900, height=600, res=120)
  if (is.null(curve_train) && is.null(curve_test)) {
    plot.new(); title(main=title_main, sub=subtitle, cex.sub=0.9)
    mtext("No curves available within data (after p90 drop).", side=3, line=-1, cex=0.9)
    dev.off(); cat("Saved (empty):", file_name, "\n"); return(invisible(NULL))
  }
  xs <- c(if (!is.null(curve_train)) curve_train$time else numeric(0),
          if (!is.null(curve_test))  curve_test$time  else numeric(0))
  ys <- c(if (!is.null(curve_train)) curve_train$est  else numeric(0),
          if (!is.null(curve_test))  curve_test$est   else numeric(0))
  xlim <- c(min(xs, na.rm=TRUE), max(xs, na.rm=TRUE))
  ylim <- c(0, max(ys, na.rm=TRUE)*1.05)
  plot(NA, NA, xlim=xlim, ylim=ylim, xaxs="i", yaxs="i",
       xlab="Days", ylab="Cumulative incidence",
       main=title_main, sub=subtitle)
  draw_step <- function(curve, col){
    if (is.null(curve)) return()
    o <- order(curve$time); x <- curve$time[o]; y <- curve$est[o]
    y0 <- 0
    for (i in seq_along(x)) {
      segments(ifelse(i==1, x[i], x[i-1]), y0, x[i], y0, col=col, lwd=2)
      segments(x[i], y0, x[i], y[i], col=col, lwd=2)
      y0 <- y[i]
    }
  }
  draw_step(curve_train, col_train)
  draw_step(curve_test,  col_test)
  labs <- c(); cols <- c()
  if (!is.null(curve_train)) { labs <- c(labs,"Train"); cols <- c(cols,col_train) }
  if (!is.null(curve_test))  { labs <- c(labs,"Test");  cols <- c(cols,col_test)  }
  if (length(labs)) legend("bottomright", legend=labs, col=cols, lwd=2, bty="n", cex=0.95)
  dev.off(); cat("Saved:", file_name, "\n")
}

# --- Build the two requested figures (Survival-only and Death-only)
# 1) Survival / Discharge (Outcome=0 -> event=2)
curve_disc_train <- get_curve(ci, event_code=2, group_label="Train")
curve_disc_test  <- get_curve(ci, event_code=2, group_label="Test")
subtitle_disc <- sprintf("Gray's test: stat = %s, p = %s", fmt_stat(stat_disc), fmt_p(p_disc))
plot_two_groups_event(curve_disc_train, curve_disc_test,
                      title_main="CIF — Survival/Discharge (Train vs Test)",
                      subtitle  =subtitle_disc,
                      file_name ="cif_survival_p90.png")

# 2) Death (Outcome=1 -> event=1)
curve_death_train <- get_curve(ci, event_code=1, group_label="Train")
curve_death_test  <- get_curve(ci, event_code=1, group_label="Test")
subtitle_death <- sprintf("Gray's test: stat = %s, p = %s", fmt_stat(stat_death), fmt_p(p_death))
plot_two_groups_event(curve_death_train, curve_death_test,
                      title_main="CIF — Death (Train vs Test)",
                      subtitle  =subtitle_death,
                      file_name ="cif_death_p90.png")

cat("\nDone. Files saved:\n - cif_survival_p90.png\n - cif_death_p90.png\n")

