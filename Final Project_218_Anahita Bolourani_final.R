############################################################
## STAT 218 Final Project (FAST VERSION)
## Anahita Bolourani
## Fall 2025
## - Scenario ERGMs: MPLE + model comparison (AIC/BIC)
## - Aggregated ERGMs: MCMLE (MCMC) + diagnostics + GOF 
## - Valued ERGM (Binomial(6)): MCMLE + diagnostics
## All results -> out_dir/text report; all plots -> out_dir/plots
############################################################

rm(list = ls())
set.seed(218)
options(stringsAsFactors = FALSE)
options(width = 120)

## =========================
## USER SPEED SETTINGS
## =========================
RUN_SCENARIO_MPLE       <- TRUE   # ERGM per scenario (fast MPLE)
RUN_AGGREGATE_MCMLE     <- TRUE   # aggregated mean-vector networks (MCMC)
RUN_VALUED_MCMLE        <- TRUE   # Binomial(6) valued ERGM (MCMC)

## Keep MCMC small (still enough to show diagnostics)
MCMC_BURNIN   <- 500
MCMC_INTERVAL <- 10
MCMC_SAMPLES  <- 200
MCMLE_MAXIT   <- 4

## GOF: fewer simulations to be fast
GOF_NSIM      <- 20
GOF_BURNIN    <- 500
GOF_INTERVAL  <- 20

## Try parallel if available (Windows PSOCK)
NCORES <- max(1, parallel::detectCores(logical = TRUE) - 1)

## =========================
## 0) Packages
## =========================
needed <- c(
  "readxl","dplyr","tidyr","stringr","purrr",
  "network","sna","ergm","ergm.count","ggplot2"
)
to_install <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(network)
  library(sna)
  library(ergm)
  library(ergm.count)
  library(ggplot2)
})

## =========================
## 1) Choose out_dir + start ONE report log
## =========================
choose_out_dir <- function(){
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()){
    out <- rstudioapi::selectDirectory(caption = "Select output directory (out_dir)")
    if (!is.null(out) && nzchar(out)) return(out)
  }
  if (.Platform$OS.type == "windows"){
    out <- utils::choose.dir(caption = "Select output directory (out_dir)")
    if (!is.na(out) && nzchar(out)) return(out)
  }
  out <- readline("Type full path for out_dir and press Enter: ")
  stopifnot(dir.exists(out))
  out
}

out_dir <- choose_out_dir()
dir.create(file.path(out_dir, "plots"), showWarnings = FALSE, recursive = TRUE)

report_path <- file.path(out_dir, "FINAL_PROJECT_REPORT.txt")
con <- file(report_path, open = "wt")
sink(con, split = TRUE)
sink(con, type = "message")

on.exit({
  sink(type = "message")
  sink()
  close(con)
}, add = TRUE)

cat("============================================================\n")
cat("STAT 218 FINAL PROJECT REPORT (FAST VERSION)\n")
cat("Timestamp:", as.character(Sys.time()), "\n")
cat("Output directory:", out_dir, "\n")
cat("Report file:", report_path, "\n")
cat("Cores used (if supported):", NCORES, "\n")
cat("============================================================\n\n")

cat("Session info:\n")
print(sessionInfo())
cat("\n\n")

## =========================
## 2) Read dataset.xlsx and clean
## =========================
cat("=== 2) Reading dataset ===\n")
dataset_path <- file.choose()
cat("Dataset selected:\n", dataset_path, "\n\n")

raw <- read_excel(dataset_path, col_names = TRUE, na = c("#NULL!"))

true_names <- raw %>% slice(1) %>% unlist(use.names = FALSE) %>% as.character()
dat <- raw %>% slice(-1)
true_names[is.na(true_names) | true_names==""] <- paste0("V", which(is.na(true_names) | true_names==""))
names(dat) <- make.unique(true_names)

id_col <- "IPAddress"
scen_col <- "Study"

scenario_levels <- c("Late","Job","Rela","FFrie","Forgot","Partn")

emotion_cols   <- c("sadness","anger","neutral","fear","worry","shame","guilt")
appraisal_cols <- c("Val","Urg","Cope","Expct","Relev","Moral","Cause")

nodal_num_cols <- c(
  "age",
  "extraversion","agreeableness","conscientiousness","neuroticism","openness",
  "STEM_score","STEU_score","gert_score",
  "EMOKFEAT","EMOKSCEN"
)
nodal_cat_cols <- c("gender")

required_cols <- c(id_col, scen_col, nodal_cat_cols, nodal_num_cols, appraisal_cols, emotion_cols)
missing_required <- setdiff(required_cols, names(dat))
if(length(missing_required) > 0){
  stop("Missing required columns: ", paste(missing_required, collapse = ", "))
}

dat <- dat %>%
  mutate(across(all_of(nodal_num_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(across(all_of(c(appraisal_cols, emotion_cols)), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(gender = as.character(.data[["gender"]])) %>%
  filter(.data[[scen_col]] %in% scenario_levels)

cat("Rows after filtering to 6 scenarios:", nrow(dat), "\n\n")

## =========================
## 3) Complete-case filtering -> n=104
## =========================
cat("=== 3) Complete-case filtering ===\n")

cc_ids <- dat %>%
  group_by(.data[[id_col]]) %>%
  filter(n() == 6) %>%
  filter(!if_any(all_of(c(nodal_cat_cols, nodal_num_cols, appraisal_cols, emotion_cols)), is.na)) %>%
  summarise(ok = TRUE, .groups = "drop") %>%
  pull(.data[[id_col]])

dat_cc <- dat %>% filter(.data[[id_col]] %in% cc_ids)

node_ids <- sort(unique(dat_cc[[id_col]]))
n <- length(node_ids)

cat("Unique participants after complete-case filter:", n, "\n")
cat("Rows after filter:", nrow(dat_cc), "\n\n")

nodes <- dat_cc %>%
  group_by(.data[[id_col]]) %>%
  slice(1) %>%
  ungroup() %>%
  select(all_of(c(id_col, nodal_cat_cols, nodal_num_cols))) %>%
  arrange(.data[[id_col]]) %>%
  as.data.frame()

stopifnot(all(nodes[[id_col]] == node_ids))

nodes$gender_num <- suppressWarnings(as.numeric(nodes$gender))
if(any(is.na(nodes$gender_num))){
  g <- tolower(trimws(nodes$gender))
  nodes$gender_num <- ifelse(g %in% c("m","male","man","1"), 1,
                             ifelse(g %in% c("f","female","woman","0"), 0, NA))
}
cat("Gender distribution:\n")
print(table(nodes$gender, useNA="ifany"))
cat("\n\n")

## =========================
## 4) Similarity computation
## =========================
dist_to_similarity <- function(X){
  Xs <- scale(X)
  D <- as.matrix(dist(Xs, method = "euclidean"))
  dmax <- max(D[upper.tri(D)], na.rm = TRUE)
  S <- if(dmax <= 0) matrix(1, nrow(D), ncol(D)) else 1 - (D / dmax)
  diag(S) <- NA_real_
  S <- pmin(pmax(S, 0), 1)
  list(D = D, S = S, dmax = dmax)
}

get_scenario_matrix <- function(df, scenario_name, feature_cols){
  df_s <- df %>% filter(.data[[scen_col]] == scenario_name) %>% arrange(.data[[id_col]])
  stopifnot(all(df_s[[id_col]] == node_ids))
  X <- df_s %>% select(all_of(feature_cols)) %>% as.matrix()
  storage.mode(X) <- "numeric"
  X
}

get_mean_matrix <- function(df, feature_cols){
  df_m <- df %>%
    group_by(.data[[id_col]]) %>%
    summarise(across(all_of(feature_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    arrange(.data[[id_col]])
  stopifnot(all(df_m[[id_col]] == node_ids))
  X <- df_m %>% select(all_of(feature_cols)) %>% as.matrix()
  storage.mode(X) <- "numeric"
  X
}

scenario_similarity <- function(df, feature_cols){
  out <- list()
  for(s in scenario_levels){
    X <- get_scenario_matrix(df, s, feature_cols)
    out[[s]] <- dist_to_similarity(X)
  }
  out
}

cat("=== 4) Computing similarities ===\n")
emo_sim <- scenario_similarity(dat_cc, emotion_cols)
app_sim <- scenario_similarity(dat_cc, appraisal_cols)
emo_mean_sim <- dist_to_similarity(get_mean_matrix(dat_cc, emotion_cols))
app_mean_sim <- dist_to_similarity(get_mean_matrix(dat_cc, appraisal_cols))
cat("Done.\n\n")

## =========================
## 5) Plot helpers
## =========================
gender_colors <- function(g){
  g <- as.factor(g)
  lev <- levels(g)
  pal <- c("dodgerblue3","deeppink3","darkgreen","orange","purple")
  cols <- setNames(pal[seq_along(lev)], lev)
  cols[as.character(g)]
}

plot_similarity_hist <- function(S, title, file){
  v <- S[upper.tri(S)]
  v <- v[!is.na(v)]
  p <- ggplot(data.frame(sim = v), aes(x = sim)) +
    geom_histogram(bins = 40) +
    geom_vline(xintercept = 0.85, linetype = 2) +
    geom_vline(xintercept = 0.90, linetype = 2) +
    labs(title = title, x = "Similarity (0–1)", y = "Dyad count") +
    theme_bw()
  ggsave(file, plot = p, width = 8, height = 5, dpi = 160)
}

plot_degree_hist <- function(deg, title, file){
  p <- ggplot(data.frame(deg = deg), aes(x = deg)) +
    geom_histogram(binwidth = 1) +
    labs(title = title, x = "Degree", y = "Count of nodes") +
    theme_bw()
  ggsave(file, plot = p, width = 8, height = 5, dpi = 160)
}

plot_network_png <- function(net, main, file){
  png(file, width = 1400, height = 1100, res = 160)
  par(mar = c(0.5,0.5,3,0.5))
  plot(net,
       main = main,
       vertex.cex = 1.2,
       vertex.col = gender_colors(get.vertex.attribute(net, "gender")),
       displaylabels = FALSE)
  legend("topleft",
         legend = levels(as.factor(get.vertex.attribute(net, "gender"))),
         col = gender_colors(levels(as.factor(get.vertex.attribute(net, "gender")))),
         pch = 19, bty = "n", cex = 1.0)
  dev.off()
}

## =========================
## 6) Network builders (safe vertex attributes)
## =========================
to_base_vec <- function(x){
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  as.vector(x)
}
set_vattr_safe <- function(net, name, values){
  v <- to_base_vec(values)
  if(length(v) != network.size(net)) stop("Attribute length mismatch: ", name)
  if(name == "gender") v <- as.character(v) else v <- suppressWarnings(as.numeric(v))
  set.vertex.attribute(net, name, v)
  net
}

make_binary_adj <- function(S, thr = 0.90){
  Y <- ifelse(S >= thr, 1, 0)
  diag(Y) <- 0
  Y[is.na(Y)] <- 0
  Y
}

make_count_adj <- function(sim_by_scenario, thr = 0.85){
  ind_list <- lapply(sim_by_scenario, function(obj) ifelse(obj$S >= thr, 1, 0))
  W <- Reduce("+", ind_list)
  diag(W) <- 0
  W[is.na(W)] <- 0
  W
}

make_binary_network <- function(Y){
  net <- network(Y, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
  network.vertex.names(net) <- as.character(nodes[[id_col]])
  
  net <- set_vattr_safe(net, "gender", nodes$gender)
  net <- set_vattr_safe(net, "gender_num", nodes$gender_num)
  for(vn in nodal_num_cols) net <- set_vattr_safe(net, vn, nodes[[vn]])
  net
}

make_valued_network_counts <- function(W, edge_attr="count"){
  net <- network(W, directed = FALSE, loops = FALSE,
                 matrix.type = "adjacency",
                 ignore.eval = FALSE, names.eval = edge_attr)
  network.vertex.names(net) <- as.character(nodes[[id_col]])
  
  net <- set_vattr_safe(net, "gender", nodes$gender)
  net <- set_vattr_safe(net, "gender_num", nodes$gender_num)
  for(vn in nodal_num_cols) net <- set_vattr_safe(net, vn, nodes[[vn]])
  net
}

## =========================
## 7) ERGM controls
## =========================
control_mple <- control.ergm(seed = 218)

control_mcmle <- control.ergm(
  MCMC.burnin   = MCMC_BURNIN,
  MCMC.interval = MCMC_INTERVAL,
  MCMC.samplesize = MCMC_SAMPLES,
  MCMLE.maxit   = MCMLE_MAXIT,
  seed = 218,
  parallel = NCORES,
  parallel.type = "PSOCK"
)

control_gof <- control.gof.ergm(
  nsim = GOF_NSIM,
  MCMC.burnin = GOF_BURNIN,
  MCMC.interval = GOF_INTERVAL,
  parallel = NCORES,
  parallel.type = "PSOCK"
)

safe_ergm <- function(formula, ..., control){
  tryCatch(
    ergm(formula, ..., control = control),
    error = function(e){
      cat("\nERGM ERROR:\n", conditionMessage(e), "\n\n")
      NULL
    }
  )
}

## =========================
## 8) FAST scenario ERGMs: MPLE (still ERGM + homophily + structure)
## =========================
fit_binary_mple_sequence <- function(net, label){
  cat("\n============================================================\n")
  cat("SCENARIO BINARY ERGM (MPLE):", label, "\n")
  cat("============================================================\n")
  
  m <- network.edgecount(net)
  dens <- network.density(net)
  cat("Edges:", m, " Density:", dens, "\n")
  
  if(m < 2 || dens > 0.95){
    cat("Skipping (too sparse or too dense).\n")
    return(invisible(NULL))
  }
  
  ## Keep STRUCTURE + HOMOPHILY blocks, but MPLE so it is fast.
  ## Use gwesp only (more stable than gwesp+gwdsp for many networks).
  f0 <- as.formula(net ~ edges + gwesp(0.5, fixed=TRUE))
  f1 <- as.formula(net ~ edges + gwesp(0.5, fixed=TRUE) +
                     nodematch("gender") + absdiff("age"))
  f2 <- as.formula(net ~ edges + gwesp(0.5, fixed=TRUE) +
                     nodematch("gender") + absdiff("age") +
                     absdiff("EMOKFEAT") + absdiff("EMOKSCEN"))
  f3 <- as.formula(net ~ edges + gwesp(0.5, fixed=TRUE) +
                     nodematch("gender") + absdiff("age") +
                     absdiff("EMOKFEAT") + absdiff("EMOKSCEN") +
                     absdiff("extraversion") + absdiff("agreeableness") +
                     absdiff("conscientiousness") + absdiff("neuroticism") +
                     absdiff("openness"))
  f4 <- as.formula(net ~ edges + gwesp(0.5, fixed=TRUE) +
                     nodematch("gender") + absdiff("age") +
                     absdiff("EMOKFEAT") + absdiff("EMOKSCEN") +
                     absdiff("extraversion") + absdiff("agreeableness") +
                     absdiff("conscientiousness") + absdiff("neuroticism") +
                     absdiff("openness") +
                     absdiff("STEM_score") + absdiff("STEU_score") + absdiff("gert_score"))
  
  forms <- list(M0_structure=f0, M1_demo=f1, M2_EI=f2, M3_B5=f3, M4_IQ=f4)
  
  fits <- lapply(forms, function(ff){
    safe_ergm(ff, estimate="MPLE", control=control_mple)
  })
  
  ## Print summaries (fast)
  for(nm in names(fits)){
    cat("\n---", nm, "---\n")
    if(is.null(fits[[nm]])) cat("FAILED\n") else print(summary(fits[[nm]]))
  }
  
  ## AIC/BIC comparison (MPLE-based; fast)
  tab <- data.frame(
    model = names(fits),
    ok = sapply(fits, function(f) !is.null(f)),
    AIC = sapply(fits, function(f) if(is.null(f)) NA_real_ else AIC(f)),
    BIC = sapply(fits, function(f) if(is.null(f)) NA_real_ else BIC(f))
  )
  cat("\nMPLE Model comparison (scenario):\n")
  print(tab)
  
  invisible(tab)
}

## =========================
## 9) Aggregated binary ERGM: MCMLE + MCMC diagnostics + GOF
## =========================
fit_binary_mcmle_main <- function(net, label){
  cat("\n============================================================\n")
  cat("AGGREGATED BINARY ERGM (MCMLE + MCMC):", label, "\n")
  cat("============================================================\n")
  
  m <- network.edgecount(net)
  dens <- network.density(net)
  cat("Edges:", m, " Density:", dens, "\n\n")
  
  if(m < 2 || dens > 0.95){
    cat("Skipping (too sparse or too dense).\n")
    return(invisible(NULL))
  }
  
  ## Use a strong but not insane “main model”.
  ## (Still has structure + demo + EI + Big5 + IQ)
  f_main <- as.formula(net ~ edges + gwesp(0.5, fixed=TRUE) +
                         nodematch("gender") + absdiff("age") +
                         absdiff("EMOKFEAT") + absdiff("EMOKSCEN") +
                         absdiff("extraversion") + absdiff("agreeableness") +
                         absdiff("conscientiousness") + absdiff("neuroticism") +
                         absdiff("openness") +
                         absdiff("STEM_score") + absdiff("STEU_score") + absdiff("gert_score"))
  
  ## Fit MCMLE but skip expensive loglik bridging:
  fit <- safe_ergm(f_main, eval.loglik=FALSE, control=control_mcmle)
  if(is.null(fit)){
    cat("Main MCMLE fit FAILED.\n")
    return(invisible(NULL))
  }
  
  cat("\n--- MAIN MCMLE MODEL SUMMARY ---\n")
  print(summary(fit))
  
  ## Save MCMC diagnostics plot
  png(file.path(out_dir, "plots", paste0(label, "_MCMC_diagnostics.png")),
      width=1600, height=1200, res=160)
  mcmc.diagnostics(fit)
  dev.off()
  
  ## GOF (fast, few sims)
  g <- gof(fit, GOF = ~ degree + espartners + distance, control=control_gof)
  cat("\n--- GOF SUMMARY (printed) ---\n")
  print(g)
  
  png(file.path(out_dir, "plots", paste0(label, "_GOF.png")),
      width=1600, height=1200, res=160)
  plot(g)
  dev.off()
  
  invisible(fit)
}

## =========================
## 10) Valued count ERGM (Binomial(6)): MCMLE + diagnostics
## =========================
fit_valued_mcmle_main <- function(net_val, label, max_m=6){
  cat("\n============================================================\n")
  cat("VALUED COUNT ERGM (Binomial(6), MCMLE + MCMC):", label, "\n")
  cat("============================================================\n")
  
  resp <- "count"
  W <- as.matrix(net_val, attrname = resp)
  wvals <- W[upper.tri(W)]
  cat("Count distribution summary:\n")
  print(summary(wvals))
  cat("\n")
  
  if(all(wvals == 0, na.rm=TRUE)){
    cat("All counts are 0 -> skipping valued ERGM.\n")
    return(invisible(NULL))
  }
  
  ## HW5 style: sum + nonzero + transitiveties + form='nonzero' covariates
  f_main <- as.formula(net_val ~ sum + nonzero + transitiveties(threshold=1.5) +
                         absdiff("gender_num", form="nonzero") +
                         absdiff("age", form="nonzero") +
                         absdiff("EMOKFEAT", form="nonzero") + absdiff("EMOKSCEN", form="nonzero") +
                         absdiff("extraversion", form="nonzero") + absdiff("agreeableness", form="nonzero") +
                         absdiff("conscientiousness", form="nonzero") + absdiff("neuroticism", form="nonzero") +
                         absdiff("openness", form="nonzero") +
                         absdiff("STEM_score", form="nonzero") + absdiff("STEU_score", form="nonzero") +
                         absdiff("gert_score", form="nonzero"))
  
  fit <- safe_ergm(f_main, response=resp, reference=~Binomial(max_m),
                   eval.loglik=FALSE, control=control_mcmle)
  if(is.null(fit)){
    cat("Valued MCMLE fit FAILED.\n")
    return(invisible(NULL))
  }
  
  cat("\n--- MAIN VALUED MCMLE MODEL SUMMARY ---\n")
  print(summary(fit))
  
  png(file.path(out_dir, "plots", paste0(label, "_MCMC_diagnostics.png")),
      width=1600, height=1200, res=160)
  mcmc.diagnostics(fit)
  dev.off()
  
  invisible(fit)
}

## =========================
## 11) Full pipeline runner (plots for all; MPLE scenarios; MCMC aggregated)
## =========================
run_type <- function(type_name, sim_by_scenario, mean_sim_obj){
  
  cat("\n\n############################################################\n")
  cat("TYPE:", type_name, "\n")
  cat("############################################################\n")
  
  ## Scenario plots + scenario MPLE ERGM
  for(s in scenario_levels){
    S <- sim_by_scenario[[s]]$S
    Y <- make_binary_adj(S, thr=0.90)
    net <- make_binary_network(Y)
    
    plot_similarity_hist(
      S,
      title = paste0(type_name, " — ", s, " — similarity distribution"),
      file  = file.path(out_dir, "plots", paste0(type_name, "_", s, "_similarity_hist.png"))
    )
    plot_network_png(
      net,
      main = paste0(type_name, " similarity network (", s, "), thr=0.90"),
      file = file.path(out_dir, "plots", paste0(type_name, "_", s, "_network.png"))
    )
    
    A <- as.matrix(net)
    deg <- rowSums(A)
    plot_degree_hist(
      deg,
      title = paste0(type_name, " — ", s, " — degree histogram"),
      file  = file.path(out_dir, "plots", paste0(type_name, "_", s, "_degree_hist.png"))
    )
    
    if(RUN_SCENARIO_MPLE){
      fit_binary_mple_sequence(net, label = paste0(type_name, "_", s))
    }
  }
  
  ## Aggregated mean-vector plots + ONE MCMLE ERGM (main)
  Smean <- mean_sim_obj$S
  Ymean <- make_binary_adj(Smean, thr=0.90)
  net_mean <- make_binary_network(Ymean)
  
  plot_similarity_hist(
    Smean,
    title = paste0(type_name, " — MEAN-VECTOR — similarity distribution"),
    file  = file.path(out_dir, "plots", paste0(type_name, "_MEANVECTOR_similarity_hist.png"))
  )
  plot_network_png(
    net_mean,
    main = paste0(type_name, " mean-vector similarity network, thr=0.90"),
    file = file.path(out_dir, "plots", paste0(type_name, "_MEANVECTOR_network.png"))
  )
  Amean <- as.matrix(net_mean)
  deg_mean <- rowSums(Amean)
  plot_degree_hist(
    deg_mean,
    title = paste0(type_name, " — MEAN-VECTOR — degree histogram"),
    file  = file.path(out_dir, "plots", paste0(type_name, "_MEANVECTOR_degree_hist.png"))
  )
  
  if(RUN_AGGREGATE_MCMLE){
    fit_binary_mcmle_main(net_mean, label = paste0(type_name, "_MEANVECTOR_MAIN_MCMLE"))
  }
  
  ## Valued count network across scenarios (>=0.85) + one valued MCMLE
  if(RUN_VALUED_MCMLE){
    W <- make_count_adj(sim_by_scenario, thr=0.85)
    net_val <- make_valued_network_counts(W, edge_attr="count")
    
    wvals <- W[upper.tri(W)]
    p_w <- ggplot(data.frame(w=wvals), aes(x=w)) +
      geom_histogram(binwidth=1) +
      labs(title=paste0(type_name, " — Count ties (>=0.85) across 6 scenarios"),
           x="Count (0..6)", y="Dyad count") +
      theme_bw()
    ggsave(file.path(out_dir, "plots", paste0(type_name, "_COUNT_hist.png")),
           plot=p_w, width=8, height=5, dpi=160)
    
    fit_valued_mcmle_main(net_val, label = paste0(type_name, "_COUNT_MAIN_MCMLE"), max_m=6)
  }
  
  invisible(TRUE)
}

cat("\n=== RUNNING PROJECT PIPELINE (FAST) ===\n")
run_type("EMOTION",  emo_sim, emo_mean_sim)
run_type("APPRAISAL", app_sim, app_mean_sim)

cat("\n============================================================\n")
cat("DONE.\nAll results are in:\n", report_path, "\n")
cat("All plots are in:\n", file.path(out_dir, "plots"), "\n")
cat("============================================================\n")
