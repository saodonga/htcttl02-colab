# =============================================================================
# HTCTTL-02 — Phân tích CFA / CB-SEM đầy đủ (phiên bản 2026-10-05)
# Sửa theo review G8 (review_2026-10-03_v1.md):
#   [CRITICAL] biến kiểm soát            -> mục 6 (SEM + controls)
#   [MAJOR]    CMB: Harman + CLF + marker -> mục 4
#   [MAJOR]    alternative model          -> mục 7 (so sánh AIC/BIC, không chỉ nhìn mũi tên)
#   [MINOR]    bổ sung hiệu ứng gián tiếp (bootstrap), HTMT, mô hình đủ chỉ báo (khớp bài báo)
# Dữ liệu: MÔ PHỎNG (tony-data-generator + add_controls.py), n = 219.
# Chạy:  Rscript analysis_sem.R > analysis_sem_output_2026-10-05.txt
# =============================================================================
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(lavaan); library(semTools); library(psych)
})
set.seed(20261005)

args <- commandArgs(trailingOnly = FALSE)
f <- sub("--file=", "", args[grep("--file=", args)])
base_dir <- if (length(f)) dirname(normalizePath(f)) else getwd()
out_dir  <- file.path(base_dir, "results_tables"); dir.create(out_dir, showWarnings = FALSE)
source("../../_shared/agents/skills/r-efa-sem-analyst/funcCFA.R")

d <- read_csv(file.path(base_dir, "seci_sem_data_219_2026-10-05.csv"), show_col_types = FALSE)
cat("n =", nrow(d), " | cột =", ncol(d), "\n")
stopifnot(nrow(d) == 219)

star <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
fmtp <- function(p) ifelse(p < .001, "< 0.001", sprintf("%.3f", p))
save_tbl <- function(x, name) write.csv(x, file.path(out_dir, name), row.names = FALSE, fileEncoding = "UTF-8")
fitrow <- function(fit) {
  m <- fitMeasures(fit, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust",
                          "rmsea.robust","srmr","aic","bic"))
  round(m, 3)
}

# ── 1. Mô tả mẫu & biến kiểm soát ────────────────────────────────────────────
cat("\n================ 1. MÔ TẢ MẪU ================\n")
ctrl_vars <- c("age","gender","education","farm_area","tail_end","income")
desc <- psych::describe(d[, ctrl_vars])[, c("n","mean","sd","min","max")]
print(round(desc, 2)); save_tbl(cbind(variable = rownames(desc), round(desc, 2)), "T0_controls_descriptives.csv")
d$farm_area_log <- log(d$farm_area)
d$age_z <- as.numeric(scale(d$age)); d$farm_area_z <- as.numeric(scale(d$farm_area_log))
d$education_z <- as.numeric(scale(d$education)); d$income_z <- as.numeric(scale(d$income))

# ── 2. CFA (7 nhân tố, đủ chỉ báo như bài báo) ───────────────────────────────
cfa_model <- '
  IQ   =~ IQ_1 + IQ_2 + IQ_3 + IQ_4
  CBPP =~ CBPP_1 + CBPP_2 + CBPP_3 + CBPP_4
  CBTT =~ CBTT_1 + CBTT_2 + CBTT_3 + CBTT_4
  CBCN =~ CBCN_1 + CBCN_2 + CBCN_3
  TRU  =~ TRU_1 + TRU_2 + TRU_3 + TRU_4
  COM  =~ COM_1 + COM_2 + COM_3 + COM_4
  COI  =~ COI_1 + COI_2 + COI_3
'
cat("\n================ 2. CFA (MLR) ================\n")
fit_cfa <- cfa(cfa_model, data = d, estimator = "MLR")
fm <- fitMeasures(fit_cfa, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.robust","tli.robust",
                             "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr"))
print(round(fm, 3))
cat("chi2/df (scaled) =", round(fm["chisq.scaled"] / fm["df.scaled"], 3), "\n")

cr_ave <- funCFA(fit_cfa, ndigit = 3)
cat("\n--- CR, AVE, MSV và ma trận tương quan (đường chéo = sqrt(AVE)) ---\n"); print(cr_ave)

std <- standardizedSolution(fit_cfa) %>% filter(op == "=~")
alpha_tab <- sapply(split(std$rhs, std$lhs), function(items) round(psych::alpha(d[, items], check.keys = FALSE)$total$raw_alpha, 3))
tab1 <- std %>% group_by(Factor = lhs) %>%
  summarise(Items = paste(rhs, collapse = ", "),
            Loading = sprintf("%.3f - %.3f", min(est.std), max(est.std)), .groups = "drop") %>%
  left_join(cr_ave[, c("Latent","CR","AVE")], by = c("Factor" = "Latent")) %>%
  mutate(Alpha = alpha_tab[Factor])
cat("\n--- BẢNG 1 (CFA) ---\n"); print(as.data.frame(tab1)); save_tbl(tab1, "T1_cfa.csv")

cat("\n--- Fornell-Larcker: sqrt(AVE) > |r| ? ---\n")
lv_cor <- lavInspect(fit_cfa, "cor.lv"); sq_ave <- sqrt(cr_ave$AVE); names(sq_ave) <- cr_ave$Latent
fl_ok <- sapply(rownames(lv_cor), function(i) all(sq_ave[i] > abs(lv_cor[i, setdiff(colnames(lv_cor), i)])))
print(fl_ok)
cat("\n--- HTCT: HTMT (ngưỡng 0.85) ---\n")
ht <- semTools::htmt(cfa_model, d); print(round(ht, 3)); cat("HTMT lớn nhất =", round(max(ht[lower.tri(ht)]), 3), "\n")
save_tbl(cbind(Factor = rownames(ht), round(as.data.frame(ht), 3)), "T1b_htmt.csv")

# ── 3. Mô hình cấu trúc cơ sở (H1–H5) ────────────────────────────────────────
sem_model <- paste0(cfa_model, '
  CBPP ~ IQ + CBTT + CBCN
  CBTT ~ IQ
  CBCN ~ IQ
  TRU  ~ CBPP + CBTT + CBCN
  COM  ~ TRU
  COI  ~ TRU
')
hyp <- data.frame(
  H = c("H1a","H1b","H1c","H2a","H2b","H3a","H3b","H3c","H4","H5"),
  lhs = c("CBPP","CBTT","CBCN","CBPP","CBPP","TRU","TRU","TRU","COM","COI"),
  rhs = c("IQ","IQ","IQ","CBTT","CBCN","CBPP","CBTT","CBCN","TRU","TRU"))
path_table <- function(fit) {
  s <- standardizedSolution(fit) %>% filter(op == "~")
  hyp %>% left_join(s, by = c("lhs","rhs")) %>%
    transmute(H, Path = paste(rhs, "->", lhs), Beta = round(est.std, 3), SE = round(se, 3),
              p = fmtp(pvalue), Sig = star(pvalue), Decision = ifelse(pvalue < .05 & est.std > 0, "Ủng hộ", "Không ủng hộ"))
}
cat("\n================ 3. SEM CƠ SỞ (MLR) ================\n")
fit_sem <- sem(sem_model, data = d, estimator = "MLR")
print(fitrow(fit_sem))
tab2 <- path_table(fit_sem); print(tab2); save_tbl(tab2, "T2_sem_baseline.csv")
r2 <- round(lavInspect(fit_sem, "r2"), 3); cat("\nR2:\n"); print(r2)
save_tbl(data.frame(Construct = names(r2), R2 = as.numeric(r2)), "T2b_r2.csv")

# ── 4. Common Method Bias: Harman, CLF, marker variable ──────────────────────
cat("\n================ 4. CMB ================\n")
items_all <- unique(std$rhs)
ev <- eigen(cor(d[, items_all]))$values
cat("Harman (PCA): nhân tố 1 giải thích", round(100 * ev[1] / length(items_all), 1), "% phương sai\n")
fit_h1 <- cfa(paste("G =~", paste(items_all, collapse = " + ")), data = d, estimator = "MLR")
cat("CFA 1 nhân tố (Harman CFA):\n"); print(round(fitMeasures(fit_h1, c("cfi.robust","rmsea.robust","srmr")), 3))

# 4b. Common Latent Factor (Podsakoff et al., 2012; Ding et al., 2023)
latents <- c("IQ","CBPP","CBTT","CBCN","TRU","COM","COI")
clf_syntax <- paste0("CLF =~ ", paste(items_all, collapse = " + "), "\n",
                     paste0("CLF ~~ 0*", latents, collapse = "\n"))
fit_cfa_clf <- tryCatch(cfa(paste(cfa_model, clf_syntax, sep = "\n"), data = d, estimator = "MLR",
                            control = list(iter.max = 5000)), error = function(e) NULL)
if (!is.null(fit_cfa_clf) && lavInspect(fit_cfa_clf, "converged")) {
  cat("\nCFA có CLF — hội tụ.\n"); print(round(fitMeasures(fit_cfa_clf, c("chisq.scaled","df.scaled","cfi.robust","rmsea.robust","srmr")), 3))
  cat("Kiểm định chênh lệch χ² (CFA vs CFA+CLF):\n"); print(lavTestLRT(fit_cfa, fit_cfa_clf))
  clf_l <- standardizedSolution(fit_cfa_clf) %>% filter(op == "=~", lhs == "CLF")
  cat("Phương sai chung do CLF giải thích (trung bình bình phương tải) =",
      round(100 * mean(clf_l$est.std^2), 1), "%\n")
  clf_share <- mean(clf_l$est.std^2)
} else { cat("\nCFA+CLF không hội tụ — ghi nhận là hạn chế.\n"); clf_share <- NA }

fit_sem_clf <- tryCatch(sem(paste(sem_model, clf_syntax, sep = "\n"), data = d, estimator = "MLR",
                            control = list(iter.max = 5000)), error = function(e) NULL)
cmp <- NULL
if (!is.null(fit_sem_clf) && lavInspect(fit_sem_clf, "converged")) {
  tab2_clf <- path_table(fit_sem_clf)
  cmp <- data.frame(H = tab2$H, Path = tab2$Path, Beta_base = tab2$Beta, Beta_CLF = tab2_clf$Beta,
                    Delta = round(tab2_clf$Beta - tab2$Beta, 3), p_base = tab2$p, p_CLF = tab2_clf$p,
                    Decision_base = tab2$Decision, Decision_CLF = tab2_clf$Decision)
  cat("\nSEM với CLF so với SEM cơ sở:\n"); print(cmp)
  cat("Chênh lệch |Δβ| lớn nhất =", max(abs(cmp$Delta)), "\n")
  save_tbl(cmp, "T3_sem_clf_comparison.csv")
} else cat("\nSEM+CLF không hội tụ.\n")

# 4c. Marker variable
cat("\nMarker variable (MRK):\n")
cfa_mrk <- paste0(cfa_model, "  MRK =~ MRK_1 + MRK_2 + MRK_3\n")
fit_mrk <- cfa(cfa_mrk, data = d, estimator = "MLR")
lc <- lavInspect(fit_mrk, "cor.lv")
r_mrk <- lc["MRK", latents]; print(round(r_mrk, 3))
rM <- max(abs(r_mrk)); cat("rM (|r| lớn nhất của MRK với các cấu trúc) =", round(rM, 3), "\n")
pairs <- t(combn(latents, 2)); n <- nrow(d)
mk <- data.frame(Pair = paste(pairs[,1], pairs[,2], sep = " ~ "),
                 r = round(apply(pairs, 1, function(p) lc[p[1], p[2]]), 3))
mk$r_adj <- round((mk$r - rM) / (1 - rM), 3)
mk$p_adj <- 2 * pt(-abs(mk$r_adj * sqrt((n - 2) / (1 - mk$r_adj^2))), n - 2)
mk$Sig_adj <- star(mk$p_adj); mk$p_adj <- fmtp(mk$p_adj)
print(mk); save_tbl(mk, "T3b_marker_adjusted_correlations.csv")

cmb_summary <- data.frame(
  Test = c("Harman PCA, nhân tố 1 (%)","CLF, phương sai chung (%)","Marker rM","max |Δβ| (CLF vs cơ sở)"),
  Value = c(round(100 * ev[1] / length(items_all), 1),
            ifelse(is.na(clf_share), NA, round(100 * clf_share, 1)), round(rM, 3),
            ifelse(is.null(cmp), NA, max(abs(cmp$Delta)))))
print(cmb_summary); save_tbl(cmb_summary, "T3c_cmb_summary.csv")

# ── 5. SEM + biến kiểm soát (review CRITICAL) ───────────────────────────────
cat("\n================ 5. SEM + BIẾN KIỂM SOÁT ================\n")
ctrl_rhs <- "age_z + gender + education_z + farm_area_z + tail_end + income_z"
endo <- c("CBPP","CBTT","CBCN","TRU","COM","COI")
ctrl_syntax <- paste0(endo, " ~ ", ctrl_rhs, collapse = "\n")
fit_sem_c <- sem(paste(sem_model, ctrl_syntax, sep = "\n"), data = d, estimator = "MLR")
print(fitrow(fit_sem_c))
tab2c <- path_table(fit_sem_c)
cmp_c <- data.frame(H = tab2$H, Path = tab2$Path, Beta_base = tab2$Beta, Beta_controls = tab2c$Beta,
                    SE_controls = tab2c$SE, p_controls = tab2c$p, Sig = tab2c$Sig,
                    Delta = round(tab2c$Beta - tab2$Beta, 3), Decision_controls = tab2c$Decision)
print(cmp_c); save_tbl(cmp_c, "T4_sem_with_controls.csv")
cat("max |Δβ| =", max(abs(cmp_c$Delta)), "\n")
ctl <- standardizedSolution(fit_sem_c) %>% filter(op == "~", rhs %in% c("age_z","gender","education_z","farm_area_z","tail_end","income_z")) %>%
  transmute(Outcome = lhs, Control = rhs, Beta = round(est.std, 3), p = fmtp(pvalue), Sig = star(pvalue))
cat("\nHệ số của biến kiểm soát (chỉ các hệ số p < 0.05):\n")
print(ctl %>% filter(Sig != "")); save_tbl(ctl, "T4b_control_coefficients.csv")
cat("R2 (có kiểm soát):\n"); print(round(lavInspect(fit_sem_c, "r2"), 3))

# ── 6. Mô hình thay thế (đảo chiều niềm tin → công bằng) ───────────────────
cat("\n================ 6. MÔ HÌNH THAY THẾ ================\n")
alt_model <- paste0(cfa_model, '
  CBPP ~ IQ + CBTT + CBCN + TRU
  CBTT ~ IQ + TRU
  CBCN ~ IQ
  TRU  ~ CBCN
  COM  ~ TRU
  COI  ~ TRU
')
fit_alt <- sem(alt_model, data = d, estimator = "MLR")
cmp_alt <- rbind(Proposed = fitrow(fit_sem), Alternative = fitrow(fit_alt))
print(cmp_alt); save_tbl(cbind(Model = rownames(cmp_alt), cmp_alt), "T5_alternative_model.csv")
cat("ΔAIC (Alt - Proposed) =", round(AIC(fit_alt) - AIC(fit_sem), 2),
    "| ΔBIC =", round(BIC(fit_alt) - BIC(fit_sem), 2), "\n")
cat("Hai mô hình có df:", fitMeasures(fit_sem, "df"), "và", fitMeasures(fit_alt, "df"),
    "— nếu df bằng nhau và χ² gần như trùng nhau, hai mô hình tương đương về thống kê và dữ liệu cắt ngang không thể phân biệt hướng.\n")

# ── 7. Hiệu ứng gián tiếp (bootstrap 1000) ───────────────────────────────────
cat("\n================ 7. HIỆU ỨNG GIÁN TIẾP (bootstrap) ================\n")
ind_model <- paste0(cfa_model, '
  CBPP ~ a1*IQ + h2a*CBTT + h2b*CBCN
  CBTT ~ a2*IQ
  CBCN ~ a3*IQ
  TRU  ~ b1*CBPP + b2*CBTT + b3*CBCN
  COM  ~ c1*TRU
  COI  ~ c2*TRU
  IQ_TRU_via_CBPP := a1*b1
  IQ_TRU_via_CBTT := a2*b2
  IQ_TRU_via_CBCN := a3*b3
  IQ_TRU_total    := a1*b1 + a2*b2 + a3*b3 + a2*h2a*b1 + a3*h2b*b1
  IQ_COM_total    := (a1*b1 + a2*b2 + a3*b3 + a2*h2a*b1 + a3*h2b*b1)*c1
  IQ_COI_total    := (a1*b1 + a2*b2 + a3*b3 + a2*h2a*b1 + a3*h2b*b1)*c2
')
fit_ind <- tryCatch(sem(ind_model, data = d, se = "bootstrap", bootstrap = 5000,
                        parallel = "multicore", ncpus = 4), error = function(e) { cat("Bootstrap lỗi:", conditionMessage(e), "\n"); NULL })
if (!is.null(fit_ind)) {
  pe <- parameterEstimates(fit_ind, boot.ci.type = "perc", standardized = TRUE) %>% filter(op == ":=") %>%
    transmute(Effect = lhs, Estimate = round(est, 3), Std = round(std.all, 3),
              CI95 = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
              Significant = ifelse(ci.lower > 0 | ci.upper < 0, "Có (CI không chứa 0)", "Không"))
  print(pe); save_tbl(pe, "T6_indirect_effects.csv")
}

# ── 8. Sơ đồ đường dẫn ──────────────────────────────────────────────────────
try({
  library(semPlot)
  png(file.path(base_dir, "path_diagram_sem_2026-10-05.png"), width = 1800, height = 1100, res = 200)
  semPaths(fit_sem, what = "std", whatLabels = "std", layout = "tree2", style = "lisrel", residuals = FALSE,
           intercepts = FALSE, nCharNodes = 0, sizeMan = 4, sizeLat = 9, edge.label.cex = 0.8,
           curvePivot = TRUE, thresholds = FALSE, rotation = 2, structural = TRUE, exoCov = FALSE)
  dev.off()
}, silent = TRUE)

cat("\nXong. Bảng CSV nằm trong:", out_dir, "\n")
sessionInfo()
