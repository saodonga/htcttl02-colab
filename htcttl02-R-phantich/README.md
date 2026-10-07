# HTCTTL-02 — Phân tích CFA/CB-SEM bằng R (lavaan)

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/saodonga/htcttl02-colab/blob/main/htcttl02-R-phantich/notebook.ipynb)

## Cấu trúc
- `notebook.ipynb`: chạy toàn bộ trên Colab
- `data/sem_output/analysis_sem.R`: script chạy theo thứ tự 1
- `data/sem_output/plot_tidySEM_final.R`: script chạy theo thứ tự 2
- `_shared/`: file dùng chung (hàm hỗ trợ)

## Chạy local
`Rscript <script>` trong thư mục chứa script.

## Gói cần cài
- R/pip: dplyr, ggplot2, lavaan, psych, readr, semPlot, semTools, tidySEM
- apt: graphviz

## Ghi chú
analysis_sem.R đặt set.seed(20261005); bootstrap 5000 lần (khoảng 1–2 phút trên máy local).
