suppressMessages({library(lavaan); library(tidySEM); library(ggplot2)})
d <- read.csv("seci_sem_data_219_2026-10-05.csv")
m <- '
IQ=~IQ_1+IQ_2+IQ_3+IQ_4
CBPP=~CBPP_1+CBPP_2+CBPP_3+CBPP_4
CBTT=~CBTT_1+CBTT_2+CBTT_3+CBTT_4
CBCN=~CBCN_1+CBCN_2+CBCN_3
TRU=~TRU_1+TRU_2+TRU_3+TRU_4
COM=~COM_1+COM_2+COM_3+COM_4
COI=~COI_1+COI_2+COI_3
CBTT~IQ; CBCN~IQ; CBPP~IQ+CBTT+CBCN
TRU~CBPP+CBTT+CBCN; COM~TRU; COI~TRU'
fit <- sem(m, d, estimator="MLR")
ss <- standardizedSolution(fit); ss <- ss[ss$op=="~",]
r2 <- lavInspect(fit,"rsquare")
star <- function(p) ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","")))
nm <- c(IQ="Minh bạch\nthông tin (IQ)", CBTT="Công bằng\nthủ tục (CBTT)", CBCN="Công bằng\ncông nhận (CBCN)",
        CBPP="Công bằng\nphân phối (CBPP)", TRU="Niềm tin\n(TRU)", COM="Tuân thủ\nđóng góp (COM)", COI="Ý định\nhợp tác (COI)")
lab <- sapply(names(nm), function(k) if (k %in% names(r2)) sprintf("%s\nR²=%.3f", nm[k], r2[k]) else nm[k])
nodes <- data.frame(name=names(nm), label=unname(lab), shape="rect", stringsAsFactors=FALSE)
edges <- data.frame(from=ss$rhs, to=ss$lhs,
  label=sprintf("%.3f%s", ss$est.std, star(ss$pvalue)),
  linetype=ifelse(ss$pvalue<.05,"solid","dashed"),
  colour=ifelse(ss$pvalue<.05,"#1b7a3a","#c0392b"),
  connect_from=NA, connect_to=NA, stringsAsFactors=FALSE)
lay <- get_layout("", "CBTT","", "",
                  "IQ","CBPP","TRU","COM",
                  "", "CBCN","", "COI", rows=3)
g <- graph_sem(edges=edges, nodes=nodes, layout=lay, rect_width=1.5, rect_height=0.9, spacing_x=4.2, spacing_y=2.6, text_size=3.4)
g <- g + ggtitle("Kết quả mô hình SEM (N = 219; hệ số chuẩn hóa)",
  subtitle="Xanh liền: p < 0.05; đỏ nét đứt: không có ý nghĩa. * p<0.05, ** p<0.01, *** p<0.001") +
  theme(plot.title=element_text(face="bold", hjust=.5), plot.subtitle=element_text(hjust=.5), plot.background=element_rect(fill="white", colour=NA), panel.background=element_rect(fill="white", colour=NA))
ggsave("plot_tidySEM_final.png", g, width=18, height=8, dpi=150, bg="white")
print(edges[,c("from","to","label")])
