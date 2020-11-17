library(openxlsx)
fico_benchmark <- read.xlsx("./plots/FICO_benchmark.xlsx",1)
fico_benchmark[, "train"] <- as.numeric(fico_benchmark[, "train"])
fico_benchmark[, "test"] <- as.numeric(fico_benchmark[, "test"])

library(ggrepel)
ggplot(fico_benchmark, aes(train, test, label = FICO)) +
  geom_point(size = 2) +
  geom_text_repel() + xlab("AUC for train dataset") +
  geom_abline() +
  theme_bw() +
  ylab("AUC for test dataset") + theme(legend.position = "bottom") +
  # ggtitle("Comparison of model performance")  +
  xlim(c(0.76, 1)) +
  ylim(c(0.765, 0.8))

ggsave("./plots/challenger_performance.pdf", width = 7, height = 4.5)
