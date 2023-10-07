cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600",
                 "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

dadosvenda <- read.csv("bancos/vendas.csv")
ggplot(dadosvenda) +
  aes(x=Data.Venda, y=Price, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",size=2) +
  labs(x="Ano", y="Preço") +
  theme_estat()
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")