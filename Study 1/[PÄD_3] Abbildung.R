library(ggplot2)
library(reshape2)

df <- data.frame(area=c("Large Cities", "Urban Counties", "Rural Counties", "Sparsely Populated"),
                 Distance=c(3.68, 0.84, 0.44, 0.07),
                 Density=c(3.73, 0.08, -0.22, -0.21),
                 Size=c(-0.77, -0.04, -0.22, -0.54))

df$area <- factor(df$area, levels=c("Large Cities", "Urban Counties", "Rural Counties", "Sparsely Populated"))

df <- melt(df)
colnames(df) <- c("area", "variable", "beta")

ggplot(data=df, aes(x=variable, y=beta)) +
  geom_bar(stat="identity", fill="lightblue", color="black", position=position_dodge()) +
  facet_grid(cols=vars(area)) +
  geom_hline(yintercept=0) +
  theme_bw() +
  theme(axis.title.x = element_blank())
