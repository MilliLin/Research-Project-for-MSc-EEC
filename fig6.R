library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
forr<- read_excel("D:/study/summer/final doc/500~500/result/forr.xlsx")

forr_data_frame <- as.data.frame(forr)
forr6<- forr_data_frame[c(TRUE, FALSE, FALSE, FALSE),]
forr6<- forr6 %>% filter(Phylogenetic_methods == "EPA" | Phylogenetic_methods == "RAxML")

custom_order <- c("Order", "Infraorder", "Superfamily", "Family", "Subfamily", "Genus")

forr6$Taxonomic_levels <- factor(forr6$Taxonomic_levels, levels = custom_order)

summary_forr6 <- forr6 %>%
  group_by(Phylogenetic_methods, Reference_tree_size, Taxonomic_levels) %>%
  summarise(
    Mean = mean(Matched_number),
    Variance = var(Matched_number)
  )

fig6<-ggplot(summary_forr6, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - sqrt(Variance), ymax = Mean + sqrt(Variance)),
                width = 0.25, position = position_dodge(width = 0.9)) +
  theme_minimal()+
  labs(x = "Phylogenetic analyses",
       y = "Mean number of total identifications") +
  scale_fill_brewer(palette = "Set2")+
  geom_text(aes(label = sprintf("%.1f", Mean)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            color = "#808080",    # Change the text color to black
            size = 4)   +
  facet_grid(rows = vars(Reference_tree_size)) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_blank() 
  )

fig6

ggsave("fig6.png", fig6, width = 7.27, height = 6.87, dpi = 300)
