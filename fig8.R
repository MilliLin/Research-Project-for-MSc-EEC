library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
forr<- read_excel("D:/study/summer/final doc/500~500/result/forr.xlsx")

forr_data_frame <- as.data.frame(forr)

forr8<- forr_data_frame[c(FALSE,TRUE, FALSE, FALSE),]
forr8 <- forr8 %>% filter(Phylogenetic_methods == "EPA" | Phylogenetic_methods == "RAxML")

custom_order <- c("Order_correct", "Infraorder_correct", "Superfamily_correct", "Family_correct", "Subfamily_correct", "Genus_correct")

forr8$Taxonomic_levels <- factor(forr8$Taxonomic_levels, levels = custom_order)

summary_forr8 <- forr8 %>%
  group_by(Taxonomic_levels, Phylogenetic_methods, Reference_tree_size) %>%
  summarise(
    Mean = mean(Matched_number),
    Variance = var(Matched_number)
  )

fig8<-ggplot(summary_forr8, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - sqrt(Variance), ymax = Mean + sqrt(Variance)),
                width = 0.25, position = position_dodge(width = 0.9)) +
  labs(x = "Phylogenetic Methods",
       y = "Mean number of correct identifications") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = sprintf("%.1f", Mean)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            color = "#808080",    # Change the text color to gray
            size = 4) +
  facet_grid(rows = vars(Reference_tree_size)) +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(), 
    panel.border = element_blank() ) 

fig8

ggsave("fig8.png", fig8, width = 7.59, height = 7.97, dpi = 300)
