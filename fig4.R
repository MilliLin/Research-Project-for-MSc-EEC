
library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
forr<- read_excel("D:/study/summer/final doc/500~500/result/forr.xlsx")

forr_data_frame <- as.data.frame(forr)

forr4<- forr_data_frame[c(FALSE, TRUE, FALSE, FALSE),]
forr4<- forr4 %>% filter(Reference_tree_size==500)

custom_order <- c("Order_correct", "Infraorder_correct", "Superfamily_correct", "Family_correct", "Subfamily_correct")

forr4$Taxonomic_levels <- factor(forr4$Taxonomic_levels, levels = custom_order)

summary_forr4 <- forr4 %>%
  group_by(Taxonomic_levels, Phylogenetic_methods) %>%
  summarise(
    Mean = mean(Matched_number),
    Variance = var(Matched_number)
  )

summary_forr4 <- summary_forr4 %>% filter(!is.na(Taxonomic_levels))

fig4<-ggplot(summary_forr4, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - sqrt(Variance), ymax = Mean + sqrt(Variance)),
                width = 0.25, position = position_dodge(width = 0.9)) +
  labs(x = "Phylogenetic methods",
       y = "Mean number of correct identifications") +
  scale_fill_brewer(palette = "Set2")+
  geom_text(aes(label = sprintf("%.1f", Mean)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            color = "#808080",    # Change the text color to black
            size = 4) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
fig4
ggsave("fig4.png", plot = fig4, width = 6.59, height = 5.57)
