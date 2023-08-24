
library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
forr<- read_excel("D:/study/summer/final doc/500~500/result/forr.xlsx")

forr_data_frame <- as.data.frame(forr)

forr6<- forr_data_frame[c(FALSE, TRUE, TRUE, FALSE),]
forr6<- forr6 %>% filter(Phylogenetic_methods == "EPA" | Phylogenetic_methods == "RAxML")

summary_forr6 <- forr6 %>%
  group_by(Phylogenetic_methods, Reference_tree_size, Taxonomic_levels) %>%
  summarise(
    Mean = mean(Matched_number),
    Variance = var(Matched_number)
  )

summary_forr6 <- summary_forr6 %>%
  group_by(Phylogenetic_methods) %>%
  mutate(Pair_Index = rep(1:(n() / 2), each = 2))

df_total6 <- summary_forr6 %>%
  group_by(Phylogenetic_methods, Pair_Index) %>%
  summarise(Total_Matched = sum(Mean), .groups = "drop")

summary_forr6 <- summary_forr6 %>%
  left_join(df_total6, by = c("Phylogenetic_methods", "Pair_Index")) %>%
  mutate(Percentage = Mean / Total_Matched * 100)

summary_forr6 <- summary_forr6 %>%
  filter(Taxonomic_levels=="Family_correct" | Taxonomic_levels=="Order_correct" | Taxonomic_levels=="Superfamily_correct" | Taxonomic_levels=="Subfamily_correct" | Taxonomic_levels=="Infraorder_correct" | Taxonomic_levels=="Genus_correct")

custom_order <- c("Order_correct", "Infraorder_correct", "Superfamily_correct", "Family_correct", "Subfamily_correct", "Genus_correct")

summary_forr6$Taxonomic_levels <- factor(summary_forr6$Taxonomic_levels, levels = custom_order)


fig6<-ggplot(summary_forr6, aes(x = Taxonomic_levels, y = Percentage, 
                                group = Phylogenetic_methods, color = Phylogenetic_methods)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_dodge(width = 0.9),
            check_overlap = TRUE, 
            vjust = -0.5,
            color = "BLACK",
            size = 3)+
  labs(
       x = "Taxonomic Levels",
       y = "Mean success ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(rows = vars(Reference_tree_size)) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


fig6pro <-fig6 + guides(color = guide_legend(title = "Phylogenetic analyses"))

fig6pro

ggsave("fig9.png", fig6pro, width = 7, height = 7)
