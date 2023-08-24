#FOR FIGURE ONLY

library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
forr<- read_excel("D:/study/summer/final doc/500~500/result/forr.xlsx")

forr_data_frame <- as.data.frame(forr)

forr2<- forr_data_frame[c(FALSE, TRUE, TRUE, FALSE),]
forr2<- forr2 %>% filter(Reference_tree_size==500)


custom_order <- c("Order_correct", "Infraorder_correct", "Superfamily_correct", "Family_correct", "Subfamily_correct")

summary_forr2 <- forr2 %>%
  group_by(Taxonomic_levels, Phylogenetic_methods) %>%
  summarise(
    Mean = mean(Matched_number),
    Variance = var(Matched_number)
  )

# First, create an index column to identify pairs within each 'Tree_building_methods'
summary_forr2  <- summary_forr2 %>%
  group_by(Phylogenetic_methods) %>%
  mutate(Pair_Index = rep(1:(n() / 2), each = 2))

# Then, calculate the total matched number for each pair within each 'Tree_building_methods' group
df_total <- summary_forr2 %>%
  group_by(Phylogenetic_methods, Pair_Index) %>%
  summarise(Total_Matched = sum(Mean), .groups = "drop")

# Join the total matched number with the original data frame
summary_forr2 <- summary_forr2 %>%
  left_join(df_total, by = c("Phylogenetic_methods", "Pair_Index")) %>%
  mutate(Percentage = Mean / Total_Matched * 100)

# Filter out rows with NA values
summary_forr2 <- summary_forr2 %>%
  filter(Taxonomic_levels=="Family_correct" | Taxonomic_levels=="Order_correct" | Taxonomic_levels=="Superfamily_correct" | Taxonomic_levels=="Subfamily_correct" | Taxonomic_levels=="Infraorder_correct")

summary_forr2$Taxonomic_levels <- factor(summary_forr2$Taxonomic_levels, levels = custom_order)
fig2<-ggplot(summary_forr2, aes(x = Taxonomic_levels, y = Percentage, 
                                group = Phylogenetic_methods, color = Phylogenetic_methods)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_dodge(width = 0.9),
            check_overlap = TRUE, 
            vjust = -0.5,
            color = "BLACK",
            size = 3)+
  labs(x = "Taxonomic Levels",
       y = "Mean success ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(
    panel.grid.minor = element_blank(),  # 去掉次要网格线# 去掉主要网格线
    panel.border = element_blank() ) # 去掉边框线


fig2pro <-fig2 + guides(color = guide_legend(title = "Phylogenetic_analyses"))
fig2pro
ggsave("fig5.png", fig2pro, width = 7, height = 7)
