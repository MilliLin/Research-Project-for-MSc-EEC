#FOR FIGURE ONLY

library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
forr<- read_excel("D:/study/summer/final doc/500~500/result/forr.xlsx")

forr_data_frame <- as.data.frame(forr)

forr2<- forr_data_frame[c(TRUE, FALSE, FALSE, FALSE),]
forr2<- forr2 %>% filter(Reference_tree_size==500)


# Assuming forr1 is a tibble containing the necessary columns
# Create a custom order for the Levels variable
custom_order <- c("Order", "Infraorder", "Superfamily", "Family", "Subfamily")

# Convert the Levels variable to a factor with custom order
forr2$Taxonomic_levels <- factor(forr2$Taxonomic_levels, levels = custom_order)

# Create the ggplot with the custom order
summary_forr2 <- forr2 %>%
  group_by(Phylogenetic_methods, Reference_tree_size, Taxonomic_levels) %>%
  summarise(
    Mean = mean(Matched_number),
    Variance = var(Matched_number)
  )

summary_forr2<- summary_forr2 %>% filter(!is.na(Taxonomic_levels))

fig2<-ggplot(summary_forr2, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
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
            size = 4) +
  theme(
    panel.grid.minor = element_blank(),# 去掉主要网格线
    panel.border = element_blank()  # 去掉边框线
  )

fig2
ggsave("fig2.png", fig2, width = 6.59, height = 5.57, dpi = 300)
