
library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
forc<- read_excel("D:/study/summer/final doc/classifier/forc.xlsx")

forc_data_frame <- as.data.frame(forc)
forc1<- forc_data_frame
forc1<- forc_data_frame[c(FALSE, TRUE, FALSE, FALSE),]


forc1$Taxonomic_levels <- factor(forc1$Taxonomic_levels, levels = custom_order)

# Assuming forr1 is a tibble containing the necessary columns
# Create a custom order for the Levels variable
custom_order <- c("Order_correct", "Infraorder_correct", "Superfamily_correct", "Family_correct", "Subfamily_correct", "Genus_correct")

# Convert the Levels variable to a factor with custom order
forc1$levels <- factor(forc1$Taxonomic_levels, levels = custom_order)

summary_forc1 <- forc1 %>%
  group_by(Taxonomic_levels, Identification_methods) %>%
  summarise(
    Mean = mean(Matched_number),
    Variance = var(Matched_number)
  )

summary_forc1$levels <- factor(summary_forc1$Taxonomic_levels, levels = custom_order)

fig10 <- ggplot(summary_forc1, aes(x = Identification_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - sqrt(Variance), ymax = Mean + sqrt(Variance)),
                width = 0.25, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  labs(x = "Identification methods",
       y = "Mean Matched Number") +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = sprintf("%.1f", Mean)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            color = "#808080",    # Change the text color to black
            size = 3.4) +
  theme(
    panel.grid.minor = element_blank(),  # 去掉次要网格线
    panel.border = element_blank()
  )


fig10
ggsave("fig10.png", plot = fig10, width = 12, height = 7, dpi = 300)

