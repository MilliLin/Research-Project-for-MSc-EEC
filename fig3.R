for500<- read_excel("D:/study/summer/final doc/500~500/result/for500.xlsx")

for500_data_frame <- as.data.frame(for500)


# ���ȣ����ر�Ҫ�Ŀ�
library(ggplot2)
library(dplyr)

for501 <- for500_data_frame %>% filter(Reference_tree_size==500)

# ʹ��dplyr������ƽ��ֵ
for501_summary <- for501 %>%
  group_by(Taxonomic_levels, Phylogenetic_methods) %>%
  summarise(
    Mean = mean(Identification_number))


# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxa_levels����
order_taxa_data <- subset(for501_summary, grepl("^Order_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot1<-ggplot(order_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(a) Order",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# ����y������ֵΪ500
  theme(
    panel.grid.minor = element_blank(),# ȥ����Ҫ������
    panel.border = element_blank()  # ȥ���߿���
  )


# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxa_levels����
infraorder_taxa_data <- subset(for501_summary, grepl("^Infraorder_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot2<-ggplot(infraorder_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(b) Infraorder",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# ����y������ֵΪ500
  theme(
    panel.grid.minor = element_blank(),# ȥ����Ҫ������
    panel.border = element_blank()  # ȥ���߿���
  )

# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxa_levels����
Superfamily_taxa_data <- subset(for501_summary, grepl("^Superfamily_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot3<-ggplot(Superfamily_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(c) Superfamily",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# ����y������ֵΪ500
  theme(
    panel.grid.minor = element_blank(),# ȥ����Ҫ������
    panel.border = element_blank()  # ȥ���߿���
  )


# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxa_levels����
Family_taxa_data <- subset(for501_summary, grepl("^Family_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot4<-ggplot(Family_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(d) Family",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# ����y������ֵΪ500
  theme(
    panel.grid.minor = element_blank(),# ȥ����Ҫ������
    panel.border = element_blank()  # ȥ���߿���
  )

# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxa_levels����
Subfamily_taxa_data <- subset(for501_summary, grepl("^Subfamily_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot5<-ggplot(Subfamily_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(e) Subfamily",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# ����y������ֵΪ500
  theme(
    panel.grid.minor = element_blank(),# ȥ����Ҫ������
    panel.border = element_blank()  # ȥ���߿���
  )
merged_plot <- arrangeGrob(
  plot1, plot2, 
  plot3, plot4, 
  plot5, 
  nrow = 3  # ָ��2��
)

merged_plot
# ����ϲ����ͼ��
ggsave("fig3.png", merged_plot, width = 12, height = 12)