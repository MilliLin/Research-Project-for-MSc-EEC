for500<- read_excel("D:/study/summer/final doc/500~500/result/for500.xlsx")

for500_data_frame <- as.data.frame(for500)


# ���ȣ����ر�Ҫ�Ŀ�
library(ggplot2)
library(dplyr)
for502 <- for500_data_frame %>% filter(Phylogenetic_methods == "EPA" | Phylogenetic_methods == "RAxML")

for502_summary <- for502 %>%
  group_by(Taxonomic_levels, Phylogenetic_methods, Reference_tree_size) %>%
  summarise(
    Mean = mean(Identification_number))

# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxonomic_levels����
order_taxa_data <- subset(for502_summary, grepl("^Order_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot1<-ggplot(order_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(a) Order",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  facet_grid(rows = vars(Reference_tree_size)) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),  # ȥ����Ҫ������# ȥ����Ҫ������
    panel.border = element_blank() ) # ȥ���߿���


# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxonomic_levels����
infraorder_taxa_data <- subset(for502_summary, grepl("^Infraorder_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot2<-ggplot(infraorder_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(b) Infraorder",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  facet_grid(rows = vars(Reference_tree_size)) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),  # ȥ����Ҫ������# ȥ����Ҫ������
    panel.border = element_blank() ) # ȥ���߿���


# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxonomic_levels����
Superfamily_taxa_data <- subset(for502_summary, grepl("^Superfamily_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot3<-ggplot(Superfamily_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(c) Superfamily",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  facet_grid(rows = vars(Reference_tree_size)) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),  # ȥ����Ҫ������# ȥ����Ҫ������
    panel.border = element_blank() ) # ȥ���߿���


# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxonomic_levels����
Family_taxa_data <- subset(for502_summary, grepl("^Family_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot4<-ggplot(Family_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(d) Family",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  facet_grid(rows = vars(Reference_tree_size)) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),  # ȥ����Ҫ������# ȥ����Ҫ������
    panel.border = element_blank() ) # ȥ���߿���


# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxonomic_levels����
Subfamily_taxa_data <- subset(for502_summary, grepl("^Subfamily_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot5<-ggplot(Subfamily_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(e) Subfamily",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  facet_grid(rows = vars(Reference_tree_size)) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),  # ȥ����Ҫ������# ȥ����Ҫ������
    panel.border = element_blank() ) # ȥ���߿���

plot5
# ʹ��subset����ɸѡ��'Order_'��ͷ��Taxonomic_levels����
Genus_taxa_data <- subset(for502_summary, grepl("^Genus_", Taxonomic_levels))

# ������״�ѵ�ͼ
plot6<-ggplot(Genus_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(f) Genus",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  facet_grid(rows = vars(Reference_tree_size)) +
  scale_fill_brewer(palette = "Set2")+
  facet_grid(rows = vars(Reference_tree_size)) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Reference Tree size",
                                         breaks = NULL))+
  theme(
    panel.grid.minor = element_blank(),  # ȥ����Ҫ������# ȥ����Ҫ������
    panel.border = element_blank() ) # ȥ���߿���


merged_plot2 <- arrangeGrob(
  plot1, plot2, plot3, 
  plot4, plot5, plot6,
  nrow = 2  # ָ��2��
)

# ����ϲ����ͼ��
ggsave("fig7.png", merged_plot2, width = 13, height = 9)