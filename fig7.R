for500<- read_excel("D:/study/summer/final doc/500~500/result/for500.xlsx")

for500_data_frame <- as.data.frame(for500)


# 首先，加载必要的库
library(ggplot2)
library(dplyr)
for502 <- for500_data_frame %>% filter(Phylogenetic_methods == "EPA" | Phylogenetic_methods == "RAxML")

for502_summary <- for502 %>%
  group_by(Taxonomic_levels, Phylogenetic_methods, Reference_tree_size) %>%
  summarise(
    Mean = mean(Identification_number))

# 使用subset函数筛选以'Order_'开头的Taxonomic_levels数据
order_taxa_data <- subset(for502_summary, grepl("^Order_", Taxonomic_levels))

# 创建柱状堆叠图
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
    panel.grid.minor = element_blank(),  # 去掉次要网格线# 去掉主要网格线
    panel.border = element_blank() ) # 去掉边框线


# 使用subset函数筛选以'Order_'开头的Taxonomic_levels数据
infraorder_taxa_data <- subset(for502_summary, grepl("^Infraorder_", Taxonomic_levels))

# 创建柱状堆叠图
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
    panel.grid.minor = element_blank(),  # 去掉次要网格线# 去掉主要网格线
    panel.border = element_blank() ) # 去掉边框线


# 使用subset函数筛选以'Order_'开头的Taxonomic_levels数据
Superfamily_taxa_data <- subset(for502_summary, grepl("^Superfamily_", Taxonomic_levels))

# 创建柱状堆叠图
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
    panel.grid.minor = element_blank(),  # 去掉次要网格线# 去掉主要网格线
    panel.border = element_blank() ) # 去掉边框线


# 使用subset函数筛选以'Order_'开头的Taxonomic_levels数据
Family_taxa_data <- subset(for502_summary, grepl("^Family_", Taxonomic_levels))

# 创建柱状堆叠图
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
    panel.grid.minor = element_blank(),  # 去掉次要网格线# 去掉主要网格线
    panel.border = element_blank() ) # 去掉边框线


# 使用subset函数筛选以'Order_'开头的Taxonomic_levels数据
Subfamily_taxa_data <- subset(for502_summary, grepl("^Subfamily_", Taxonomic_levels))

# 创建柱状堆叠图
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
    panel.grid.minor = element_blank(),  # 去掉次要网格线# 去掉主要网格线
    panel.border = element_blank() ) # 去掉边框线

plot5
# 使用subset函数筛选以'Order_'开头的Taxonomic_levels数据
Genus_taxa_data <- subset(for502_summary, grepl("^Genus_", Taxonomic_levels))

# 创建柱状堆叠图
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
    panel.grid.minor = element_blank(),  # 去掉次要网格线# 去掉主要网格线
    panel.border = element_blank() ) # 去掉边框线


merged_plot2 <- arrangeGrob(
  plot1, plot2, plot3, 
  plot4, plot5, plot6,
  nrow = 2  # 指定2行
)

# 保存合并后的图像
ggsave("fig7.png", merged_plot2, width = 13, height = 9)
