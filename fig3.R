for500<- read_excel("D:/study/summer/final doc/500~500/result/for500.xlsx")

for500_data_frame <- as.data.frame(for500)


# 首先，加载必要的库
library(ggplot2)
library(dplyr)

for501 <- for500_data_frame %>% filter(Reference_tree_size==500)

# 使用dplyr来计算平均值
for501_summary <- for501 %>%
  group_by(Taxonomic_levels, Phylogenetic_methods) %>%
  summarise(
    Mean = mean(Identification_number))


# 使用subset函数筛选以'Order_'开头的Taxa_levels数据
order_taxa_data <- subset(for501_summary, grepl("^Order_", Taxonomic_levels))

# 创建柱状堆叠图
plot1<-ggplot(order_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(a) Order",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# 设置y轴的最大值为500
  theme(
    panel.grid.minor = element_blank(),# 去掉主要网格线
    panel.border = element_blank()  # 去掉边框线
  )


# 使用subset函数筛选以'Order_'开头的Taxa_levels数据
infraorder_taxa_data <- subset(for501_summary, grepl("^Infraorder_", Taxonomic_levels))

# 创建柱状堆叠图
plot2<-ggplot(infraorder_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(b) Infraorder",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# 设置y轴的最大值为500
  theme(
    panel.grid.minor = element_blank(),# 去掉主要网格线
    panel.border = element_blank()  # 去掉边框线
  )

# 使用subset函数筛选以'Order_'开头的Taxa_levels数据
Superfamily_taxa_data <- subset(for501_summary, grepl("^Superfamily_", Taxonomic_levels))

# 创建柱状堆叠图
plot3<-ggplot(Superfamily_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(c) Superfamily",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# 设置y轴的最大值为500
  theme(
    panel.grid.minor = element_blank(),# 去掉主要网格线
    panel.border = element_blank()  # 去掉边框线
  )


# 使用subset函数筛选以'Order_'开头的Taxa_levels数据
Family_taxa_data <- subset(for501_summary, grepl("^Family_", Taxonomic_levels))

# 创建柱状堆叠图
plot4<-ggplot(Family_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(d) Family",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# 设置y轴的最大值为500
  theme(
    panel.grid.minor = element_blank(),# 去掉主要网格线
    panel.border = element_blank()  # 去掉边框线
  )

# 使用subset函数筛选以'Order_'开头的Taxa_levels数据
Subfamily_taxa_data <- subset(for501_summary, grepl("^Subfamily_", Taxonomic_levels))

# 创建柱状堆叠图
plot5<-ggplot(Subfamily_taxa_data, aes(x = Phylogenetic_methods, y = Mean, fill = Taxonomic_levels)) +
  geom_bar(stat = "identity") +
  labs(title = "(e) Subfamily",
       x = "Phylogenetic methods",
       y = "Mean various state number") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  ylim(0, 500) +# 设置y轴的最大值为500
  theme(
    panel.grid.minor = element_blank(),# 去掉主要网格线
    panel.border = element_blank()  # 去掉边框线
  )
merged_plot <- arrangeGrob(
  plot1, plot2, 
  plot3, plot4, 
  plot5, 
  nrow = 3  # 指定2行
)

merged_plot
# 保存合并后的图像
ggsave("fig3.png", merged_plot, width = 12, height = 12)
