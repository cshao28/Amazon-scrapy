# 加载包
library(dplyr)
library(ggplot2)
library(readr)
library(MASS)
library(reshape2)
library(ggplot2)
library(mgcv)
library(gratia)

###############################################################################
###############################Read and Clean Data#############################
###############################################################################

df <- read_csv("cleaned_data.csv")

str(df)

df$Price <- as.numeric(df$Price)
df$`Purchased in Last Month` <- as.numeric(df$`Purchased in Last Month`)

df$`Parent Category` <- as.factor(df$`Parent Category`)


###############################################################################
################################Rating vs sales################################
###############################################################################
p<-ggplot(df, aes(x = Rating, y = `Purchased in Last Month`)) +
  geom_bin2d(bins = 30) +  # 二维直方图
  scale_fill_gradient(low = "lightblue", high = "red") +
  scale_y_log10() +   # 销量通常右偏，取 log 更直观
  theme_minimal() +
  labs(title = "Rating vs Sales (2D Heatmap)",
       x = "Rating",
       y = "Purchases (log scale)",
       fill = "Count")
ggsave(filename = "/Users/shaochuyan/Desktop/Rating_vs_Sales_heatmap.png",
       plot = p,
       width = 8, height = 6, units = "in", dpi = 300)


###############################################################################
################################Reviews vs Sales###############################
###############################################################################
g<-ggplot(df, aes(x = `Number of Reviews`, y = `Purchased in Last Month`)) +
  geom_point(alpha = 0.5, aes(color = `Parent Category`)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Reviews vs Sales (log-log scale)",
       x = "Number of Reviews (log)",
       y = "Purchases (log)")
ggsave(filename = "reviews_vs_sales_loglog.png",
       plot = g,
       width = 8, height = 6, units = "in", dpi = 300)



###############################################################################
###########################GAM model and Smooth Curve##########################
###############################################################################
names(df)[names(df) == "Purchased in Last Month"] <- "Purchased_Last_Month"

gam_model <- gam(log(Purchased_Last_Month) ~ s(Rating), data = df)
summary(gam_model)

png("smooth_curve.png", width = 8, height = 6, units = "in", res = 300)
plot(gam_model, shade = TRUE, rug = TRUE, main = "Fitted smooth curve for Rating in the GAM")
dev.off()






###############################################################################
##############################Rating vs Sales per Category#####################
###############################################################################

df_clean <- df %>%
  filter(!is.na(`Purchased in Last Month`),
         !is.na(Rating)) %>%
  group_by(`Parent Category`) %>%
  filter(n() > 20)   # 只保留样本量>20的类别

a<-ggplot(df_clean, aes(x = Rating, y = `Purchased in Last Month`)) +
  geom_point(aes(size = `Number of Reviews`), alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~`Parent Category`) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Rating vs Sales per Category (filtered)",
       x = "Rating",
       y = "Purchases (log scale)",
       size = "No. of Reviews")

ggsave(filename = "category.png",
       plot = a,
       width = 8, height = 6, units = "in", dpi = 300)
