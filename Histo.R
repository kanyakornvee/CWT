install.packages("gridExtra")
install.packages("cowplot")

library(cowplot)
library(gridExtra)
library(dplyr)
library(tibble)
library(caret)
library(ggplot2)

df1 <- read.csv('../Dataset/Cleaned Data/provider_level_data.csv')

str(df)
df <- filter(df1, cancer_type == "ALL CANCERS")
df$period <- format(as.Date(df$period), "%Y-%m-%d")
df$performance = round(df$performance*100, 2)
df$region_name <- as.factor(df$region_name)
levels(df$region_name) <- list(Unknown = "",
                               London = "LONDON COMMISSIONING REGION",
                               Midlands = "MIDLANDS AND EAST OF ENGLAND COMMISSIONING REGION",
                               North = "NORTH OF ENGLAND COMMISSIONING REGION",
                               South = "SOUTH OF ENGLAND COMMISSIONING REGION")
df$standard <- as.factor(df$standard)
levels(df$standard) <- list(	"31 Days" = "31 Days",
                              "62 Days" = "62 Days",
                              "62 Days - Consult" = "62 Days (Consultant)",
                              "62 Days - Screen" = "62 Days (Screening)",
                              "62 Days - Upgrade" = "62 Days (Upgrade)")
grouped_period <- df %>%
  group_by(period) %>%
  summarise(sum_treated = sum(total_treated))
df_pre <- filter(df, period < "2020-3-1")
str(df)

df_un <- subset(df, region_name == "Unknown")
df_un_pre <- subset(df_pre, region_name == "Unknown")

df_lon <- subset(df, region_name == "London")
df_lon_pre <- subset(df_pre, region_name == "London")

df_mid <- subset(df, region_name == "Midlands")
df_mid_pre <- subset(df_pre, region_name == "Midlands")

df_nor <- subset(df, region_name == "North")
df_nor_pre <- subset(df_pre, region_name == "North")

df_sou <- subset(df, region_name == "South")
df_sou_pre <- subset(df_pre, region_name == "South")

hist_reg_per <- ggplot(df, aes(x=performance, color = 'light grey', 
                               fill = factor(region_name))) + 
  geom_histogram(color = 'grey') +
  facet_grid(vars(region_name),vars(standard), scales = "free") +
  theme(legend.title = element_blank()) +
  labs(title="Performance Across Region", 
       subtitle = "Divided by CWT Standard", 
       x="Frequency", y = "Performance") +
  geom_vline(xintercept = mean(performance), color='grey', linetype = "dotted") +
  theme(strip.text.x = element_text(size = 7)) 

hist_reg_per_bbc <- hist_reg_per + bbc_style() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        strip.text.y = element_text(size = 7)) 



