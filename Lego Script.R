library (tidyverse)
library (ggplot2)
library (dplyr)
library (gridExtra)

# Data loading

lego_data <- read_csv(file.choose())

# Data overview

str(lego_data)
view(lego_data)

lego_data %>%
  summarise_all(~ list(unique(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Unique_Values")


##### Data cleaning

# 1. Taking down unnecessary columns.

lego_data <- lego_data %>%
  select(-...1, -Item_Number, -Amazon_Price, -Pages, -Price)

# 2. Remove NA values from "Set_Name"

lego_data <- lego_data %>%
  drop_na(Set_Name)

# 3. Divide weights by KG and LB in different variables.

lego_data <- lego_data %>%
  mutate(
    Weight_kg = str_extract(Weight, "\\d+\\.\\d+(?=Kg)"),  
    Weight_lb = str_extract(Weight, "(?<=\\().*?(?=\\slb)")  
  ) %>%
  mutate(
    Weight_kg = as.numeric(Weight_kg),
    Weight_lb = as.numeric(Weight_lb)
  )%>%
  select(-Weight)

# 4.Removing NA values from ages.

lego_data <- lego_data %>%
  filter(Ages != "Ages_NA")


# 5. New variable "LowerAge", taking the lowest values from "Ages".

lego_data <- lego_data %>%
  mutate(LowerAge = as.numeric(str_extract(Ages, "\\d+")))

# 6. Age groups reduced to 3: Kids, Teens and Adults

lego_data <- lego_data %>%
  mutate(
    LegoKids = ifelse(LowerAge >= 1 & LowerAge <= 10, 1, 0),
    LegoTeens = ifelse(LowerAge >= 11 & LowerAge <= 17, 1, 0),
    LegoAdults = ifelse(LowerAge >= 18, 1, 0)
  )



##### Data visualization

# 1. Lego sets per year and size.

sets_per_year_size <- lego_data %>%
  group_by(Year, Size) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(Year, desc(count))


sets_per_year_size <- sets_per_year_size %>%
  group_by(Year) %>%
  mutate(Size = fct_reorder(Size, count, .desc = TRUE)) %>%
  ungroup()


ggplot(sets_per_year_size, aes(x = Year, y = count, fill = Size)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Lego Sets per Year and Size",
       x = "Year",
       y = "Number of Sets",
       fill = "Size") +
  scale_fill_viridis_d(alpha = 0.5,
                       option = "plasma") +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()  
  )

# 2. Lego sets per availability.

sets_per_year_availability <- lego_data %>%
  filter(!is.na(Availability)) %>%
  group_by(Year, Availability) %>%
  summarize(count = n(), .groups = 'drop')%>%
  arrange(desc(count))%>%
  mutate(Availability = factor(Availability, levels = unique(Availability)))

ggplot(sets_per_year_availability, aes(x = Year, y = count, fill = Availability)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Lego Sets per Year and Availability",
       x = "Year",
       y = "Number of Sets",
       fill = "Availability") +
  scale_fill_viridis_d(alpha = 0.7,
                       option = "turbo") +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()  
  )

# 3. Lego sets per packaging.

sets_per_packaging <- lego_data %>%
  filter(!is.na(Packaging)) %>%  
  group_by(Packaging) %>%
  summarize(count = n(), .groups = 'drop')%>%
  arrange(desc(count))%>%
  mutate(Packaging = factor(Packaging, levels = Packaging))

ggplot(sets_per_packaging, aes(x = "", y = count, fill = Packaging)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Lego Sets per Packaging",
       x = "Packaging",
       y = "Number of Sets",
       fill = "Packaging") +
  scale_fill_viridis_d(alpha = 0.5,
                       option = "turbo") +  
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) 

# 4. Lego sets by year and packaging.


sets_per_year_packaging <- lego_data %>%
  filter(!is.na(Packaging)) %>%  
  group_by(Year, Packaging) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(Year, count)  


sets_per_year_packaging <- sets_per_year_packaging %>%
  group_by(Year) %>%
  mutate(Packaging = fct_reorder(Packaging, count, .desc = TRUE)) %>%
  ungroup()


ggplot(sets_per_year_packaging, aes(x = Year, y = count, fill = Packaging)) +
  geom_bar(stat = "identity", position = "stack") +  
  labs(title = "Number of Lego Sets per Year by Packaging",
       x = "Year",
       y = "Number of Sets",
       fill = "Packaging Type") +
  scale_fill_viridis_d(alpha = 0.6,
                       option = "turbo") +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# 5. Density functions for unique pieces, pieces, weight (kg) and minifigures.


variables_to_plot <- c("unique_pieces", "Pieces", "Weight_kg", "Minifigures")

variable_titles <- c(
  unique_pieces = "Unique Pieces",
  Pieces = "Pieces",
  Weight_kg = "Weight (kg)",
  Minifigures = "Minifigures"
)

plots <- list()

for (var in variables_to_plot) {
  plots[[var]] <- ggplot(lego_data %>% filter(!is.na(get(var)), !is.na(Year)), aes_string(x = var)) +
    geom_histogram(aes(fill = factor(Year)), alpha = 0.7, bins = 30, position = "identity") +
    labs(
      title = paste("Histogram of", variable_titles[[var]], "by Year"),
      x = variable_titles[[var]],
      y = "Count",
      fill = "Year"
    ) +
    scale_fill_viridis_d(alpha = 0.7) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    facet_wrap(~ Year, ncol = 1, scales = "free_y")  
}

for (var in variables_to_plot) {
  print(plots[[var]])
}


# 6. Lego sets correlation between pieces and weight.


model1 <- lm(log10(Weight_kg) ~ log10(Pieces), data = lego_data)
r2_1 <- summary(model1)$r.squared
print(paste("R^2 for Pieces vs Weight:", r2_1))


ggplot(lego_data, aes(x = Pieces, y = Weight_kg, color = Pieces)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "yellow", se = TRUE) +  
  labs(
    title = "Correlation Between Pieces and Weight of Lego Sets",
    x = "Number of Pieces",
    y = "Weight (kg)"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis_c(alpha = 0.5) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2_1, 3)), 
           hjust = 1.1, vjust = 2, size = 4, color = "black")


# 7. Lego sets correlation between unique pieces and pieces.


model2 <- lm(log10(unique_pieces) ~ log10(Pieces), data = lego_data)
r2_2 <- summary(model2)$r.squared
print(paste("R^2 for Pieces vs Unique Pieces:", r2_2))


ggplot(lego_data, aes(x = Pieces, y = unique_pieces)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "yellow", se = TRUE) +  
  labs(
    title = "Correlation Between Pieces and Unique Pieces of Lego Sets",
    x = "Number of Pieces",
    y = "Unique Pieces"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_viridis_c(alpha = 0.5) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2_2, 3)), 
           hjust = 1.1, vjust = 2, size = 4, color = "black")


# 8. Lego sets correlation between unique pieces and weight.


model3 <- lm(log10(Weight_kg) ~ log10(unique_pieces), data = lego_data)
r2_3 <- summary(model3)$r.squared
print(paste("R^2 for Unique Pieces vs Weight:", r2_3))


ggplot(lego_data, aes(x = unique_pieces, y = Weight_kg)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "yellow", se = TRUE) +  
  labs(
    title = "Correlation Between Unique Pieces and Weight of Lego Sets",
    x = "Unique Pieces",
    y = "Weight (kg)"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_viridis_c(alpha = 0.5) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2_3, 3)), 
           hjust = 1.1, vjust = 2, size = 4, color = "black")


# 9. Lego sets correlation between minifigures and pieces


model4 <- lm(Pieces ~ Minifigures, data = lego_data)
r2_4 <- summary(model4)$r.squared
print(paste("R^2 for Minifigures vs Pieces:", r2_4))


ggplot(lego_data, aes(x = Minifigures, y = Pieces)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "yellow", se = TRUE) +  
  labs(
    title = "Correlation Between Minifigures and Pieces of Lego Sets",
    x = "Minifigures",
    y = "Pieces"
  ) +
  scale_colour_viridis_c(alpha = 0.5) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2_4, 3)), 
           hjust = 1.1, vjust = 2, size = 4, color = "black")


# 10. Lego sets correlation between minifigures and unique pieces


model5 <- lm(unique_pieces ~ Minifigures, data = lego_data)
r2_5 <- summary(model5)$r.squared
print(paste("R^2 for Minifigures vs Unique Pieces:", r2_5))


ggplot(lego_data, aes(x = Minifigures, y = unique_pieces)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", color = "yellow", se = TRUE) +  
  labs(
    title = "Correlation Between Minifigures and Unique Pieces of Lego Sets",
    x = "Minifigures",
    y = "Unique Pieces"
  ) +
  scale_colour_viridis_c(alpha = 0.5) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", round(r2_5, 3)), 
           hjust = 1.1, vjust = 2, size = 4, color = "black")


# 11. Lego sets per ages.

sets_per_age <- lego_data %>%
  filter(!is.na(Ages)) %>%  
  group_by(Ages) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  mutate(Ages = factor(Ages, levels = Ages))

ggplot(sets_per_age, aes(x = Ages, y = count, fill = Ages)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Lego Sets by Age Group",
       x = "Age Group",
       y = "Number of Sets") +
  scale_fill_viridis_d(alpha = 0.5) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.x = element_text(angle = 45, hjust = 1))


# 12. Top 10 age groups by Lego sets.

top_ages <- sets_per_age %>%
  arrange(desc(count)) %>%
  head(10)

ggplot(top_ages, aes(x = reorder(Ages, -count), y = count, fill = Ages)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Age Groups by Number of Lego Sets",
       x = "Age Group",
       y = "Number of Sets") +
  scale_fill_viridis_d(alpha = 0.7) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 13. Lego sets per age group (LegoKids, LegoTeens and LegoAdults).

class_counts <- lego_data %>%
  summarize(
    LegoKids = sum(LegoKids, na.rm = TRUE),
    LegoTeens = sum(LegoTeens, na.rm = TRUE),
    LegoAdults = sum(LegoAdults, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Class", values_to = "Count") %>%
  mutate(Class = factor(Class, levels = c("LegoKids", "LegoTeens", "LegoAdults")))


ggplot(class_counts, aes(x = Class, y = Count, fill = Class)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Lego Sets by Age Group",
    x = "Age Group",
    y = "Number of Sets"
  ) +
  scale_fill_viridis_d(alpha = 0.8) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# 14. Top 10 Lego themes by the amount of Lego sets.

sets_per_theme <- lego_data %>%
  filter(!is.na(Theme)) %>%   
  group_by(Theme) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%    
  slice_head(n = 10)          


ggplot(sets_per_theme, aes(x = reorder(Theme, -count), y = count, fill = Theme)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Lego Themes by Number of Sets",
       x = "Theme",
       y = "Number of Sets") +
  theme_minimal() +
  scale_fill_viridis_d(alpha = 0.8,
                       option = "turbo") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  
  )



# 15. Lego sets by top 10 themes and age group.


top_themes <- lego_data %>%
  mutate(total_count = LegoKids + LegoTeens + LegoAdults) %>%
  group_by(Theme) %>%
  summarize(total_count = sum(total_count), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  head(10) %>%
  pull(Theme)


theme_age_group_counts <- lego_data %>%
  filter(Theme %in% top_themes) %>%
  pivot_longer(cols = c(LegoKids, LegoTeens, LegoAdults),
               names_to = "AgeGroup", values_to = "Flag") %>%
  filter(Flag == 1) %>%
  group_by(Theme, AgeGroup) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(
    Theme = factor(Theme, levels = top_themes),  
    AgeGroup = factor(AgeGroup, levels = c("LegoAdults", "LegoTeens", "LegoKids"))  
  )


ggplot(theme_age_group_counts, aes(x = Theme, y = count, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Lego Sets by Theme and Age Group (Top 10 Themes)",
       x = "Theme",
       y = "Number of Sets",
       fill = "Age Group") +
  scale_fill_viridis_d(alpha = 0.7) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()


# 16. Evolution year by year of Lego sets within the top 10 themes.

top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(Theme)


sets_per_year_theme <- lego_data %>%
  filter(Theme %in% top_themes) %>%
  group_by(Year, Theme) %>%
  summarize(count = n(), .groups = 'drop')


ggplot(sets_per_year_theme, aes(x = Year, y = count, color = Theme)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  
  scale_x_continuous(breaks = seq(min(sets_per_year_theme$Year), max(sets_per_year_theme$Year), by = 1)) +  
  labs(
    title = "Evolution of Lego Sets per Year for Top 10 Themes",
    x = "Year",
    y = "Number of Lego Sets",
    color = "Theme"
  ) +
  scale_colour_viridis_d(alpha = 0.5,
                         option = "turbo") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


# 17. Pieces, unique pieces and minifigures count by theme (top 10 themes)


top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(Theme)


summary_data <- lego_data %>%
  filter(Theme %in% top_themes) %>%
  group_by(Theme) %>%
  summarize(
    Pieces = sum(Pieces, na.rm = TRUE),
    Unique_Pieces = sum(unique_pieces, na.rm = TRUE),
    Minifigures = sum(Minifigures, na.rm = TRUE),
    .groups = 'drop'
  )


summary_long <- summary_data %>%
  pivot_longer(cols = c(Pieces, Unique_Pieces, Minifigures),
               names_to = "Variable",
               values_to = "Count") %>%
  mutate(Theme = factor(Theme, levels = top_themes))  


ggplot(summary_long, aes(x = Theme, y = Count, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +  
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 3) + 
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) +  
  labs(title = "Count of Variables Within the Top 10 Themes",
       x = "Theme",
       y = NULL,  
       fill = "Variable") +
  scale_fill_viridis_d(alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        strip.text = element_text(size = 14), 
        panel.spacing = unit(1, "lines"),  
        panel.grid = element_blank(),  
        axis.title.y = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank())


# 18. Top 10 themes: total sets and average pieces per set 


top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Theme)


top_theme_data <- lego_data %>%
  filter(Theme %in% top_themes)


sets_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(total_sets = n(), .groups = 'drop')


avg_pieces_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(avg_pieces = mean(Pieces, na.rm = TRUE), .groups = 'drop')


plot_total_sets <- ggplot(sets_per_theme, aes(x = reorder(Theme, -total_sets), y = total_sets, fill = Theme)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Total Number of Lego Sets by Theme", x = "Theme", y = "Total Sets") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


plot_avg_pieces <- ggplot(avg_pieces_per_theme, aes(x = reorder(Theme, -avg_pieces), y = avg_pieces, fill = Theme)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Average Number of Pieces per Set by Theme", x = "Theme", y = "Average Pieces per Set") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


grid.arrange(plot_total_sets, plot_avg_pieces, ncol = 2)



# 19. Top 10 themes: total sets and average unique pieces per set. 

top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Theme)


top_theme_data <- lego_data %>%
  filter(Theme %in% top_themes)


sets_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(total_sets = n(), .groups = 'drop')


avg_unique_pieces_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(avg_unique_pieces = mean(unique_pieces, na.rm = TRUE), .groups = 'drop')


plot_total_sets <- ggplot(sets_per_theme, aes(x = reorder(Theme, -total_sets), y = total_sets, fill = Theme)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Total Number of Lego Sets by Theme", x = "Theme", y = "Total Sets") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


plot_avg_unique_pieces <- ggplot(avg_unique_pieces_per_theme, aes(x = reorder(Theme, -avg_unique_pieces), y = avg_unique_pieces, fill = Theme)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Average Number of Unique Pieces by Theme", x = "Theme", y = "Average Unique Pieces") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


grid.arrange(plot_total_sets, plot_avg_unique_pieces, ncol = 2)


# 20. Top 10 themes: total sets and average minifigures per set. 

top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Theme)


top_theme_data <- lego_data %>%
  filter(Theme %in% top_themes)


sets_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(total_sets = n(), .groups = 'drop')


avg_minifigures_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(avg_minifigures = mean(Minifigures, na.rm = TRUE), .groups = 'drop')


plot_total_sets <- ggplot(sets_per_theme, aes(x = reorder(Theme, -total_sets), y = total_sets, fill = Theme)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Total Number of Lego Sets by Theme", x = "Theme", y = "Total Sets") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


plot_avg_minifigures <- ggplot(avg_minifigures_per_theme, aes(x = reorder(Theme, -avg_minifigures), y = avg_minifigures, fill = Theme)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Average Number of Minifigures by Theme", x = "Theme", y = "Average Minifigures") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


grid.arrange(plot_total_sets, plot_avg_minifigures, ncol = 2)



# 21. Distribution of pieces across the three age groups.

age_group_data <- lego_data %>%
  select(Pieces, LegoKids, LegoTeens, LegoAdults) %>%
  pivot_longer(cols = c(LegoKids, LegoTeens, LegoAdults), names_to = "AgeGroup", values_to = "Flag") %>%
  filter(Flag == 1)  

age_group_data$AgeGroup <- factor(age_group_data$AgeGroup, levels = c("LegoKids", "LegoTeens", "LegoAdults"))

ggplot(age_group_data, aes(x = AgeGroup, y = Pieces, fill = AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution of Pieces Across Age Groups",
    x = "Age Group",
    y = "Number of Pieces"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )



# 22. Distribution of unique pieces across the three age groups.

age_group_data <- lego_data %>%
  select(unique_pieces, LegoKids, LegoTeens, LegoAdults) %>%
  pivot_longer(cols = c(LegoKids, LegoTeens, LegoAdults), names_to = "AgeGroup", values_to = "Flag") %>%
  filter(Flag == 1)  


age_group_data$AgeGroup <- factor(age_group_data$AgeGroup, levels = c("LegoKids", "LegoTeens", "LegoAdults"))


ggplot(age_group_data, aes(x = AgeGroup, y = unique_pieces, fill = AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution of Unique Pieces Across Age Groups",
    x = "Age Group",
    y = "Number of Unique Pieces"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# 23. Distribution of minifigures across the three age groups.

age_group_data <- lego_data %>%
  select(Minifigures, LegoKids, LegoTeens, LegoAdults) %>%
  pivot_longer(cols = c(LegoKids, LegoTeens, LegoAdults), names_to = "AgeGroup", values_to = "Flag") %>%
  filter(Flag == 1)  


age_group_data$AgeGroup <- factor(age_group_data$AgeGroup, levels = c("LegoKids", "LegoTeens", "LegoAdults"))


ggplot(age_group_data, aes(x = AgeGroup, y = Minifigures, fill = AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution of Minifigures Across Age Groups",
    x = "Age Group",
    y = "Number of Minifigures"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# 24. Distribution with boxplots: Pieces across the top 10 themes.


top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Theme)


top_theme_data <- lego_data %>%
  filter(Theme %in% top_themes)


sets_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(total_sets = n())


top_theme_data <- top_theme_data %>%
  left_join(sets_per_theme, by = "Theme")


ggplot(top_theme_data, aes(x = reorder(Theme, -total_sets), y = Pieces, fill = Theme)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Distribution of Pieces Across Top 10 Lego Themes",
    x = "Theme",
    y = "Number of Pieces"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# 25. Distribution with boxplots: Unique pieces across the top 10 themes.


top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Theme)


top_theme_data <- lego_data %>%
  filter(Theme %in% top_themes)


sets_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(total_sets = n())


top_theme_data <- top_theme_data %>%
  left_join(sets_per_theme, by = "Theme")


ggplot(top_theme_data, aes(x = reorder(Theme, -total_sets), y = unique_pieces, fill = Theme)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Distribution of Unique Pieces Across Top 10 Lego Themes",
    x = "Theme",
    y = "Number of Unique Pieces"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# 26. Distribution with boxplots: Minifigures across the top 10 themes.


top_themes <- lego_data %>%
  count(Theme, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Theme)


top_theme_data <- lego_data %>%
  filter(Theme %in% top_themes)


sets_per_theme <- top_theme_data %>%
  group_by(Theme) %>%
  summarize(total_sets = n())


top_theme_data <- top_theme_data %>%
  left_join(sets_per_theme, by = "Theme")


ggplot(top_theme_data, aes(x = reorder(Theme, -total_sets), y = Minifigures, fill = Theme)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Distribution of Minifigures Across Top 10 Lego Themes",
    x = "Theme",
    y = "Number of Minifigures"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )