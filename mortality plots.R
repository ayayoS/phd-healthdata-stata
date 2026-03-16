
# Import data from an Excel file
overall <- read.csv("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Data/overall.csv", header = T)


# View the first few rows of the data
head(overall)

# Install and load the ggplot2 package 
install.packages("ggplot2")
library(ggplot2)


# Plot
overall_plot <- ggplot(overall, aes(x = year, y = prop)) +
  geom_line(aes(color = "Proportion"), size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% CI"), alpha = 0.5) +
  scale_x_continuous(limits = c(2005, 2020), breaks = seq(2005, 2020, by = 3)) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 3)) +
  scale_color_manual(name = NULL, values = c("Proportion" = "red")) +
  scale_fill_manual(name = NULL, values = c("95% CI" = "pink")) +
  labs(x = "Admission year",
       y = "Mortality proportion (%)") +
theme(legend.position = "bottom", 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 14),  # Bigger axis titles
      axis.text = element_text(size = 12),   # Bigger axis tick labels
      legend.text = element_text(size = 12), # Bigger legend text
      legend.title = element_text(size = 13) # Optional: legend title size
      
      
)

overall_plot

ggsave("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Output/Overall_plot.tiff", plot = overall_plot, dpi = 300, width = 7, height = 5)


# Mortality by AMI subtypes

ami <- read.csv("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Data/ami.csv", header = T)

head(ami)

# Plot
subtypes_plot <- ggplot(ami, aes(x = year, y = prop, color = diag)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = diag), alpha = 0.5, color = NA) +
  scale_x_continuous(limits = c(2005, 2020), breaks = seq(2005, 2020, by = 3)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 3)) +
  scale_color_manual(name = NULL, values = c("NSTEMI" = "blue", "STEMI" = "red")) +
  scale_fill_manual(name = "95% CI", values = c("NSTEMI" = "lightblue", "STEMI" = "pink")) +
  labs(x = "Admission year",
       y = "Mortality proportion (%)") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),  # Bigger axis titles
        axis.text = element_text(size = 12),   # Bigger axis tick labels
        legend.text = element_text(size = 12), # Bigger legend text
        legend.title = element_text(size = 13)) # Optional: legend title size
       guides(color = guide_legend(override.aes = list(fill = NA)),
       fill = guide_legend(override.aes = list(color = NA)))

subtypes_plot

ggsave("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Output/ami_subtypes.tiff", plot = subtypes_plot, dpi = 300, width = 7, height = 5)
       
# Mortality by Sex
       
sex <- read.csv("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Data/sex.csv", header = T)
       
head(sex)
       
# Plot
sex_plot <- ggplot(sex, aes(x = year, y = prop, color = sex)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex), alpha = 0.5, color = NA) +
    scale_x_continuous(limits = c(2005, 2020), breaks = seq(2005, 2020, by = 3)) +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 3)) +
    scale_color_manual(name = NULL, values = c("Male" = "blue", "Female" = "red")) +
    scale_fill_manual(name = "95% CI", values = c("Male" = "lightblue", "Female" = "pink")) +
    labs(x = "Admission year",
        y = "Mortality proportion (%)") +
    theme(legend.position = "bottom", 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "black"),
          axis.title = element_text(size = 14),  # Bigger axis titles
          axis.text = element_text(size = 12),   # Bigger axis tick labels
          legend.text = element_text(size = 12), # Bigger legend text
          legend.title = element_text(size = 13)) # Optional: legend title size
      guides(color = guide_legend(override.aes = list(fill = NA)),
      fill = guide_legend(override.aes = list(color = NA)))
sex_plot

ggsave("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Output/sex_plot.tiff", plot = sex_plot, dpi = 300, width = 7, height = 5)
      
# Mortality by Age
age <- read.csv("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Data/age.csv", header = T)
      
head(age)
view(age)

# set age-group to factor - I already changed the values manually in the dataset, so this step is not necessary
age$age_cat <- factor(age$age_cat, levels = c("18-39 years", "40-59 years", "60-79 years", "80-100 years"))

# Plot
age_plot <- ggplot(age, aes(x = year, y = prop, color = age_cat)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = age_cat), alpha = 0.5, color = NA) +
  scale_x_continuous(limits = c(2005, 2020), breaks = seq(2005, 2020, by = 3)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +
  scale_color_manual(name = NULL, values = c("18-39 years" = "blue", "40-59 years" = "orange", "60-79 years" = "green", "80-100 years" = "red")) +
  scale_fill_manual(name = "95% CI", values = c("18-39 years" = "lightblue", "40-59 years" = "orange", "60-79 years" = "lightgreen", "80-100 years" = "pink")) +
  labs(x = "Admission year",
      y = "Mortality proportion (%)") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),  # Bigger axis titles
        axis.text = element_text(size = 12),   # Bigger axis tick labels
        legend.text = element_text(size = 12), # Bigger legend text
        legend.title = element_text(size = 13)) + # Optional: legend title size 
  guides(color = guide_legend(override.aes = list(fill = NA),  nrow = 2, byrow = TRUE),
  fill = guide_legend(override.aes = list(color = NA), nrow = 2, byrow = TRUE))

age_plot

ggsave("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Output/age_plot.tiff", plot = age_plot, dpi = 300, width = 7, height = 5)

  
# Mortality by ethnicity
ethnicity <- read.csv("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Data/ethnicity.csv", header = T)
  
head(ethnicity)
view(ethnicity)
  
  
# Plot
ethnicity_plot <- ggplot(ethnicity, aes(x = year, y = prop, color = ethnicity)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = ethnicity), alpha = 0.3, color = NA) +
  scale_x_continuous(limits = c(2005, 2020), breaks = seq(2005, 2020, by = 3)) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 3)) +
  scale_color_manual(name = NULL, values = c("White" = "blue", "Black" = "orange", "Asian" = "green", "Mixed" = "red")) +
  scale_fill_manual(name = "95% CI", values = c("White" = "lightblue", "Black" = "orange", "Asian" = "lightgreen", "Mixed" = "pink")) +
  labs(x = "Admission year",
        y = "Mortality proportion (%)") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),  # Bigger axis titles
        axis.text = element_text(size = 12),   # Bigger axis tick labels
        legend.text = element_text(size = 12), # Bigger legend text
        legend.title = element_text(size = 13)) # Optional: legend title size 
guides(color = guide_legend(override.aes = list(fill = NA)),
        fill = guide_legend(override.aes = list(color = NA)))

ethnicity_plot

ggsave("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/MINAP/R/Output/ethnicity_plot.tiff", plot = ethnicity_plot, dpi = 300, width = 7, height = 5)

