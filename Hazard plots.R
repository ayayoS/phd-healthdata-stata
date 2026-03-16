#To create hazard plots for each covariate pattern

install.packages(c("haven", "dplyr", "tidyr", "ggplot2", "patchwork"))
library(haven)    # for reading .dta files
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Import data from stata file
haz_BFnD <- read_dta("C:/Users/m84006sa/The University of Manchester Dropbox/Sharon Ayayo/PhD Sharon/Multistate modelling/Data/hazard_BFnD71.dta")

# Select hazard and CI columns and reshape to long format
BMnD_long <- haz_BMnD %>%
  pivot_longer(
    cols = -tempvar,
    names_to = "varname",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      grepl("_lci$", varname) ~ "lci",
      grepl("_uci$", varname) ~ "uci",
      grepl("_hazard$", varname) ~ "hazard",
      TRUE ~ NA_character_
    ),
    transition = gsub("_hazard(_lci|_uci)?$", "", varname)
  ) %>%
  filter(!is.na(metric)) %>%
  select(tempvar, transition, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(time = tempvar)


#To order the legend from highest to lowest
# Step 1: Get the last time point for each transition - DO NOT CHANGE THIS FOR OTHER DATASETS
line_order <- BMD_long %>%
  group_by(transition) %>%
  filter(time == max(time)) %>%
  arrange(desc(hazard)) %>%
  pull(transition)

# Step 2: Reorder the factor levels - Change this one for the other datasets
BMnD_long$transition <- factor(BMnD_long$transition, levels = line_order)


#Plot
# Define custom colors (as many as you have transitions)
custom_colors <- c(
  "adm_bleed" = "#FFD700",          # yellow
  "adm_reinf" = "#000000",          # black
  "adm_death" = "#228B22",          # green
  "bleed_reinf" = "#A0522D",        # brown
  "bleed_death" = "#800080",        # purple
  "reinf_bleed" = "#008080",        # teal
  "reinf_death" = "#DC143C",        # red
  "bleedreinf_death" = "#1E90FF",   # blue
  "reinfbleed_death" = "#FFA500"    # orange
)

# Labels for the legend

pretty_labels <- c(
  "adm_bleed" = "A->B",
  "adm_reinf" = "A->R",
  "adm_death" = "A->D",
  "bleed_reinf" = "B->R",
  "bleed_death" = "B->D",
  "reinf_bleed" = "R->B",
  "reinf_death" = "R->D",
  "bleedreinf_death" = "B->R->D",
  "reinfbleed_death" = "R->B->D"
)

# Plot with custom colors
BMD_plot <- ggplot(BMD_long, aes(x = time, y = hazard, color = transition, fill = transition)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, color = NA) +
  #geom_hline(yintercept = 0, color = "black") +  # x-axis line
  #geom_vline(xintercept = 1, color = "black") +   # y-axis line (starting at 1 if your x-axis starts there)
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  scale_x_continuous(limits = c(1,5), breaks = 1:5, labels = c("0–1", "1–2", "2–3", "3–4", "4–5")) +
  scale_color_manual(
    name = "Transitions",
    values = custom_colors,
    labels = pretty_labels) +
  scale_fill_manual(
    name = "Transitions",
    values = custom_colors,
    labels = pretty_labels) +
  labs(
    title = "Black male aged 62.0 years with diabetes",
    x = "Time period since admission (years)",
    y = "Hazard rate (%)",
    color = "Transition",
    fill = "Transition"
  ) +
  theme_minimal(base_size = 14) +
  theme(
  plot.title = element_text(size = 12, face = "bold")  
)

BMD_plot

#combine plots - separate x and y axes

#(WM_plot / WF_plot) + plot_layout(guides = "collect") #stacks them vertically
#Stack the plots side by side

BlacksD <- (BMD_plot | BFD_plot) + 
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.2, "cm"),
    #plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
   )
   
BlacksD


#Shared x axis when stacked vertically - Didn't end up using this
# Apply blank x-axis ONLY to first plot
WM_clean <- WM_plot + theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)


#Save workspace
save.image(file = "C:\\Users\\m84006sa\\The University of Manchester Dropbox\\Sharon Ayayo\\PhD Sharon\\Multistate modelling\\R\\hazardplots_workspace.RData")

#Load workspace
load("C:\\Users\\m84006sa\\The University of Manchester Dropbox\\Sharon Ayayo\\PhD Sharon\\Multistate modelling\\R\\hazardplots_workspace.RData")

