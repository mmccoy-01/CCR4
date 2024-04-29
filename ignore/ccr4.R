---
title: "CCR4-CART Data"
output:
  flexdashboard::flex_dashboard:
    source_code: embed
    vertical_layout: fill
    orientation: columns
    theme: 
      version: 4
      bootswatch: cosmo
runtime: shiny
---

```{r setup, message=FALSE}
# Set root.dir to the current file's directory
knitr::opts_knit$set(root.dir = dirname(rstudioapi::getSourceEditorContext()$path))
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
library(tidyverse)
library(gridExtra)
library(svglite)
```

## Plots

```{r Group Plot}
# Plot includes key

# Set the working directory to the current script's directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  filter(trt != "Non-study") %>% 
  mutate(trt = reorder(trt, trt_factor)) %>%
  mutate(days_from_trt = round(as.numeric(difftime(imaging_date, trt_injection_date, units = "days")))) %>%
  mutate(Injection = round(as.numeric(difftime(tumor_injection_date, trt_injection_date, units = "days"))))

# List of filtering variables
filter_vars <- c('TH116', 'TH75', 'ETP1', 'ETP8', 'ALL8', 'TH34')

# Create a list to store the plots
plot_list <- list()

# Loop through each filtering variable
for (i in 1:length(filter_vars)) {
  # Filter the data
  filtered_data <- subset(processed_data, tumor_injection == filter_vars[i])
  
  # Calculate mean total_flux and standard error grouped by trt and days_from_trt
  mean_flux <- aggregate(total_flux ~ trt + days_from_trt, data = filtered_data, FUN = mean)
  se_flux <- aggregate(total_flux ~ trt + days_from_trt, data = filtered_data, FUN = function(x) sd(x) / sqrt(length(x)))
  names(se_flux) <- c("trt", "days_from_trt", "se_flux")
  
  # Merge mean_flux and se_flux
  mean_flux <- merge(mean_flux, se_flux, by = c("trt", "days_from_trt"))
  
  # Create the plot
  p <- ggplot(mean_flux, aes(x = days_from_trt, y = total_flux, color = trt)) +
    geom_point() +
    geom_errorbar(aes(ymin = total_flux - se_flux, ymax = total_flux + se_flux), width = 0.2) + # Add standard error bars
    geom_line(alpha = 0.5, linewidth = 1.3) +
    labs(title = paste(filter_vars[i]),
         x = "Days from Treatment",
         y = "Mean Flux [p/s]",
         color = "Arm") +
    scale_y_log10(labels = scales::scientific_format()) + # Change to logarithmic scale for y-axis
    theme_minimal(base_size = 14) + # Increase plot size by changing the base size
    scale_x_continuous(breaks = c(unique(mean_flux$days_from_trt), unique(filtered_data$Injection)),
                       labels = c(unique(mean_flux$days_from_trt), unique(filtered_data$Injection))) + # Set x-axis tick breaks and labels
    theme(panel.grid.major = element_blank(), # Remove major gridlines
          panel.grid.minor = element_blank()) + # Remove minor gridlines
    geom_vline(data = filtered_data, aes(xintercept = Injection), linetype = "dashed", color = "black") 
  # Store the plot in the list
  plot_list[[i]] <- p
}

# Output the plots
grid.arrange(grobs = plot_list, ncol = 2)
```

```{r Individual Plot}
# Plot does NOT include key

# Set the working directory to the current script's directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load extrafont package
library(extrafont)

# Path to bookman old style font file
font_import(paths = "data/processed/")

# Load the extrafont package for using the imported fonts
loadfonts()

# Load data
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  filter(trt != "Non-study") %>% 
  mutate(trt = reorder(trt, trt_factor)) %>%
  mutate(days_from_trt = round(as.numeric(difftime(imaging_date, trt_injection_date, units = "days")))) %>%
  mutate(Injection = round(as.numeric(difftime(tumor_injection_date, trt_injection_date, units = "days"))))

# List of filtering variables
filter_vars <- c('TH75', 'ETP1', 'ALL8')

# Create another row of 6 plots grouped by trt
plot_list_grouped <- list()

# Loop through each filtering variable
for (i in 1:length(filter_vars)) {
  # Filter the data
  filtered_data <- subset(processed_data, tumor_injection == filter_vars[i])
  
  # Calculate adjusted days_from_trt values
  adjusted_days_from_trt <- unique(c(filtered_data$days_from_trt, filtered_data$Injection))
  
  # Determine the x-axis limit and label
  x_limit <- ifelse(filter_vars[i] == "ETP1", -2, -1)
  x_label <- ifelse(x_limit == -2, 0, 0)
  
  # Create the plot
  p <- ggplot(filtered_data, aes(x = days_from_trt, y = total_flux, color = factor(trt), group = id)) +
    geom_point(size = 3) + # Adjust size of points
    geom_line(alpha = 1, size = 1.3) + # Adjust size of lines
    labs(title = NULL,
         x = "Days from Treatment",
         y = "Flux [p/s]",
         color = "Arm") + # Renaming x-axis
    scale_y_log10(labels = scales::scientific_format()) + # Change to logarithmic scale for y-axis
    scale_color_manual(values = c("Untreated" = "#fe9003",
                                   "Untransduced T-cells" = "#c6122f",
                                   "CCR4-CART" = "#3ab54b",
                                   "CART38" = "#5b266c",
                                  "CART19" = "#5b266c")) + # Specifying colors for each trt group
    theme_minimal(base_size = 14) + # Increase plot size by changing the base size
    scale_x_continuous(breaks = adjusted_days_from_trt, labels = ifelse(adjusted_days_from_trt == x_limit, 0, adjusted_days_from_trt), limits = c(x_limit, max(adjusted_days_from_trt))) + # Set x-axis tick breaks, labels, and limits
    theme(panel.grid.major = element_blank(), # Remove major gridlines
          panel.grid.minor = element_blank(), # Remove minor gridlines
          legend.position = "none", # Remove legend
          axis.text.x = element_text(size = 20, family = "Bookman Old Style"), # Adjust x-axis tick label size and font
          axis.text.y = element_text(size = 20, family = "Bookman Old Style"), # Adjust y-axis tick label size and font
          axis.title.x = element_text(size = 20, family = "Bookman Old Style"), # Adjust x-axis title font size and font
          axis.title.y = element_text(size = 20, family = "Bookman Old Style"), # Adjust y-axis title font size and font
          text = element_text(family = "Bookman Old Style")) # Set all text to Bookman Old Style font
  
  # Store the plot in the list
  plot_list_grouped[[i]] <- p
}

# Reorder plot_list_grouped based on the specified order of tumor_injection
order <- match(c('ALL8', 'TH75', 'ETP1'), filter_vars)
plot_list_grouped <- plot_list_grouped[order]

# Output the plots with grouping by trt
my_plot <- grid.arrange(grobs = plot_list_grouped, ncol = 3)

# Save the plot as an SVG file (vector format)
# Save the plot as a PDF file with embedded fonts
# Open PDF device
# Save the plot as a PDF file with embedded fonts
ggsave("my_plot.pdf", plot = my_plot, device = cairo_pdf, width = 34, height = 10.5,
       units = "in", dpi = 300, family = "Bookman Old Style")
```

```{r Individual Plot with Key}
# Set the working directory to the current script's directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load extrafont package
library(extrafont)

# Path to bookman old style font file
font_import(paths = "data/processed/")

# Load the extrafont package for using the imported fonts
loadfonts()

# Load data
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  filter(trt != "Non-study") %>% 
  mutate(trt = reorder(trt, trt_factor)) %>%
  mutate(days_from_trt = round(as.numeric(difftime(imaging_date, trt_injection_date, units = "days")))) %>%
  mutate(Injection = round(as.numeric(difftime(tumor_injection_date, trt_injection_date, units = "days"))))

# List of filtering variables
filter_vars <- c('TH116', 'TH75', 'ETP1', 'ETP8', 'ALL8', 'TH34')

# Create another row of 6 plots grouped by trt
plot_list_grouped <- list()

# Loop through each filtering variable
for (i in 1:length(filter_vars)) {
  # Filter the data
  filtered_data <- subset(processed_data, tumor_injection == filter_vars[i])
  
  # Calculate adjusted days_from_trt values
  adjusted_days_from_trt <- unique(c(filtered_data$days_from_trt, filtered_data$Injection))
  
  # Determine the x-axis limit and label
  x_limit <- ifelse(filter_vars[i] == "ETP1", -2, -1)
  x_label <- ifelse(x_limit == -2, 0, 0)
  
  # Create the plot
  p <- ggplot(filtered_data, aes(x = days_from_trt, y = total_flux, color = factor(trt), group = id)) +
    geom_point() +
    geom_line(alpha = 0.5, linewidth = 1.3) +
    labs(title = NULL,
         x = "Days from Treatment",
         y = "Flux [p/s]",
         color = "Arm") + # Renaming x-axis
    scale_y_log10(labels = scales::scientific_format()) + 
    scale_color_manual(values = c("Untreated" = "#fe9003",
                                   "Untransduced T-cells" = "#c6122f",
                                   "CCR4-CART" = "#3ab54b",
                                   "CART38" = "#5b266c",
                                  "CART19" = "#5b266c")) + # Change to logarithmic scale for y-axis
    theme_minimal(base_size = 14) + # Increase plot size by changing the base size
    scale_x_continuous(breaks = adjusted_days_from_trt, labels = ifelse(adjusted_days_from_trt == x_limit, 0, adjusted_days_from_trt), limits = c(x_limit, max(adjusted_days_from_trt))) + # Set x-axis tick breaks, labels, and limits
    theme(panel.grid.major = element_blank(), # Remove major gridlines
          panel.grid.minor = element_blank(), # Remove minor gridlines
          legend.position = "bottom", # Change legend position to bottom
          legend.direction = "horizontal", # Set legend direction to horizontal
          legend.title = element_text(size = 18), # Set legend title font size
          axis.text.x = element_text(size = 18, family = "Bookman Old Style"), # Adjust x-axis tick label size and font
          axis.text.y = element_text(size = 18, family = "Bookman Old Style"), # Adjust y-axis tick label size and font
          legend.text = element_text(size = 16, family = "Bookman Old Style"), # Set legend text size and font
          text = element_text(family = "Bookman Old Style"), # Set all text to Bookman Old Style font
          legend.key.width = unit(2, "cm")) # Adjust the width of legend key
  
  # Store the plot in the list
  plot_list_grouped[[i]] <- p
}

# Output the plots with grouping by trt
my_plot <- grid.arrange(grobs = plot_list_grouped, ncol = 1)

# Save the plot as a PDF file with embedded fonts
ggsave("my_plot.pdf", plot = my_plot, device = cairo_pdf, width = 13, height = 10.5,
       units = "in", dpi = 300, family = "Bookman Old Style")
```

## Descriptives

```{r}


```

## Treatment ANOVA

```{r}
save_output <- "y"
  
# Load data and filter PDXs where CCR4-CART did not work
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  filter(trt != "Non-study") %>% 
  mutate(
    trt = factor(trt),
    tumor_injection = factor(tumor_injection)
  ) %>%
  distinct(tumor_injection, id, trt, imaging_date)

tryCatch(
  {
    anova <- jmv::ANOVA(
      formula = trt ~ tumor_injection + imaging_date,
      data = processed_data,
      effectSize = "eta",
      homo = TRUE,
      norm = TRUE,
      postHoc = ~ trt + tumor_injection,
      postHocCorr = c("tukey", "bonf"),
      postHocES = "d",
      postHocEsCi = TRUE,
      emmPlots = FALSE
    )
    # Check if save_output is equal to "y"
    if (save_output == "y") {
      # Save ANOVA results as a .txt file
      capture.output(anova, file = "data/processed/analysis/treatment_anova.txt", append = TRUE)
    }
    
    # Display ANOVA results
    anova
  },
  error = function(e) {
    if (grepl("incorrect number of dimensions", e$message)) {
      cat("Error: The data has incorrect dimensions for ANOVA analysis.\n")
    } else {
      cat("Error occurred during ANOVA analysis:\n")
      cat(e$message, "\n")
    }
  }
)
```

## Survival ANOVA

```{r}
save_output <- "y"
  
# Load data and filter PDXs where CCR4-CART did not work
processed_data <- read.csv("data/processed/processed_data.csv") %>%
  filter(trt != "Non-study",
         tumor_injection != "TH116",
         tumor_injection != "ETP8",
         tumor_injection != "TH34") %>% 
  mutate(
    trt = factor(trt),
    tumor_injection = factor(tumor_injection)
  ) %>%
  distinct(tumor_injection, id, trt, death_date, trt_injection_date) %>% 
  mutate(
    death_date = as.Date(death_date),
    trt_injection_date = as.Date(trt_injection_date),
    event = ifelse(!is.na(death_date), 1, 0),
    time = ifelse(!is.na(death_date), round(as.numeric(difftime(death_date, trt_injection_date, units = "days"))), as.numeric(difftime(Sys.Date(), trt_injection_date, units = "days")))
  )

tryCatch(
  {
    anova <- jmv::ANOVA(
      formula = time ~ trt + tumor_injection,
      data = processed_data,
      effectSize = "eta",
      homo = TRUE,
      norm = TRUE,
      postHoc = ~ trt + tumor_injection,
      postHocCorr = c("tukey", "bonf"),
      postHocES = "d",
      postHocEsCi = TRUE,
      emmPlots = FALSE
    )
    # Check if save_output is equal to "y"
    if (save_output == "y") {
      # Save ANOVA results as a .txt file
      capture.output(anova, file = "data/processed/analysis/survival_anova.txt", append = TRUE)
    }
    
    # Display ANOVA results
    anova
  },
  error = function(e) {
    if (grepl("incorrect number of dimensions", e$message)) {
      cat("Error: The data has incorrect dimensions for ANOVA analysis.\n")
    } else {
      cat("Error occurred during ANOVA analysis:\n")
      cat(e$message, "\n")
    }
  }
)

```