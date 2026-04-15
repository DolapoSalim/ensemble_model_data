getwd()

library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(reshape2)
library(scales)
library(fmsb)

# ============================================================
# 0. DATA SETUP
# ============================================================

# --- Per class data ---
per_class_v8 <- read.csv("data/YOLO8/yolov8x_test_comprehensive_per_class_detailed.csv")
per_class_v9 <- read.csv("data/YOLO9/yolov9x_test_comprehensive_per_class_detailed.csv")
per_class_v11 <- read.csv("data/YOLO11/yolov11x_test_comprehensive_per_class_detailed.csv")

per_class_v8$model  <- "YOLOv8"
per_class_v9$model  <- "YOLOv9"
per_class_v11$model <- "YOLOv11"

per_class_all <- rbind(per_class_v8, per_class_v9, per_class_v11)

# --- Summary data (BoundingBox rows only) ---
get_bbox_summary <- function(path, model_name) {
  df <- read.csv(path)
  df <- df[df$category == "BoundingBox", ]
  wide <- pivot_wider(df, names_from = metric, values_from = value)
  wide$model <- model_name
  return(wide)
}

summary_v8  <- get_bbox_summary("data/YOLO8/yolov8x_test_comprehensive_summary_statistics.csv",  "YOLOv8")
summary_v9  <- get_bbox_summary("data/YOLO9/yolov9x_test_comprehensive_summary_statistics.csv",  "YOLOv9")
summary_v11 <- get_bbox_summary("data/YOLO11/yolov11x_test_comprehensive_summary_statistics.csv", "YOLOv11")

# --- WBF Ensemble row (from evaluation script output) ---
ensemble_summary <- data.frame(
  model          = "Ensemble",
  mean_precision = 0.580,       # not available from supervision mAP output
  mean_recall    = 0.780,
  mean_f1        = 0.668,
  `mAP@0.5`      = 0.7688,
  `mAP@0.5:0.95` = 0.6623,
  check.names    = FALSE
)

# Combine all summaries
summary_all <- bind_rows(summary_v8, summary_v9, summary_v11, ensemble_summary)
summary_all$model <- factor(summary_all$model, levels = c("YOLOv8", "YOLOv9", "YOLOv11", "Ensemble"))

# Color palette — consistent across all plots
model_colors <- c(
  "YOLOv8"   = "#E63946",
  "YOLOv9"   = "#457B9D",
  "YOLOv11"  = "#2A9D8F",
  "Ensemble" = "#F4A261"
)

species_colors <- c(
  "Cystoseira sl"       = "#6A0572",
  "Posidonia oceanica"  = "#1A7A4A",
  "Sargassum vulgare"   = "#C77D29"
)

# ============================================================
# 1. OVERALL mAP COMPARISON BAR CHART
# ============================================================

map_long <- summary_all %>%
  select(model, `mAP@0.5`, `mAP@0.5:0.95`) %>%
  pivot_longer(cols = c(`mAP@0.5`, `mAP@0.5:0.95`),
               names_to  = "metric",
               values_to = "value") %>%
  filter(!is.na(value))

p_map <- ggplot(map_long, aes(x = model, y = value, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  facet_wrap(~metric) +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
  labs(title    = "Overall mAP Comparison Across Models",
       subtitle = "Evaluated on test set",
       x = NULL, y = "mAP Score", fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "bottom",
        strip.text       = element_text(face = "bold"),
        plot.title       = element_text(face = "bold"))

# ============================================================
# 2. PRECISION, RECALL, F1 COMPARISON (individual models only)
# ============================================================

prf_long <- summary_all %>%
  filter(model != "Ensemble") %>%
  select(model, mean_precision, mean_recall, mean_f1) %>%
  pivot_longer(cols      = c(mean_precision, mean_recall, mean_f1),
               names_to  = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         mean_precision = "Precision",
                         mean_recall    = "Recall",
                         mean_f1        = "F1 Score"))

p_prf <- ggplot(prf_long, aes(x = metric, y = value, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
  labs(title    = "Precision, Recall & F1 Score Comparison",
       subtitle = "Bounding box metrics on test set",
       x = NULL, y = "Score", fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold"))

# ============================================================
# 3. PER CLASS mAP50 — ALL MODELS
# ============================================================

p_perclass_map50 <- ggplot(per_class_all,
                           aes(x = class_name, y = box_ap50, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", box_ap50)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
  labs(title    = "Per Class mAP@0.5 Comparison",
       subtitle = "Bounding box detection per species",
       x = "Species", y = "AP@0.5", fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "bottom",
        axis.text.x      = element_text(angle = 15, hjust = 1),
        plot.title       = element_text(face = "bold"))

# ============================================================
# 4. PER CLASS PRECISION & RECALL HEATMAP
# ============================================================

heatmap_data <- per_class_all %>%
  select(class_name, model, box_precision, box_recall, box_f1_score) %>%
  pivot_longer(cols      = c(box_precision, box_recall, box_f1_score),
               names_to  = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         box_precision = "Precision",
                         box_recall    = "Recall",
                         box_f1_score  = "F1 Score"))

p_heatmap <- ggplot(heatmap_data,
                    aes(x = model, y = class_name, fill = value)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f", value)), size = 3.5, fontface = "bold") +
  facet_wrap(~metric) +
  scale_fill_gradient2(low      = "#d73027",
                       mid      = "#ffffbf",
                       high     = "#1a9850",
                       midpoint = 0.7,
                       limits   = c(0, 1),
                       labels   = percent_format(),
                       name     = "Score") +
  labs(title    = "Per Class Performance Heatmap",
       subtitle = "Bounding box metrics across models and species",
       x = NULL, y = "Species") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x  = element_text(angle = 15, hjust = 1),
        strip.text   = element_text(face = "bold"),
        plot.title   = element_text(face = "bold"))

# ============================================================
# 5. CONFUSION MATRICES — NORMALISED
# ============================================================

plot_confusion_matrix <- function(cm_path, model_name) {
  cm <- read.csv(cm_path, row.names = 1)
  
  # Keep only true class rows (exclude Background row for normalisation)
  species <- c("Cystoseira sl", "Posidonia oceanica", "Sargassum vulgare")
  
  # Row-wise normalisation (per true class)
  cm_norm <- cm
  for (i in 1:nrow(cm)) {
    row_sum <- sum(cm[i, ], na.rm = TRUE)
    if (row_sum > 0) cm_norm[i, ] <- cm[i, ] / row_sum
  }
  
  # Clean row/col names for display
  rownames(cm_norm) <- gsub("True_", "", rownames(cm_norm))
  colnames(cm_norm) <- gsub("Pred_", "", colnames(cm_norm))
  
  cm_melt <- melt(as.matrix(cm_norm))
  colnames(cm_melt) <- c("True", "Predicted", "value")
  
  ggplot(cm_melt, aes(x = Predicted, y = True, fill = value)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.2f", value)),
              size = 4, fontface = "bold",
              color = ifelse(cm_melt$value > 0.5, "white", "black")) +
    scale_fill_gradient(low  = "#f7fbff",
                        high = "#08519c",
                        limits = c(0, 1),
                        labels = percent_format(),
                        name   = "Proportion") +
    labs(title = paste(model_name, "— Normalised Confusion Matrix"),
         x = "Predicted Class", y = "True Class") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x  = element_text(angle = 20, hjust = 1),
          plot.title   = element_text(face = "bold"))
}

p_cm_v8  <- plot_confusion_matrix("data/YOLO8/yolov8x_test_comprehensive_confusion_matrix.csv",  "YOLOv8")
p_cm_v9  <- plot_confusion_matrix("data/YOLO9/yolov9x_test_comprehensive_confusion_matrix.csv",  "YOLOv9")
p_cm_v11 <- plot_confusion_matrix("data/YOLO11/yolov11x_test_comprehensive_confusion_matrix.csv", "YOLOv11")

p_cm_all <- ggarrange(p_cm_v8, p_cm_v9, p_cm_v11,
                      ncol = 3, nrow = 1)
p_cm_all <- annotate_figure(p_cm_all,
                            top = text_grob("Normalised Confusion Matrices Across Models",
                                            face = "bold", size = 14))

# ============================================================
# 6. SPEED COMPARISON
# ============================================================

get_speed <- function(path, model_name) {
  df <- read.csv(path)
  df <- df[df$category == "Speed_ms", ]
  df$model <- model_name
  df
}

speed_v8  <- get_speed("data/YOLO8/yolov8x_test_comprehensive_summary_statistics.csv",  "YOLOv8")
speed_v9  <- get_speed("data/YOLO9/yolov9x_test_comprehensive_summary_statistics.csv",  "YOLOv9")
speed_v11 <- get_speed("data/YOLO11/yolov11x_test_comprehensive_summary_statistics.csv", "YOLOv11")

speed_all <- rbind(speed_v8, speed_v9, speed_v11) %>%
  filter(metric == "inference") %>%
  mutate(fps = round(1000 / value, 1))

p_speed <- ggplot(speed_all, aes(x = model, y = value, fill = model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(value, 1), " ms\n(", fps, " FPS)")),
            vjust = -0.3, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 35)) +
  labs(title    = "Inference Speed Comparison",
       subtitle = "Milliseconds per image on test set",
       x = NULL, y = "Inference Time (ms)", fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title      = element_text(face = "bold"))

# ============================================================
# 7. RADAR CHART — OVERALL MODEL COMPARISON
# ============================================================


radar_data <- summary_all %>%
  filter(model != "Ensemble") %>%
  select(model, mean_precision, mean_recall, mean_f1, `mAP@0.5`, `mAP@0.5:0.95`) %>%
  column_to_rownames("model")

colnames(radar_data) <- c("Precision", "Recall", "F1", "mAP@0.5", "mAP@0.5:95")

# Add max/min rows required by fmsb
radar_data <- rbind(
  max = rep(1, ncol(radar_data)),
  min = rep(0, ncol(radar_data)),
  radar_data
)

radar_colors <- c(
  rgb(0.90, 0.22, 0.27, 0.7),  # YOLOv8
  rgb(0.27, 0.48, 0.62, 0.7),  # YOLOv9
  rgb(0.16, 0.61, 0.56, 0.7)   # YOLOv11
)

dir.create("plots", showWarnings = FALSE)

# Save radar chart separately since fmsb uses base R plotting
png("plots/radar_chart.png", width = 800, height = 700, res = 120)
radarchart(radar_data,
           axistype  = 1,
           pcol      = radar_colors,
           pfcol     = adjustcolor(radar_colors, alpha.f = 0.2),
           plwd      = 2.5,
           cglcol    = "grey70",
           cglty     = 1,
           axislabcol = "grey40",
           caxislabels = c("0", "0.25", "0.5", "0.75", "1.0"),
           vlcex     = 0.9)
legend("topright",
       legend  = c("YOLOv8", "YOLOv9", "YOLOv11"),
       col     = radar_colors,
       lty     = 1, lwd = 2.5,
       bty     = "n", cex = 0.9)
title(main = "Overall Model Performance Radar Chart", font.main = 2)
dev.off()

# ============================================================
# 8. SAVE ALL GGPLOT FIGURES
# ============================================================

ggsave("plots/01_overall_mAP_comparison.png",     p_map,             width = 10, height = 6, dpi = 300)
ggsave("plots/02_precision_recall_f1.png",         p_prf,             width = 9,  height = 6, dpi = 300)
ggsave("plots/03_per_class_mAP50.png",             p_perclass_map50,  width = 10, height = 6, dpi = 300)
ggsave("plots/04_per_class_heatmap.png",           p_heatmap,         width = 12, height = 5, dpi = 300)
ggsave("plots/05_confusion_matrices.png",          p_cm_all,          width = 18, height = 6, dpi = 300)
ggsave("plots/06_speed_comparison.png",            p_speed,           width = 7,  height = 5, dpi = 300)

cat("\nAll plots saved to /plots folder\n")

# ============================================================
# 9. PRINT SUMMARY TABLE
# ============================================================


summary_table <- summary_all %>%
  select(model, mean_precision, mean_recall, mean_f1, `mAP@0.5`, `mAP@0.5:0.95`) %>%
  mutate(across(where(is.numeric), ~round(., 4)))
print(summary_table)
