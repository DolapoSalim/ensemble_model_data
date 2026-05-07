library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(reshape2)
library(scales)
library(beepr)

# ============================================================
# 0. DATA SETUP
# ============================================================

per_class_v8  <- read.csv("data/YOLO8/yolov8x_test_comprehensive_per_class_detailed.csv")
per_class_v9  <- read.csv("data/YOLO9/yolov9x_test_comprehensive_per_class_detailed.csv")
per_class_v11 <- read.csv("data/YOLO11/yolov11x_test_comprehensive_per_class_detailed.csv")

per_class_v8$model  <- "YOLOv8"
per_class_v9$model  <- "YOLOv9"
per_class_v11$model <- "YOLOv11"

# WBF per-class (from wbf_ensemble_per_class_detailed.csv)
per_class_wbf <- read.csv("data/WBF/wbf_ensemble_per_class_detailed.csv")
per_class_wbf$model <- "Ensemble"

per_class_all <- bind_rows(per_class_v8, per_class_v9, per_class_v11)

# --- Summary data ---
get_bbox_summary <- function(path, model_name) {
  df   <- read.csv(path)
  bbox <- df[df$category == "BoundingBox", ]
  wide <- pivot_wider(bbox, names_from = metric, values_from = value)
  wide$model <- model_name
  return(wide)
}

summary_v8  <- get_bbox_summary("data/YOLO8/yolov8x_test_comprehensive_summary_statistics.csv",  "YOLOv8")
summary_v9  <- get_bbox_summary("data/YOLO9/yolov9x_test_comprehensive_summary_statistics.csv",  "YOLOv9")
summary_v11 <- get_bbox_summary("data/YOLO11/yolov11x_test_comprehensive_summary_statistics.csv", "YOLOv11")

#WBF
wbf_summary_raw <- read.csv("data/WBF/wbf_ensemble_summary_statistics.csv")
ensemble_summary <- data.frame(
  model          = "Ensemble",
  mean_precision = wbf_summary_raw$value[wbf_summary_raw$metric == "mean_precision"],
  mean_recall    = wbf_summary_raw$value[wbf_summary_raw$metric == "mean_recall"],
  mean_f1        = wbf_summary_raw$value[wbf_summary_raw$metric == "mean_f1"],
  `mAP@0.5`      = wbf_summary_raw$value[wbf_summary_raw$metric == "mAP@0.5"],
  `mAP@0.5:0.95` = wbf_summary_raw$value[wbf_summary_raw$metric == "mAP@0.5:0.95"],
  check.names    = FALSE
)

summary_all <- bind_rows(summary_v8, summary_v9, summary_v11, ensemble_summary)
summary_all$model <- factor(summary_all$model,
                            levels = c("YOLOv8", "YOLOv9", "YOLOv11", "Ensemble"))

# Color palette
model_colors <- c(
  "YOLOv8"   = "#E63946",
  "YOLOv9"   = "#457B9D",
  "YOLOv11"  = "#2A9D8F",
  "Ensemble" = "#F4A261"
)

dir.create("plots", showWarnings = FALSE)


# 1.GROUPED BAR CHART

metrics_df <- summary_all %>%
  select(model, mean_precision, mean_recall, mean_f1, `mAP@0.5`, `mAP@0.5:0.95`) %>%
  rename(Precision = mean_precision,
         Recall    = mean_recall,
         F1        = mean_f1,
         mAP50     = `mAP@0.5`,
         mAP50_95  = `mAP@0.5:0.95`)

# Best single model per metric (exclude ensemble)
best_single <- metrics_df %>%
  filter(model != "Ensemble") %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))

# Grouped bar plot
p_grouped <- metrics_df %>%
  pivot_longer(cols = -model, names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("mAP50_95", "mAP50", "F1", "Recall", "Precision"))) %>%
  ggplot(aes(x = metric, y = value, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%.3f", value)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Model Performance Across Metrics",
    x = NULL,
    y = "Score",
    fill = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title       = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave("plots/01_model_performance_across_matrics.png", p_grouped,
       width = 11, height = 6, dpi = 300)


# 2. OVERALL mAP BAR CHART — ZOOMED Y AXIS


map_long <- summary_all %>%
  select(model, `mAP@0.5`, `mAP@0.5:0.95`) %>%
  pivot_longer(cols      = c(`mAP@0.5`, `mAP@0.5:0.95`),
               names_to  = "metric",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(metric = factor(metric, levels = c("mAP@0.5", "mAP@0.5:0.95")))

p_map <- ggplot(map_long, aes(x = model, y = value, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.4f", value)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  facet_wrap(~metric) +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0.55, 0.82),
                     oob    = rescale_none,
                     labels = percent_format(accuracy = 1)) +
  labs(title    = "mAP Comparison Across Models",
       subtitle = "Y-axis zoomed to highlight differences",
       x = NULL, y = "mAP Score", fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 12),
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey40", size = 10)
  )

ggsave("plots/02_overall_mAP_comparison.png", p_map,
       width = 10, height = 6, dpi = 300)

# ============================================================
# 3. PRECISION, RECALL, F1 — ALL MODELS INCLUDING ENSEMBLE
# ============================================================

prf_long <- summary_all %>%
  select(model, mean_precision, mean_recall, mean_f1) %>%
  filter(!is.na(mean_precision)) %>%
  pivot_longer(cols      = c(mean_precision, mean_recall, mean_f1),
               names_to  = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         mean_precision = "Precision",
                         mean_recall    = "Recall",
                         mean_f1        = "F1 Score"),
         metric = factor(metric, levels = c("Precision", "Recall", "F1 Score")))

p_prf <- ggplot(prf_long, aes(x = metric, y = value, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0.55, 1.0),
                     oob    = rescale_none,
                     labels = percent_format(accuracy = 1)) +
  labs(title    = "Precision, Recall & F1 Score Comparison",
       subtitle = "Bounding box metrics on test set",
       x = NULL, y = "Score", fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "grey40", size = 10)
  )

ggsave("plots/03_precision_recall_f1.png", p_prf,
       width = 9, height = 6, dpi = 300)

# ============================================================
# 4. PER CLASS mAP50 — ALL MODELS
# ============================================================

p_perclass_map50 <- ggplot(per_class_all,
                           aes(x = class_name, y = box_ap50, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", box_ap50)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 1), labels = percent_format()) +
  labs(title    = "Per Class AP@0.5 Comparison",
       subtitle = "Bounding box detection per species",
       x = "Species", y = "AP@0.5", fill = "Model") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 15, hjust = 1, face = "italic"),
    plot.title      = element_text(face = "bold")
  )

ggsave("plots/04_per_class_mAP50.png", p_perclass_map50,
       width = 10, height = 6, dpi = 300)

# ============================================================
# 5. PER CLASS HEATMAP — ALL MODELS + ENSEMBLE
# ============================================================

# Add ensemble per-class data (precision, recall, f1 only — no mAP per class)
heatmap_individual <- per_class_all %>%
  select(class_name, model, box_precision, box_recall, box_f1_score) %>%
  rename(Precision = box_precision,
         Recall    = box_recall,
         F1        = box_f1_score)

heatmap_ensemble <- per_class_wbf %>%
  select(class_name, model, precision, recall, f1_score) %>%
  rename(Precision = precision,
         Recall    = recall,
         F1        = f1_score)

heatmap_combined <- bind_rows(heatmap_individual, heatmap_ensemble) %>%
  pivot_longer(cols      = c(Precision, Recall, F1),
               names_to  = "metric",
               values_to = "value") %>%
  mutate(
    model  = factor(model, levels = c("YOLOv8", "YOLOv9", "YOLOv11", "Ensemble")),
    metric = factor(metric, levels = c("Precision", "Recall", "F1"))
  )

p_heatmap <- ggplot(heatmap_combined,
                    aes(x = model, y = class_name, fill = value)) +
  geom_tile(color = "white", linewidth = 1.0) +
  geom_text(aes(label = sprintf("%.3f", value),
                color  = ifelse(value > 0.72, "white", "black")),
            size = 3.8, fontface = "bold") +
  facet_wrap(~metric) +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "#ffffbf",
    high     = "#1a9850",
    midpoint = 0.72,
    limits   = c(0.4, 1.0),
    oob      = squish,
    labels   = percent_format(accuracy = 1),
    name     = "Score"
  ) +
  scale_color_identity() +
  labs(
    title    = "Per Class Performance Heatmap",
    subtitle = "Precision, Recall and F1 per species across all models",
    x = NULL, y = "Species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 15, hjust = 1),
    axis.text.y  = element_text(face = "italic"),
    strip.text   = element_text(face = "bold", size = 12),
    plot.title   = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40", size = 10)
  )

ggsave("plots/05_per_class_heatmap.png", p_heatmap,
       width = 13, height = 5, dpi = 300)

# ============================================================
# 6. CONFUSION MATRICES — ULTRALYTICS STYLE
# Each model gets its own separate plot
# ============================================================

plot_cm_ultralytics <- function(cm_path, model_name, out_path) {
  
  cm_raw <- read.csv(cm_path, row.names = 1, check.names = FALSE)
  
  # Clean row and col names
  rownames(cm_raw) <- gsub("True_", "", rownames(cm_raw))
  colnames(cm_raw) <- gsub("Pred_", "", colnames(cm_raw))
  
  # Row-wise normalisation
  cm_mat  <- as.matrix(cm_raw)
  row_sum <- rowSums(cm_mat)
  cm_norm <- sweep(cm_mat, 1, ifelse(row_sum == 0, 1, row_sum), "/")
  
  cm_melt <- melt(cm_norm) %>%
    rename(True = Var1, Predicted = Var2, value = value) %>%
    mutate(
      True      = factor(True,      levels = rev(rownames(cm_norm))),
      Predicted = factor(Predicted, levels = colnames(cm_norm)),
      label     = sprintf("%.2f", value)
    )
  
  n_classes   <- ncol(cm_norm)
  text_colors <- ifelse(cm_melt$value > 0.55, "white", "black")
  
  ggplot(cm_melt, aes(x = Predicted, y = True, fill = value)) +
    geom_tile() +
    geom_text(aes(label = label),
              color     = text_colors,
              size      = 4.5,
              fontface  = "bold") +
    scale_fill_gradientn(
      colours = c(
        "#ffffff",
        "#deebf7",
        "#9ecae1",
        "#6baed6",
        "#3182bd",
        "#08519c",
        "#08306b" 
      ),
      values  = rescale(c(0, 0.1, 0.25, 0.45, 0.65, 0.82, 1.0)),
      limits  = c(0, 1),
      #labels  = percent_format(accuracy = 1),
      name    = "Normalised\nCount"
    ) +
    scale_x_discrete(
      position = "bottom",
      labels = function(x) {
        parsed <- ifelse(
          x != "Background",
          paste0("italic('", gsub("_", "~", x), "')"),
          paste0("'", x, "'")
        )
        parse(text = parsed)
      }
    ) +
    scale_y_discrete(
      labels = function(y) {
        parsed <- ifelse(
          y != "Background",
          paste0("italic('", gsub("_", "~", y), "')"),
          paste0("'", y, "'")
        )
        parse(text = parsed)
      }
    ) +
    labs(
      title = model_name,
      x = "Predicted",
      y = "True"
    ) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      plot.title       = element_text(color = "black", face = "bold",
                                      size = 14, hjust = 0),
      plot.subtitle    = element_text(color = "grey", size = 9, hjust = 0),
      axis.title       = element_text(color = "black", size = 11, face = "bold"),
      axis.text        = element_text(color = "black", size = 10),
      axis.ticks       = element_blank(),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text      = element_text(color = "black", size = 9),
      legend.title     = element_text(color = "black", size = 9, face = "bold"),
      plot.margin      = margin(16, 16, 16, 16),
    )
}


p_cm_v8  <- plot_cm_ultralytics(
  "data/YOLO8/yolov8x_test_comprehensive_confusion_matrix.csv",  "YOLOv8",  "plots/06a_cm_v8.png")
p_cm_v9  <- plot_cm_ultralytics(
  "data/YOLO9/yolov9x_test_comprehensive_confusion_matrix.csv",  "YOLOv9",  "plots/06b_cm_v9.png")
p_cm_v11 <- plot_cm_ultralytics(
  "data/YOLO11/yolov11x_test_comprehensive_confusion_matrix.csv", "YOLOv11", "plots/06c_cm_v11.png")
p_cm_ens <- plot_cm_ultralytics(
  "data/WBF/wbf_ensemble_confusion_matrix.csv",                "WBF Ensemble", "plots/06d_cm_ensemble.png")

# Save each individually
ggsave("plots/06a_cm_YOLOv8.png",     p_cm_v8,  width = 14, height = 11.5, dpi = 300)
ggsave("plots/06b_cm_YOLOv9.png",     p_cm_v9,  width = 14, height = 11.5, dpi = 300)
ggsave("plots/06c_cm_YOLOv11.png",    p_cm_v11, width = 14, height = 11.5, dpi = 300)
ggsave("plots/06d_cm_Ensemble.png",   p_cm_ens, width = 14, height = 11.5, dpi = 300)

# Also save all 4 in one panel
p_cm_all <- ggarrange(p_cm_v8, p_cm_v9, p_cm_v11, p_cm_ens,
                      ncol = 2, nrow = 2)
ggsave("plots/06_confusion_matrices_all.png", p_cm_all,
       width = 17, height = 12, dpi = 350,
       bg = "#1a1a2e")

# ============================================================
# 7. SPEED COMPARISON
# ============================================================

get_speed <- function(path, model_name) {
  df <- read.csv(path)
  df[df$category == "Speed_ms" & df$metric == "inference", ] %>%
    mutate(model = model_name,
           fps   = round(1000 / value, 1))
}

speed_all <- bind_rows(
  get_speed("data/YOLO8/yolov8x_test_comprehensive_summary_statistics.csv",  "YOLOv8"),
  get_speed("data/YOLO9/yolov9x_test_comprehensive_summary_statistics.csv",  "YOLOv9"),
  get_speed("data/YOLO11/yolov11x_test_comprehensive_summary_statistics.csv", "YOLOv11")
) %>% mutate(model = factor(model, levels = c("YOLOv8", "YOLOv9", "YOLOv11")))

p_speed <- ggplot(speed_all, aes(x = model, y = value, fill = model)) +
  geom_bar(stat = "identity", width = 0.55) +
  geom_text(aes(label = paste0(round(value, 1), " ms\n", fps, " FPS")),
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 32)) +
  labs(title    = "Inference Speed Comparison",
       subtitle = "Milliseconds per image — lower is better",
       x = NULL, y = "Inference Time (ms)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "grey40", size = 10)
  )

ggsave("plots/07_speed_comparison.png", p_speed,
       width = 7, height = 5, dpi = 300)

# ============================================================
# 8. SUMMARY TABLE — PRINT TO CONSOLE
# ============================================================

cat("\n", strrep("=", 70), "\n")
cat("OVERALL SUMMARY TABLE\n")
cat(strrep("=", 70), "\n")

summary_table <- summary_all %>%
  select(model, mean_precision, mean_recall, mean_f1,
         `mAP@0.5`, `mAP@0.5:0.95`) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print(summary_table, row.names = FALSE)

cat("\n", strrep("=", 70), "\n")
cat("All plots saved to /plots/\n")
cat(strrep("=", 70), "\n")

beep("coin")
