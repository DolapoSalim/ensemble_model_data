getwd()

library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(readxl)
library(lubridate)
library(broom)
library(CockR)

# load my data

data_v8 <- read.csv("data/YOLO8/results_V8.csv")
data_v9 <- read.csv("data/YOLO9/results_V9.csv")
data_v11 <- read.csv("data/YOLO11/results_V11.csv")

# HELPER FUNCTIONS
# function to list all column names in each data frame

list_column_names <- function(df) {
  return(colnames(df))
}

# function to standardise all column names
clean_names <- function(df) {
  colnames(df) <- gsub("[. ]+", "_", colnames(df))  # replace dots/spaces with _
  colnames(df) <- gsub("_+", "_", colnames(df))     # collapse multiple underscores
  colnames(df) <- gsub("_$", "", colnames(df))      # remove trailing underscores
  return(df)
}

?gsub

# check the names of the columns
df <-data_v8
list_column_names(df)

data_v8  <- clean_names(data_v8)
data_v9  <- clean_names(data_v9)
data_v11 <- clean_names(data_v11)

df <-data_v11
list_column_names(df)

#name columns to keep
keep_cols <- c("epoch",
               "time",
               "train_box_loss",
               "train_seg_loss",
               "train_cls_loss",
               "train_dfl_loss",
               "metrics_precision_B",
               "metrics_recall_B",
               "metrics_mAP50_B",
               "metrics_mAP50_95_B",
               "metrics_precision_M",
               "metrics_recall_M",
               "metrics_mAP50_M",
               "metrics_mAP50_95_M",
               "val_box_loss",
               "val_seg_loss",
               "val_cls_loss",
               "val_dfl_loss"
)
# pass the data with these preferred columns into eacch dataframe
data_v8  <- data_v8[, keep_cols]
data_v9  <- data_v9[, keep_cols]
data_v11 <- data_v11[, keep_cols]

taster_palettes_discrete()

#cols <-c(taster_palettes_discrete()$espresso_martini_negroni[2], taster_palettes_discrete()$blue_lagoon_mai_tai[4] )

# I'm bad with colors so I will randomly select...hehh heehh
all_cols <- unlist(taster_palettes_discrete())
cols <- sample(all_cols, 2)

names(cols) <- c(keep_cols[3], keep_cols[4])

#simple plot to check
plot_1 <- ggplot(data_v8, aes(x = epoch)) +
  geom_line(aes(y = train_box_loss, color = "train_box_loss"), linewidth = 1.2) +
  geom_line(aes(y = train_seg_loss, color = "train_seg_loss"), linewidth = 1.2) +
  labs(title = "YOLOv8 Loss Curves", x = "Epoch", y = "Loss") +
  scale_color_manual(values = cols,
                     name = "Loss Type",
                     labels = c("Training Box Loss", "Training Segmentation Loss")) +
  theme_minimal()
plot_1

?geom_line()
?scale_color_manual()

# function to plot all the dataframes with the same format
plot_for_all <- function(df) {
  ggplot(df, aes(x = epoch)) +
    geom_line(aes(y = train_box_loss, color = "train_box_loss"), linewidth = 1.2) +
    geom_line(aes(y = train_seg_loss, color = "train_seg_loss"), linewidth = 1.2) +
    labs(x = "Epoch", y = "Loss") +
    scale_color_manual(values = cols,
                       name = "Loss Type",
                       labels = c("Training Box Loss", "Training Segmentation Loss")) +
    theme_minimal()
}



plot_1 <- plot_for_all(data_v8)  + labs(title = "YOLOv8 Loss Curves") #To include the title in the plot for each version
plot_2 <- plot_for_all(data_v9)  + labs(title = "YOLOv9 Loss Curves")
plot_3 <- plot_for_all(data_v11) + labs(title = "YOLOv11 Loss Curves")

plot_all <- ggarrange(plot_1, plot_2, plot_3,
                      ncol = 3, nrow = 1,
                      common.legend = TRUE,  # single shared legend
                      legend = "bottom")

plot_all <- annotate_figure(plot_all,
                top = text_grob("Training Loss Curves across YOLO Versions", 
                                face = "bold", size = 14))
plot_all

?ggsave

ggsave(
  filename = "training_loss_curves.png",
  plot = plot_all,
  path = "plots",
  width = 12, height = 6, units = "in",
  dpi = 300,
  bg = "white"
)

#Plot precision and recal curves for both Box and Mask (seg)
cols <- sample(all_cols, 4)

names(cols) <- c(
  keep_cols[7],
  keep_cols[11],
  keep_cols[8],
  keep_cols[12]
)


plot_2 <- ggplot(data_v11, aes(x = epoch)) +
  geom_line(aes(y = metrics_precision_B, color = "metrics_precision_B")) +
  geom_line(aes(y = metrics_precision_M, color = "metrics_precision_M")) +
  geom_line(aes(y = metrics_recall_B, color = "metrics_recall_B")) +
  geom_line(aes(y = metrics_recall_M, color = "metrics_recall_M")) +
  labs(title = "YOLOv8 Precision and Recall Curves", x = "Epoch", y = "Value") +
  scale_color_manual(values = cols,
                     name = "Metric Type",
                     labels = c("Precision (B)", "Precision (M)", "Recall (B)", "Recall (M)")) +
  theme_minimal()
plot_2
