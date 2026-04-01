library(tidyverse)
library(readxl)
library(ggpmisc)
library(ggpubr)
library(ggrepel)
library(viridis)
library(ggnewscale)

generate_grouped_annotations <- function(data) {
  # 1. Strictly sort by CLASS, then TREATMENT
  sorted_data <- data %>% 
    arrange(CLASS, TREATMENT)
  
  # 2. Identify unique treatments in the order they appear after sorting
  unique_treatments <- unique(sorted_data$TREATMENT)
  
  # Find the global minimum X to prevent text cutoff
  min_x <- min(sorted_data$START_rel, na.rm = TRUE)
  
  layers <- list()
  
  for (i in seq_along(unique_treatments)) {
    tr_name <- unique_treatments[i]
    tr_rows <- sorted_data %>% filter(TREATMENT == tr_name)
    tr_color <- tr_rows$COLOR[1]
    
    # Define vertical lanes for each treatment
    y_min <- i - 0.9
    y_max <- i - 0.1
    y_mid <- (y_min + y_max) / 2
    
    # 3. Add Rectangles
    rect_layer <- annotate(
      "rect",
      xmin = tr_rows$START_rel,
      xmax = tr_rows$END_rel,
      ymin = y_min,
      ymax = y_max,
      alpha = 0.8,
      fill = tr_color
    )
    
    # 4. Add Treatment Labels (placed at the start of the first interval)
    # Using 'min_x - 10' or similar helps prevent the cutoff
    label_layer <- annotate(
      "text",
      label = tr_name,
      x = min(tr_rows$START_rel) - 2, 
      y = y_mid,
      hjust = 1, # Right-align text so it ends at the start of the bar
      size = 3.5,
      color = "#3B4252"
    )
    
    layers <- c(layers, list(rect_layer, label_layer))
  }
  
  return(layers)
}

generate_grouped_annotations <- function(data, highlight_days = NULL) {
  
  # 1. Force 'hospitalisation' to be the first level (bottom)
  # We find all unique classes, remove hospitalisation, then put it back at the start
  other_classes <- sort(setdiff(unique(data$CLASS), "hospitalisation"))
  class_levels <- c("hospitalisation", other_classes)
  
  # 2. Convert to factor and sort
  sorted_data <- data %>% 
    mutate(CLASS = factor(CLASS, levels = class_levels)) %>% 
    arrange(CLASS, TREATMENT)
  
  unique_treatments <- unique(sorted_data$TREATMENT)
  layers <- list()
  
  # --- Treatment Loop ---
  for (i in seq_along(unique_treatments)) {
    tr_name <- unique_treatments[i]
    tr_rows <- sorted_data %>% filter(TREATMENT == tr_name)
    tr_color <- tr_rows$COLOR[1]
    
    y_min <- i - 0.9
    y_max <- i - 0.1
    y_mid <- (y_min + y_max) / 2
    
    rect_layer <- annotate(
      "rect",
      xmin = tr_rows$START_rel, xmax = tr_rows$END_rel,
      ymin = y_min, ymax = y_max,
      alpha = 0.8, fill = tr_color
    )
    
    label_layer <- annotate(
      "text",
      label = tr_name,
      x = min(tr_rows$START_rel, na.rm = TRUE) - 2, 
      y = y_mid,
      hjust = 1, size = 3.5, color = "#3B4252"
    )
    
    layers <- c(layers, list(rect_layer, label_layer))
  }
  
  # --- Highlight Lines ---
  if (!is.null(highlight_days)) {
    vline_layer <- annotate(
      "segment",
      x = highlight_days, xend = highlight_days,
      y = -Inf, yend = +Inf,
      linetype = "dashed",
      color = "#4C566A",
      linewidth = 0.6, alpha = 0.3
    )
    layers <- c(layers, list(vline_layer))
  }
  
  return(layers)
}

generate_class_gantt <- function(data, highlight_days = NULL) {
  
  data <- data %>%
    mutate(CLASS = factor(CLASS)) %>%
    arrange(CLASS, START_rel)
  
  class_levels <- levels(data$CLASS)
  layers <- list()
  
  for (i in seq_along(class_levels)) {
    
    cl <- class_levels[i]
    cl_data <- data %>% filter(CLASS == cl)
    
    y_min <- i - 0.4
    y_max <- i + 0.4
    y_mid <- i
    
    # --- Bars ---
    rect_layer <- annotate(
      "rect",
      xmin = cl_data$START_rel,
      xmax = cl_data$END_rel,
      ymin = y_min,
      ymax = y_max,
      fill = cl_data$COLOR,
      alpha = 0.8
    )
    
    layers <- c(layers, list(rect_layer))
    
    # --- Labels (LEFT of each bar) ---
    label_layer <- geom_text(
      data = cl_data,
      aes(
        x = START_rel,
        y = y_mid,
        label = TREATMENT
      ),
      hjust = 1,              # right-align text
      nudge_x = -1.5,         # move slightly left of bar
      size = 3.5
    )
    
    layers <- c(layers, list(label_layer))
    
    # --- Class label ---
    class_label <- annotate(
      "text",
      x = min(data$START_rel, na.rm = TRUE) - 8,
      y = y_mid,
      label = cl,
      hjust = 1,
      size = 4,
      fontface = "bold"
    )
    
    layers <- c(layers, list(class_label))
  }
  
  # --- Vertical lines ---
  if (!is.null(highlight_days)) {
    vline_layer <- annotate(
      "segment",
      x = highlight_days, xend = highlight_days,
      y = -Inf, yend = Inf,
      linetype = "dashed",
      color = "#4C566A",
      alpha = 0.3
    )
    layers <- c(layers, list(vline_layer))
  }
  
  return(layers)
}




df_a <- read_excel("~/2026_TP53/treatment_TP53.xlsx")
df_a$START_rel <- as_datetime(paste(df_a$START,strftime(df_a$START, "%H:%M:%S")))
df_a$START_rel <- difftime(df_a$START_rel, ymd_hms("2025-06-03 00:00:00"), units = "days")
df_a$END_rel <- as_datetime(paste(df_a$END,strftime(df_a$START, "%H:%M:%S")))
df_a$END_rel <- difftime(df_a$END_rel, ymd_hms("2025-06-03 00:00:00"), units = "days")
#df_a <- df_a %>% filter(`START_rel` > -30)
df_a <- df_a %>% filter(PATIENTID == "Case 1")

df_b <- read_excel("~/2026_TP53/labvals.xlsx")
df_b$DateTime <- as_datetime(paste(df_b$date,strftime(df_b$time, "%H:%M:%S")))
df_b$reldate <- difftime(df_b$DateTime, ymd_hms("2025-06-03 00:00:00"), units = "days")
df_b$`rel date` <- df_b$reldate
#df_b <- df_b %>% filter(`rel date` > -30)
#df_b <- df_b %>% filter(parameter == "WBC (/킠)" & value > 100)
df_b <- df_b %>% filter(patientID == "Case 1")

df <- df_b


df_segments <- df %>%
  arrange(`rel date`) %>%
  mutate(end_date = lead(`rel date`))


Case = "Case 1"
coords = coord_cartesian(xlim = c(-100, 102))
p1_data <-  df_b #%>% filter(!parameter %in% c("albumin (g/L)", "WBC (/킠)"))  


p1 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == "A"), aes(x = reldate, y = value , col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") + 
  scale_color_manual(values = c("#BF616A", "#5E81AC", "#EBCB8B"))


p2 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == "B"), aes(x = reldate, y = value , col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") +
  scale_color_manual(values=c("#5E81AC", "#EBCB8B", "#B48EAD", "#A3BE8C"))       
#  facet_wrap(~category, nrow = 4, scales = "free_y")  
p3 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == "C"), aes(x = reldate, y = value , col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") + 
  annotate("rect", xmin=19, xmax=38, ymin=-Inf, ymax=Inf, alpha = 0.1, fill = "blue")+ 
  annotate("rect", xmin=43, xmax=53, ymin=-Inf, ymax=Inf, alpha = 0.1, fill = "purple")
facet_wrap(~category, nrow = 4, scales = "free_y")  

ymin_val <- min(p1_data %>% filter(parameter == "WBC (/킠)") %>% pull(value)) 
ymax_val <- max(p1_data %>% filter(parameter == "WBC (/킠)") %>% pull(value)) 
ticks <- 2:9
ooms <- 10^seq(2, 4)

ticks %o% ooms
breaks <- as.vector(ticks %o% ooms)

p3 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == c("A")), aes(x = reldate, y = value, col = parameter)) +
  geom_point(data = p1_data %>% filter(category == c("B")), aes(x = reldate, y = value, col = parameter)) +
  geom_line(data = p1_data %>% filter(category == c("C")), aes(x = reldate, y = value, col = parameter)) +
  
    theme_bw() + scale_x_continuous(name = "")  + ylab("") +  xlim(-10,max(p1_data$`rel date`)) +
  scale_color_manual(values=c("#BF616A", "#EBCB8B", "#B48EAD"))     +
  scale_y_continuous(trans = "log10", limits = c(ymin_val, ymax_val))+ annotation_logticks(sides = "l", linewidth = .2)
# scale_color_manual(values=c("#EBCB8B", "#BF616A", "#B48EAD"))    

p4 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == c("C")), aes(x = reldate, y = value, col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") +
  scale_y_continuous(trans = "log10", limits = c(ymin_val, ymax_val))+ annotation_logticks(sides = "l", linewidth = .2)+
  scale_color_manual(values=c("#5E81AC", "#B48EAD", "#A3BE8C",  "#EBCB8B", "#BF616A"))       
#facet_wrap(~category, nrow = 4, scales = "free_y")  


# 1. Define your special days
important_days <- c(0, 29)

# 2. Build the plot
ggplot() +
  generate_class_gantt(df_a, highlight_days = important_days) +
  # Use scale_x_continuous to force these ticks onto the axis
  scale_x_continuous(
    breaks = sort(unique(c(seq(-100, 500, by = 100), important_days))),
    name = "Time (days) relative to diagnosis"
  ) +
  theme_minimal()


# To use it:
# my_plot_layers <- generate_grouped_annotations(df_a)
# ggplot() + my_plot_layers + theme_minimal()
p5 <- ggplot()+generate_grouped_annotations(df_a, highlight_days = important_days)+
  coord_cartesian(clip = "off") +
  theme_bw()+xlab("Time (days) relative to diagnosis") + scale_x_continuous(
    breaks = sort(unique(c(seq(-0, 500, by = 100), important_days))),
    name = "Time (days) relative to diagnosis"
  ) + xlim(-10,max(p1_data$`rel date`))#+ scale_x_continuous(limits = layer_scales(p1)$x$range$range)


case1 <- ggarrange(
  #p4 +scale_x_continuous(position = "top") + rremove("xlab") +
  #   theme(legend.position = "right", legend.title=element_blank()),
  p3 + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank() )+
    theme(legend.position = "right", legend.title=element_blank()),# + theme(legend.position = "right", legend.title=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()),
  # p3,# + theme(legend.position = "right",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()),
  #p1 +theme(legend.position = "right", legend.title=element_blank()),# + theme(legend.position = "right", axis.text.y=element_blank(), axis.ticks.y=element_blank()),
  p5 +theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()), 
  ncol = 1,
  nrow = 2,
  legend =  NULL, 
  common.legend = FALSE,
  heights = c(.7, .3),
  align = 'v'
)

case1

ggsave(
  "~/test.svg",
  plot = last_plot(),
  width = 16.8, # 14.1 x 5.05 in 358 x 256 mm 
  height = 7,# 
  units = "in",
  dpi = 300,
  device = "svg"
)



############### case 2


df_a <- read_excel("~/2026_TP53/treatment_TP53.xlsx")
df_a$START_rel <- as_datetime(paste(df_a$START,strftime(df_a$START, "%H:%M:%S")))
df_a$START_rel <- difftime(df_a$START_rel, ymd_hms("2025-08-12 00:00:00"), units = "days")
df_a$END_rel <- as_datetime(paste(df_a$END,strftime(df_a$START, "%H:%M:%S")))
df_a$END_rel <- difftime(df_a$END_rel, ymd_hms("2025-08-12 00:00:00"), units = "days")
#df_a <- df_a %>% filter(`START_rel` > -30)
df_a <- df_a %>% filter(PATIENTID == "Case 2")


df_b <- read_excel("~/2026_TP53/labvals.xlsx")
df_b$DateTime <- as_datetime(paste(df_b$date,strftime(df_b$time, "%H:%M:%S")))
df_b$reldate <- difftime(df_b$DateTime, ymd_hms("2025-08-12 00:00:00"), units = "days")
df_b$`rel date` <- df_b$reldate
#df_b <- df_b %>% filter(`rel date` > -30)
#df_b <- df_b %>% filter(parameter == "WBC (/킠)" & value > 100)
df_b <- df_b %>% filter(patientID  == "Case 2")

df <- df_b


df_segments <- df %>%
  arrange(`rel date`) %>%
  mutate(end_date = lead(`rel date`))


Case = "Case 2"
coords = coord_cartesian(xlim = c(-100, 102))
p1_data <-  df_b #%>% filter(!parameter %in% c("albumin (g/L)", "WBC (/킠)"))  


p1 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == "A"), aes(x = reldate, y = value , col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") + 
  scale_color_manual(values = c("#BF616A", "#5E81AC", "#EBCB8B"))


p2 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == "B"), aes(x = reldate, y = value , col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") +
  scale_color_manual(values=c("#5E81AC", "#EBCB8B", "#B48EAD", "#A3BE8C"))       
#  facet_wrap(~category, nrow = 4, scales = "free_y")  
p3 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == "C"), aes(x = reldate, y = value , col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") + 
  annotate("rect", xmin=19, xmax=38, ymin=-Inf, ymax=Inf, alpha = 0.1, fill = "blue")+ 
  annotate("rect", xmin=43, xmax=53, ymin=-Inf, ymax=Inf, alpha = 0.1, fill = "purple")
facet_wrap(~category, nrow = 4, scales = "free_y")  

ymin_val <- 1 #min(p1_data %>% filter(parameter == "WBC (/킠)") %>% pull(value))
ymax_val <- max(p1_data %>% filter(parameter == "WBC (/킠)") %>% pull(value)) 
ticks <- 2:9
ooms <- 10^seq(2, 4)

ticks %o% ooms
breaks <- as.vector(ticks %o% ooms)

p3 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == c("A")), aes(x = reldate, y = value, col = parameter)) +
  geom_point(data = p1_data %>% filter(category == c("B")), aes(x = reldate, y = value, col = parameter)) +
  geom_line(data = p1_data %>% filter(category == c("C")), aes(x = reldate, y = value, col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") +
  scale_color_manual(values=c("#BF616A", "#EBCB8B", "#B48EAD"))     +  xlim(-10,max(p1_data$`rel date`)) +
  scale_y_continuous(trans = "log10", limits = c(ymin_val, ymax_val))+ annotation_logticks(sides = "l", linewidth = .2)
# scale_color_manual(values=c("#EBCB8B", "#BF616A", "#B48EAD"))    

p4 <- ggplot() + labs() +
  geom_line(data = p1_data %>% filter(category == c("C")), aes(x = reldate, y = value, col = parameter)) +
  theme_bw() + scale_x_continuous(name = "")  + ylab("") +
  scale_y_continuous(trans = "log10", limits = c(ymin_val, ymax_val))+ annotation_logticks(sides = "l", linewidth = .2)+
  scale_color_manual(values=c("#5E81AC", "#B48EAD", "#A3BE8C",  "#EBCB8B", "#BF616A"))       
#facet_wrap(~category, nrow = 4, scales = "free_y")  


# 1. Define your special days
important_days <- c(0, 29)

# 2. Build the plot
ggplot() +
  generate_class_gantt(df_a, highlight_days = important_days) +
  # Use scale_x_continuous to force these ticks onto the axis
  scale_x_continuous(
    breaks = sort(unique(c(seq(-100, 500, by = 100), important_days))),
    name = "Time (days) relative to diagnosis"
  ) + xlim(-10,max(p1_data$`rel date`)) +
  theme_minimal()


# To use it:
# my_plot_layers <- generate_grouped_annotations(df_a)
# ggplot() + my_plot_layers + theme_minimal()
p5 <- ggplot()+generate_grouped_annotations(df_a, highlight_days = important_days)+
  coord_cartesian(clip = "off") +
  theme_bw()+xlab("Time (days) relative to diagnosis") + scale_x_continuous(
    breaks = sort(unique(c(seq(-0, 500, by = 100), important_days))),
    name = "Time (days) relative to diagnosis"
  ) + xlim(-10,max(p1_data$`rel date`))#+ scale_x_continuous(limits = layer_scales(p1)$x$range$range)


case2 <- ggarrange(
  #p4 +scale_x_continuous(position = "top") + rremove("xlab") +
  #   theme(legend.position = "right", legend.title=element_blank()),
  p3 + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank() )+
    theme(legend.position = "right", legend.title=element_blank()),# + theme(legend.position = "right", legend.title=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()),
  # p3,# + theme(legend.position = "right",  panel.grid.major = element_blank(), panel.grid.minor = element_blank()),
  #p1 +theme(legend.position = "right", legend.title=element_blank()),# + theme(legend.position = "right", axis.text.y=element_blank(), axis.ticks.y=element_blank()),
  p5 +theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()), 
  ncol = 1,
  nrow = 2,
  legend =  NULL, 
  common.legend = FALSE,
  heights = c(.6, .4),
  align = 'v'
)

ggarrange(case1, case2, ncol = 1)

ggsave(
  "~/2026_TP53/test.svg",
  plot = last_plot(),
  width = 16.8, # 14.1 x 5.05 in 358 x 256 mm 
  height = 7,# 
  units = "in",
  dpi = 300,
  device = "svg"
)

ggsave(
  "~/2026_TP53/test.png",
  plot = last_plot(),
  width = 14, # 14.1 x 5.05 in 358 x 256 mm 
  height = 16,# 
  units = "in",
  dpi = 300,
  device = "png"
)
