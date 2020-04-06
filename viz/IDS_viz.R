#############
## IDS viz ##
#############

## config
setwd("~/git/ids/") ## set your directory to a clone of the GitHub repository
library(tidyverse)
library(patchwork)
library(ggrepel)
library(maps)
library(viridis)

## read in all datasets
d1 <- read.csv("./viz/vizData/IDS_fig1.csv")
d2 <- read.csv("./viz/vizData/IDS_fig2.csv")
d3 <- read.csv("./viz/vizData/IDS_fig3.csv")
d4a <- read.csv("./results/lasso_cat_inspeaker_confusion.csv")
d4b <- read.csv("./results/performance_cvsite.csv")
d5 <- read.csv("./viz/vizData/IDS_fig5.csv")
ds2 <- read.csv("./viz/vizData/IDS_figS2.csv")

## colors
# eltblue .8: 133 193 231 // #85c1e7
# midblue .8: 22 132 252 // #1684fc
# orange .7: 253 127 35 // #fd7f23
# orange_red .8: 253 71 30 // #fd471e
catcolors <- c("#85c1e7", "#1684fc", "#fd7f23", "#fd471e")

## fig 1 ####################################################################
## map of fieldsites

# add accent to Sápara (unicode ugh)
d1 <- d1 %>% mutate(culture = str_replace(culture,"Sapara","Sápara"))

mapWorld <- borders("world", colour="gray80", fill="gray80")
mp <- ggplot() + mapWorld
idsmap.x <- d1$long
idsmap.y <- d1$lat
(
  fig1 <- mp +
    geom_text_repel(aes(x = idsmap.x,
                        y = idsmap.y,
                        label = d1$culture),
                    size = 4,
                    vjust = .5) +
    geom_point(aes(x = idsmap.x, 
                   y = idsmap.y,
                   shape = factor(d1$urban)),
               size = 2,
               alpha = 0.6) + 
    scale_shape_manual(values = c(21, 23)) + 
    coord_cartesian(ylim = c(-50, 90)) +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    )
  )
ggsave("./viz/pdf/IDS_fig1.pdf", width = 8, height = 4)

## fig 2 ####################################################################
## accuracy and RT from naïve listener experiment

# reorder cats
d2 <- d2 %>% mutate(cats = fct_relevel(cat, "id song", "id speech", "ad song", "ad speech"))

# uses geom_flat_violin from https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R
source("./viz/vizData/geom_flat_violin.R")

# panel A: infant-directedness
(
  fig2a <- ggplot(data = d2, aes(y = baby,
                               x = cats,
                               fill = cats)) +
    geom_hline(yintercept = .5,
               linetype = "dashed",
               alpha = .8) +
    geom_point(aes(y = baby, 
                   color = cats),
               position = position_jitter(width = .15,
                                          seed = 6012),
               size = .5, alpha = .8) +
    scale_color_manual(values = catcolors) +
    geom_line(aes(group = pers),
              position = position_jitter(width = .15, seed = 6012),
              alpha = .05) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0),
                     trim = TRUE,
                     alpha = .8) + 
    scale_fill_manual(values = catcolors) +
    geom_boxplot(
      fill = "white",
      notch = TRUE,
      alpha = .8,
      width = .05,
      position = position_nudge(x = .2),
      outlier.shape = NA,
      coef = 0
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black", size = 10),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'n'
    ) +
    scale_x_discrete(labels = c("ID Song", "ID Speech", "AD Song", "AD Speech")) +
    ylab("Perceived infant-directedness \n(proportion of \"baby\" responses)") +
    xlab("") +
    expand_limits(x = c(1.2,4.8)) +
    ggtitle("(A)")
  )

# panel B: RT
(
  fig2b <- ggplot(data = d2, aes(y = rt,
                                 x = cats,
                                 fill = cats)) +
    geom_point(aes(y = rt, 
                   color = cats),
               position = position_jitter(width = .15, seed = 6012), size = .5, alpha = .8) +
    scale_color_manual(values = catcolors) +
    geom_line(aes(group = pers),
              position = position_jitter(width = .15, seed = 6012),
              alpha = .05) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0),
                     trim = TRUE,
                     alpha = .8
                     ) + 
    scale_fill_manual(values = catcolors) +
    geom_boxplot(
      fill = "white",
      notch = TRUE,
      alpha = .8,
      width = .05,
      position = position_nudge(x = .2),
      outlier.shape = NA,
      coef = 0
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black", size = 10),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'n'
    ) +
    scale_x_discrete(labels = c("ID Song", "ID Speech", "AD Song", "AD Speech")) +
    ylab("Response time (in seconds) when answering correctly") +
    xlab("") +
    expand_limits(x = c(1.2,4.8)) +
    ggtitle("(B)")
)

# merge for full fig
fig2 <- fig2a + fig2b
ggsave("./viz/pdf/IDS_fig2.pdf", width = 10, height = 5)

## Fig 3 ####################################################################
## boxplots for all acoustic features across 3 comparison types

# Panel A: ID vs AD
(
  ggplot(d3, aes(x = factor(feat), z, fill = infantdir)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             alpha = .6) +
  geom_boxplot(color = "black",
               notch = TRUE,
               outlier.shape = NA,
               lwd = 0.2) +
  scale_fill_manual(values = c("#fd471e", "#1684fc")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black",
                               size = 12),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'n'
  ) +
  scale_x_discrete(limits = rev(levels(d3$feat))) +
  xlab("") +
  ylab("") +
  coord_flip() +
  ggtitle("ID vs. AD (overall)")
)
ggsave("./viz/pdf/IDS_fig3A.pdf", width = 2.5, height = 10)

# Panel B: ID song vs AD song
(
  ggplot(subset(d3, song = 1), aes(feat, z, fill = infantdir)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             alpha = .6) +
  geom_boxplot(color = "black",
               notch = TRUE,
               outlier.shape = NA,
               lwd = 0.2) +
  scale_fill_manual(values = c("#fd7f23", "#85c1e7")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black",
                               size = 12),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'n'
  ) +
  scale_x_discrete(limits = rev(levels(d3$feat))) +
  xlab("") +
  ylab("") +
  coord_flip() +
  ggtitle("ID Song vs. AD Song")
)
ggsave("./viz/pdf/IDS_fig3B.pdf", width = 2.5, height = 10)

# Panel C: ID song vs ID speech
(
  ggplot(subset(d3, infantdir = 1), aes(feat, z, fill = song)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             alpha = .6) +
  geom_boxplot(color = "black",
               notch = TRUE,
               outlier.shape = NA,
               lwd = 0.2) +
  scale_fill_manual(values = c("#1684fc", "#85c1e7")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black",
                             size = 12),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'n'
    ) +
  scale_x_discrete(limits = rev(levels(d3$feat))) +
  xlab("") +
  ylab("") +
  coord_flip() +
  ggtitle("ID Song vs. ID Speech")
)
ggsave("./viz/pdf/IDS_fig3C.pdf", width = 2.5, height = 10)

## Fig 4 ####################################################################
## Heatmap for lasso confusion matrix / barplot of lasso performance

# Panel A: heatmap for lasso confusion
# reorder vocalization types
d4a <- d4a %>% mutate(pred = fct_relevel(pred, "infant_song", "infant_speech", "adult_song", "adult_speech"))
d4a <- d4a %>% mutate(actual = fct_relevel(actual, "infant_song", "infant_speech", "adult_song", "adult_speech"))

(
  fig4a <- ggplot(d4a, aes(x = actual, y = pred, fill = N),
                  guide = FALSE) +
    geom_tile() +
    scale_fill_viridis(option = "cividis") +
    geom_text(aes(label = N),
              color = "white",
              fontface = "bold",
              size = 6) +
    theme_bw() +
    theme(text = element_text(size = 14,
                              color = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12,
                                   color = "black"),
          legend.position = 'n') +
    scale_x_discrete(labels = c("ID Song","ID Speech","AD Song","AD Speech"),
                     expand = c(0,0)) +
    scale_y_discrete(labels = c("ID Song","ID Speech","AD Song","AD Speech"),
                     expand = c(0,0)) +
    ylab("LASSO Prediction") +
    xlab("Vocalization Type") +
    ggtitle("(A)")
)

# Panel B: lasso accuracy
(
  fig4b <- ggplot(data = slice(d4b, 3:6), aes(x = comparison, y = acc)) +
    geom_hline(yintercept = .50, color = "navy", size = 1.5, linetype = "dashed") +
    geom_bar(stat = "identity", alpha = .9, colour = "black", fill = "#1991AA") +
    geom_errorbar(aes(ymin = cilo, ymax = cihi), width = .1) +
    theme_bw() +
    theme(text = element_text(size = 14,
                              color = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 12, 
                                   color = "black"),
          axis.text.x = element_text(size = 10,
                                     color = "black")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,1.05),
                       breaks = c(.10,.20,.3,.4,.5,.6,.7,.8,.9,1),
                       labels = c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")) + 
    scale_x_discrete(labels = c("ID vs. AD \n(overall)", "ID Song vs. \nAD Song", "ID Speech vs. \nAD Speech", "ID Song vs.\nID Speech")) +
    ylab("Accuracy") +
    xlab("Comparison") +
    ggtitle("(B)")
)

# combine
(
  fig4 <- fig4a + fig4b
)
ggsave("./viz/pdf/IDS_fig4.pdf", width = 12, height = 5)

## Fig 5 ####################################################################
## grid of scatterplots for acoustic features predicting IDness

# reorder cats
d5 <- d5 %>% mutate(cats = fct_relevel(cat, "id song", "id speech", "ad song", "ad speech"))

# legend sizing for patchwork
fig5guide <- guide_legend(override.aes = list(alpha = 1,
                                              size = 4))

# 12 plots
r2 <- bquote(paste(italic(R)^{2}," = ",0.31, " "))
r2str <- as.character(as.expression(r2))
(
  fig5a <- ggplot(data = subset(d5, praat_f0_median_c < 1.25), aes(x = praat_f0_median_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
  scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
  geom_smooth(
    method = lm,
    se = TRUE,
    color = "black",
    alpha = .2
  ) +
  geom_line(
    stat = "smooth",
    method = lm,
    size = 0,
    alpha = .2
  ) +
  geom_line(
    stat = "smooth",
    method = loess,
    size = 1,
    alpha = .5,
    color = "blue"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = 'n'
  ) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  xlab("Pitch (Median)") +
  ylab("Infant-directedness") +
  ggtitle("(A)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.27, " "))
r2str <- as.character(as.expression(r2))
(
  fig5b <- ggplot(subset(d5, praat_f0_iqr_c < 1.8 & praat_f0_iqr_c >-2), aes(x = praat_f0_iqr_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
  scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
  geom_smooth(
    method = lm,
    se = TRUE,
    color = "black",
    alpha = .2
  ) +
  geom_line(
    stat = "smooth",
    method = lm,
    size = 0,
    alpha = .2
  ) +
  geom_line(
    stat = "smooth",
    method = loess,
    size = 1,
    alpha = .5,
    color = "blue"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = 'n'
  ) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  xlab("Pitch (IQR)") +
  ylab("") +
  ggtitle("(B)")
  )

r2 <- bquote(paste(italic(R)^{2}," = ",0.12, " "))
r2str <- as.character(as.expression(r2))
(
  fig5c <- ggplot(subset(d5, praat_ittrv_median_c < 2 & praat_ittrv_median_c > -2), 
                  aes(x = praat_ittrv_median_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Intensity Space (Median)") +
    ylab("") +
    ggtitle("(C)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.06, " "))
r2str <- as.character(as.expression(r2))
(
  fig5d <- ggplot(subset(d5, abs(tm_std_hz_c)<2),
                  aes(x = tm_std_hz_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Temporal Modulation (SD)") +
    ylab("Infant-directedness") +
    ggtitle("(D)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.06, " "))
r2str <- as.character(as.expression(r2))
(
  fig5e <- ggplot(subset(d5, abs(mir_roughness_iqr_c)<0.5),
                  aes(x = mir_roughness_iqr_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Roughness (IQR)") +
    ylab("") +
    ggtitle("(E)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.04, " "))
r2str <- as.character(as.expression(r2))
(
  fig5f <- ggplot(d5, aes(x = mir_inharmonicity_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Inharmonicity") +
    ylab("") +
    ggtitle("(F)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.04, " "))
r2str <- as.character(as.expression(r2))
(
  fig5g <- ggplot(subset(d5, abs(mir_roughness_med_c)<0.5),
                  aes(x = mir_roughness_med_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Roughness (Median)") +
    ylab("Infant-directedness") +
    ggtitle("(G)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.03, " "))
r2str <- as.character(as.expression(r2))
(
  fig5h <- ggplot(d5, aes(x = praat_f0trv_iqr_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Pitch Space (IQR)") +
    ylab("") +
    ggtitle("(H)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.03, " "))
r2str <- as.character(as.expression(r2))
(
  fig5i <- ggplot(subset(d5, abs(mir_attack_med_c)<2),
                  aes(x = mir_attack_med_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Attack Curve Slope (Median)") +
    ylab("") +
    ggtitle("(I)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.02, " "))
r2str <- as.character(as.expression(r2))
(
  fig5j <- ggplot(subset(d5, abs(mir_rolloff85_c)<2),
                  aes(x = mir_rolloff85_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("Energy Roll-off (85th %ile)") +
    ylab("Infant-directedness") +
    ggtitle("(J)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.02, " "))
r2str <- as.character(as.expression(r2))
(
  fig5k <- ggplot(subset(d5, abs(praat_f1_median_c)<2),
                  aes(x = praat_f1_median_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n',
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("First Formant (Median)") +
    ylab("") +
    ggtitle("(K)")
)

r2 <- bquote(paste(italic(R)^{2}," = ",0.01, " "))
r2str <- as.character(as.expression(r2))
(
  fig5l <- ggplot(d5, aes(x = npvi_phrase_c, y = baby)) +
    annotate(geom = "text",
             x = Inf,
             y = 0,
             label = r2str,
             parse = TRUE,
             vjust = "inward", 
             hjust = "inward") +
    geom_point(
      aes(color = factor(cats)),
      alpha = .5,
      size = 0.5
    ) +
    scale_color_manual(guide = fig5guide, values = catcolors, labels = c("ID Song","ID Speech","AD Song","AD Speech")) +
    geom_smooth(
      method = lm,
      se = TRUE,
      color = "black",
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = lm,
      size = 0,
      alpha = .2
    ) +
    geom_line(
      stat = "smooth",
      method = loess,
      size = 1,
      alpha = .5,
      color = "blue"
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # legend.position = 'n'
    ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    xlab("nPVI (per phrase)") +
    ylab("") +
    ggtitle("(L)")
)

# combined plot
fig5 <- (fig5a + fig5b + fig5c) / 
    (fig5d + fig5e + fig5f) / 
    (fig5g + fig5h + fig5i) /
    (fig5j + fig5k + fig5l) & theme(legend.position = "bottom",
                                    legend.title = element_blank())
fig5 + plot_layout(guides = "collect")
ggsave("./viz/pdf/IDS_fig5.pdf", width = 10, height = 12)


## Fig S2 ####################################################################
## forestplot of IDness by cat by fieldsite

ds2 <- ds2 %>% mutate(cats = fct_relevel(cat, "id song", "id speech", "ad song", "ad speech"))
ds2 <- ds2 %>% mutate(fieldsite = fct_reorder(fieldsite, n, .desc = FALSE))

(
  figS2 <- ggplot(ds2, aes(x = fieldsite, 
                           y = baby, 
                           fill = cats)) +
    geom_hline(yintercept = .5,
               alpha = .5,
               linetype = "dashed") +
    geom_boxplot(width = .5,
                 outlier.alpha = .5,
                 outlier.size = .2) +
    scale_fill_manual(values = catcolors,
                      labels = c("ID Song", "ID Speech", "AD Song", "AD Speech")) +
    scale_x_discrete(labels = c("Quechua",
                                "Arawak",
                                "Enga",
                                "Colombian Mestizos",
                                "Afro-Colombians",
                                "Hadza",
                                "Krakow",
                                "Tsimane",
                                "Mbendjele",
                                "Sápara & Achuar",
                                "Toposa",
                                "Rural Poland",
                                "Nyangatom",
                                "Mentawai Islanders",
                                "Jenu Kurubas",
                                "Turku",
                                "Tannese Vanuatuans",
                                "Beijing",
                                "San Diego",
                                "Toronto",
                                "Wellington")) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black",
                               size = 12),
      axis.text.x = element_text(angle = 45,
                                 hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    ) +
    xlab("") +
    ylab("Infant-directedness") +
    labs(fill = "Vocalization Type")
)
ggsave("./viz/pdf/IDS_figS2.pdf", width = 15, height = 8)
