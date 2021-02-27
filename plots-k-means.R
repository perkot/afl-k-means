p1 <- 
  autoplot(M1.pca, data = M1,
           loadings = TRUE, 
           loadings.colour = '#36332e',
           loadings.size = 0.5,
           loadings.label.colour = '#36332e',
           loadings.label = TRUE,
           loadings.label.size = 4,
           loadings.label.repel = T
  ) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(size = 3, color = "#E4796A") +
  labs(title = "PCA Plot",
       subtitle = "Depicting data for first two principal components + variable loadings") +
  # scale_fill_manual(values = alpha(c("#E4796A"),0.2)) + 
  # scale_color_manual(values = alpha(c("#E4796A"),0.2)) +
  theme_minimal() +
  plot_theme 

p1$layers[[2]]$aes_params$size <- 0.1 # change line thickness 
p1



wss.df %>% 
  ggplot(aes(y=wss, x=1:15)) +
  geom_line(colour="#E4796A") +
  geom_point(colour="#E4796A") +
  scale_x_continuous(
    breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
    labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  scale_y_continuous(
    breaks=c(0,2000,4000,6000,8000,1000),
    labels=c(0,2000,4000,6000,8000,1000)) +
  labs(x = "k",
       y = "within cluster sum of squares",
       title = "Elbow plot",
       subtitle = "Depicting within cluster sum of squares against number of clusters") +
  theme_minimal() +
  plot_theme 



sil.df %>% 
  ggplot(aes(y=M1_sil_values, x=2:15)) +
  geom_line(colour="#E4796A") +
  geom_point(colour="#E4796A") +
  scale_x_continuous(
    breaks=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15),
    labels=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  scale_y_continuous(
    breaks=c(0.1,0.2,0.3,0.4,0.5),
    labels=c(0.1,0.2,0.3,0.4,0.5)) +
  labs(x = "k",
       y = "average silhouettes",
       title = "Silhouette plot",
       subtitle = "Depicting average silhouettes against number of clusters") +
  theme_minimal() +
  plot_theme



plot.data <- fviz_cluster(k, data = M1) # save to access $data
data <- plot.data$data # this is all you need
hull_data <- data %>%
  group_by(cluster) %>%
  slice(chull(x, y))
ggplot(data, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_polygon(data = hull_data, 
               alpha = 0.5, 
               aes(fill = cluster, linetype = cluster)) +
  scale_colour_manual(values = colour_theme) +
  scale_fill_manual(values = colour_theme) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(alpha = 0.01) +
  labs(title = "Cluster Plot",
       subtitle = "Depicting k = 4 for first two principal components") +
  theme_minimal() +
  plot_theme



# join cluster groupings back to data
M1[,"Cluster"] <- k$clust
# coerce to factor 
M1$Cluster <- as.factor(M1$Cluster)
# plot now with k means clusters
p1 <-
  autoplot(M1.pca, 
           data = M1,
           colour = "Cluster", # need to join k$clust back to M1
           loadings = TRUE, 
           loadings.colour = '#666257',
           loadings.size = 0.5,
           loadings.label.colour = '#666257',
           loadings.label = TRUE,
           loadings.label.size = 4,
           loadings.label.repel = T
  ) +
  scale_colour_manual(values = colour_theme) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(alpha = 0.01, size = 6) +
  labs(title = "Cluster Plot",
       subtitle = "Depicting k = 4 for first two principal components + variable loadings") +
  theme_minimal() +
  plot_theme
p1$layers[[2]]$aes_params$size <- 0.1 # change line thickness 
p1




p2 <-
  autoplot(M1.pca, 
           data = M1,
           colour = "Cluster",
  ) +
  scale_colour_manual(values = colour_theme) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(alpha = 0.01) +
  geom_text(vjust=-1, 
            label=M1.pca.pc3$Player_Top,
            size = 3,
            colour = "#4E4F4E") +
  labs(title = "Cluster Plot",
       subtitle = "Depicting k = 4 for first two principal components + variable loadings") +
  theme_minimal() +
  plot_theme
p2$layers[[2]]$aes_params$size <- 0.1 # change line thickness 
p2

# join cluster groupings back to data
M1[,"Position"] <- df.players$PositionType
# coerce to factor 
M1$Position <- as.factor(M1$Position)
# plot now with k means clusters
p3 <-
  autoplot(M1.pca, 
           data = M1,
           colour = "Position", # need to join k$clust back to M1
           loadings = TRUE, 
           loadings.colour = '#666257',
           loadings.size = 0.5,
           loadings.label.colour = '#666257',
           loadings.label = TRUE,
           loadings.label.size = 4,
           loadings.label.repel = T
  ) +
  scale_colour_manual(values = colour_theme) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(alpha = 0.01, size = 6) +
  labs(title = "Cluster Plot",
       subtitle = "Depicting player position for first two principal components + variable loadings") +
  theme_minimal() +
  plot_theme
p3$layers[[2]]$aes_params$size <- 0.1 # change line thickness 
p3




