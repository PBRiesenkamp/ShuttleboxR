#' Perform a PCA on the project data and calculate outliers
#'
#' This function performs a Principal Component Analysis, and calculates outliers according to the Mahalanobis distance and DBSCAN clustering.
#' The function produces a list object containing the pca, pca loadings, pca scores, a table containing the outliers, a PCA biplot, a screeplot, a contribution plot and a plot of a chosen variable against the first principal component
#'
#' @param data (Subset of) project data containg shuttle-box metrics, as output by calc_project_data
#' @param mahalanobis_th, threshold for mahalanobis distance outlier cutoff, default is 0.7
#' @param dbscan_th, threshold for DBSCAN clustering outlier cutoff, default is 1
#' @param print_labels Boolean to print labels for each datapoint, default is TRUE
#' @param id_col Column to be used for identifier labels in plots, default is "fileID"
#' @param var_col Column to plot against principal component, default is "Tpref"
#' @param biplot_variables Boolean to print overlaying of shuttle-box metrics on biplot
#' @import FactoMineR factoextra ggrepel dbscan ggplot2
#' @return List object containing PCA output
#' @export



pca <- function(data, mahalanobis_th = 0.7, dbscan_th = 1, print_labels = TRUE, id_col = "fileID", var_col = "Tpref", biplot_variables = T) {
  
  # Change the id column into row names 
  # Remove all non-numeric columns and perform a PCA
  pca_data <- data
  rownames(pca_data) <- pca_data[[id_col]]
  pca_data[[id_col]] <- NULL
  pca_data <- pca_data[, sapply(pca_data, is.numeric)]
  
  pca <- FactoMineR::PCA(pca_data, scale.unit = TRUE, graph = FALSE)
  pca_scores <- pca$ind$coord
  
  # Compute the Mahalanobis distance
  center <- colMeans(pca_scores)  # Mean of the PCA scores
  cov_matrix <- cov(pca_scores)  # Covariance matrix of PCA scores
  mahalanobis_dist <- mahalanobis(pca_scores, center, cov_matrix)
  mahalanobis_dist_df<-as.data.frame(mahalanobis_dist)
  
  threshold <- qchisq(mahalanobis_th, df = ncol(pca_scores))  # Chi-squared threshold
  outliers_mahalanobis <- rownames(mahalanobis_dist_df)[mahalanobis_dist_df$mahalanobis_dist > threshold]
  
  dbscan_result <- dbscan::dbscan(pca_scores[, 1:2], eps = dbscan_th, minPts = 5)  # Using PC1 and PC2 for DBSCAN
  outliers_dbscan <- rownames(data)[dbscan_result$cluster == 0]  # Points with cluster = 0 are outliers
  
  combined_outliers <- matrix(NA, nrow = 2, ncol = max(length(outliers_mahalanobis), length(outliers_dbscan)))
  combined_outliers[1, 1:length(outliers_mahalanobis)] <- outliers_mahalanobis
  combined_outliers[2, 1:length(outliers_dbscan)] <- outliers_dbscan
  combined_outliers <- as.data.frame(combined_outliers)
  rownames(combined_outliers) <- c("Mahalanobis Outliers", "DBSCAN Outliers")
  
  p1 <- factoextra::fviz_screeplot(pca, addlabels = print_labels, main = "Scree Plot")
  if (print_labels) {
    label_content <- "all"
  }else {label_content <- "var"
  }
  
  p2 <- factoextra::fviz_pca_biplot(pca, label = label_content,
                                    repel = TRUE,  # Avoid overlapping labels
                                    col.var = if (biplot_variables) "blue" else NA,  , # Color for variables
                                    col.ind = "red",  # Color for individuals
                                    title = "PCA Biplot")
  
  p3 <- factoextra::fviz_contrib(pca, choice = "var", axes = 1)+
    ggplot2::ggtitle("Contribution of variables to PC1")
  
  plot_dat <- data.frame(id = data[[id_col]], PC1 = pca$ind$coord[, 1], var_y = data[[var_col]])
  if(print_labels == F){
    plot_dat$id <- ""
  }
  plot_dat[!is.na(plot_dat$id), ]
  
  p4 <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = PC1, y = var_y)) +
    ggplot2::geom_point(color = "#F79518") +
    ggplot2::geom_smooth(method = "lm", color = "black") +  # Adding 95% CI shading
    ggrepel::geom_text_repel(ggplot2::aes(label = id), vjust = -1, hjust = 1.5) +
    ggplot2::labs(title = paste0("PCA Scores from the First Component vs ",var_col," with 95% CI"),
                  x = "PCA First Component Score",
                  y = paste0(var_col)) +
    ggplot2::theme_light()
  
  pca_loadings <- pca$var$coord
  
  output<-list(pca = pca,
               pca_loadings = pca_loadings,
               pca_scores = pca_scores,
               outliers = combined_outliers, 
               plots = list (screeplot = p1, biplot = p2, pc1contributionplot = p3, varplot = p4))
  
  return(output)
  
}
