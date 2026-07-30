#' Perform principal component analysis on project data
#'
#' Performs a scaled principal component analysis (PCA) of numeric project-level
#' shuttle-box metrics. The returned object includes scores, loadings, two
#' complementary outlier screens, and four ready-to-display plots.
#'
#' @param data Project-results data containing one row per trial or individual.
#' @param mahalanobis_th Probability used for the chi-squared Mahalanobis
#'   distance cutoff. Default is 0.7, retained for compatibility with earlier
#'   ShuttleboxR versions.
#' @param dbscan_th `eps` value supplied to DBSCAN. Default is 1.
#' @param print_labels Logical. Show individual labels on PCA plots. Default is
#'   `TRUE`.
#' @param id_col Identifier column. Default is `"fileID"`.
#' @param var_col Variable plotted against PC1. Default is `"Tpref"`.
#' @param biplot_variables Logical. Show variable vectors on the biplot.
#'
#' @return A list containing the PCA object, loadings, scores, outlier table,
#'   retained row identifiers, and plots.
#'
#' @examples
#' example_file <- system.file(
#'   "extdata", "project_database_example.csv", package = "ShuttleboxR"
#' )
#' project_data <- read_project_database(example_file)
#'
#' pca_data <- project_data[c(
#'   "fileID", "mass", "Tpref", "Tavoid_lower", "Tavoid_upper",
#'   "Tpref_range", "tot_distance", "nr_shuttles"
#' )]
#'
#' pca_result <- pca(pca_data, print_labels = FALSE)
#' pca_result$plots$biplot
#'
#' @import FactoMineR factoextra ggrepel dbscan ggplot2
#' @export
pca <- function(data,
                mahalanobis_th = 0.7,
                dbscan_th = 1,
                print_labels = TRUE,
                id_col = "fileID",
                var_col = "Tpref",
                biplot_variables = TRUE) {
  data <- .standardise_project_data(data)

  if (!id_col %in% names(data)) {
    stop("Identifier column `", id_col, "` was not found.", call. = FALSE)
  }
  if (!var_col %in% names(data) || !is.numeric(data[[var_col]])) {
    stop("`var_col` must name a numeric column in `data`.", call. = FALSE)
  }
  if (!is.numeric(mahalanobis_th) || length(mahalanobis_th) != 1L ||
      is.na(mahalanobis_th) || mahalanobis_th <= 0 || mahalanobis_th >= 1) {
    stop("`mahalanobis_th` must be a number between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(dbscan_th) || length(dbscan_th) != 1L ||
      is.na(dbscan_th) || dbscan_th <= 0) {
    stop("`dbscan_th` must be greater than zero.", call. = FALSE)
  }

  identifiers <- as.character(data[[id_col]])
  numeric_data <- data[vapply(data, is.numeric, logical(1))]

  # Numeric identifier fields should not influence the PCA.
  numeric_data[[id_col]] <- NULL
  numeric_data[["ID"]] <- NULL

  usable <- vapply(
    numeric_data,
    function(x) {
      finite <- x[is.finite(x)]
      length(finite) > 1L && stats::sd(finite) > 0
    },
    logical(1)
  )
  numeric_data <- numeric_data[, usable, drop = FALSE]

  if (ncol(numeric_data) < 2L) {
    stop("At least two numeric variables with non-zero variance are required.", call. = FALSE)
  }

  complete_rows <- stats::complete.cases(numeric_data) &
    is.finite(data[[var_col]]) & !is.na(identifiers)

  if (sum(complete_rows) < 5L) {
    stop("At least five complete rows are required for PCA.", call. = FALSE)
  }
  if (!all(complete_rows)) {
    warning(
      sum(!complete_rows),
      " row(s) with missing or non-finite values were omitted.",
      call. = FALSE
    )
  }

  pca_data <- numeric_data[complete_rows, , drop = FALSE]
  retained_ids <- identifiers[complete_rows]
  rownames(pca_data) <- make.unique(retained_ids)

  pca_result <- FactoMineR::PCA(pca_data, scale.unit = TRUE, graph = FALSE)
  pca_scores <- pca_result$ind$coord

  score_sd <- apply(pca_scores, 2, stats::sd, na.rm = TRUE)
  score_keep <- is.finite(score_sd) & score_sd > sqrt(.Machine$double.eps)
  distance_scores <- pca_scores[, score_keep, drop = FALSE]

  center <- colMeans(distance_scores)
  covariance <- stats::cov(distance_scores)
  mahalanobis_dist <- stats::mahalanobis(
    distance_scores,
    center,
    covariance
  )
  mahalanobis_cutoff <- stats::qchisq(
    mahalanobis_th,
    df = ncol(distance_scores)
  )
  mahalanobis_outliers <- rownames(pca_scores)[
    mahalanobis_dist > mahalanobis_cutoff
  ]

  if (ncol(pca_scores) >= 2L) {
    dbscan_result <- dbscan::dbscan(
      pca_scores[, 1:2, drop = FALSE],
      eps = dbscan_th,
      minPts = 5
    )
    dbscan_outliers <- rownames(pca_scores)[dbscan_result$cluster == 0]
  } else {
    dbscan_outliers <- character(0)
  }

  outliers <- data.frame(
    method = c(
      rep("Mahalanobis", length(mahalanobis_outliers)),
      rep("DBSCAN", length(dbscan_outliers))
    ),
    fileID = c(mahalanobis_outliers, dbscan_outliers),
    stringsAsFactors = FALSE
  )

  label_content <- if (isTRUE(print_labels)) "all" else "var"
  invisible_elements <- if (isTRUE(biplot_variables)) "none" else "var"

  p1 <- factoextra::fviz_screeplot(
    pca_result,
    addlabels = TRUE,
    main = "Scree plot"
  )

  p2 <- factoextra::fviz_pca_biplot(
    pca_result,
    label = label_content,
    invisible = invisible_elements,
    repel = TRUE,
    col.var = "steelblue",
    col.ind = "firebrick",
    title = "PCA biplot"
  )

  p3 <- factoextra::fviz_contrib(
    pca_result,
    choice = "var",
    axes = 1
  ) + ggplot2::ggtitle("Contribution of variables to PC1")

  plot_data <- data.frame(
    id = retained_ids,
    PC1 = pca_scores[, 1],
    variable = data[[var_col]][complete_rows],
    stringsAsFactors = FALSE
  )
  if (!isTRUE(print_labels)) {
    plot_data$id <- ""
  }

  p4 <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = PC1, y = variable)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggrepel::geom_text_repel(ggplot2::aes(label = id)) +
    ggplot2::labs(
      title = paste("PC1 scores versus", var_col),
      x = "PC1 score",
      y = var_col
    ) +
    ggplot2::theme_light()

  list(
    pca = pca_result,
    pca_loadings = pca_result$var$coord,
    pca_scores = pca_scores,
    outliers = outliers,
    retained_ids = retained_ids,
    plots = list(
      screeplot = p1,
      biplot = p2,
      pc1contributionplot = p3,
      varplot = p4
    )
  )
}
