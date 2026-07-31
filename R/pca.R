#' Perform principal component analysis on project data
#'
#' Performs a scaled principal component analysis (PCA) of numeric project-level
#' shuttle-box metrics. PCA summarises correlated metrics as new axes. Fish close
#' together have similar multivariate profiles, while separated fish differ in
#' one or more metrics. Variable arrows show the direction in which each metric
#' increases and help identify which measurements may be driving a flagged fish.
#'
#' Two complementary screens are provided. Mahalanobis distance identifies fish
#' far from the multivariate centre while accounting for covariance among PCA
#' dimensions. DBSCAN identifies fish in locally sparse regions of the PC1-PC2
#' plot. These screens identify candidates for review, not automatic exclusions.
#'
#' @param data Project-results data containing one row per trial or individual.
#' @param mahalanobis_th Probability used for the chi-squared Mahalanobis
#'   distance cutoff. Default is 0.975. Smaller values flag more fish.
#' @param dbscan_th `eps` value supplied to DBSCAN. Default is 1. Smaller values
#'   generally classify more fish as locally isolated.
#' @param print_labels Logical. Show labels for all individuals on PCA plots.
#'   Default is `TRUE`. Flagged fish are labelled when `highlight_outliers` is
#'   `TRUE` even when `print_labels = FALSE`.
#' @param id_col Identifier column. Default is `"fileID"`.
#' @param var_col Variable plotted against PC1. Default is `"Tpref"`.
#' @param biplot_variables Logical. Show variable vectors on the biplot.
#' @param highlight_outliers Logical. Circle and label fish flagged by either
#'   outlier screen on the biplot and PC1-variable plot. Default is `TRUE`.
#' @param n_driver_variables Number of unusually high or low original variables
#'   reported for each flagged fish. Default is 3.
#'
#' @return A list containing the PCA object, loadings, scores, method-level
#'   outlier table, an `outlier_details` table describing potential drivers,
#'   retained row identifiers, variance explained, and plots.
#'
#' @examples
#' example_file <- system.file(
#'   "extdata", "project_database_example.csv", package = "ShuttleboxR"
#' )
#' project_data <- read_project_database(example_file)
#'
#' pca_data <- project_data[c(
#'   "fileID", "Tpref", "Tpref_range", "grav_time",
#'   "tot_distance", "nr_shuttles", "t_near_max", "t_near_min"
#' )]
#'
#' pca_result <- pca(pca_data, print_labels = FALSE)
#' pca_result$plots$biplot
#' pca_result$outlier_details
#'
#' @import FactoMineR factoextra ggrepel dbscan ggplot2
#' @export
pca <- function(data,
                mahalanobis_th = 0.975,
                dbscan_th = 1,
                print_labels = TRUE,
                id_col = "fileID",
                var_col = "Tpref",
                biplot_variables = TRUE,
                highlight_outliers = TRUE,
                n_driver_variables = 3) {
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
  if (!is.numeric(n_driver_variables) || length(n_driver_variables) != 1L ||
      is.na(n_driver_variables) || n_driver_variables < 1) {
    stop("`n_driver_variables` must be a positive whole number.", call. = FALSE)
  }
  n_driver_variables <- as.integer(n_driver_variables)

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
  variance_explained <- pca_result$eig

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
    dbscan_result <- NULL
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

  flagged_ids <- unique(outliers$fileID)
  scaled_original <- scale(pca_data)
  driver_count <- min(n_driver_variables, ncol(scaled_original))

  outlier_details <- data.frame(
    fileID = flagged_ids,
    methods = character(length(flagged_ids)),
    mahalanobis_distance = numeric(length(flagged_ids)),
    potential_drivers = character(length(flagged_ids)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(flagged_ids)) {
    id <- flagged_ids[[i]]
    methods <- unique(outliers$method[outliers$fileID == id])
    row_index <- match(id, rownames(pca_scores))
    z_values <- scaled_original[row_index, ]
    driver_order <- order(abs(z_values), decreasing = TRUE)[seq_len(driver_count)]
    driver_text <- paste(
      paste0(
        colnames(scaled_original)[driver_order],
        ifelse(z_values[driver_order] >= 0, " high", " low"),
        " (", round(z_values[driver_order], 1), " SD)"
      ),
      collapse = "; "
    )

    outlier_details$methods[[i]] <- paste(methods, collapse = " + ")
    outlier_details$mahalanobis_distance[[i]] <- mahalanobis_dist[[row_index]]
    outlier_details$potential_drivers[[i]] <- driver_text
  }

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
    col.ind = "grey45",
    title = "PCA biplot"
  )

  score_plot_data <- data.frame(
    fileID = rownames(pca_scores),
    PC1 = pca_scores[, 1],
    PC2 = if (ncol(pca_scores) >= 2L) pca_scores[, 2] else 0,
    flagged = rownames(pca_scores) %in% flagged_ids,
    stringsAsFactors = FALSE
  )

  if (isTRUE(highlight_outliers) && any(score_plot_data$flagged)) {
    flagged_plot_data <- score_plot_data[score_plot_data$flagged, , drop = FALSE]
    p2 <- p2 +
      ggplot2::geom_point(
        data = flagged_plot_data,
        ggplot2::aes(x = PC1, y = PC2),
        inherit.aes = FALSE,
        shape = 21,
        size = 3.6,
        stroke = 1.2,
        fill = NA,
        colour = "firebrick"
      ) +
      ggrepel::geom_text_repel(
        data = flagged_plot_data,
        ggplot2::aes(x = PC1, y = PC2, label = fileID),
        inherit.aes = FALSE,
        colour = "firebrick",
        max.overlaps = Inf
      )
  }

  p3 <- factoextra::fviz_contrib(
    pca_result,
    choice = "var",
    axes = 1
  ) + ggplot2::ggtitle("Contribution of variables to PC1")

  plot_data <- data.frame(
    id = retained_ids,
    PC1 = pca_scores[, 1],
    variable = data[[var_col]][complete_rows],
    flagged = retained_ids %in% flagged_ids,
    stringsAsFactors = FALSE
  )
  plot_data$display_label <- if (isTRUE(print_labels)) plot_data$id else ""
  if (isTRUE(highlight_outliers)) {
    plot_data$display_label[plot_data$flagged] <- plot_data$id[plot_data$flagged]
  }

  p4 <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = PC1, y = variable)
  ) +
    ggplot2::geom_point(ggplot2::aes(colour = flagged)) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "grey35") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = display_label, colour = flagged),
      show.legend = FALSE,
      max.overlaps = Inf
    ) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "grey45", `TRUE` = "firebrick")) +
    ggplot2::labs(
      title = paste("PC1 scores versus", var_col),
      subtitle = "Flagged fish are highlighted for return to single-trial inspection",
      x = "PC1 score",
      y = var_col,
      colour = "Flagged"
    ) +
    ggplot2::theme_light()

  list(
    pca = pca_result,
    pca_loadings = pca_result$var$coord,
    pca_scores = pca_scores,
    variance_explained = variance_explained,
    outliers = outliers,
    outlier_details = outlier_details,
    mahalanobis_cutoff = mahalanobis_cutoff,
    dbscan = dbscan_result,
    retained_ids = retained_ids,
    plots = list(
      screeplot = p1,
      biplot = p2,
      pc1contributionplot = p3,
      varplot = p4
    )
  )
}
