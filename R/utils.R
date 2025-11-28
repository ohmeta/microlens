condition2_color <-
  c(
    "Infant stool" = "#d55e00",
    "Infant nasal" = "#56b4e9"
  )

condition2_color_t <-
  c(
    "Birth" = "#d55e00",
    "Two months" = "#56b4e9"
  )

condition2_color_t_v2 <-
  c(
    "One day" = "#d55e00",
    "One month" = "#56b4e9"
  )

birth_mode_color <- c(
  "sc" = "#d55e00",
  "vag" = "#56b4e9"
)

condition4_color <-
  c(
    "Infant stool" = "#d55e00",
    "Infant nasal" = "#56b4e9",
    "Negative control" = "#bababa",
    "water" = "#404040"
  )

timepoints_4_color <- c(
  "One day" = "#a6dba0",
  "One week" = "#008837",
  "Two weeks" = "#c2a5cf",
  "One month" = "#7b3294"
)

condition_4_color <-
  c(
    "Gut D0" = "#a6dba0",
    "Gut M2" = "#008837",
    "Nasal D0" = "#c2a5cf",
    "Nasal M2" = "#7b3294"
  )

condition_4_color_v2 <-
  c(
    "Gut D1" = "#a6dba0",
    "Gut M1" = "#008837",
    "Nasal D1" = "#c2a5cf",
    "Nasal M1" = "#7b3294"
  )

condition_8_color <-
  c(
    "Nasal D1" = "#e7d4e8",
    "Nasal W1" = "#c2a5cf",
    "Nasal W2" = "#9970ab",
    "Nasal M1" = "#762a83",

    "Gut D1" = "#d9f0d3",
    "Gut W1" = "#a6dba0",
    "Gut W2" = "#5aae61",
    "Gut M1" = "#1b7837"
  )

labtheme <-
  theme(
    axis.text.x = element_text(
      size = 15
    ),
    axis.text.y = element_text(
      size = 15
    )
  ) +
  theme(
    strip.text.x = element_text(
      size = 15
    ),
    strip.text.y = element_text(
      size = 15
    ),
    legend.title = element_text(
      size = 15,
      color = "black"
    ),
    legend.text = element_text(
      size = 15,
      color = "black"
    )
  )


gen_comparisons <- function(groups) {
  if (is.factor(groups)) {
    comparisons <-
      utils::combn(levels(droplevels(groups)), 2) %>%
      apply(2, list) %>%
      unlist(recursive = FALSE)
  } else {
    comparisons <-
      utils::combn(unique(groups), 2) %>%
      apply(2, list) %>%
      unlist(recursive = FALSE)
  }

  return(comparisons)
}


# --------------------------------------------------------------------
# Remove rows whose *sum* is exactly zero
# --------------------------------------------------------------------
remove_zero_sum_rows <- function(x, na.rm = TRUE) {
  if (is.matrix(x) | is.data.frame(x)) {
    keep <- rowSums(x, na.rm = na.rm) != 0
    x[keep, , drop = FALSE]
  } else {
    stop("Unsupported type: only matrices or data.frames are allowed")
  }
}

# --------------------------------------------------------------------
# Remove columns whose *sum* is exactly zero
# --------------------------------------------------------------------
remove_zero_sum_cols <- function(x, na.rm = TRUE) {
  if (is.matrix(x) | is.data.frame(x)) {
    keep <- colSums(x, na.rm = na.rm) != 0
    x[, keep, drop = FALSE]
  } else {
    stop("Unsupported type: only matrices or data.frames are allowed")
  }
}


to_pa <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ as.integer(. > 0)))
}


unique_count <- function(df, group) {
  df %>%
    dplyr::group_by(!!rlang::sym(group)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
}


unique_count_multi <- function(df, groups) {
  df %>%
    dplyr::group_by(!!!rlang::syms(groups)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
}

# Define a clean theme variable to keep code tidy
ohmeta_theme <- ggpubr::theme_pubr() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12), # Standard sans-serif
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
    strip.background = ggplot2::element_rect(fill = "transparent", color = NA), # Clean facet headers
    strip.text = ggplot2::element_text(face = "bold", size = 11),
    axis.title = ggplot2::element_text(face = "bold"),
    legend.position = "right",
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ) # Crisp border
  )


# 3D ordination visualization
plot_3d_ordination_auto <- function(
  data,
  group_col,
  sample_col,
  color_list = NULL,
  radius = 2.5,
  axis_pattern = "^PCo|^PC|^Axis|^MDS"
) {
  # --- 1. Auto-detect & SORT Axis Columns ---

  # Step A: Find all columns matching the pattern
  raw_matches <- grep(axis_pattern, colnames(data), value = TRUE)

  if (length(raw_matches) < 3) {
    stop(paste(
      "Error: Found less than 3 columns matching pattern:",
      axis_pattern
    ))
  }

  # Step B: Extract the first number found in these column names
  # This handles "PCo1 (50%)", "Axis.1", "PC1", etc.
  # \\d+ matches the first sequence of digits
  axis_nums <- as.numeric(regmatches(raw_matches, regexpr("\\d+", raw_matches)))

  # Step C: Check if extraction worked (in case names have no numbers)
  if (any(is.na(axis_nums))) {
    warning(
      "Could not extract numbers from axis names. Falling back to column order."
    )
    sorted_cols <- raw_matches
  } else {
    # Step D: Sort the column names based on the extracted numbers (1, 2, 3...)
    sorted_cols <- raw_matches[order(axis_nums)]
  }

  # Step E: Assign X, Y, Z based on the SORTED order
  x_c <- sorted_cols[1]
  y_c <- sorted_cols[2]
  z_c <- sorted_cols[3]

  message(sprintf(
    "Auto-detected and Sorted Axes:\n  X: %s\n  Y: %s\n  Z: %s",
    x_c,
    y_c,
    z_c
  ))

  # --- 2. Calculate Initial Camera Position ---
  eye_pos <- radius * cos(pi / 4)

  # --- 3. Build the Plot ---
  fig <- plotly::plot_ly(
    data = data,
    x = ~ .data[[x_c]],
    y = ~ .data[[y_c]],
    z = ~ .data[[z_c]],
    color = ~ .data[[group_col]],
    colors = color_list,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 5,
      width = 2,
      opacity = 0.9,
      line = list(color = 'white', width = 1)
    ),
    projection = list(
      x = list(show = TRUE, opacity = 0.2, scale = 1),
      y = list(show = TRUE, opacity = 0.2, scale = 1),
      z = list(show = TRUE, opacity = 0.2, scale = 1)
    ),
    text = ~ paste0(
      "<b>Sample:</b> ",
      .data[[sample_col]],
      "<br><b>Group:</b> ",
      .data[[group_col]],
      "<br><b>",
      x_c,
      ":</b> ",
      round(.data[[x_c]], 3),
      "<br><b>",
      y_c,
      ":</b> ",
      round(.data[[y_c]], 3),
      "<br><b>",
      z_c,
      ":</b> ",
      round(.data[[z_c]], 3)
    ),
    hoverinfo = "text"
  )

  # --- 4. Setup Layout ---
  fig <- plotly::layout(
    p = fig,
    title = list(text = "3D Ordination Plot", y = 0.95),
    legend = list(
      title = list(
        text = paste0('<b>', group_col, '</b>'),
        font = list(size = 14)
      ),
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = 1.0,
      itemsizing = "constant",
      font = list(size = 14)
    ),
    margin = list(l = 50, r = 50, b = 50, t = 80),
    scene = list(
      xaxis = list(
        title = x_c,
        gridcolor = "lightgrey",
        backgroundcolor = "white"
      ),
      yaxis = list(
        title = y_c,
        gridcolor = "lightgrey",
        backgroundcolor = "white"
      ),
      zaxis = list(
        title = z_c,
        gridcolor = "lightgrey",
        backgroundcolor = "white"
      ),
      camera = list(eye = list(x = eye_pos, y = eye_pos, z = 0.8))
    )
  )

  # --- 5. Inject JavaScript ---
  js_code <- sprintf(
    "
    function(el, x) {
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var cnt = Math.PI / 4; 
      function run() {
        cnt += 0.005; 
        var radius = %f; 
        var x_eye = radius * Math.cos(cnt);
        var y_eye = radius * Math.sin(cnt);
        Plotly.relayout(gd, 'scene.camera.eye', {x: x_eye, y: y_eye, z: 0.8});
        requestAnimationFrame(run);
      }
      run();
    }
  ",
    radius
  )

  fig <- htmlwidgets::onRender(fig, js_code)

  return(fig)
}
