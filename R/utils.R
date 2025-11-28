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


#' Create an Auto-Rotating 3D Ordination Plot
#'
#' @param data The dataframe containing ordination results and metadata.
#' @param group_col The column name for grouping/coloring (string).
#' @param sample_col The column name for sample IDs (string).
#' @param color_list A named vector of colors. If NULL, default plotly colors are used.
#' @param radius The zoom distance for the camera (default 2.5). Larger = smaller plot.
#' @param axis_pattern Regex pattern to auto-detect axes. Default looks for PCo, PC, or Axis.
#'
#' @return A plotly htmlwidget
# 3D ordination visualization
plot_3d_ordination_auto <- function(
  data,
  title = "3D Ordination Plot",
  group_col,
  sample_col,
  color_list = NULL,
  radius = 2.5,
  axis_pattern = "^PCo|^PC|^Axis|^MDS",
  manual_eye = NULL
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
  # If manual_eye is provided (for GIF generation), use it.
  # Otherwise, calculate default start position.
  if (!is.null(manual_eye)) {
    camera_setting <- manual_eye
  } else {
    eye_pos <- radius * cos(pi / 4)
    camera_setting <- list(x = eye_pos, y = eye_pos, z = 0.8)
  }

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
    title = list(text = title, y = 0.95),
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
      # Use the determined camera
      camera = list(eye = camera_setting)
    )
  )

  # --- 5. Inject JavaScript ---
  # Only inject the auto-rotation JS if we are NOT manually controlling the camera
  if (is.null(manual_eye)) {
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
  }

  return(fig)
}


#' Save 3D Ordination Animation to GIF or MP4
#'
#' This function generates a frame-by-frame animation of a 3D Plotly object
#' and exports it as a high-quality video file.
#'
#' @param data The dataframe containing ordination results.
#' @param filename String. Output filename (must end in .gif or .mp4).
#' @param frames Integer. Total number of frames in the full 360-degree rotation.
#'        Higher = smoother but longer rendering time.
#' @param fps Integer. Frames Per Second. Controls the speed of the video.
#' @param radius Numeric. Camera zoom level (distance from center).
#' @param width Integer. Output pixel width.
#' @param height Integer. Output pixel height.
#' @param ... Additional arguments passed to `plot_3d_ordination_auto`
#'        (e.g., group_col, sample_col, color_list).
#'
#' @details
#' This function works by creating a "headless" Chrome browser session.
#' It generates the plot N times, rotating the camera angle slightly each time,
#' takes a screenshot, and stitches the images together.
#'
#' @return No return value. Saves a file to disk.
#' @export
save_3d_rotation <- function(
  data,
  title,
  filename = "ordination_rotation.gif",
  frames = 90,
  fps = 20,
  radius = 2.5,
  width = 800,
  height = 600,
  ...
) {
  # --- 1. Environment Setup ---

  # Create a temporary directory in the system's temp folder.
  # We use this to store the intermediate HTML and PNG files so we don't
  # clutter the user's working directory.
  img_dir <- tempdir()

  # Pre-allocate a vector to store the full paths of the generated PNGs.
  # This ensures the stitching function knows exactly which files to use and in what order.
  png_files <- character(frames)

  message(paste0("Initializing rendering engine for ", frames, " frames..."))

  # Initialize a progress bar to keep the user informed (rendering can be slow).
  pb <- txtProgressBar(min = 0, max = frames, style = 3)

  # --- 2. The Rendering Loop ---

  for (i in 1:frames) {
    # --- A. Calculate Camera Geometry ---
    # We need to calculate the X and Y coordinates for the camera on a 2D circle.
    # Logic:
    # 1. (i - 1) / frames: Calculates the percentage of progress (0.0 to 1.0).
    # 2. * 2 * pi: Converts that percentage to Radians (0 to 2π, a full circle).
    # 3. + (pi / 4): Adds a 45-degree offset. This ensures the video starts
    #    at the exact same angle as the interactive plot's default view.
    angle <- (2 * pi * (i - 1) / frames) + (pi / 4)

    # Define the 3D coordinate for the camera eye.
    # We keep Z constant (0.8) so the camera rotates around the "equator"
    # but looks slightly downward.
    eye_pos <- list(
      x = radius * cos(angle),
      y = radius * sin(angle),
      z = 0.8
    )

    # --- B. Generate Static Plot ---
    # We call the main plotting function.
    # CRITICAL: We pass 'manual_eye = eye_pos'.
    # This tells 'plot_3d_ordination_auto' to DISABLE the JavaScript auto-rotation
    # and instead freeze the camera at these specific coordinates.
    p <- plot_3d_ordination_auto(
      data = data,
      title = title,
      manual_eye = eye_pos,
      radius = radius,
      ...
    )

    # --- C. Save to HTML ---
    # Plotly is JavaScript-based, so it must live in an HTML file first.
    html_file <- file.path(img_dir, paste0("frame_", i, ".html"))

    # saveWidget converts the R Plotly object into a standalone HTML file.
    # selfcontained = FALSE makes it faster by not embedding dependencies in every file.
    htmlwidgets::saveWidget(p, html_file, selfcontained = FALSE)

    # --- D. Screenshot via Webshot ---
    # Define the output PNG path.
    png_file <- file.path(img_dir, sprintf("frame_%03d.png", i))

    # webshot2 uses a headless Chrome browser to open the HTML and take a picture.
    webshot2::webshot(
      url = html_file,
      file = png_file,
      vwidth = width,
      vheight = height,

      # 'delay': Wait 0.2s before snapping to ensure the WebGL canvas is fully drawn.
      # If you see blank or partial plots, increase this value.
      delay = 0.2,

      # 'zoom': Capture at 2x resolution (Retina quality) for sharp text/lines.
      zoom = 2
    )

    # Store the filename and update progress
    png_files[i] <- png_file
    setTxtProgressBar(pb, i)

    # Optional: Remove the HTML file immediately to save disk space
    unlink(html_file)
  }

  close(pb)

  # --- 3. Stitching and Export ---

  # Determine the desired output format based on the file extension provided.
  ext <- tolower(tools::file_ext(filename))

  if (ext == "gif") {
    message("Stitching frames into GIF...")

    # gifski creates highly optimized GIFs.
    # delay = 1/fps converts "frames per second" to "seconds per frame".
    gifski::gifski(
      png_files = png_files,
      gif_file = filename,
      delay = 1 / fps,
      width = width,
      height = height
    )
  } else if (ext == "mp4") {
    message("Encoding frames into MP4 video...")

    # av uses FFMPEG to encode video. It is generally higher quality/smoother than GIF.
    av::av_encode_video(
      input = png_files,
      output = filename,
      framerate = fps
    )
  } else {
    stop(
      "Error: Unknown file extension. Please ensure filename ends in '.gif' or '.mp4'"
    )
  }

  # Clean up: You might want to remove the temp PNGs here,
  # though tempdir() is usually cleaned by the OS upon restart.
  # unlink(png_files)

  message(paste("Success! Animation saved to:", normalizePath(filename)))
}
