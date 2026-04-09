# -----------------------------------------------------------------------------
# This is the plotting.R file.
#
# This is where we've stored the functions for creating the plots in the app.
#
# It is up to you whether you put all plots in this script, move the plots to
# the helper_functions.R script or have a multiple scripts or even a folder of
# scripts that contain your custom plotting functions.
# -----------------------------------------------------------------------------

# Pupil vs Teacher timeseries ---------------------------

# axis locks for dual axis primary and secondary graphs

primary_lock <- list(
  p0           = 3600000,
  p_max        = 4800000,
  pup_step     = 200000,
  t0           = 180000,
  t_max        = 240000,
  teach_step   = 10000,
  force_limits = TRUE
)


secondary_lock <- list(
  p0           = 1800000,
  p_max        = 3800000,
  pup_step     = 200000,
  t0           = 180000,
  t_max        = 380000,
  teach_step   = 20000,
  force_limits = TRUE
)

# pupil teacher timeseries plot

plot_pupil_teacher_timeseries <- function(
    df, phase = NULL,
    axis_lock = NULL) {
  #--------------------------
  # Set y axis name, projection years, legend position
  #--------------------------

  pupils_axis_name <- paste(phase, "pupil numbers (FTE)")
  teachers_axis_name <- paste(phase, "teacher numbers (FTE)")

  last_census_year <- 2024

  legend_pos <- if (phase == "Secondary") c(0.4, 0.4) else c(0.4, 0.15)

  #--------------------------
  # Axis-lock settings
  #--------------------------
  use_axis_lock <- !is.null(axis_lock)

  if (use_axis_lock) {
    p0 <- axis_lock$p0
    t0 <- axis_lock$t0
    pup_step <- axis_lock$pup_step
    teach_step <- axis_lock$teach_step
    p_max <- axis_lock$p_max
    t_max <- axis_lock$t_max
    force_lim <- isTRUE(axis_lock$force_limits)

    r <- pup_step / teach_step # pupil scalar per teacher step
  } else {
    r <- max(df$pupil_numbers, na.rm = TRUE) / max(df$teacher_numbers, na.rm = TRUE)
  }

  #--------------------------
  # Prepare data
  #--------------------------
  df2 <- df %>%
    dplyr::mutate(
      academic_year = paste0(start_year, "/", sprintf("%02d", (start_year + 1) %% 100)),
      is_projection = start_year > last_census_year,
      tooltip = dplyr::if_else(
        is_projection,
        paste0(
          "<p>", academic_year, "</p>",
          "<p><b>Projected ", tolower(phase), " pupil numbers (left):</b> ",
          scales::comma(pupil_numbers), "</p>",
          "<p><b>Projected ", tolower(phase), " teacher demand (right):</b> ",
          scales::comma(teacher_numbers), "</p>"
        ),
        paste0(
          "<p>", academic_year, "</p>",
          "<p><b>", phase, " pupil numbers (left):</b> ",
          scales::comma(pupil_numbers), "</p>",
          "<p><b>", phase, " teacher numbers (right):</b> ",
          scales::comma(teacher_numbers), "</p>"
        )
      ),
      hover_id = paste0("year-", start_year)
    )

  #--------------------------
  # Long format for segment plotting
  #--------------------------
  df_long <- df2 %>%
    tidyr::pivot_longer(
      cols = c(pupil_numbers, teacher_numbers),
      names_to = "series_raw",
      values_to = "value_raw"
    ) %>%
    dplyr::mutate(
      series = factor(ifelse(series_raw == "pupil_numbers", "Pupils", "Teachers"),
        levels = c("Pupils", "Teachers")
      ),
      value = dplyr::if_else(
        series == "Teachers",
        if (use_axis_lock) (value_raw - t0) * r + p0 else value_raw * r,
        value_raw
      )
    ) %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(start_year) %>%
    dplyr::mutate(
      next_year = dplyr::lead(start_year),
      next_value = dplyr::lead(value),
      segment_linetype = ifelse(start_year >= last_census_year, "Projected", "Historic"),
      seg_type = paste(series, segment_linetype)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(next_year))

  #--------------------------
  # Axis breaks
  #--------------------------
  year_breaks <- df2$start_year[df2$start_year %% 2 == 0]

  primary_limits <- NULL
  if (use_axis_lock) {
    if (force_lim) {
      primary_breaks <- seq(p0, p_max, by = pup_step)
      secondary_breaks <- seq(t0, t_max, by = teach_step)
      primary_limits <- c(p0, p_max)
    } else {
      # fallback auto behaviour (never used in your use-case)
      transformed_teacher <- (df2$teacher_numbers - t0) * r + p0
      y_min <- min(df2$pupil_numbers, transformed_teacher, na.rm = TRUE)
      y_max <- max(df2$pupil_numbers, transformed_teacher, na.rm = TRUE)
      start <- p0 + floor((y_min - p0) / pup_step) * pup_step
      end <- p0 + ceiling((y_max - p0) / pup_step) * pup_step

      primary_breaks <- seq(start, end, by = pup_step)
      secondary_breaks <- (primary_breaks - p0) / r + t0
    }
  }

  #--------------------------
  # Build plot
  #--------------------------
  p <- ggplot(df2, aes(x = start_year)) +
    ggiraph::geom_vline_interactive(
      aes(xintercept = start_year, tooltip = tooltip, data_id = hover_id),
      linetype = "dashed", linewidth = 1, color = "grey40", alpha = 0
    ) +
    ggiraph::geom_segment_interactive(
      data = df_long,
      aes(
        x = start_year, xend = next_year,
        y = value, yend = next_value,
        colour = series,
        linetype = seg_type,
        tooltip = tooltip, data_id = hover_id
      ),
      linewidth = 1
    ) +
    geom_point(aes(y = pupil_numbers), color = "#F46A25", shape = 8, size = 3) +
    geom_point(
      aes(y = if (use_axis_lock) (teacher_numbers - t0) * r + p0 else teacher_numbers * r),
      color = "#12436D", shape = 21, fill = "#12436D", size = 2
    ) +
    ggiraph::geom_point_interactive(
      aes(y = pupil_numbers, tooltip = tooltip, data_id = hover_id),
      alpha = 0, size = 8
    ) +
    ggiraph::geom_point_interactive(
      aes(
        y = if (use_axis_lock) (teacher_numbers - t0) * r + p0 else teacher_numbers * r,
        tooltip = tooltip, data_id = hover_id
      ),
      alpha = 0, size = 8
    ) +
    scale_x_continuous(
      name = "Academic year",
      breaks = year_breaks,
      labels = paste0(year_breaks, "/", sprintf("%02d", (year_breaks + 1) %% 100))
    ) +

    #--------------------------
    # Conditional Y‑axis using list()
    #--------------------------
    (
      if (use_axis_lock) {
        list(
          scale_y_continuous(
            name = pupils_axis_name,
            breaks = primary_breaks,
            labels = scales::comma,
            limits = primary_limits,
            expand = c(0, 0),
            sec.axis = sec_axis(
              transform = ~ (. - p0) / r + t0,
              name      = teachers_axis_name,
              breaks    = secondary_breaks,
              labels    = scales::comma
            )
          )
        )
      } else {
        list(
          scale_y_continuous(
            name = pupils_axis_name,
            labels = scales::comma,
            sec.axis = sec_axis(~ . / r, name = teachers_axis_name, labels = scales::comma)
          )
        )
      }) +
    coord_cartesian(ylim = primary_limits, clip = "on") +
    scale_colour_manual(
      name = "", values = c("Pupils" = "#F46A25", "Teachers" = "#12436D")
    ) +
    scale_linetype_manual(
      name = "",
      values = c(
        "Pupils Historic"    = "solid",
        "Teachers Historic"  = "solid",
        "Pupils Projected"   = "dotted",
        "Teachers Projected" = "dotted"
      ),
      breaks = c("Pupils Projected", "Teachers Projected"),
      labels = c(
        "Pupils Projected"   = "Projected pupil numbers",
        "Teachers Projected" = "Projected teacher demand"
      )
    ) +
    guides(
      colour = guide_legend(
        order = 1, # series appears first
        nrow = 1,
        title = NULL
      ),
      linetype = guide_legend(
        order = 2, # projections appear second
        nrow = 1,
        title = NULL,
        override.aes = list(
          colour = c("#F46A25", "#12436D"), # orange, blue
          linetype = c("dotted", "dotted"),
          size = 0.8
        )
      )
    ) +
    afcharts::theme_af() +
    theme(
      axis.title.y.left = element_text(color = "#F46A25", angle = 90, vjust = 0.5),
      axis.title.y.right = element_text(color = "#12436D", angle = 270, vjust = 0.5),
      axis.text.y.left = element_text(color = "#F46A25"),
      axis.text.y.right = element_text(color = "#12436D"),
      legend.position = "inside",
      legend.justification = "left",
      legend.box = "vertical", # stack colour row above projection row
      legend.direction = "horizontal",
      legend.box.margin = margin(t = -5, l = 0),
      legend.position.inside = legend_pos, # dynamic legend pos based on phase
      legend.spacing.x = unit(0.4, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA)
    )

  return(p)
}





# pgitt trainee need timeseries

plot_pgitt_need_timeseries <- function(df) {
  df2 <- df %>%
    dplyr::mutate(
      academic_year = paste0(start_year, "/", sprintf("%02d", (start_year + 1) %% 100)),
      # Tooltip with year, phase, subject and PGITT need
      # Only show subject if secondary is selected
      subject_line = ifelse(
        phase == "Secondary",
        paste0("<p><b>Subject:</b> ", subject, "</p>"),
        ""
      ),
      tooltip = paste0(
        "<p>", academic_year, "</p>",
        "<p><b>Phase:</b> ", phase, "</p>",
        subject_line,
        "<p><b>PGITT trainee need:</b> ", scales::comma(pgitt_trainee_need), "</p>"
      )
    ) %>%
    dplyr::arrange(subject, start_year)

  # Axis helpers
  year_breaks <- sort(unique(df2$start_year))
  year_labels <- df2 %>%
    dplyr::distinct(start_year, academic_year) %>%
    dplyr::arrange(start_year) %>%
    dplyr::pull(academic_year)

  p <- ggplot(
    df2,
    aes(
      x = start_year,
      y = pgitt_trainee_need
    )
  ) +
    ggiraph::geom_col_interactive(
      aes(
        tooltip = tooltip,
        data_id = paste(subject, start_year, sep = "_")
      ),
      fill = "#801650",
      width = 0.6
    ) +
    afcharts::theme_af() +
    xlab("Academic year") +
    ylab("PGITT trainee need") +
    theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12), family = "dejavu"),
      axis.title.y = element_text(
        angle = 90, vjust = 0.5,
        margin = margin(r = 12), family = "dejavu"
      )
    ) +
    scale_x_continuous(
      breaks = year_breaks,
      labels = year_labels
    ) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, NA),
    )
  p
}

# Drivers analysis waterfall graph --------------------------------------------------------------------------------


plot_drivers_waterfall <- function(df_raw) {
  # Labels used to identify the first and last bars in the waterfall chart
  start_label <- "2025/26 PGITT need"
  end_label <- "2026/27 PGITT need"

  # Definitions for each driver (shown inside tooltip)
  defs <- c(
    "2025/26 PGITT need" = "Last year's PGITT trainee need.",
    "Demand growth YOY" = "Change in teacher demand growth driven by pupil projections. Orange = lower demand growth; Green = higher demand growth.",
    "Leavers" = "Teachers leaving the sector between years. Orange = fewer leavers; Green = more leavers.",
    "Working hour losses" = "Reduction in working hours for individual teachers between years. Orange = fewer hours lost; Green = more hours lost.",
    "Returners" = "Teachers re-entering service after previously working in the state-funded sector. Orange = more returners expected; Green = fewer returners expected.",
    "NTSF" = "Teachers new to the state-funded sector (including deferrer NQEs). Orange = more NTSF expected; Green = fewer NTSF expected.",
    "NQEs from other sources" = "Newly qualified entrants not from PGITT (e.g., UGITT, AO, devolved nations, overseas recognition). Orange = more expected; Green = fewer expected.",
    "ITT-NQE conversion rate" = "Adjustment accounting for trainees not completing ITT, entering employment post-ITT, and NQEs that are not employed full-time. Orange = more favourable conversion; Green = less favourable conversion.",
    "Under-supply adjustment" = "Adjustment countering estimated undersupply where relevant resulting from previous two ITT cycles.  Orange = smaller adjustment. Green = larger adjustment. No bar = no adjustment needed.",
    "2026/27 PGITT need" = "This year's PGITT trainee need."
  )

  # Data preparation
  df <- df_raw %>%
    dplyr::filter(driver != "Overall difference") %>% # Remove summary row
    dplyr::mutate(
      type = dplyr::case_when(
        driver == start_label ~ "start", # First bar
        driver == end_label ~ "end", # Last bar
        TRUE ~ "delta" # All middle 'change' bars
      ),
      order_id = dplyr::row_number()
    )

  # Extract starting value (used to calculate cumulative changes)
  start_val <- df$value[df$type == "start"][1]

  # Calculate bottom/top of each bar for the waterfall
  df <- df %>%
    dplyr::mutate(
      delta_val = ifelse(type == "delta", value, 0),
      cum_delta_before = dplyr::lag(cumsum(delta_val), default = 0),
      level_before = start_val + cum_delta_before,

      # ymin/ymax define the vertical extent of each bar
      ymin = dplyr::case_when(
        type == "start" ~ 0,
        type == "delta" ~ level_before,
        type == "end" ~ 0
      ),
      ymax = dplyr::case_when(
        type == "start" ~ value,
        type == "delta" ~ level_before + value,
        type == "end" ~ value
      ),

      # Colour by whether the driver increases or decreases need
      fill_col = dplyr::case_when(
        type != "delta" ~ "total",
        value >= 0 ~ "increase",
        TRUE ~ "decrease"
      ),

      # Keep drivers in original order on x-axis
      driver = factor(driver, levels = driver),

      # Tooltip text shown when hovering on each bar
      tooltip = paste0(
        "<b>", as.character(driver), ":</b><br/>", defs[as.character(driver)]
      ),

      # Required by ggiraph for hover behaviour.
      # Converts driver text into a “safe” ID with only letters/numbers/d
      data_id = paste0(
        "bar-",
        tolower(gsub("[^A-Za-z0-9_-]", "-", as.character(driver)))
      )
    )

  # Build the interactive waterfall chart
  ggplot(df, aes(x = driver)) +
    # Rectangle for each bar
    ggiraph::geom_rect_interactive(
      aes(
        xmin = as.numeric(driver) - 0.45,
        xmax = as.numeric(driver) + 0.45,
        ymin = ymin,
        ymax = ymax,
        fill = fill_col,
        tooltip = tooltip,
        data_id = data_id
      ),
      colour = "grey40",
      linewidth = 0.3
    ) +

    # Large invisible hover points improve tooltip reliability
    ggiraph::geom_point_interactive(
      data = df,
      inherit.aes = FALSE,
      aes(
        x = as.numeric(driver),
        y = (ymin + ymax) / 2, # Middle of the bar
        tooltip = tooltip,
        data_id = data_id
      ),
      size = 30,
      alpha = 0
    ) +
    # Numeric label above each bar
    geom_text(
      aes(
        x = as.numeric(driver),
        # Place label on top of each bar
        y = ifelse(type == "delta", pmax(ymin, ymax), ymax),
        label = scales::comma(value) # Value data label
      ),
      vjust = -0.25,
      size = 4
    ) +
    # Colour palette
    scale_fill_manual(
      values = c(increase = "#28A197", decrease = "#F46A25", total = "#12436D"),
      guide = "none"
    ) +
    # Y axis formatting
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    # Wrap long x-axis labels
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
    # Axis titles
    labs(x = NULL, y = "PGITT trainees") +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(
        size = 12,
        margin = margin(t = 6),
        lineheight = 0.95
      )
    )
}

# plot flow trajectories -----------------------------------------------------------

plot_flow_trajectories <- function(df) {
  leaver_types <- c("Total leaver rate", "55+ leaver rate", "Under 55 leaver rate")

  # --- DEFINE per-row last census year based on version ------------------------
  df <- df %>%
    dplyr::mutate(
      last_census_year_row = dplyr::case_when(
        version == "Last year" ~ 2023L,
        version == "This year (dummy data)" ~ 2024L,
        TRUE ~ 2023L # fallback/default; adjust if you have other versions
      )
    )

  # ---------- labels & tooltip ----------

  df <- df %>%
    dplyr::mutate(
      academic_year_label = paste0(year, "/", sprintf("%02d", (year + 1) %% 100)),
      is_trajectory = year > last_census_year_row,
      type_lower = ifelse(type %in% c("Newly qualified entrants", "New to state-funded sector entrants"), type, tolower(type)),
      value_formatted = dplyr::case_when(
        type %in% leaver_types ~ scales::label_percent(accuracy = 0.1)(value),
        TRUE ~ paste0(scales::label_number(accuracy = 1, big.mark = ",")(value), " (FTE)")
      ),
      tooltip = ifelse(
        is_trajectory,
        paste0(
          "<p>", academic_year_label, "</p>",
          "<p><b>Phase:</b> ", phase, "</p>",
          "<p><b>Subject:</b> ", subject, "</p>",
          "<p><b>Publication year:</b> ", publication_year, "</p>",
          "<p><b>", type, " trajectory:</b> ", value_formatted, "</p>"
        ),
        paste0(
          "<p>", academic_year_label, "</p>",
          "<p><b>Phase:</b> ", phase, "</p>",
          "<p><b>Subject:</b> ", subject, "</p>",
          "<p><b>Publication year:</b> ", publication_year, "</p>",
          "<p><b>", type, ":</b> ", value_formatted, "</p>"
        )
      )
    )


  # ---------- y scale ----------
  unique_type <- unique(df$type)
  if (all(df$type %in% leaver_types)) {
    y_scale <- ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 0.1),
      limits = c(0, NA)
    )
    y_title <- paste0(unique_type, " (%)")
  } else {
    y_scale <- ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      limits = c(0, NA)
    )
    y_title <- paste0(unique_type, " (FTE)")
  }

  # ---------- build segment data ----------
  # group by any series identifiers that exist (phase/subject/type) so joins are correct

  df_seg <- df %>%
    dplyr::arrange(year) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("phase", "subject", "type", "version")))) %>%
    dplyr::mutate(
      next_year = dplyr::lead(year),
      next_value = dplyr::lead(value),
      # IMPORTANT: compare the segment's start year against its row-specific cutover
      segment_linetype = ifelse(year >= last_census_year_row, "Trajectory", "Historic"),
      tooltip_seg = tooltip
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(next_year))

  # Robust x axis breaks/labels based only on data

  # Keep only years present in df
  years_available <- df %>%
    dplyr::pull(year) %>%
    unique() %>%
    sort()

  # Apply biennial pattern relative to the actual data
  years_for_axis <- years_available[years_available %% 2 == (min(years_available) %% 2)]

  # Axis labels for those years
  axis_labels <- df %>%
    dplyr::distinct(year, academic_year_label) %>%
    dplyr::filter(year %in% years_for_axis) %>%
    dplyr::arrange(year) %>%
    dplyr::pull(academic_year_label)

  # ---------- plot ----------
  ggplot2::ggplot(df, ggplot2::aes(x = year)) +

    # Lines as segments with linetype mapped to Historic/Trajectory
    ggiraph::geom_segment_interactive(
      data = df_seg,
      ggplot2::aes(
        x = year, xend = next_year,
        y = value, yend = next_value,
        linetype = segment_linetype,
        colour = version,
        tooltip = tooltip_seg
      ),
      linewidth = 1
    ) +

    # Points remain interactive
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = value, colour = version, tooltip = tooltip),
      shape = 16, size = 2.5, na.rm = TRUE
    ) +

    # Theme & labels
    afcharts::theme_af() +
    ggplot2::xlab("Academic year") +
    ggplot2::ylab(y_title) +
    ggplot2::theme(
      text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 12), family = "dejavu"),
      axis.title.y = element_text(angle = 90, vjust = 0.5, margin = margin(r = 12), family = "dejavu"),
      axis.line = element_line(linewidth = 0.75),
      # show a small legend for trajectory (dotted)
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.18),
      legend.justification = "right",
      legend.background = element_rect(fill = "transparent", colour = NA)
    ) +

    # X axis: biennial ticks (unchanged)
    ggplot2::scale_x_continuous(
      breaks = years_for_axis,
      labels = axis_labels
    ) +

    # Linetype scale & legend entry for trajectories
    scale_linetype_manual(
      name = "",
      values = c("Historic" = "solid", "Trajectory" = "dotted"),
      breaks = "Trajectory",
      guide = "none"
    ) +
    scale_colour_manual(
      name = "",
      values = c(
        "This year (dummy data)" = "#801650",
        "Last year" = "#28A197"
      ),
      labels = c(
        "This year (dummy data)" = "2026 publication DUMMY data (dotted line = trajectory)",
        "Last year" = "2025 publication DUMMY data (dotted line = trajectory)"
      )
    ) +
    guides(
      colour = guide_legend(order = 1)
    ) +

    # y scale as computed
    y_scale
}
