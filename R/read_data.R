# -----------------------------------------------------------------------------
# Script with functions to read in the data files.
#
# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.
#
# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.
# -----------------------------------------------------------------------------

# Pupil and teacher numbers data ----------------------------------------------

read_pupil_teacher_numbers <- function(
  file = "data/1_pupil_teacher_numbers_2026-04-23.parquet"
) {
  df <- read_parquet(file) %>%
    # convert column names to snake_case
    clean_names()

  # required columns
  required_cols <- c(
    "academic_year",
    "pupil_numbers",
    "teacher_numbers",
    "projection",
    "phase"
  )

  # check required columns
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in pupil/teacher numbers file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Warn if pupil or teacher numbers contain missing values
  if (any(is.na(df$pupil_numbers)) || any(is.na(df$teacher_numbers))) {
    warning(
      "Missing values detected in pupil_numbers or teacher_numbers.",
      call. = FALSE
    )
  }

  # Warn if pupil or teacher numbers contain negative values
  if (
    any(df$pupil_numbers < 0, na.rm = TRUE) ||
      any(df$teacher_numbers < 0, na.rm = TRUE)
  ) {
    warning(
      "Negative values detected in pupil_numbers or teacher_numbers.",
      call. = FALSE
    )
  }

  # Check academic_year format (yyyy/yy)
  invalid_academic_year <- !grepl(
    "^\\d{4}/\\d{2}$",
    df$academic_year
  ) & !is.na(df$academic_year)

  if (any(invalid_academic_year)) {
    warning(
      paste0(
        "❌ Invalid academic_year format detected. ",
        "Expected format: yyyy/yy (e.g. 2024/25). ",
        "Invalid values: ",
        paste(
          unique(df$academic_year[invalid_academic_year]),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # transformations
  df <- df %>%
    mutate(
      # create start year column
      start_year = as.integer(substr(academic_year, 1, 4)),
      # use round_five_up() instead of round() to avoid banker's rounding
      # (round-to-even behaviour for x.5 values).
      teacher_numbers = dfeR::round_five_up(teacher_numbers, dp = 0),
      pupil_numbers = dfeR::round_five_up(pupil_numbers, dp = 0)
    )

  df
}


# PGITT need time series data --------------------------------------------------

read_pgitt_need_timeseries <- function(
  file = "data/2_pgitt_need_timeseries_2026-04-23.parquet"
) {
  df <- read_parquet(file) %>%
    # convert column names to snake_case
    clean_names()

  # required columns
  required_cols <- c(
    "time_period",
    "subject",
    "education_phase",
    "pgitt_trainee_need_count",
    "difference_to_previous_year_count",
    "difference_to_previous_year_percent"
  )

  # check required columns
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in pgitt need time series file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Warn if pgitt_trainee_need_count contains missing values
  if (any(is.na(df$pgitt_trainee_need_count))) {
    warning(
      "Missing values detected in pgitt_trainee_need_count.",
      call. = FALSE
    )
  }

  # Warn if pgitt_trainee_need_count contains negative values
  if (
    any(df$pgitt_trainee_need_count < 0, na.rm = TRUE)
  ) {
    warning(
      "Negative values detected in pgitt_trainee_need_count.",
      call. = FALSE
    )
  }

  df <- df %>%
    rename(phase = education_phase) %>% # rename column from pub names
    mutate(
      # create start year column
      start_year = as.integer(substr(time_period, 1, 4)),
      # create academic year column
      academic_year = paste0(
        start_year,
        "/",
        sprintf("%02d", (start_year + 1) %% 100)
      ),
      # format all numeric columns to appropriate number of dps
      # use round_five_up() instead of round() to avoid banker's rounding
      # (round-to-even behaviour for x.5 values).
      pgitt_trainee_need_count = dfeR::round_five_up(
        pgitt_trainee_need_count,
        dp = 0
      ),
      difference_to_previous_year_count = dfeR::round_five_up(
        difference_to_previous_year_count,
        dp = 0
      ),
      difference_to_previous_year_percent = dfeR::round_five_up(
        difference_to_previous_year_percent,
        dp = 1
      )
    )

  df
}


# Drivers analysis data -----------------------------------------------------------

read_drivers_data <- function(
  file = "data/3_drivers_analysis_2026-04-23.parquet"
) {
  df <- read_parquet(file) %>%
    # convert column names to snake_case
    clean_names()

  # required columns
  required_cols <- c(
    "driver",
    "value",
    "phase",
    "subject"
  )

  # check required columns
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in drivers file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Warn if value column contains missing values
  if (any(is.na(df$value))) {
    warning(
      "Missing values detected in value column.",
      call. = FALSE
    )
  }

  # round values to 1 dp
  # use round_five_up() instead of round() to avoid banker's rounding
  # (round-to-even behaviour for x.5 values).
  df <- df %>%
    mutate(value = dfeR::round_five_up(value, dp = 1))

  df
}


# Flow trajectories data ------------------------------------------------------------------------------------------

# shared helper for reading flow trajectory publication datasets
# used by both the 2025 and 2026 publication readers because the file
# structure and processing steps are identical

read_flows_publication_data <- function(file, publication_year) {
  df <- read_parquet(file) %>%
    # convert column names to snake_case
    clean_names()

  # required columns

  required_cols <- c(
    "phase",
    "subject",
    "type",
    "academic_year",
    "value",
    "unit",
    "historic_or_trajectory",
    "publication_year"
  )

  # check required columns

  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in flows ",
        publication_year,
        " file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Warn if value column contains negative values
  if (
    any(df$value < 0, na.rm = TRUE)
  ) {
    warning(
      "Negative values detected in value column.",
      call. = FALSE
    )
  }

  # Check academic_year format (yyyy/yy)
  invalid_academic_year <- !grepl(
    "^\\d{4}/\\d{2}$",
    df$academic_year
  ) & !is.na(df$academic_year)

  if (any(invalid_academic_year)) {
    warning(
      paste0(
        "❌ Invalid academic_year format detected. ",
        "Expected format: yyyy/yy (e.g. 2024/25). ",
        "Invalid values: ",
        paste(
          unique(df$academic_year[invalid_academic_year]),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  df <- df %>%
    mutate(
      # create start year column
      start_year = as.integer(substr(academic_year, 1, 4))
    ) %>%
    # NQE trajectories are only for two years ahead
    # remove 3rd year row which has NA data
    # to prevent the table/downloads having an NA row
    filter(!is.na(value)) %>%
    # round values using round_five_up() because base R's round() uses
    # banker's rounding (round-to-even) for values ending in .5.
    # leaver rates are stored as proportions (e.g. 0.056 = 5.6%), so keep 3 dp.
    # entrant values are counts, so round to 0 dp.
    mutate(
      value = case_when(
        grepl("leaver", type, ignore.case = TRUE) ~
          dfeR::round_five_up(value, dp = 3),
        TRUE ~
          dfeR::round_five_up(value, dp = 0)
      )
    )

  df
}

# Read 2025 publication flow trajectory dataset

read_flows_2025_publication_data <- function(
  file = "data/4_flow_trajectories_2025_publication_2026-04-23.parquet"
) {
  read_flows_publication_data(
    file = file,
    publication_year = 2025
  )
}

# Read 2026 publication flow trajectory dataset

read_flows_2026_publication_data <- function(
  file = "data/5_flow_trajectories_2026_publication_2026-04-23.parquet"
) {
  read_flows_publication_data(
    file = file,
    publication_year = 2026
  )
}

# School workforce census (SWC) measures data ---------------------------------------------------------------------

# Display names for the three SWC measures shown on the
# School workforce census measures tab
swc_measure_names <- c(
  vacancies = "Teacher vacancy rate",
  entrants = "Qualified teacher entrant rate",
  leavers = "Qualified teacher leaver rate",
  ptr_qualified = "Pupil to qualified teacher ratio",
  ptr_all = "Pupil to teacher ratio (qualified and unqualified teachers)"
)

# Measures which are ratios (pupils per teacher) rather than rates (stored as
# proportions and shown as percentages)
swc_ratio_measures <- swc_measure_names[c("ptr_qualified", "ptr_all")]

# The two pupil to teacher ratio datasets are shown as a single option in the
# measure filter. A second filter then selects whether unqualified teachers are
# included. Names are the labels shown in that filter, values are the measure
# names in the data.
swc_ptr_label <- "Pupil to teacher ratio"

swc_ptr_teacher_types <- c(
  "Qualified teachers only" = swc_measure_names[["ptr_qualified"]],
  "Qualified and unqualified teachers" = swc_measure_names[["ptr_all"]]
)

# Display names for the measures shown on the Quality metrics tab
swc_quality_measure_names <- c(
  in_service = "Percentage of teachers still in service after qualifying",
  qts = "Percentage of teachers with qualified teacher status (QTS)",
  occasional = "Headcount of occasional teachers"
)

# Display name for the measure shown on the Subject level metrics tab
swc_subject_measure_names <- c(
  specialism = "Specialism rate"
)

# Measures which are counts (headcounts) rather than rates or ratios
swc_count_measures <- swc_quality_measure_names["occasional"]

# shared helper for reading the SWC measure datasets (vacancies, entrants,
# leavers and pupil to teacher ratios) because the file structure and
# processing steps are identical.
#
# Files are in wide format: one row per academic year (column 'category') and
# one column per breakdown (e.g. entrant type). Rates are stored as
# proportions (e.g. 0.056 = 5.6%). Ratios are stored as pupils per teacher
# (e.g. 17.3), selected with value_type = "ratio". Counts are headcounts
# (e.g. 16021), selected with value_type = "count".
#
# Returns long format with columns:
# measure, breakdown, academic_year, start_year, value

read_swc_measure_data <- function(
  file,
  measure,
  breakdown_order = NULL,
  dp = 3,
  value_type = c("rate", "ratio", "count")
) {
  value_type <- match.arg(value_type)

  df <- read_parquet(file)

  # required columns
  if (!"category" %in% names(df)) {
    stop(
      paste0("❌ Missing required columns in SWC ", measure, " file: category"),
      call. = FALSE
    )
  }

  breakdown_cols <- setdiff(names(df), "category")

  if (length(breakdown_cols) == 0) {
    stop(
      paste0(
        "❌ Missing required columns in SWC ",
        measure,
        " file: at least one breakdown column"
      ),
      call. = FALSE
    )
  }

  # breakdown columns must be numeric rates
  non_numeric <- breakdown_cols[!vapply(df[breakdown_cols], is.numeric, logical(1))]

  if (length(non_numeric) > 0) {
    stop(
      paste0(
        "❌ Non-numeric columns in SWC ",
        measure,
        " file: ",
        paste(non_numeric, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Warn if values are negative, or if rates are above 100%
  all_values <- unlist(df[breakdown_cols], use.names = FALSE)

  if (value_type == "rate") {
    if (any(all_values < 0, na.rm = TRUE) || any(all_values > 1, na.rm = TRUE)) {
      warning(
        paste0(
          "Values outside the range 0 to 1 detected in SWC ",
          measure,
          " file. Rates should be stored as proportions."
        ),
        call. = FALSE
      )
    }
  } else if (value_type == "ratio" && any(all_values <= 0, na.rm = TRUE)) {
    warning(
      paste0(
        "Zero or negative values detected in SWC ",
        measure,
        " file. Ratios should be positive."
      ),
      call. = FALSE
    )
  } else if (value_type == "count" && any(all_values < 0, na.rm = TRUE)) {
    warning(
      paste0("Negative values detected in SWC ", measure, " file."),
      call. = FALSE
    )
  }

  # Check academic_year format (yyyy/yy)
  invalid_academic_year <- !grepl("^\\d{4}/\\d{2}$", df$category) &
    !is.na(df$category)

  if (any(invalid_academic_year)) {
    warning(
      paste0(
        "❌ Invalid academic_year format detected. ",
        "Expected format: yyyy/yy (e.g. 2024/25). ",
        "Invalid values: ",
        paste(unique(df$category[invalid_academic_year]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Order breakdowns as requested, with any others after in file order
  breakdown_levels <- c(
    intersect(breakdown_order, breakdown_cols),
    setdiff(breakdown_cols, breakdown_order)
  )

  df %>%
    rename(academic_year = category) %>%
    tidyr::pivot_longer(
      cols = all_of(breakdown_cols),
      names_to = "breakdown",
      values_to = "value"
    ) %>%
    mutate(
      measure = measure,
      breakdown = factor(breakdown, levels = breakdown_levels),
      start_year = as.integer(substr(academic_year, 1, 4)),
      # round using round_five_up() because base R's round() uses banker's
      # rounding. Rates are proportions, so dp = 3 keeps 0.1 percentage points
      value = dfeR::round_five_up(value, dp = dp)
    ) %>%
    filter(!is.na(value)) %>%
    arrange(start_year, breakdown) %>%
    select(measure, breakdown, academic_year, start_year, value)
}

# Read SWC teacher vacancy rate dataset
# Vacancy rates are small so are kept to 4 dp (0.01 percentage points)

read_swc_vacancies <- function(
  file = "data/6_swc_vacancies_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_measure_names[["vacancies"]],
    breakdown_order = c(
      "State-funded nursery and primary",
      "State-funded secondary"
    ),
    dp = 4
  )
}

# Read SWC qualified teacher entrant rate dataset

read_swc_entrants <- function(
  file = "data/7_swc_entrants_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_measure_names[["entrants"]],
    breakdown_order = c(
      "Total",
      "Newly qualified entrants",
      "Returners to state funded sector",
      "Deferred newly qualified entrants",
      "Entrants new to state funded sector"
    ),
    dp = 3
  )
}

# Read SWC qualified teacher leaver rate dataset

read_swc_leavers <- function(
  file = "data/8_swc_leavers_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_measure_names[["leavers"]],
    breakdown_order = c("Total", "Out of service", "Retired", "Deceased"),
    dp = 3
  )
}

# Read SWC pupil to qualified teacher ratio dataset
# Ratios are pupils per teacher, kept to 1 dp as in the source

read_swc_ptr_qualified <- function(
  file = "data/9_swc_ptr_qualified_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_measure_names[["ptr_qualified"]],
    breakdown_order = c(
      "State-funded nursery and primary",
      "State-funded secondary"
    ),
    dp = 1,
    value_type = "ratio"
  )
}

# Read SWC pupil to teacher ratio dataset (qualified and unqualified teachers)

read_swc_ptr_all <- function(
  file = "data/10_swc_ptr_all_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_measure_names[["ptr_all"]],
    breakdown_order = c(
      "State-funded nursery and primary",
      "State-funded secondary"
    ),
    dp = 1,
    value_type = "ratio"
  )
}

# Read SWC percentage of teachers still in service after qualifying dataset
# Breakdowns are the number of years after qualifying. Later qualifying years
# have no value for longer periods because that time has not yet passed.

read_swc_in_service <- function(
  file = "data/11_swc_in_service_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_quality_measure_names[["in_service"]],
    breakdown_order = c(
      "1 year after qualifying",
      "3 years after qualifying",
      "5 years after qualifying",
      "10 years after qualifying"
    ),
    dp = 3
  )
}

# Read SWC percentage of teachers with qualified teacher status (QTS) dataset

read_swc_qts <- function(
  file = "data/12_swc_qts_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_quality_measure_names[["qts"]],
    breakdown_order = c(
      "State-funded nursery and primary",
      "State-funded secondary"
    ),
    dp = 3
  )
}

# Read SWC headcount of occasional teachers dataset
# Values are counts of teachers, so kept to 0 dp

read_swc_occasional <- function(
  file = "data/13_swc_occasional_teachers_2026-11-02.parquet"
) {
  read_swc_measure_data(
    file = file,
    measure = swc_quality_measure_names[["occasional"]],
    breakdown_order = c(
      "Total",
      "State-funded nursery and primary",
      "State-funded secondary"
    ),
    dp = 0,
    value_type = "count"
  )
}

# Read SWC specialism rate dataset
#
# The specialism rate (proportion of hours taught by a teacher with a relevant
# post A level qualification) is supplied across two files which hold
# different subjects, both including 'All subjects'. They are read separately
# and combined, keeping one row per subject and academic year.
# 'All subjects' is listed first, followed by subjects alphabetically.

read_swc_specialism <- function(
  files = c(
    "data/14_swc_specialism_a_2026-11-02.parquet",
    "data/15_swc_specialism_b_2026-11-02.parquet"
  )
) {
  df <- files %>%
    lapply(
      read_swc_measure_data,
      measure = swc_subject_measure_names[["specialism"]],
      dp = 3
    ) %>%
    bind_rows() %>%
    mutate(breakdown = as.character(breakdown))

  # Subjects which appear in both files should have identical values
  conflicting <- df %>%
    group_by(breakdown, academic_year) %>%
    filter(n_distinct(value) > 1) %>%
    ungroup()

  if (nrow(conflicting) > 0) {
    warning(
      paste0(
        "Conflicting specialism rates found for the same subject and ",
        "academic year across files: ",
        paste(unique(conflicting$breakdown), collapse = ", "),
        ". The first value has been kept."
      ),
      call. = FALSE
    )
  }

  subject_levels <- c(
    "All subjects",
    sort(setdiff(unique(df$breakdown), "All subjects"))
  )

  df %>%
    distinct(breakdown, academic_year, .keep_all = TRUE) %>%
    mutate(breakdown = factor(breakdown, levels = subject_levels)) %>%
    arrange(start_year, breakdown)
}
