



# Functions for summarising the dataset / cohort ----

## Function to create a tibble that summarises a single vector ----
## this is later used in the table1_summary function

summarise_vector <- function(x, x_class = c("continuous", "cateogrical")) {
  # get Ns for total observations, and total missing and non-missing
  counts_overall <-
    tibble(
      N_obs = length(x),
      N_nonmiss = sum(!is.na(x)),
      N_miss = sum(is.na(x))
    )

  if (x_class == "categorical") {
    # get all possible values for categorical variable
    if (is.factor(x)) {
      levels <- levels(x)
    } else if (is.logical(x)) {
      levels <- c("FALSE", "TRUE")
    } else {
      levels <- sort(unique(as.character(x[!is.na(x)])))
    }

    # add NA values if any are present
    if (any(is.na(x))) {
      levels <- c(levels, NA_character_)
    }

    # count frequency of each value
    counts <-
      tibble(
        variable_level = as.character(x)
      ) |>
      count(variable_level, name = "n")

    # combine all
    summary <-
      # one row per possible value (including those not represented)
      tibble(variable_level = levels) |>
      # add counts
      left_join(counts, by = "variable_level") |>
      # add overall counts
      bind_cols(counts_overall) |>
      mutate(
        n = coalesce(n, 0L),
        prop_nonmiss = if_else(!is.na(variable_level), n / N_nonmiss, NA_real_),
        prop_obs = n / N_obs
      )
  }

  if (x_class == "continuous") {
    x <- x[!is.na(x)]

    summary <-
      tibble(
        variable_level = NA_character_,
        mean = mean(x),
        sd = sd(x),
        median = median(x),
        # use type=1 as it plays nicely with dates
        p10 = unname(quantile(x, .10, type = 1)),
        p25 = unname(quantile(x, .25, type = 1)),
        p75 = unname(quantile(x, .75, type = 1)),
        p90 = unname(quantile(x, .90, type = 1))
      ) |>
      # add overall counts
      bind_cols(counts_overall)
  }

  if (x_class == "unsupported") {
    summary <-
      counts_overall |>
      mutate(
        variable_level = NA_character_
      )
  }

  summary |>
    # reorder columns
    select(
      variable_level,
      N_obs,
      N_nonmiss,
      N_miss,
      everything()
    )
}


## Create table1-style summary of characteristics, with SDC applied ----

table1_summary <- function(
  data,
  .by,
  variables,
  rounding.function
) {
  # find out what class each variable is
  variable_class_lookup <-
    data |>
    select(any_of(variables)) |>
    (\(df) {
      tibble(
        variable = names(df),
        logical = map_lgl(df, is.logical),
        factor = map_lgl(df, is.factor),
        character = map_lgl(df, is.character),
        integer = map_lgl(df, is.integer),
        numeric = map_lgl(df, is.numeric),
        Date = map_lgl(df, is.Date),
      ) |>
        mutate(
          categorical = logical | factor | character,
          continuous = integer | numeric,
          cat_or_cont = case_when(
            categorical ~ "categorical",
            continuous ~ "continuous",
            .default = "unsupported"
          ),
          variable_class = case_when(
            logical ~ "logical",
            factor ~ "factor",
            character ~ "character",
            integer ~ "integer",
            numeric ~ "numeric",
            Date ~ "Date",
            .default = ""
          )
        )
    })()

  # should we use cateogrical or continuous summaries
  cat_or_cont <- variable_class_lookup |> pull(cat_or_cont)

  # loop through all selected variables in the dataset and summarise each one
  summary_unredacted <-
    data |>
    dplyr::group_by({{ .by }}) |>
    dplyr::group_map(
      \(group_data, group_keys) {
        # apply summarise_vector function to each column, for each selected group separately
        map2(
          group_data[variables],
          cat_or_cont,
          ~ summarise_vector(x = .x, x_class = .y)
        ) |>
          # combine the list into a single tibble
          list_rbind(names_to = "variable") |>
          mutate(group_keys, .before = 1)
      },
      .keep = TRUE
    ) |>
    # combine across groups
    list_rbind() |>
    # add variable class
    left_join(
      variable_class_lookup |> select(variable, variable_class),
      by = "variable"
    )

  # apply disclosure control if a rounding function is supplied
  if (!missing(rounding.function)) {
    summary_unredacted |>
      mutate(
        N_obs = rounding.function(N_obs),
        N_nonmiss = rounding.function(N_nonmiss),
        N_miss = rounding.function(N_miss),
        n = rounding.function(n),
        prop_nonmiss = n / N_nonmiss,
        prop_obs = n / N_obs,
        across(
          any_of(c("mean", "sd", "p10", "p25", "median", "p75", "p90")),
          ~if_else(N_nonmiss == 0, NA_real_, .)
        )
      )
  } else {
    summary_unredacted
  }
}






# Distribution of most recent prior dose ----
# This produces data and a plot showing the distribution of most recent prior vaccination date (to the nearest 4 weeks)
# if no documented prior vaccination, then values are stacked on the LHS of the chart

plot_date_of_last_dose <- function(data, subgroup) {

  over2years_dummy_date <- (snapshot_date - ceiling_any(365.25 * 2, 7))

  summary_by <-
    data |>
    mutate(
      subgroup = data[[subgroup]]
    ) |>
    lazy_dt() |>
    group_by(subgroup, last_vax_product, last_vax_period) |>
    summarise(
      n = round_sdc(n())
    ) |>
    ungroup() |>
    mutate(
      # if last vaccination date was over 2 years ago, replace with dummy date
      last_vax_period = case_when(
        (last_vax_period < over2years_dummy_date)  ~ over2years_dummy_date - 42,
        is.na(last_vax_period) ~ over2years_dummy_date - 42,
        .default = last_vax_period
      )
    ) |>
    as_tibble() |>
    complete(
      subgroup, last_vax_product, last_vax_period,
      fill = list(n = 0)
    )

  breaks <- seq(as.Date("2021-01-01"),  as.Date("2028-06-01"), by = "6 month")

  temp_plot <-
    summary_by |>
    ggplot() +
    geom_col(
      aes(x = last_vax_period, y = n, fill = last_vax_product, group = last_vax_product),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = temporal_resolution_history
    ) +
    facet_grid(
      rows = vars(subgroup),
      switch = "y",
      space = "free_x",
      scales = "free_x"
    ) +
    labs(
      x = "Date of most recent COVID-19 vaccine",
      y = NULL,
      fill = NULL
    ) +
    scale_fill_brewer(
      palette = "Set2",
      na.value = "grey50",
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "Other"
        breaks
      }
    ) +
    scale_x_date(
      breaks = c(over2years_dummy_date - 42, breaks),
      date_minor_breaks = "month",
      # labels = ~{c("Unvaccinated", scales::label_date("%Y")(.x[-1]))},
      labels = c("+2 years prior or unvaccinated)", scales::label_date("%Y-%b")(breaks)),
    ) +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      axis.ticks.x = element_line(),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  print_interactive(temp_plot)

  ggsave(fs::path(output_dir, glue("last_vax_date_{subgroup}.png")), plot = temp_plot)

  # write tables that capture underlying plotting data
  # write_csv(summary_by, fs::path(output_dir, glue("last_vax_date_{subgroup}.csv")))
}

# plot_date_of_last_dose(data_combined, "sex")



# Distribution of number of prior doses ----
# This produces data and a plot showing the number of prior doses

plot_vax_count <- function(data, subgroup) {

  summary_by <-
    data |>
    mutate(
      subgroup = data[[subgroup]],
      vax_count_group = cut(vax_count, c(-Inf, 0, 2, 4, Inf), labels = c("0", "1-2", "3-4", "5+")),
    ) |>
    lazy_dt() |>
    group_by(subgroup, vax_count_group) |>
    summarise(
      n = round_sdc(n()),
    ) |>
    ungroup() |>
    as_tibble() |>
    complete(
      subgroup, vax_count_group,
      fill = list(n = 0)
    ) |>
    group_by(subgroup) |>
    mutate(
      row_total = sum(n),
      prop = ifelse(row_total > 0, n / row_total * 100, 0),
    ) |>
    ungroup()

  temp_plot <-
    ggplot(summary_by) +
    geom_bar(
      aes(x = prop, y =  subgroup, width = row_total, fill = vax_count_group),
      stat = "identity", # position = "fill",
      position = position_stack(reverse = TRUE),
    ) +
    facet_grid(
      rows = vars(subgroup),
      scales = "free_y",
      space = "free_y") +
    labs(
      x = "%",
      y = NULL,
      fill = "Vaccine count"
    ) +
    scale_fill_brewer(
      palette = "Set2",
      na.value = "grey50",
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "(Missing)"
        breaks
      }
    ) +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      axis.ticks.x = element_line(),
      strip.text = element_blank(),
      legend.position = "bottom"
    ) +
    NULL

  print_interactive(temp_plot)

  ggsave(fs::path(output_dir, glue("vax_count_{subgroup}.png")), plot = temp_plot)

  # write tables that capture underlying plotting data
  write_csv(summary_by, fs::path(output_dir, glue("vax_count_{subgroup}.csv")))
}

# plot_vax_count(data_combined, "sex")



# Prior vaccination in a table ----
# function to print table for an arbitrary number of grouping variables

table_prior_vax_summary <- function(data, ...) {

  # groups are passed as strings via dots (...)
  group_names <- c(...)

  summary_table <-
    data |>
    group_by(across(all_of(group_names))) |>
    lazy_dt() |>
    summarise(
      # Dose counts
      total = round_sdc(n()),
      count_n0 = round_sdc(sum(vax_count == 0, na.rm = TRUE)),
      count_n1 = round_sdc(sum(vax_count == 1, na.rm = TRUE)),
      count_n2 = round_sdc(sum(vax_count == 2, na.rm = TRUE)),
      count_n3 = round_sdc(sum(vax_count == 3, na.rm = TRUE)),
      count_n4 = round_sdc(sum(vax_count == 4, na.rm = TRUE)),
      count_n5plus = round_sdc(sum(vax_count >= 5, na.rm = TRUE)),
      # Dose summary
      count_median = quantile(vax_count, probs = 0.5, na.rm = TRUE),
      count_p10 = quantile(vax_count, probs = 0.10, na.rm = TRUE),
      count_p25 = quantile(vax_count, probs = 0.25, na.rm = TRUE),
      count_p75 = quantile(vax_count, probs = 0.75, na.rm = TRUE),
      count_p90 = quantile(vax_count, probs = 0.90, na.rm = TRUE),
      # Vaccination in past 12 and 24 months
      days_since_n12m = round_sdc(sum(last_vax_time_since <= 365, na.rm = TRUE)),
      days_since_n24m = round_sdc(sum(last_vax_time_since <= 365 * 2, na.rm = TRUE)),
      # Time since last dose
      days_since_median = quantile(last_vax_time_since, probs = 0.5, na.rm = TRUE),
      days_since_p10 = quantile(last_vax_time_since, probs = 0.10, na.rm = TRUE),
      days_since_p25 = quantile(last_vax_time_since, probs = 0.25, na.rm = TRUE),
      days_since_p75 = quantile(last_vax_time_since, probs = 0.75, na.rm = TRUE),
      days_since_p90 = quantile(last_vax_time_since, probs = 0.90, na.rm = TRUE),

      .groups = "drop"
    ) |>
    mutate(
      n_under_sdc_threshold = total <= sdc_threshold,
      # Dose percentages - put this here and not in earlier summarise step so that it works with dtplyr
      count_pct0 = round(count_n0 * 100 / total, 1),
      count_pct1 = round(count_n1 * 100 / total, 1),
      count_pct2 = round(count_n2 * 100 / total, 1),
      count_pct3 = round(count_n3 * 100 / total, 1),
      count_pct4 = round(count_n4 * 100 / total, 1),
      count_pct5plus = round(count_n5plus * 100 / total, 1),
      # Vaccination % in past 12 and 24 months
      days_since_pct12m = round(days_since_n12m * 100 / total, 1),
      days_since_pct24m = round(days_since_n24m * 100 / total, 1),
    ) |>
    mutate(
      across(
        starts_with(c("count", "days_since")),
        ~ if_else(!n_under_sdc_threshold, .x, NA)
      )
    ) |>
    select(-n_under_sdc_threshold) |>
    as_tibble()

  # subgroup_name <- map_chr(rlang::quos(...), rlang::as_name) |> paste0(collapse = "_")

  # Write table to a CSV file
  # write_csv(summary_table, fs::path(output_dir, glue("prior_vax_table_{subgroup_name}.csv")))

  return(summary_table)
}


# for testing function interactively
# table_prior_vax_summary(data = data_combined, "all", "all")
# table_prior_vax_summary(data = data_combined, "all", "ageband4")
# table_prior_vax_summary(data = data_combined, "ageband4", "all")
# table_prior_vax_summary(data = data_combined, "ageband4", "sex")



# Calculate KM estimates for a given stratification variable ----

# group variables are provided as characters via dots (...)
# resolution argument is the precision used for the time dimension. If zero, then original resolution is used.
km_estimates <- function(data, group_name1, group_name2, event_name, event_time, event_indicator, resolution = 1L) {

  group_names <- c(group_name1, group_name2)

  # if (is.na(group_name2)) {
  #   group_name2 <- "all"
  # }

  data_outcome <-
    data |>
    select(
      patient_id,
      all_of(group_names),
      event_time = any_of(event_time),
      event_indicator = any_of(event_indicator)
    ) |>
    mutate(event_time = ceiling_any(event_time, resolution))

  data_km <-
    data_outcome |>
    group_by(across(all_of(group_names))) |>
    nest() |>
    mutate(
      surv_obj_tidy = map(
        data,
        ~ {
          survfit(
            Surv(event_time, event_indicator) ~ 1,
            data = .x,
            conf.type = "log-log"
          ) |>
            broom::tidy() |>
            add_row(
              time = 0L, # assumes time origin is zero
              n.risk = 0L,
              n.event = 0L,
              n.censor = 0L,
              estimate = 1,
              conf.low = 1,
              conf.high = 1,
              .before = 1L
            ) |>
            complete(
              time = seq(0L, selected_campaign_info$final_milestone_days, resolution), # fill in 1 row for each period (defined by resolution) of follow up
              fill = list(n.event = 0L, n.censor = 0L) # fill in zero events on those days
            ) |>
            fill(
              n.risk,
              .direction = "up"
            ) |>
            fill(
              estimate,
              conf.low,
              conf.high,
              .direction = "down"
            )
        }
      ),
    ) |>
    select(-data) |>
    unnest(surv_obj_tidy) |>
    mutate(
      # disclosure control
      # here we round KM estimates to multiples of r / n_r
      # where r is the rounding precision, and n_r is the number at risk a t=0, rounded to a precision of r
      # so if the rounding precision is 10, we get a step for every 10 _estimated_ events,
      # where the estimated number of events is the the estimated incidence time the number at risk at t=0
      # ie, estimated total events including those unobserved due to censoring      #
      n.risk = round_sdc(n.risk),
      estimate = round_any(estimate, sdc_precision / nth(n.risk, 2)), # use 2nd value as this skips the t=0 row where n.risk=0
      conf.low = round_any(conf.low, sdc_precision / nth(n.risk, 2)),
      conf.high = round_any(conf.high, sdc_precision / nth(n.risk, 2)),

      # cumulative incidence
      cmlinc = 1 - estimate,
      cmlinc.low = 1 - conf.high,
      cmlinc.high = 1 - conf.low,
    )

  rm(data_outcome)

  print_interactive(data_km)

  # write tables that capture underlying plotting data
  data_km_nozero <-
    data_km |>
    arrange(across(all_of(group_names))) |>
    ungroup() |>
    filter(time != 0) |>
    select(
      all_of(group_names),
      time,
      cmlinc,
      cmlinc.low,
      cmlinc.high,
    )

  # commented out because outputting the entire data creates a dataset that is too large to output-check on it's own
  # write_csv(data_km_nozero, fs::path(output_dir, glue("km_{event_name}_{paste0(group_names, collapse='_')}.csv")))

  return(data_km_nozero)

}

# for testing function interactively
# km_estimates <- partial(
#   km_estimates, data = data_combined, event_name = "vax", event_time = "vax_time", event_indicator = "vax_indicator", resolution = 7L
# )
# km_estimates(data_combined, "all", "all", "vax", "vax_time", "vax_indicator",)
# km_estimates(data_combined, "all", "ageband4", "vax", "vax_time", "vax_indicator", resolution = 7L)
# km_estimates(data_combined, "ageband4", "all", "vax", "vax_time", "vax_indicator",)
# km_estimates(data_combined, "ageband4", "sex", "vax", "vax_time", "vax_indicator",)



# KM plots ----

# function to make KM plots for the data
# it takes the output of the km_estimate function
km_plot <- function(km_data, event_name, group1, group2) {

  group_names <- c(group1, group2)

  # plot km curves locally for checking (but probs not for release as these can be reconstructed from released data)
  coverage_plot <-
    bind_rows(
      # this bit adds an extra row of data so that the firsrt line of the KM plot shows
      km_data |> summarise(
        time = 0L,
        cmlinc = 0, cmlinc.low = 0, cmlinc.high = 0, lagtime = 0L,
        .by = c("group1", "group1_value", "group2", "group2_value")
      ),
      km_data |> mutate(
        lagtime = lag(time, 1L, 0L), # assumes the time-origin is zero

        .by = c("group1", "group1_value", "group2", "group2_value")
      )
    ) |>
    ggplot() +
    geom_step(aes(x = time, y = cmlinc, group = group2_value, colour = group2_value), direction = "vh") +
    geom_rect(aes(xmin = lagtime, xmax = time, ymin = cmlinc.low, ymax = cmlinc.high, group = group2_value, fill = group2_value), alpha = 0.1, colour = "transparent") +
    facet_grid(rows = "group1_value") +
    scale_color_brewer(type = "qual", palette = "Set1", na.value = "grey") +
    scale_fill_brewer(type = "qual", palette = "Set1", guide = "none", na.value = "grey") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Days since start of campaign",
      y = "Cumulative Incidence",
      colour = NULL,
      title = NULL
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(colour = "black"),
      panel.grid.minor.x = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(.05, .95),
      legend.justification = c(0, 1),
    )

  ggsave(fs::path(output_dir, glue("km_{event_name}_{paste0(group_names, collapse='_')}.png")), plot = coverage_plot)
  print_interactive(coverage_plot)
}

# km_plot(km_estimates_all |> filter(group1 == "all", group2 == "crd"), "vax", "all", "crd")


# HRs and IRRs for events within different groups ----
# Function to output HRs and IRRs for disease burden comparing different subgroups
# note that parglm is faster, but produces an annoying warning that "'mustart' will not be used"
# don't know how to get rid of it!
adjusted_estimates <- function(data, subgroup, event_time, event_indicator) {
    
  # list of reference categories, which overwrites default reference category if needed
  contrasts_reference_levels <- list(
    ageband4 = "65-74",
    ageband13 = "65-69"
  )
 
  # use age-splines unless age is the subgroup of interest
  poisson_formula <- as.formula(glue("event_indicator ~ {subgroup} + sex + ns(age, 3)"))
  if (subgroup == "ageband4") poisson_formula <- as.formula(glue("event_indicator ~ ageband4 + sex"))
  if (subgroup == "ageband13") poisson_formula <- as.formula(glue("event_indicator ~ ageband13 + sex"))

  # prepare dataset
  data_outcome <-
    data |>
    mutate(
      event_time = .data[[event_time]],
      event_indicator = .data[[event_indicator]]
    ) |>
    select(
      all_of(subgroup),
      sex,
      age,
      event_time,
      event_indicator
    ) |>
    # to save memory, reduce dataset size by counting all distinct rows
    # then use these counts as weights when modelling / summarising later
    summarise(
      count = n(),
      .by = all_of(c(subgroup, "sex", "age",  "event_time", "event_indicator"))
    )

  # how many possible values of the group are there
  n_values <- n_distinct(data_outcome[[subgroup]])

  # all levels of variable that exist in the data, sorted by factor (or alphanumeric if not)
  # the first of these is the default reference level
  all_levels <- unique(as.character(sort(data_outcome[[subgroup]])))

  # explicitly include reference_level, and define contrasts if unusual
  # for example, if we wanted to use not the default reference level for a factor
  # this object is used inside a glm() call, to be passed to the `contrasts` argument
  if (
    (subgroup %in% names(contrasts_reference_levels)) &
      (n_values > 0) &
      ifelse(
        is.null(contrasts_reference_levels[[subgroup]]), 
        FALSE, 
        contrasts_reference_levels[[subgroup]] %in% all_levels
      )
  ) {
    # create the object passed to `contrasts` argument in glm call to use a different reference category
    subgroup_contrasts <- list(contr.treatment(all_levels,  which(all_levels == contrasts_reference_levels[[subgroup]])))
    names(subgroup_contrasts) <- subgroup
    reference_level <- contrasts_reference_levels[[subgroup]]
  } else {
    subgroup_contrasts <- NULL
    reference_level <- all_levels[1]
  }

  # summarise total people, events, and person-time
  # and add contrast label for merging with model output later
  data_summary <-
    data_outcome |>
    mutate(label = .data[[subgroup]]) |>
    arrange(label) |>
    summarise(
      variable = subgroup,
      n_obs_under_sdc_threshold = (sum(count) <= sdc_threshold),
      n_event_under_sdc_threshold = (sum(event_indicator * count) <= sdc_threshold),
      n_obs = round_sdc(sum(count)),
      n_event = round_sdc(sum(event_indicator * count)),
      exposure = round_sdc(sum(event_time * count)),

      .by = label
    ) |>
    mutate(
      label = as.character(label),
      reference_row = label == reference_level,
      contrast = glue("{subgroup}{label}")
    )

  # IRR model

  if (n_values > 1) {

    # fit the model
    # if there is an error, just return an empty dataset rather than fail
    data_poisson0 <-
      tryCatch(
        expr = {
          data_outcome |>
            mutate(
              event_indicator = as.integer(event_indicator)
            ) |>
            glm(
              data = _,
              formula = poisson_formula,
              family = poisson,
              offset = log(event_time),
              method = fastglm_fit,
              weights = count,
              contrasts = subgroup_contrasts
            ) |>
            broom.helpers::tidy_and_attach(
              tidy_fun = broom.helpers::tidy_parameters,
              ci_method = "wald"
            ) |>
            # broom.helpers::tidy_add_reference_rows() |>
            broom.helpers::tidy_add_term_labels() |>
            filter(variable == subgroup) |>
            select(variable, label, estimate, std.error, conf.low, conf.high)
          # note: the filter above is the same as doing marginaleffects::avg_comparisons(model, type = "link", variables = subgroup, comparison = "difference"),
          # as long as there are no interaction terms between subgroup and anything else
          # we use broom.helpers functions because it gives us the really nice variable and label info formatting for the outputted tidy dataset
          # if we want to use avg_comparisons in future, then attach the nicely formatted meta info onto a broom::tidy(avg_comparisons) object
          # or see "get_estimates_using_marginaleffects.R" script for a clue
        },
        error = function(e) {
          cat("error for subgroup", subgroup, ":", conditionMessage(e), "\n")
          data_summary |>
            select(variable, label) |>
            mutate(
              estimate = NA_real_,
              std.error = NA_real_,
              conf.low = NA_real_,
              conf.high = NA_real_
            )
        }
      )

    # combine summary and model outputs
    data_poisson <-
      full_join(
        data_summary,
        data_poisson0,
        by = c("variable", "label"),
      ) |>
      transmute(
        variable,
        label,
        reference_row,
        n_obs_under_sdc_threshold,
        n_event_under_sdc_threshold,
        n_obs,
        n_event,
        exposure,
        ir = n_event / exposure,
        irr_unadjusted = ir / ir[reference_row],
        irr = exp(estimate),
        irr.low = exp(conf.low),
        irr.high = exp(conf.high),
        irr.ln.std.error = std.error,
      ) |>
      # redact estimates if counts are low
      mutate(
        across(
          any_of(c("irr", "irr.low", "irr.high", "irr.ln.std.error")),
          ~ if_else(!(n_obs_under_sdc_threshold) & !(n_event_under_sdc_threshold), .x, NA_real_)
        )
      ) 

  } else {
    data_poisson <- 
      data_summary |> 
      select(-contrast) |>
      mutate(
        ir = n_event / exposure,
        irr_unadjusted = 1,
      )
  }

  rm(data_outcome)
  gc()
  
  return(data_poisson)

}

#  adjusted_estimates(data_combined, "ageband13", "covid_admitted_time", "covid_admitted_indicator")


# Length of stay ----

## Function to output length of stay quantiles for different subgroups ----
los_estimates <- function(data, subgroup, event_los) {

  # prepare dataset
  data_outcome <-
    data |>
    mutate(
      event_los = .data[[event_los]]
    ) |>
    select(
      all_of(subgroup),
      event_los
    )

  # LoS summary stats

  data_los <-
    data_outcome |>
    mutate(
      variable = subgroup,
      label = .data[[subgroup]],
    ) |>
    summarise(
      n_under_sdc_threshold = n() <= sdc_threshold,
      n = round_sdc(n()),
      n_at_least_1_event = round_sdc(sum(!is.na(event_los))),
      median_los = quantile(event_los, 0.5, na.rm = TRUE),
      p10 = quantile(event_los, 0.1, na.rm = TRUE),
      p25 = quantile(event_los, 0.25, na.rm = TRUE),
      p75 = quantile(event_los, 0.75, na.rm = TRUE),
      p90 = quantile(event_los, 0.9, na.rm = TRUE),

      .by = c(variable, label)
    ) |>
    mutate(
      across(
        c( n_at_least_1_event, median_los, p10, p25, p75, p90), 
        ~ if_else(!n_under_sdc_threshold, .x, NA)
      )
    ) |> 
    select(-n_under_sdc_threshold)

  return(data_los)
}


# los_estimates(data_combined, "ckd", "covid_admitted_los")
