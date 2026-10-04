#setwd("~/software/repos/RProjectNASA")
getwd()

list.files(".", all.files = TRUE)

list.files(
  "data",
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE
)

# so R studio's working directory is the home folder, NOT where the .r file is stored upon open unless you use a project.
# Womp Womp.
#

# this includes libs for graphing / data sorting
library(R.matlab)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(ggplot2)
library(here)
here::here()

# this looks for the .mat files from NASA.
mat_files <- list.files(
  here::here("data", "nasa_battery"),
  pattern = "\\.mat$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
mat_files

# creates a list of all the graphs for outputting as a PDF:
plots <- list()


# these are helper funcs for grouping data
get_operation <- function(cycle_array, i) {
  fields <- dimnames(cycle_array)[[1]]
  
  setNames(
    lapply(seq_along(fields), function(j) cycle_array[[j, 1, i]]),
    fields
  )
}

get_data_fields <- function(operation) {
  fields <- dimnames(operation$data)[[1]]
  
  setNames(
    lapply(seq_along(fields), function(j) operation$data[[j, 1, 1]]),
    fields
  )
}



extract_discharge_data <- function(mat_file) {
  raw <- R.matlab::readMat(mat_file)
  
  battery_id <- names(raw)[1]
  battery <- raw[[battery_id]]
  
  cycle_array <- battery[[1]]
  n_operations <- dim(cycle_array)[3]
  
  map_dfr(seq_len(n_operations), function(i) {
    operation <- get_operation(cycle_array, i)
    operation_type <- as.character(operation$type)[1]
    
    # Keep only discharge records, since Capacity is defined here.
    if (operation_type != "discharge") {
      return(NULL)
    }
    
    data <- get_data_fields(operation)
    
    capacity_ah <- if ("Capacity" %in% names(data)) {
      as.numeric(data$Capacity)[1]
    } else {
      NA_real_
    }
    
    temperature_values <- if ("Temperature.measured" %in% names(data)) {
      as.numeric(data$Temperature.measured)
    } else {
      numeric(0)
    }
    
    voltage_values <- if ("Voltage.measured" %in% names(data)) {
      as.numeric(data$Voltage.measured)
    } else {
      numeric(0)
    }
    
    time_values <- if ("Time" %in% names(data)) {
      as.numeric(data$Time)
    } else {
      numeric(0)
    }
    voltage_values <- if ("Voltage.measured" %in% names(data)) {
      as.numeric(data$Voltage.measured)
    } else {
      numeric(0)
    }
    
    current_values <- if ("Current.measured" %in% names(data)) {
      as.numeric(data$Current.measured)
    } else {
      numeric(0)
    }
    
    energy_wh <- if (
      length(time_values) >= 2 &&
      length(voltage_values) == length(time_values) &&
      length(current_values) == length(time_values)
    ) {
      # Discharge current is negative in this NASA file.
      # Negative sign makes delivered energy positive.
      -sum(
        diff(time_values) *
          (head(voltage_values, -1) + tail(voltage_values, -1)) / 2 *
          (head(current_values, -1) + tail(current_values, -1)) / 2
      ) / 3600
    } else {
      NA_real_
    }
    
    
    
    tibble(
      battery_id = battery_id,
      operation_index = i,
      ambient_temperature_c = as.numeric(operation$ambient.temperature)[1],
      capacity_ah = capacity_ah,
      mean_temperature_c = mean(temperature_values, na.rm = TRUE),
      max_temperature_c = max(temperature_values, na.rm = TRUE),
      mean_voltage_v = mean(voltage_values, na.rm = TRUE),
      discharge_duration_sec = max(time_values, na.rm = TRUE),
      energy_wh = energy_wh,
      n_measurements = length(time_values)
    )
  }) |>
    mutate(
      discharge_cycle = row_number(),
      battery_id = factor(battery_id)
    ) |>
    select(
      battery_id,
      discharge_cycle,
      operation_index,
      capacity_ah,
      mean_temperature_c,
      ambient_temperature_c,
      max_temperature_c,
      mean_voltage_v,
      discharge_duration_sec,
      n_measurements,
      energy_wh
    )
}

all_capacity <- map_dfr(mat_files, extract_discharge_data)


energy_by_battery <- all_capacity |>
  group_by(battery_id) |>
  summarise(
    total_energy_wh = sum(energy_wh, na.rm = TRUE),
    mean_energy_per_discharge_wh = mean(energy_wh, na.rm = TRUE),
    number_of_discharge_cycles = n(),
    .groups = "drop"
  ) |>
  mutate(
    percent_of_total_energy = 100 * total_energy_wh / sum(total_energy_wh)
  )

energy_by_battery

temp <- ggplot(
  energy_by_battery,
  aes(
    x = "",
    y = total_energy_wh,
    fill = battery_id
  )
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        battery_id,
        "\n",
        round(percent_of_total_energy, 1),
        "%"
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Share of Total Energy Delivered by Each Battery",
    subtitle = "Summed across all recorded discharge cycles",
    fill = "Battery ID"
  ) +
  theme_void()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["PieChart_Energy"]] <- temp

temp <- ggplot(
  energy_by_battery,
  aes(
    x = reorder(battery_id, total_energy_wh),
    y = total_energy_wh,
    fill = battery_id
  )
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = round(total_energy_wh, 1)),
    vjust = -0.4
  ) +
  labs(
    title = "Total Energy Delivered Across the Observed Test Lifetime",
    x = "Battery ID",
    y = "Total delivered energy (Wh)"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["Histogram_EnergyLifeTime"]] <- temp






analysis_data <- all_capacity |>
  mutate(
    health_group = case_when(
      capacity_ah >= 1.70 ~ "Healthy (≥ 1.70 Ah)",
      capacity_ah >= 1.40 ~ "Moderate degradation (1.40–1.69 Ah)",
      TRUE ~ "Near end of life (< 1.40 Ah)"
    ),
    health_group = factor(
      health_group,
      levels = c(
        "Healthy (≥ 1.70 Ah)",
        "Moderate degradation (1.40–1.69 Ah)",
        "Near end of life (< 1.40 Ah)"
      )
    )
  )

analysis_data |>
  count(health_group)

analysis_data |>
  group_by(battery_id, health_group) |>
  summarise(
    observations = n(),
    .groups = "drop"
  )


health_counts <- analysis_data |>
  count(health_group, name = "number_of_discharge_cycles") |>
  mutate(percent = round(100 * number_of_discharge_cycles / sum(number_of_discharge_cycles), 1))

health_counts

temp <- ggplot(
  health_counts,
  aes(x = "", y = number_of_discharge_cycles, fill = health_group)
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(percent, "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold"
  ) +
  labs(
    title = "Distribution of Discharge Cycles by Battery Health",
    fill = "Health group"
  ) +
  theme_void()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["PieChart_DischargeCycles"]] <- temp


temp <- ggplot(
  health_counts,
  aes(x = health_group, y = number_of_discharge_cycles, fill = health_group)
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = number_of_discharge_cycles),
    vjust = -0.4
  ) +
  labs(
    title = "Discharge Cycles by Battery Health Category",
    x = "Battery health category",
    y = "Number of discharge-cycle observations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 12, hjust = 0.5))
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["Histogram_BatteryHealth"]] <- temp



temp <- ggplot(
  analysis_data,
  aes(
    x = discharge_cycle,
    y = capacity_ah,
    color = battery_id
  )
) +
  geom_hline(
    yintercept = 1.4,
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.8
  ) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 0.8, alpha = 0.65) +
  labs(
    title = "Capacity Fade Across NASA Lithium-Ion Batteries",
    subtitle = "Dashed line marks the 1.4 Ah end-of-life reference",
    x = "Discharge-cycle number within each battery",
    y = "Measured discharge capacity (Ah)",
    color = "Battery"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["LineChart_CapacityFade"]] <- temp

temp <- ggplot(
  analysis_data,
  aes(
    x = discharge_duration_sec / 60,
    y = capacity_ah,
    color = battery_id
  )
) +
  geom_point(alpha = 0.65, size = 1.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8
  ) +
  labs(
    title = "Discharge Capacity Versus Discharge Duration",
    subtitle = "Each point represents one discharge operation",
    x = "Discharge duration (minutes)",
    y = "Measured capacity (Ah)",
    color = "Battery"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["LineChart_CapacityVSDuration"]] <- temp

cor.test(
  analysis_data$capacity_ah,
  analysis_data$discharge_duration_sec,
  use = "complete.obs"
)


temp <- ggplot(
  analysis_data,
  aes(
    x = mean_temperature_c,
    y = capacity_ah,
    color = battery_id
  )
) +
  geom_point(alpha = 0.6, size = 1.4) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.8
  ) +
  labs(
    title = "Capacity and Mean Temperature During Discharge",
    x = "Mean measured discharge temperature (°C)",
    y = "Discharge capacity (Ah)",
    color = "Battery"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["ScatterPlot_CapacityVSTemp"]] <- temp



temp <- ggplot(
  analysis_data,
  aes(x = battery_id, y = discharge_duration_sec / 60, fill = battery_id)
) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Distribution of Discharge Duration by Battery",
    x = "Battery ID",
    y = "Discharge duration (minutes)"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["BoxPlot_DischargeDurration"]] <- temp

# Check the dataset before analysis.
glimpse(all_capacity)
nrow(all_capacity)
summary(all_capacity)

# Save the clean analysis dataset.
dir.create(here::here("output"), showWarnings = FALSE)

write_csv(
  all_capacity,
  here::here("output", "nasa_battery_discharge_cycles.csv")
)



capacity_stats <- tibble(
  variable = "Discharge capacity (Ah)",
  statistic = c("Mean", "Median", "Standard deviation",
                "Minimum", "Q1", "Q3", "Maximum"),
  value = c(
    mean(all_capacity$capacity_ah, na.rm = TRUE),
    median(all_capacity$capacity_ah, na.rm = TRUE),
    sd(all_capacity$capacity_ah, na.rm = TRUE),
    min(all_capacity$capacity_ah, na.rm = TRUE),
    quantile(all_capacity$capacity_ah, 0.25, na.rm = TRUE),
    quantile(all_capacity$capacity_ah, 0.75, na.rm = TRUE),
    max(all_capacity$capacity_ah, na.rm = TRUE)
  )
)

temperature_stats <- tibble(
  variable = "Mean discharge temperature (°C)",
  statistic = c("Mean", "Median", "Standard deviation",
                "Minimum", "Q1", "Q3", "Maximum"),
  value = c(
    mean(all_capacity$mean_temperature_c, na.rm = TRUE),
    median(all_capacity$mean_temperature_c, na.rm = TRUE),
    sd(all_capacity$mean_temperature_c, na.rm = TRUE),
    min(all_capacity$mean_temperature_c, na.rm = TRUE),
    quantile(all_capacity$mean_temperature_c, 0.25, na.rm = TRUE),
    quantile(all_capacity$mean_temperature_c, 0.75, na.rm = TRUE),
    max(all_capacity$mean_temperature_c, na.rm = TRUE)
  )
)

capacity_stats
temperature_stats

# First graph:
# histogram of the discharge capacity in Amp-Hours vs number of discharge cycles.
# this shows 
temp <- ggplot(all_capacity, aes(x = capacity_ah)) +
  geom_histogram(
    binwidth = 0.05,
    fill = "#2C7FB8",
    color = "white"
  ) +
  labs(
    title = "Distribution of Battery Discharge Capacity",
    x = "Discharge capacity (Ah)",
    y = "Number of discharge cycles"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["Histogram_BatteryDischargeCapacity"]] <- temp


# line graph, not really needed but good graph:
# here's the battery lifespan compared with the number of discharge cycles.
# this should be a steady decline as the discharge cycles goes down.
temp <- ggplot(
  all_capacity,
  aes(
    x = discharge_cycle,
    y = capacity_ah,
    color = battery_id
  )
) +
  geom_hline(
    yintercept = 1.4,
    linetype = "dashed", # 1.4Ah battery is reaching end of life.
    color = "firebrick", # the graph shows this to be around >100  
    linewidth = 0.8
  ) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 0.8, alpha = 0.6) +
  labs(
    title = "Battery Capacity Decline Over Discharge Cycles",
    subtitle = "Dashed line: 1.4 Ah end-of-life reference threshold",
    x = "Discharge-cycle number",
    y = "Discharge capacity (Ah)",
    color = "Battery ID"
  ) +
  #theme_minimal()
  theme_gray()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["LineChart_CapacityDeclineVSDischargeCycles"]] <- temp

# Histogram of the avg temp of the battery compared to
# the number of charge cycles it experienced.
# the temps are nominal, nothing too interesting here.
temp <- ggplot(all_capacity, aes(x = mean_temperature_c)) +
  geom_histogram(
    binwidth = 1,
    fill = "#F28E2B",
    color = "white"
  ) +
  labs(
    title = "Distribution of Mean Discharge Temperature",
    x = "Mean measured temperature (°C)",
    y = "Number of discharge cycles"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["BarChart_AVGDischargeTemp"]] <- temp

# Box Plot of Discharge Capacity per battery. 
temp <- ggplot(
  all_capacity,
  aes(x = battery_id, y = capacity_ah, fill = battery_id)
) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Discharge Capacity by Battery",
    x = "Battery ID",
    y = "Discharge capacity (Ah)"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["BoxPlot_DischargeCap"]] <- temp

  # Box Plot of Average Discharge Temperature per battery.
  # Battery Temp for B0018 is considerably lower then the others.
  temp <- ggplot(
    all_capacity,
    aes(x = battery_id, y = mean_temperature_c, fill = battery_id)
  ) +
    geom_boxplot(show.legend = FALSE) +
    labs(
      title = "Mean Discharge Temperature by Battery",
      x = "Battery ID",
      y = "Mean measured temperature (°C)"
    ) +
    theme_minimal()
  # push back the graph to a list for outputting all graphs at once.
  #plots <- append(plots, list(temp))
  plots[["BoxPlot_DischargeTempAVG"]] <- temp


battery_counts <- all_capacity |>
  count(battery_id, name = "number_of_discharge_cycles") |>
  mutate(percent = 100 * number_of_discharge_cycles / sum(number_of_discharge_cycles))

battery_counts

# BAR Chart:
# looks like each battery was very similar, 
# with Battery B0018 having less discharge cycles then the other 3.
temp <- ggplot(
  battery_counts,
  aes(x = battery_id, y = number_of_discharge_cycles, fill = battery_id)
) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Number of Discharge-Cycle Observations by Battery",
    x = "Battery ID",
    y = "Number of discharge cycles"
  ) +
  theme_minimal()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["BarChart_DischargeCycleCount"]] <- temp

# pie chart: number_of_discharge_cycles per battery
# this one seems pretty equal for all 4 batteries
temp <- ggplot(
  battery_counts,
  aes(
    x = "",
    y = number_of_discharge_cycles,
    fill = battery_id
  )
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(
    title = "Share of Discharge-Cycle Observations by Battery",
    fill = "Battery ID"
  ) +
  theme_void()
# push back the graph to a list for outputting all graphs at once.
#plots <- append(plots, list(temp))
plots[["PieChart_DischargeCycle"]] <- temp


dir.create(here::here("output"), showWarnings = FALSE)

pdf(
  file = here::here("output", "battery_graphs.pdf"),
  width = 10,
  height = 7,
  onefile = TRUE
)

for (plot_name in names(plots)) {
  print(plots[[plot_name]])
}

dev.off()

