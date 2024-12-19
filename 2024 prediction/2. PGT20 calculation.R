library(ggplot2)
library(stringr)
library(extrafont)

# best-performing model 
equation <- "PGT20 = -114.45 +  MinT4wk*-10.26 + MinT2wk*8.04 + MaxT5wk*26.26 + Rain4wk*-0.86 +  MaxT4wk*-20.87"

PGT20_prop_ossrr <- function(df, equation) {
  # Extract the response variable from the equation
  response_var <- strsplit(equation, "=")[[1]][1]
  
  # Extract the predictor variables from the equation
  predictor_vars <- strsplit(equation, "=")[[1]][2]
  predictor_varq <- gsub("[/^]", "", predictor_vars)
  predictor_vart <- unlist(strsplit(predictor_varq, "[+_\\*]"))
  predictor_final<- gsub("\\s+", "", predictor_vart)
  
  # Check if each element starts with a number
  is_numeric <- grepl("^[0-9-]", predictor_final)
  
  # Convert numeric elements to numeric
  char_coff<- as.numeric(predictor_final[is_numeric])
  
  # Replace non-numeric elements with variable names
  pr_vrbl <- gsub("[^[:alpha:][:digit:]]", "", 
                  predictor_final[!is_numeric])
  
  # Initialize variables
  coef_first <- char_coff[1] 
  coef_list <- char_coff[-1] 
  pred_vars <- pr_vrbl
  predicted <- char_coff[1] 
  
  
  # Loop to calculate the predicted values
  for (i in 1:length(coef_list)) {
    predicted <- predicted+ coef_list[i] * df[, pred_vars[i]] 
  }
  
  
  return(predicted)
}


calculate_metrics <- function(df) {
  df <- df %>%
    mutate(
      MinT4wk = rollapply(TMIN, width = 28, FUN = mean, fill = NA, align = "right"),
      MinT2wk = rollapply(TMIN, width = 14, FUN = mean, fill = NA, align = "right"),
      MaxT5wk = rollapply(TMAX, width = 35, FUN = mean, fill = NA, align = "right"),
      Rain4wk = rollapply(PRCP, width = 28, FUN = sum, fill = NA, align = "right"),
      MaxT4wk = rollapply(TMAX, width = 28, FUN = mean, fill = NA, align = "right")
    )
  return(df)
}


process_data_list <- function(data_list, equation) {
  # Using map to iterate over each state's list of data frames
  data_list %>%
    purrr::map(~ map_df(.x, ~ {
      df <- calculate_metrics(.x)
      df <- df %>% 
        mutate(PGT20 = PGT20_prop_ossrr(df, equation))
      return(df)
    }, .id = "Dataframe")) %>%
    purrr::map_df(bind_rows, .id = "State")  # Combine all the results into a single data frame
}


dataset <- process_data_list(consolidated_data_list , equation)

dataset$PGT20$MinT4wk <- ifelse(dataset$PGT20$MinT4wk < 0, 0, dataset$PGT20$MinT4wk)

dataset$Dataframe <- recode(dataset$Dataframe, 
       USC00013251.csv ="AL Geneva",
       USW00013839.csv = "AL Dothan",
       USW00063872.csv = "AL Eufaula",
       DOUGLAS.csv = "GA Douglas", 
       MOULTRIE.csv = "GA Moultrie", 
       TIFTON.csv = "GA Tifton",
       Jay.csv = "FL Jay",
       live_oak.csv = "FL Live Oak", 
       Marianna.csv = "FL Marianna",
       `North Farm Starkv.csv` = "MS North Farm Starkv",
       Port_Gibson.csv = "MS Port Gibson", 
       Stoneville_F10.csv = "MS Stoneville")


start_date <- as.Date("2024-09-15")

graph <- ggplot(dataset, aes(x=DATE, y=PGT20$MinT4wk, 
                             group = Dataframe, color = Dataframe))+
  geom_line(linewidth = 1.5)+
  facet_wrap(~State, nrow =4) +
  ylim(0,45) +
  labs(x = "Date",
       y = "PGT20 (%)",
       color = "Stations", 
       title = "Daily PGT20 prediction (as of 11/05/2024)") +
  scale_color_brewer(palette = "Set3") +
  scale_x_date(limits = c(start_date, max(dataset$DATE, na.rm = TRUE))) +
  theme_bw()+
  theme(title = element_text(family = "Arial", size = 12),
        text = element_text(family = "Arial", size = 12),
        axis.title= element_text(family = "Arial", size = 12),
        axis.text = element_text(family = "Arial", size = 12),
        legend.text = element_text(family = "Arial", size = 12))

graph  


timestamp <- format(Sys.time(), "%Y%m%d")
filename <- paste0("plot_", timestamp, ".png")

# Save the plot with the generated filename
ggsave(filename = filename, plot = graph, width = 170, height = 150, units = "mm")
