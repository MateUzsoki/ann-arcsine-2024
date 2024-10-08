

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)
# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)
# Visualization
library(cowplot)
# Preprocessing
library(recipes)
# Sampling / Accuracy
library(rsample)
library(yardstick) 
# Modeling
library(keras)

tidy_acf <- function(data, value, lags = 0:20) {
  
  value_expr <- enquo(value)
  
  acf_values <- data %>%
    pull(value) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}
lstr_calc <- function(DF_LFR)
{

  input = GetANNInputData()
  
  periods_train <- 250 * 5
  periods_test  <- 250 * 1
  skip_span     <- 250 * 
  
  rolling_origin_resamples <- rolling_origin(
    input,
    initial    = periods_train,
    assess     = periods_test,
    cumulative = FALSE,
    skip       = skip_span
  )
  rolling_origin_resamples
  
  split    <- rolling_origin_resamples$splits[[10]]
  split_id <- rolling_origin_resamples$id[[10]]
  
  
  df_trn <- training(split)
  df_tst <- testing(split)
  df <- bind_rows(
    df_trn %>% add_column(key = "training"),
    df_tst %>% add_column(key = "testing")
  ) %>% 
    as_tbl_time(index = index)
  df
  
  # 5.1.3 Preprocessing
  rec_obj <- recipe(value ~ ., df) %>%
    step_sqrt(value) %>%
    step_center(value) %>%
    step_scale(value) %>%
    prep()
  df_processed_tbl <- bake(rec_obj, df)
  df_processed_tbl
  
  center_history <- rec_obj$steps[[2]]$means["value"]
  scale_history  <- rec_obj$steps[[3]]$sds["value"]
  
  c("center" = center_history, "scale" = scale_history)
  
  # Model inputs
  lag_setting  <- 120 # = nrow(df_tst)
  batch_size   <- 40
  train_length <- 440
  tsteps       <- 1
  epochs       <- 300
  
  # Training Set
  lag_train_tbl <- df_processed_tbl %>%
    mutate(value_lag = lag(value, n = lag_setting)) %>%
    filter(!is.na(value_lag)) %>%
    filter(key == "training") %>%
    tail(train_length)
  x_train_vec <- lag_train_tbl$value_lag
  x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
  y_train_vec <- lag_train_tbl$value
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
  # Testing Set
  lag_test_tbl <- df_processed_tbl %>%
    mutate(
      value_lag = lag(value, n = lag_setting)
    ) %>%
    filter(!is.na(value_lag)) %>%
    filter(key == "testing")
  x_test_vec <- lag_test_tbl$value_lag
  x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
  y_test_vec <- lag_test_tbl$value
  y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))
  
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units            = 50, 
               input_shape      = c(tsteps, 1), 
               batch_size       = batch_size,
               return_sequences = TRUE, 
               stateful         = TRUE) %>% 
    layer_lstm(units            = 50, 
               return_sequences = FALSE, 
               stateful         = TRUE) %>% 
    layer_dense(units = 1)
  model %>% 
    compile(loss = 'mae', optimizer = 'adam')
  model
  
  for (i in 1:epochs) {
    model %>% fit(x          = x_train_arr, 
                  y          = y_train_arr, 
                  batch_size = batch_size,
                  epochs     = 1, 
                  verbose    = 1, 
                  shuffle    = FALSE)
    
    model %>% reset_states()
    cat("Epoch: ", i)
    
  }
}

# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spots_time_summary <- sun_spots %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(sun_spots_time_summary$start, 
                              sun_spots_time_summary$end))
  }
  
  return(g)
}

# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}


predict_keras_lstm <- function(split, epochs = 300, ...) {
  
  lstm_prediction <- function(split, epochs, ...) {
    
    # 5.1.2 Data Setup
    df_trn <- training(split)
    df_tst <- testing(split)
    
    df <- bind_rows(
      df_trn %>% add_column(key = "training"),
      df_tst %>% add_column(key = "testing")
    ) %>% 
      as_tbl_time(index = index)
    
    # 5.1.3 Preprocessing
    rec_obj <- recipe(value ~ ., df) %>%
      step_sqrt(value) %>%
      step_center(value) %>%
      step_scale(value) %>%
      prep()
    
    df_processed_tbl <- bake(rec_obj, df)
    
    center_history <- rec_obj$steps[[2]]$means["value"]
    scale_history  <- rec_obj$steps[[3]]$sds["value"]
    
    # 5.1.4 LSTM Plan
    lag_setting  <- 25 # = nrow(df_tst)
    batch_size   <- 2
    train_length <- 100
    tsteps       <- 1
    epochs       <- epochs
    
    # 5.1.5 Train/Test Setup
    lag_train_tbl <- df_processed_tbl %>%
      mutate(value_lag = lag(value, n = lag_setting)) %>%
      filter(!is.na(value_lag)) %>%
      filter(key == "training") %>%
      tail(train_length)
    
    x_train_vec <- lag_train_tbl$value_lag
    x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
    
    y_train_vec <- lag_train_tbl$value
    y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
    
    lag_test_tbl <- df_processed_tbl %>%
      mutate(
        value_lag = lag(value, n = lag_setting)
      ) %>%
      filter(!is.na(value_lag)) %>%
      filter(key == "testing")
    
    x_test_vec <- lag_test_tbl$value_lag
    x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
    
    y_test_vec <- lag_test_tbl$value
    y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))
    
    # 5.1.6 LSTM Model
    model <- keras_model_sequential()
    
    model %>%
      layer_lstm(units            = 50, 
                 input_shape      = c(tsteps, 1), 
                 batch_size       = batch_size,
          
                 return_sequences = TRUE, 
                 stateful         = TRUE) %>% 
      layer_lstm(units            = 50, 
                 return_sequences = FALSE, 
                 stateful         = TRUE) %>% 
      layer_dense(units = 1)
    
    model %>% 
      compile(loss = 'mae', optimizer = 'adam')
    
    # 5.1.7 Fitting LSTM
    for (i in 1:epochs) {
      model %>% fit(x          = x_train_arr, 
                    y          = y_train_arr, 
                    batch_size = batch_size,
                    epochs     = 1, 
                    verbose    = 1, 
                    shuffle    = FALSE)
      
      model %>% reset_states()
      cat("Epoch: ", i)
      
    }
    
    # 5.1.8 Predict and Return Tidy Data
    # Make Predictions
    pred_out <- model %>% 
      predict(x_test_arr, batch_size = batch_size) %>%
      .[,1] 
    
    # Retransform values
    pred_tbl <- tibble(
      index   = lag_test_tbl$index,
      value   = (pred_out * scale_history + center_history)^2
    ) 
    
    # Combine actual data with predictions
    tbl_1 <- df_trn %>%
      add_column(key = "actual")
    
    tbl_2 <- df_tst %>%
      add_column(key = "actual")
    
    tbl_3 <- pred_tbl %>%
      add_column(key = "predict")
    
    # Create time_bind_rows() to solve dplyr issue
    time_bind_rows <- function(data_1, data_2, index) {
      index_expr <- enquo(index)
      bind_rows(data_1, data_2) %>%
        as_tbl_time(index = !! index_expr)
    }
    
    ret <- list(tbl_1, tbl_2, tbl_3) %>%
      reduce(time_bind_rows, index = index) %>%
      arrange(key, index) %>%
      mutate(key = as_factor(key))
    
    return(ret)
    
  }
  
  safe_lstm <- possibly(lstm_prediction, otherwise = NA)
  
  safe_lstm(split, epochs, ...)
  
}

acfStats <- function(DF_LFR)
{
  DF_LFR = GetANNInputData()
  es_difference =  DF_LFR
  es_difference["value"] = DF_LFR["ES_D_REAL"]
  es_difference = es_difference["value"]
  rownames(es_difference) <- DF_LFR[,4]

    ES_tibble <- es_difference %>%
    tk_tbl() %>%
    mutate(index = as_date(index)) %>%
    as_tbl_time(index = index)
  
    #ES_tibble <- ES_tibble%>%mutate(value=ESR-lag(ESR,default=first(ESR)))
  
    
  p1 <- ES_tibble %>%
    ggplot(aes(index, value)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    theme_tq() +
    labs(
      title = "From 1989 to 2017 (Full Data Set)"
    )
  p2 <- ES_tibble %>%
    filter_time("start" ~ "1990") %>%
    ggplot(aes(index, value)) +
    geom_line(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(color = palette_light()[[1]]) +
    geom_smooth(method = "loess", span = 0.2, se = FALSE) +
    theme_tq() +
    labs(
      title = "1989 to 1995 (Zoomed In To Show Cycle)",
      caption = "datasets::sunspot.month"
    )
  p_title <- ggdraw() + 
    draw_label("Sunspots", size = 18, fontface = "bold", colour = palette_light()[[1]])
  plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))
  
  max_lag <- 250 
  ES_tibble %>% tidy_acf(value, lags = 0:max_lag)
  
  ES_tibble %>%
    tidy_acf(value, lags = 0:max_lag) %>%
    ggplot(aes(lag, acf)) +
    geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
    geom_vline(xintercept = 120, size = 1, color = palette_light()[[2]]) +
    annotate("text", label = "120", x = 130, y = 0.8, 
             color = palette_light()[[2]], size = 6, hjust = 0) +
    theme_tq() +
    labs(title = "ACF: arcsine error")

  optimal_lag_setting <- ES_tibble %>%
    tidy_acf(value, lags = 100:140) %>%
    filter(acf == max(acf)) %>%
    pull(lag)
  optimal_lag_setting
  
  
}


