# split    <- rolling_origin_resamples$splits[[9]]
# split_id <- rolling_origin_resamples$id[[10]]
# 
# plot_split(split, expand_y_axis = FALSE, size = 0.5) +
#   theme(legend.position = "bottom") +
#   ggtitle(glue("Split: {split_id}"))

predict_keras_lstm(split, epochs = 10)



reticulate::py_config() 
reticulate::py_module_available("keras")