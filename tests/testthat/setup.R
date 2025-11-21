# Ensure empty graphics device is opened to prevent tests that call
# ggplot2::ggplot_gtable() from opening a new device
grDevices::pdf(NULL)