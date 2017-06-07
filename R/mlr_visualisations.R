#' @exportdev

mlr_line <- function(x_data, y_data, colour = "#AE93B5", x_label = "x_data", y_label = "y_data", point_labels = ""){
    z <- data.frame(x_data, y_data)
    ggplot(z, aes(z$x_data, z$y_data)) +
    geom_line(color = colour, size = 0.8) +
    geom_point(color = colour, size = 2) +
    labs(x = x_label) +
    labs(y = y_label) +
    geom_text(aes(label=point_labels), colour="black") +
    mlr_minimal()
}

mlr_ribbon <- function(x_data, y_data, colour = "#AE93B5", x_label = "x_data", y_label = "y_data", point_labels = "", ribbon_lower = "", ribbon_upper = ""){
  z <- data.frame(x_data, y_data)
  ggplot(z, aes(z$x_data, z$y_data)) +
    geom_line(color = colour, size = 0.8) +
    geom_point(color = colour, size = 2) +
    geom_ribbon(aes(ymin=ribbon_lower, ymax=ribbon_upper), alpha=0.1) +
    labs(x = x_label) +
    labs(y = y_label) +
    geom_text(aes(label=point_labels), colour="black") +
    mlr_minimal()
}

mlr_bar <- function(x_data, y_data, colour = "#4A90E2", x_label = "x_data", y_label = "y_data", title = "", point_labels = ""){
  z <- data.frame(x_data, y_data)
  ggplot(z, aes(x=reorder(z$x_data,-z$y_data,sum), z$y_data, fill = colour)) +
    geom_bar(stat = "identity", size = 0.8) +
    labs(x = x_label) +
    labs(y = y_label) +
    labs(title = title) +
    geom_text(aes(label=point_labels, vjust = 1.5),  colour="black", position = position_stack(vjust = 1)) +
    mlr_minimal()
}

mlr_bar_time <- function(x_data, y_data, colour = "#4A90E2", x_label = "x_data", y_label = "y_data", title = "", point_labels = ""){
  z <- data.frame(x_data, y_data)
  ggplot(z, aes(x=factor(z$x_data), z$y_data, fill = colour)) +
    geom_bar(stat = "identity", size = 0.8) +
    labs(x = x_label) +
    labs(y = y_label) +
    labs(title = title) +
    geom_text(aes(label=point_labels, vjust = 1.5),  colour="black", position = position_stack(vjust = 1)) +
    scale_fill_manual(values=c("#EA6532", "#4A90E2", "#47B191", "#FBC75D")) +
    mlr_minimal()
}

mlr_bubble <- function(x_data, y_data, size_data, colour = "#4A90E2", x_label = "x_data", y_label = "y_data"){
  z <- data.frame(x_data, y_data)
  ggplot(z, aes(x = z$x_data, y = z$y_data, size = size_data)) +
    geom_point(colour = colour, show.legend = FALSE) +
    scale_size_continuous(range = c(5,20)) +
    labs(x = x_label) +
    labs(y = y_label) +
    mlr_minimal()
}

mlr_scatter <- function(x_data, y_data, colour = "#4A90E2", x_label = "x_data", y_label = "y_data"){
  z <- data.frame(x_data, y_data)
  ggplot(z, aes(x = z$x_data, y = z$y_data)) +
    geom_point(colour = colour, show.legend = FALSE) +
    labs(x = x_label) +
    labs(y = y_label) +
    mlr_minimal()
}


mlr_basic <- function(base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(colour = "#4A4A4A"),
      axis.title.x = element_text(colour = "#4A4A4A", vjust=0.1),
      axis.title.y = element_text(colour = "#4A4A4A", angle = 90, vjust=10),
      axis.ticks = element_line(color = "gray"),
      panel.background = element_blank(),
      panel.grid.minor.y = element_line(size=3),
      panel.grid.major = element_line(colour = "gray"),
      plot.background = element_rect(fill="white")
    )
}

mlr_minimal <-   function(base_size = 11, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.ticks = element_blank(), legend.background = element_blank(),
          legend.key = element_blank(), panel.background = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          plot.background = element_blank(), complete = TRUE, plot.title = element_text(size = 16))
}
