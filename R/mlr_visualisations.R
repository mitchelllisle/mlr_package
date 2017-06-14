#' @exportdev

mlr_line <- function(x_data, y_data, colour = "#F76A63", x_label = "x_data", y_label = "y_data", point_labels = ""){
    z <- data.frame(x_data, y_data)
    ggplot(z, aes(z$x_data, z$y_data)) +
    geom_line(color = colour, size = 1.5) +
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

mlr_bar_time <- function(x_data, y_data, colour = "#4A90E2", x_label = "x_data", y_label = "y_data", title = "", point_labels = "", group_colours = c("#EA6532", "#4A90E2", "#47B191", "#FBC75D", "#AE93B5", "#FBC75D")){
  z <- data.frame(x_data, y_data)
  ggplot(z, aes(x=factor(z$x_data), z$y_data, fill = colour)) +
    geom_bar(stat = "identity", size = 0.8) +
    labs(x = x_label) +
    labs(y = y_label) +
    labs(title = title) +
    geom_text(aes(label=point_labels, vjust = 1.5),  colour="black", position = position_stack(vjust = 1)) +
    scale_fill_manual(values=group_colours) +
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

mlr_minimal <- function(base_size = 11, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.ticks = element_blank(), legend.background = element_blank(),
          legend.key = element_blank(), panel.background = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          plot.background = element_blank(), complete = TRUE, plot.title = element_text(size = 16))
}

mlr_minimal_dark <- function(base_size = 11, base_family = ""){
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "#242424"),  
      legend.key = element_rect(color = "white",  fill = "#242424"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "none",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "horizontal",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "#242424", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey20", linetype = "dashed"),  
      panel.grid.minor = element_line(color = "grey20", linetype = "dashed"),  
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "#242424", fill = "#242424"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}
