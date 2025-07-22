# Define Custom Function A ----
make_plotly_A <- function(plot_obj, y2_title = "Scheduled Quantity (TJ/d)", font_family = "Helvetica") {
  p <- ggplotly(plot_obj)
  
  # Ensure p$x$data exists and has content before modifying
  if (!is.null(p$x$data)) {
    for (i in seq_along(p$x$data)) {
      p$x$data[[i]]$yaxis <- "y2"
    }
  }
  
  # Apply layout with right axis and margins
  p <- layout(
    p,
    yaxis = list(
      showticklabels = FALSE,
      title = "",
      showline = FALSE,
      zeroline = FALSE
    ),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = list(
        text = y2_title,
        font = list(family = font_family, size = 12),
        standoff = 30
      ),
      tickfont = list(family = font_family, size = 12),
      showline = TRUE
    ),
    xaxis = list(
      titlefont = list(family = font_family, size = 12)
    ),
    title = list(
      font = list(family = font_family, size = 14)
    ),
    font = list(family = font_family, size = 12, color = "black"),
    margin = list(l = 40, r = 100, t = 60, b = 60)
  )
  
  return(p)
}

make_plotly_B <- function(plot_obj, y2_title = "Scheduled Quantity (TJ/d)", font_family = "Helvetica") {
  p <- ggplotly(plot_obj)
  
  if (!is.null(p$x$data)) {
    for (i in seq_along(p$x$data)) {
      p$x$data[[i]]$yaxis <- "y2"
    }
  }
  
  p <- layout(
    p,
    yaxis = list(
      showticklabels = FALSE,
      title = "",
      showline = FALSE,
      zeroline = FALSE
    ),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = list(
        text = y2_title,
        font = list(family = font_family, size = 12),
        standoff = 30
      ),
      tickfont = list(family = font_family, size = 12),
      showline = TRUE
    ),
    xaxis = list(
      titlefont = list(family = font_family, size = 12),
      tickfont = list(family = font_family, size = 12)
    ),
    title = list(
      font = list(family = font_family, size = 14)
    ),
    font = list(family = font_family, size = 12, color = "black"),
    legend = list(
      x = -0.3,
      xanchor = "left",
      y = 1,
      title = list(text = "")
    ),
    margin = list(l = 80, r = 80, t = 60, b = 60)
  )
  
  return(p)
}