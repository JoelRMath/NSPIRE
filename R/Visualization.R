# ==========================================
# NSPIRE Visualization Helpers
# ==========================================

#' Plot Monte Carlo Score Distribution
#'
#' Visualizes the distribution of final NSPIRE scores from a Monte Carlo simulation,
#' highlighting the 5th percentile risk tail and the HUD failure threshold (60).
#'
#' @param mc_results A \code{data.frame} output from \code{run_monte_carlo}.
#' @param title Character. The title of the plot.
#' @param failure_threshold Numeric. The score below which enforcement actions trigger (default 60).
#' @export
plot_score_distribution <- function(mc_results, title = "NSPIRE Score Distribution", failure_threshold = 60) {
  scores <- mc_results$final_score
  
  # Calculate Key Metrics
  mean_score <- mean(scores)
  p05_score <- quantile(scores, 0.05)
  fail_prob <- mean(scores < failure_threshold) * 100
  
  # Setup the plot area
  dens <- density(scores, from = max(0, min(scores)-5), to = 100)
  
  plot(dens, main = title, xlab = "Final NSPIRE Score", ylab = "Density", 
       lwd = 2, col = "darkblue", xlim = c(min(scores), 100))
  
  # Shade the failure zone (below 60)
  polygon(c(dens$x[dens$x <= failure_threshold], failure_threshold, min(dens$x)), 
          c(dens$y[dens$x <= failure_threshold], 0, 0), 
          col = rgb(1, 0, 0, 0.2), border = NA)
  
  # Add vertical lines for Mean and 5th Percentile
  abline(v = mean_score, col = "black", lwd = 2, lty = 2)
  abline(v = p05_score, col = "red", lwd = 2, lty = 2)
  
  # Add Legend
  legend("topleft", 
         legend = c(
           sprintf("Mean Score: %.1f", mean_score),
           sprintf("5th Percentile: %.1f", p05_score),
           sprintf("Failure Risk: %.1f%%", fail_prob)
         ),
         col = c("black", "red", "white"), 
         lty = c(2, 2, 0), lwd = 2, bty = "n")
}