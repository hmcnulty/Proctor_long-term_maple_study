#===============================================================================
# Script to plot initial SS for exploration
#-------------------------------------------------------------------------------

# Source SS ----
source("0_read_data.R")

# Change plot margins ----
par(mar = c(5, 5, 1, 1))

# Calculate the means ----
sum <- tmp %>% group_by(t, year) %>% 
  summarise(m = mean(ssc, na.rm = TRUE),
            sd = sd(ssc, na.rm = TRUE)) %>% print(n = 36)

# plot with pane for 2024 SS ----
plot (x = sum$year[sum$t == 2] - 0.1,
      y = sum$m[sum$t == 2],
      pch = 19, lty = 1, lwd = 0.5, col = "#1f77b4aa",
      axes = FALSE, ylim = c(0, 3),
      xlab = "", ylab = "Soluble sugar concentration (°Brix)") 
segments(x0 = sum$year[sum$t == 2] - 0.1,
         x1 = sum$year[sum$t == 2] - 0.1,
         y0 = sum$m[sum$t == 2] - sum$sd[sum$t == 2],
         y1 = sum$m[sum$t == 2] + sum$sd[sum$t == 2],
         col = "#1f77b4aa", lwd = 2)
axis (side = 1) 
axis (side = 2, las = 1)
points(x = sum$year [sum$t == 3] + 0.1,
       y = sum$m [sum$t == 3], 
       col = "#ff7f0eaa", pch = 19)
segments(x0 = sum$year[sum$t == 3] + 0.1,
         x1 = sum$year[sum$t == 3] + 0.1,
         y0 = sum$m[sum$t == 3] - sum$sd[sum$t == 3],
         y1 = sum$m[sum$t == 3] + sum$sd[sum$t == 3],
         col = "#ff7f0eaa", lwd = 2)

#===============================================================================