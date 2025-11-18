#===============================================================================
# Script to plot initial SS for exploration
#-------------------------------------------------------------------------------

# Source SS ----
source("0_read_data.R")

# Create treatment means and standard deviations ----
means2024 <- SS %>% mutate(week = date) %>% 
  group_by (tn, week) %>% 
  summarise(m_ssc = mean (ssc, na.rm = TRUE),
            sd_ssc = sd (ssc, na.rm = TRUE), .groups = "keep")
means2025 <- SS %>% mutate(week = floor_date(date, unit = "week")) %>% 
  group_by (tn, week) %>% 
  summarise(m_ssc = mean (ssc, na.rm = TRUE),
            sd_ssc = sd (ssc, na.rm = TRUE), .groups = "keep")

# Change plot margins ----
par(mar = c(5, 5, 1, 1))

# plot with pane for 2024 SS ----
plot (x = SS$date[year(SS$date) == 2024 & SS$tn == 2],
      y = SS$ssc[year(SS$date) == 2024 & SS$tn == 2],
      pch = 19, lty = 1, lwd = 0.5, col = "white",
      axes = FALSE, ylim = c(0, 7),
      xlab = "", ylab = "Soluble sugar concentration (°Brix)") 
axis (side = 1, 
      at = as_date(c("2024-02-26", "2024-03-04", "2024-03-11", "2024-03-18", "2024-03-25", "2024-04-01")),
      labels = c("26 Feb", "4 Mar", "11 Mar", "18 Mar", "25 Mar", "1 Apr"))
axis (side = 2, las = 1)
for (t in unique(SS$tree)){
  con <- year(SS$date) == 2024 & SS$tree == t 
  tr <- unique(SS$tn [SS$tree == t])
  lines (x = SS$date [con],
         y = SS$ssc [con], 
         col = ifelse(tr == 2, "#1f77b4aa", ifelse (tr == 3, "#ff7f0eaa", "#7f7f7f")), 
         lwd = 0.3)
}
points (x = jitter(as.numeric(SS$date[year(SS$date) == 2024 & SS$tn == 2]), amount = 0.2),
        y = SS$ssc[year(SS$date) == 2024 & SS$tn == 2],
        pch = 19, lty = 1, lwd = 0.5, col = "#1f77b4aa")# Gravity-tapped trees
points (x = jitter(as.numeric(SS$date[year(SS$date) == 2024 & SS$tn == 3]), amount = 0.2),
        y = SS$ssc[year(SS$date) == 2024 & SS$tn == 3],
        pch = 19, lty = 1, lwd = 0.5, col = "#ff7f0eaa")# Vacuum-tapped trees
points (x = jitter(as.numeric(SS$date[year(SS$date) == 2024 & SS$tn == 4]), amount = 0.2),
        y = SS$ssc[year(SS$date) == 2024 & SS$tn == 4],
        pch = 19, lty = 1, lwd = 0.5, col = "#7f7f7faa")# Control trees

# Overlay the mean on the SS ----
dates <- as_date(c("2024-02-27", "2024-03-05", "2024-03-26", "2024-03-30", "2024-04-02"))
points(x = dates,
       y = means2024$m_ssc[year(means2024$week) == 2024 & means2024$tn == 2], 
       pch = 19,cex = 4, col = "#1f77b4cc")
points(x = dates,
       y = means2024$m_ssc[year(means2024$week) == 2024 & means2024$tn == 3], 
       pch = 19,cex = 4, col = "#ff7f04cc")
points(x = dates,
       y = means2024$m_ssc[year(means2024$week) == 2024 & means2024$tn == 4], 
       pch = 19,cex = 4, col = "#7f7f7fcc")


# plot with pane for 2025 SS ----
plot (x = SS$date[year(SS$date) == 2025 & SS$tn == 2],
      y = SS$ssc[year(SS$date) == 2025 & SS$tn == 2],
      pch = 19, lty = 1, lwd = 0.5, col = "white",
      axes = FALSE, ylim = c(0, 7),
      xlab = "", ylab = "Soluble sugar concentration (°Brix)") 
axis (side = 1, 
      at = as_date(c("2025-03-04", "2025-03-11", "2025-03-18", "2025-03-25", "2025-04-01", "2025-04-08", "2025-04-15", "2025-04-22")),
      labels = c("4 Mar", "11 Mar", "18 Mar", "25 Mar", "1 Apr", "8 Apr", "15 Apr", "23 Apr"))
axis (side = 2, las = 1)
for (t in unique(SS$tree)){
  con <- year(SS$date) == 2025 & SS$tree == t 
  tr <- unique(SS$tn [SS$tree == t])
  lines (x = SS$date [con],
         y = SS$ssc [con], 
         col = ifelse(tr == 2, "#1f77b4aa", ifelse (tr == 3, "#ff7f0eaa", "#7f7f7f")), 
         lwd = 0.3)
}
points (x = jitter(as.numeric(SS$date[year(SS$date) == 2025 & SS$tn == 2]), amount = 0.2),
        y = SS$ssc[year(SS$date) == 2025 & SS$tn == 2],
        pch = 19, lty = 1, lwd = 0.5, col = "#1f77b4aa")# Gravity-tapped trees
points (x = jitter(as.numeric(SS$date[year(SS$date) == 2025 & SS$tn == 3]), amount = 0.2),
        y = SS$ssc[year(SS$date) == 2025 & SS$tn == 3],
        pch = 19, lty = 1, lwd = 0.5, col = "#ff7f0eaa")# Vacuum-tapped trees
points (x = jitter(as.numeric(SS$date[year(SS$date) == 2025 & SS$tn == 4]), amount = 0.2),
        y = SS$ssc[year(SS$date) == 2025 & SS$tn == 4],
        pch = 19, lty = 1, lwd = 0.5, col = "#7f7f7faa")# Control trees

# Overlay the mean on the SS ----
dates <- as_date(c("2025-03-05", "2025-03-12", "2025-03-19", "2025-03-26", "2025-04-03", "2025-04-10", "2025-04-17"))
points(x = dates,
       y = means2025$m_ssc[year(means2025$week) == 2025 & means$tn == 2], 
       pch = 19,cex = 4, col = "#1f77b4cc")
points(x = dates,
       y = means2025$m_ssc[year(means2025$week) == 2025 & means$tn == 4], 
       pch = 19,cex = 4, col = "#7f7f7fcc")
dates <- as_date(c("2025-03-06", "2025-03-12", "2025-03-20", "2025-03-26", "2025-04-04", "2025-04-10", "2025-04-17"))
points(x = dates,
       y = means2025$m_ssc[year(means2025$week) == 2025 & means$tn == 3], 
       pch = 19,cex = 4, col = "#ff7f04cc")


#===============================================================================
