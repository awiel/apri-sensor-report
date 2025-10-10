# Install packages if needed
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("systemfonts", quietly = TRUE)) install.packages("systemfonts")
if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext")

library(ggplot2)
library(systemfonts)
library(showtext)

# Detect OS
os <- Sys.info()["sysname"]
cat("Operating system:", os, "\n")

# Detect default graphics device
dev <- names(dev.cur())
cat("Active graphics device:", dev, "\n")

# Check ggplot default theme font
theme_font <- theme_get()$text$family
if (theme_font == "") theme_font <- "device default"
cat("ggplot2 default font:", theme_font, "\n")

# List some available system fonts safely
fonts <- system_fonts()
cols <- intersect(c("family", "face", "face_name"), colnames(fonts))
cat("Some available system fonts:\n")
print(head(fonts[, cols], 10))

# Optional: set a cross-platform font using showtext
# Example: DejaVu Sans on Linux, Arial on Windows, Helvetica on macOS
font_to_use <- switch(os,
                      "Windows" = "Arial",
                      "Darwin"  = "Helvetica",
                      "Linux"   = "DejaVu Sans",
                      "sans")  # fallback

cat("Using font:", font_to_use, "\n")
showtext_auto()

# Example ggplot using the selected font
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = paste("Example plot on", os)) +
  theme_minimal(base_family = font_to_use, base_size = 14)
