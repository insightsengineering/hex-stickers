# Creates a Statistical Engineering Hex Sticker
#
# Requires Roche Sans fonts
# Available here: https://branding.roche.com/document/26#/typography/typography-guidelines/download
# also on gdrive: https://drive.google.com/drive/folders/1gGKZKE2Hit6eC3o8EKBwwuplN5eXQOHP
#
# Requires imagemagick, tinytex and pdftocairo tools

require(tinytex)
require(grImport2)
require(hexSticker)
require(ggplot2)
require(grid)
require(showtext)
require(magick)

# Make the Roche font available
font_dir <- "/Library/Fonts/Roche Sans" # Mac OS

font_add(
  "RocheSans",
  file.path(font_dir, "RocheSans-Regular.ttf"),
  file.path(font_dir, "RocheSans-Bold.ttf"),
  file.path(font_dir, "RocheSans-Italic.ttf"),
  file.path(font_dir, "RocheSans-BoldItalic.ttf")
)

# Create the weighted least squares equation
# Render latex, convert to Cairo SVG and import as grid object
temp_dir <- tempdir(TRUE)
tex_file_path <- file.path(temp_dir, "StatEng_eqn.tex")

cat(
  "\\documentclass[preview]{standalone}
\\begin{document}
$(X^TWX)\\hat{\\beta} = X^TWy$
\\end{document}
",
  file = tex_file_path
)
out_pdf <- tinytex::latexmk(tex_file_path)
out_svg <- file.path(temp_dir, "StatEng_eqn.svg")
system(paste("pdftocairo -svg", out_pdf, out_svg))
eqn_pic <- grImport2::readPicture(out_svg)
eqn_gtree <- grid::gTree(children = gList(pictureGrob(eqn_pic)))


# Create the "bridge"
parabola <- function(x, a = 1, b = 0, c = 0) a * x^2 + b * x + c
s1 <- seq(-0.5, 1, length = 10)
s2 <- seq(1, 0, length = 10)
s3 <- seq(0, 1, length = 10)
s4 <- seq(1, -0.5, length = 10)

bridge_data <-
  data.frame(
    x = c(seq(-5, 19, length = 10), seq(21.5, 78.5, length = 20), seq(81, 105, length = 10)),
    y = c(
      s1,
      parabola(s2),
      parabola(s3),
      s4
    ),
    w = c(seq(4, 1, length = 4), rep(1, 32), seq(1, 4, length = 4))
  )

feature_color <- "#1881C2"
bridge_fill <- "#BFDBED"

bridge_plot <- ggplot(bridge_data, aes(x = x, y = y, weight = exp(w))) +
  stat_smooth(method = "lm", formula = y ~ x, col = feature_color, fill = bridge_fill, se = TRUE, fullrange = TRUE) +
  geom_point(aes(size = w^5), show.legend = FALSE, color = feature_color, shape = 16, alpha = 0.75) +
  annotate("path", x = c(20, 20), y = c(-1, 1), linetype = 2) +
  annotate("path", x = c(80, 80), y = c(-1, 1), linetype = 2) +
  ylim(-1, 2) +
  xlim(-10, 110) +
  theme_void() +
  annotation_custom(eqn_gtree, xmin = 16, xmax = 84, ymin = -1, ymax = -0.2)


# export SVG
sticker(
  package = "Statistical\nEngineering",
  p_size = 5.5,
  p_color = "#1881C2",
  lineheight = .85,
  p_y = 1.45,
  p_family = "RocheSans",
  p_fontface = "bold",
  subplot = bridge_plot,
  s_x = 1,
  s_y = 1,
  s_width = 2,
  s_height = 1.2,
  h_fill = "#FFFFFF",
  h_color = "#1881C2",
  filename = "SVG/StatisticalEngineering.svg",
  dpi = 300
)

image_write(
  image_read_svg("SVG/StatisticalEngineering.svg", height = 1200),
  path = "PNG/StatisticalEngineering.png"
)
