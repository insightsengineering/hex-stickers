# Script to automatically convert files between formats

files_info <- function(path) {
  lf <- list.files(path = path, full.names = TRUE,
                   all.files = FALSE, recursive = FALSE)
  file.info(lf)
}

svg <- files_info("SVG")
png <- files_info("PNG")
ico <- files_info("ICO")
jpg <- files_info("jpg")
svg_f <- tools::file_path_sans_ext(basename(rownames(svg)))
png_f <- tools::file_path_sans_ext(basename(rownames(png)))
ico_f <- tools::file_path_sans_ext(basename(rownames(ico)))
jpg_f <- tools::file_path_sans_ext(basename(rownames(jpg)))
all_icons <- unique(c(svg_f, png_f, ico_f, jpg_f))

df <- data.frame(
  icons = all_icons,
  svg = all_icons %in% svg_f,
  ico = all_icons %in% ico_f,
  jpg = all_icons %in% jpg_f,
  png = all_icons %in% png_f)

# Incomplete cases of icons:
sum(!df$svg | !df$ico | !df$png | df$jpg)
# Complete cases of icons
sum(df$svg & df$ico & df$png & df$jpg)

df


# create images in all formats
library("magick")
# Preference of format: svg, png, jpg, ico
configs <- magick::magick_config()
formats <- c("svg", "png", "jpeg", "ico")
relevant_configs <- configs[formats]
names(relevant_configs) <- formats
relevant_configs

# filter files already present in all formats
file2convert <- df[rowSums(df[, -1]) != ncol(df) -1, ]

create_logos <- function(row) {
  file_name <- row[, "icons"]
  formats <- unlist(row[, -1, drop = TRUE], recursive = FALSE)
  formats_available <- names(formats)[formats]
  missing_formats <- setdiff(names(formats), formats_available)

  order_preference_formats <- c("svg", "jpg", "png", "ico")
  preference <- pmatch(formats_available, order_preference_formats)
  base_format <- order_preference_formats[min(preference, na.rm = TRUE)]

  input_file <- file.path(toupper(base_format),
                          paste0(file_name, ".", base_format))
  if (base_format == "svg") {
    image <- image_read_svg(path = input_file)
  } else {
    image <- image_read(input_file)

  }


  message("processing ", file_name, "\n\tConverting from ", base_format,
          " to ", paste(missing_formats, collapse = ", "))
  browser(expr = file_name == "NEST")
  for (format in missing_formats) {
    if (format == "ico") next
    new_file <- file.path(toupper(format), paste0(file_name, ".", format))
    # if (file.exists(new_file)) next
    ic <- image_convert(image, format = format, matte = TRUE)
    image_write(ic, path = new_file, format = format, depth = 8)
  }
  image_destroy(image)
}


# Be aware that one needs to check the output with the original file.
for (file in seq_len(nrow(file2convert))) {
  create_logos(file2convert[file, ])
}


