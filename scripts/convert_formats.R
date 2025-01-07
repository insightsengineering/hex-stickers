# Professional Script for Automatic Logo Conversion

library("magick")

# Function to retrieve file information from a directory
get_file_info <- function(path) {
  files <- list.files(path = path, full.names = TRUE, recursive = FALSE)
  tools::file_path_sans_ext(basename(files))
}

# Get file names (sans extensions) from each format directory
svg_files <- get_file_info("SVG")
png_files <- get_file_info("PNG")
ico_files <- get_file_info("ICO")
jpg_files <- get_file_info("jpg")

# Consolidate all unique file names
all_icons <- unique(c(svg_files, png_files, ico_files, jpg_files))

# Create a dataframe indicating the presence of each format
icons_df <- data.frame(
  icons = all_icons,
  svg = tolower(all_icons) %in% tolower(svg_files),
  png = tolower(all_icons) %in% tolower(png_files),
  ico = tolower(all_icons) %in% tolower(ico_files),
  jpg = tolower(all_icons) %in% tolower(jpg_files)
)

# Filter files that need conversion (not present in all formats less ICO)
files_to_convert <- icons_df[rowSums(icons_df[, -1]) < ncol(icons_df) - 2, ]

# Function to convert and save logos in missing formats
convert_logos <- function(row) {
  file_name <- row["icons"]
  available_formats <- names(row[-1])[unlist(row[-1])]
  missing_formats <- setdiff(c("svg", "png", "jpg", "ico"), available_formats)

  # Determine the best base format to use for conversion
  format_preference <- c("svg", "jpg", "png", "ico")
  min_match <- min(match(available_formats, format_preference), na.rm = TRUE)
  base_format <- format_preference[min_match]

  input_file <- file.path(toupper(base_format), paste0(file_name, ".", base_format))
  image <- if (base_format == "svg") {
    image_read_svg(input_file)
  } else {
    image_read(input_file)
  }

  missing_formats <- setdiff(missing_formats, "ico")
  if (length(missing_formats) == 0L) {
    invisible(return(NULL))
  }
  message("Converting ", file_name, " from ", base_format, " to ", paste(missing_formats, collapse = ", "))
  for (format in missing_formats) {
    if (format == "ico") next # Skip ICO conversion (optional handling)
    output_file <- file.path(toupper(format), paste0(file_name, ".", format))
    converted_image <- image_convert(image, format = format, matte = TRUE)
    image_write(converted_image, path = output_file, format = format, depth = 8)
  }

  image_destroy(image)
}

# Perform conversion for each incomplete file
for (r in seq_len(nrow(files_to_convert))) {
  convert_logos(files_to_convert[r, ])
}

# Move SVG logos to corresponding package directories
move_logos_to_packages <- function(path_pacakges = "..") {
  svg_logos <- list.files("SVG", full.names = TRUE, recursive = FALSE)
  package_dirs <- list.dirs(path_pacakges, recursive = FALSE)
  package_dirs <- package_dirs[!endsWith(package_dirs, ".Rcheck")]

  logo_names <- tools::file_path_sans_ext(basename(svg_logos))
  matching_packages <- intersect(tolower(logo_names), basename(package_dirs))

  destination_paths <- file.path(path_pacakges, matching_packages, "man", "figures", "logo.svg")
  origin_paths <- svg_logos[tools::file_path_sans_ext(basename(svg_logos)) %in% matching_packages]

  valid_destinations <- dir.exists(dirname(destination_paths)) & !file.exists(destination_paths)

  if (any(valid_destinations)) {
    message("Copying to", paste(destination_paths[valid_destinations], collapse = ", "))
    file.copy(from = origin_paths[valid_destinations], to = destination_paths[valid_destinations])
  }

  # Remove existing PNG logos if applicable
  png_logos <- file.path(path_pacakges, matching_packages, "man", "figures", "logo.png")
  if (any(file.exists(png_logos))) {
    message("Removing ", paste(png_logos[file.exists(png_logos)], collapse = ", "))
    file.remove(png_logos[file.exists(png_logos)])
  }
}

move_logos_to_packages()
