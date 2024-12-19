
# Script to move logos from this repository to the right repository
# Select the svg logos
svg_logos <- list.files("SVG", recursive = FALSE, full.names = TRUE)
# list the folder with the package repositories
path_to_dir_pkgs <- ".."

paths_pkgs <- list.dirs(path_to_dir_pkgs, recursive = FALSE)


# Find which are needed
pkg_logos <- tools::file_path_sans_ext(basename(svg_logos))
paths_pkgs <- paths_pkgs[!endsWith(paths_pkgs, ".Rcheck")]
moving_logos <- intersect(tolower(pkg_logos), basename(paths_pkgs))

logos <- file.path("..", moving_logos, "man", "figures", "logo.svg")
# Check that it has a man folder (usually packages)
exists_logo <- dir.exists(dirname(logos)) & !file.exists(logos)
new_logos <- logos[exists_logo]
origin_logos <- svg_logos[tools::file_path_sans_ext(basename(svg_logos)) %in% moving_logos][exists_logo]
# Note we assume that ordering will be the same... double check

if (length(origin_logos)) {
  file.copy(from = origin_logos, to = new_logos)
}

# remove directories with logo.png
png_logos <- file.path(path_to_dir_pkgs, moving_logos, "man", "figures", "logo.png")
rm_logos <- png_logos[file.exists(png_logos)]
if (length(rm_logos)) {
  file.remove(rm_logos)
}
