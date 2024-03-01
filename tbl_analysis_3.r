# Note on file structure: 
# To run this script, all .tbl files should be named with the same conventions
# (e.g. GCA_000775305.1.fna.tbl). Place all .tbl files in a folder named 
# tbl_files and make sure this script and the tbl_files folder share the same 
# parent folder.

# Load libraries
library(stringr)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggtext)

# Get a list of the tbl files names
filenames <- list.files("tbl_files")

# Create placeholders for each column in the dataframe
Species = c(); Category = c(); Percentage = c()

# Define the categories we want to extract genome percentages from
classifed_repeats = c("Retroelements", "DNA transposons", "Satellites", 
                      "Simple repeats", "Low complexity", "Rolling-circles")

# For each file, parse and add the necessary info to the columns
for (file in filenames) {
  # Identify the species from the accession number in the filename
  
  accession_num = sub("\\.fna\\.tbl$", "", file)
  df = read_excel("Accession Numbers.xlsx")
  info <- df[df$Accession == accession_num, c("Order", "Family", "Genus", "Species")]
  species <- paste0(info$Order, " (", info$Family, ")\n", 
                    info$Genus, " ", info$Species)
  
  # Add labels to species and category columns
  Species = c(Species, rep(species, 2))
  Category = c(Category, c("Classified repeats", "Unclassified repeats"))
  
  # Read the file
  file_content = readLines(paste0("tbl_files/",file))
  
  # Remove excess whitespace and split the file's text into lines
  file_content = gsub("^\\s+|\\s+$", "", file_content, perl = TRUE)
  lines = strsplit(file_content, "\n")

  # Sum up the classified repeats category %s & add to Percentage column
  classifed_percent = 0
  for (category in classifed_repeats) {
    linenum = grep(category, file_content)
    line = as.character(lines[[linenum]])
    percent = str_extract(line, "\\d+\\.\\d+")
    percent = as.numeric(percent)
    classifed_percent = classifed_percent + percent
  }
  Percentage = c(Percentage, classifed_percent)
  
  # Add the unclassified repeats percentage
  linenum = grep("Unclassified", file_content)
  line = as.character(lines[[linenum]])
  unclassified_percent = str_extract(line, "\\d+\\.\\d+")
  unclassified_percent = as.numeric(unclassified_percent)
  Percentage = c(Percentage, unclassified_percent)
}

# Create a df from the columns
data = data.frame(Species, Category, Percentage)

# Format the percentage labels
data$Percentage_label <- paste0(data$Percentage, "%")

# Reorder the dataframe from smallest to largest total bars
summary_data <- data %>%
  group_by(Species) %>%
  summarize(total = sum(Percentage))
species_totals <- summary_data %>%
  arrange(desc(total)) %>%
  pull(Species)
data$Species <- factor(data$Species, levels = species_totals)

# Get the axis limit by rounding up the highest % to the nearest interval of 5
axis_limit <- ceiling(max(summary_data$total) / 5) * 5

# Create custom colors 
custom_colors = rep(c("#17aeac", "#bb4135"), length(filenames))

# Stacked barplot
p = ggplot(data, aes(fill = Category, y = Percentage, x = Species)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = custom_colors) + # custom fill colors
  scale_y_continuous(limits = c(0, axis_limit)) +   # Scale axis
  labs(
    x = "Taxa",  # x-axis label
    y = "Percentage of Genome",  # y-axis label
    fill = NULL # no legend title
  ) +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.87, 0.9)) + 
  coord_flip() # Flip bars horizontally

# Add the percenatge labels to the chart
p + geom_text(aes(label = Percentage_label, angle = ifelse(Percentage < 1, 90, 0)),
              position = position_stack(vjust = 0.5),
              color = "white",
              size = 3)

