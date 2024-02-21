# Load libraries
library(stringr)
library(ggplot2)
library(readxl)

# Read the text files and split into a vector of files
filenames = "GCA_000775305.1.fna.tbl
            GCA_001718145.1.fna.tbl
            GCA_001412225.1.fna.tbl
            GCA_002217175.1.fna.tbl"
filenames = strsplit(filenames, "\\s+")[[1]]

# Create placeholders for each column in the dataframe
Species = c(); Category = c(); Percentage = c()

# Define the categories we want to extract genome percentages from
categories = c("Retroelements", "DNA transposons", "Unclassified", 
               "Other repeats")
other_categories = c("Satellites", "Simple repeats", 
                     "Low complexity", "Rolling-circles")

# For each file, parse and add the necessary info to the columns
for (file in filenames) {
  # Identify the species from the accession number in the filename
  
  accession_num = sub("\\.fna\\.tbl$", "", file)
  df = read_excel("Accession Numbers.xlsx")
  info <- df[df$Accession == accession_num, c("Order", "Family", "Genus")]
  species = paste(info$Order, info$Family, info$Genus, sep = "\n")

  # Add labels to species and category columns
  Species = c(Species, rep(species, 4))
  Category = c(Category, categories)
  
  # Read the file
  file_content = readLines(file)
  
  # Remove excess whitespace and split the file's text into lines
  file_content = gsub("^\\s+|\\s+$", "", file_content, perl = TRUE)
  lines = strsplit(file_content, "\n")
  
  # For each category, extract the genome percentage and add to Percentage column
  for (category in categories[1:3]) {
    linenum = grep(category, file_content)
    line = as.character(lines[[linenum]])
    percent = str_extract(line, "\\d+\\.\\d+")
    percent = as.numeric(percent)
    Percentage = c(Percentage, percent)
  }
  
  # Add up the "Other repeats" categories
  other_percent = 0
  for (other_cat in other_categories) {
    linenum = grep(other_cat, file_content)
    line = as.character(lines[[linenum]])
    percent = str_extract(line, "\\d+\\.\\d+")
    percent = as.numeric(percent)
    other_percent = other_percent + percent
  }
  
  Percentage = c(Percentage, other_percent)
}

# Create a df from the columns
data = data.frame(Species, Category, Percentage)
data$Percentage_label <- paste0(data$Percentage, "%")

# Create custom colors and orders of the categories
data$Category <- gsub("Unclassified", "Unclassified repeats", data$Category)
custom_order = c("Other repeats", "DNA transposons", 
                 "Retroelements", "Unclassified repeats")
custom_colors = rep(c("#1f3a4c", "#186372", "#17aeac", "#bb4135"), 
                    length(filenames))
data$Category <- factor(data$Category, levels = custom_order)

# Stacked barplot
p = ggplot(data, aes(fill = Category, y = Percentage, x = Species)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = custom_colors) + # custom fill colors
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +   # Scale y axis to 100
  labs(
    title = "Percentages of Repeats Across Hexapod Genomes",
    x = "Genera",  # x-axis label
    y = "Percentage of Genome"  # y-axis label
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

filtered_data <- data[data$Percentage > 0.1, ]
p + geom_text(data = filtered_data, aes(label = Percentage_label),
              position = position_stack(vjust = 0.5),
              color = "white",
              size = 3)