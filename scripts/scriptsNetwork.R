
##################################################################
##             GENERATE DATA PIPELINE VISUALIZATION             ##
##################################################################

packages = c(
  "network", "igraph", "ggraph", "graphlayouts", "ggforce"
)

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Define a function to recursively list all .R files
list_r_files <- function(directory) {
  list.files(directory, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
}

# Function to parse scripts for write.csv and read.csv commands
parse_scripts <- function(files) {
  dependencies <- data.frame(script = character(),
                             folder = character(),
                             type = character(),
                             filename = character(),
                             stringsAsFactors = FALSE)
  
  for (file in files) {
    script_lines <- readLines(file)
    
    # Find lines with write.csv
    written_csv <- unlist(lapply(str_match_all(script_lines, "write\\.csv\\(.*\"([^\"]+)\""), function(x) x[,2]))
    written_csv <- written_csv[!is.na(written_csv)]
    
    # Find lines with read.csv
    read_csv <- unlist(lapply(str_match_all(script_lines, "read\\.csv\\(.*\"([^\"]+)\""), function(x) x[,2]))
    read_csv <- read_csv[!is.na(read_csv)]
    
    if (length(written_csv) > 0 || length(read_csv) > 0) {
      dependencies = rbind(dependencies, 
                            expand.grid(script = basename(file),
                                       folder = basename(dirname(file)),
                                       type = "written",
                                       filename = written_csv,
                                       stringsAsFactors = FALSE)) %>% 
        rbind(., expand.grid(script = basename(file),
                            folder = basename(dirname(file)),
                            type = "read",
                            filename = read_csv,
                            stringsAsFactors = FALSE))
    }
  }
  
  return(dependencies)
}

create_dependency_graph <- function(dependencies) {
  # Create edges based on matching written and read filenames
  edges <- do.call(rbind, lapply(unique(dependencies$filename), function(csv_file) {
    writers <- dependencies$script[dependencies$type == "written" & dependencies$filename == csv_file]
    readers <- dependencies$script[dependencies$type == "read" & dependencies$filename == csv_file]
    
    if (length(writers) > 0 && length(readers) > 0) {
      return(expand.grid(from = writers, to = readers, filename = csv_file, stringsAsFactors = FALSE))
    }
    return(NULL)
  }))
  
  edges = edges %>% 
    group_by(from, to) %>%
    count() %>% 
    ungroup()
  
  vertices = dependencies %>% 
    group_by(script) %>% 
    summarise(
      name = unique(script),
      color = unique(folder)
    )
  
  g = graph_from_data_frame(edges, directed = T, vertices)
  
  g = set_edge_attr(g, "weight", value = edges$n)
  

  V(g)$degree <- degree(g, mode = "in")
  
  ggraph(g, layout="fr") +
    geom_edge_link(aes(alpha = n, end_cap = circle(node2.degree + 2, "pt")), width = 1, color = "grey30",
                   arrow = arrow(
                     angle = 20,
                     length = unit(0.1, "inches"),
                     ends = "last",
                     type = "closed"
                   )) +
    geom_node_point(aes(color = factor(color)),size = 5)+
    geom_node_text(aes(label =name), repel = T) +
    theme_void() +
    theme(legend.position ="bottom")
  
}

# Run
r_files <- list_r_files("scripts/")
dependencies <- parse_scripts(r_files)
create_dependency_graph(dependencies)
