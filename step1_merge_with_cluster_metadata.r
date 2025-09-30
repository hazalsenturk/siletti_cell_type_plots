######## Cleanup the cell type analysis output and merge with cluster metadata
library(dplyr)

merge_cluster_data <- function(df1, df2, output_filename) {
  library(dplyr)
  
  standardize_cluster_column <- function(df) {
    possible_names <- c("VARIABLE", "CLUSTER", "Cluster.ID", "Cluster ID", "Cluster")
    existing <- intersect(names(df), possible_names)
    
    if (length(existing) == 0) {
      stop("No valid cluster column found in the data frame.")
    }
    
    old_name <- existing[1]
    if(old_name != "Cluster") {
      df <- df %>% rename(Cluster = !!old_name)
    }
    
    df <- df %>% 
      mutate(Cluster = as.integer(sub("Cluster", "", Cluster)))
    
    return(df)
  }
  
  df1_std <- standardize_cluster_column(df1)
  df2_std <- standardize_cluster_column(df2)
  
  df1_clean <- df1_std %>%
    select(any_of(c("Cluster", "TYPE", "NGENES", "BETA", "BETA_STD", "SE", "P")))
  
  df_merged <- df1_clean %>%
    left_join(df2_std, by = "Cluster") %>%
    arrange(Cluster)
  
  write.csv(df_merged, file = output_filename, row.names = FALSE)
  
  return(df_merged)
}

## Cluster Info File 
## Please download the Siletti et al. (2023) Supplementary Files from https://www.science.org/doi/10.1126/science.add7046#supplementary-materials and use science.add7046_table_s3.csv:
cluster_file_path <- "~/dev/science.add7046_table_s3.csv"
cluster_df <- read.csv(cluster_file_path, stringsAsFactors=FALSE)

## Sample function calls:
pheno_magma_2nd_step_output_file_path <- "Siletti_l2_contispe_PHENOTYPE.gsa.out"
pheno_magma_2nd_step_output_df <- read.table(pheno_magma_2nd_step_output_file_path,header = TRUE)

pheno_magma_2nd_step_output_df <- merge_cluster_data(pheno_magma_2nd_step_output_df,cluster_df,output_filename = "Siletti_l2_contispe_PHENOTYPE.csv")
