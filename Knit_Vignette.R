rmarkdown::render("pokeAPI_Vignette.Rmd", 
                 output_format = "github_document",
                 output_file = "README.md",
                 output_dir = "README_files",
                 output_options = list(
                    df_print = "default",
                    toc = TRUE,
                    number_sections = FALSE,
                    keep_html = FALSE)
)

