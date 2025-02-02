
# WD is project source
# getwd()

# install.packages("tidyverse")
library(tidyverse)
library(gptr)

# Read in .tex papers as text
all_papers_txt <- read.delim("content/process_papers/papers.txt",
                         header = F, sep = "\n",
                         quote = "", comment.char = "%")
all_papers_txt <- paste0(all_papers_txt[[1]], collapse = "")

# Separate each paper

paper_year <- str_extract_all(all_papers_txt, "\\\\paperyear\\{([^}]*)\\}")[[1]]
paper_txt <- str_split(all_papers_txt, "\\\\paperyear\\{([^}]*)\\}")[[1]]


# Process each paper

all_papers <- data.frame(
    year = character(),
    authors = character(),
    paper = character(),
    journal = character(),
    link = character()
)

for (i in 1:length(paper_year)) {
    tst_paper_year <- paper_year[i]
    tst_paper_txt <- paper_txt[i+1]

    ## For each paper:
    # get year
    paper_yr <- str_extract(tst_paper_year, "20\\d\\d")

    # Remove the \\textbf{Avanzi, B} for easier processing
    tst_paper_txt <- str_replace(tst_paper_txt, "\\\\textbf\\{Avanzi, B.\\}", "Avanzi, B.")

    # Extract the authors, assumes that it is the first line before \\vspace{-1ex}
    authors <- str_match(tst_paper_txt, "(.*?)\\\\vspace\\{-1ex\\}")[2]
    # Extract the link, assumes paperlink is the first link that appears
    paper_link <- str_match(tst_paper_txt, "\\\\href\\{([^}]*)\\}")[2]
    # Extract paper name, assumes that it is the first \emph
    paper_name <- str_match(tst_paper_txt, "\\\\emph\\{([^}]*)\\}")[2]

    # Extract the journal, it is always underlined, assume it is the only underline
    journal_name <- str_match(tst_paper_txt, "\\\\underline\\{([^}]*)\\}")[2]

    # chatgpt summaries
    gpt_summary <- gptr::get_response(
        user_input = sprintf("Provide a 2-3 sentence summary for the following link %s",
                             paper_link),
        api_key = Sys.getenv("OPENAI_API_KEY"),
        print_response = F)


    all_papers <- rbind(all_papers,
                        data.frame(
                            year = paper_yr,
                            authors = authors,
                            paper = paper_name,
                            journal = journal_name,
                            link = paper_link
                        )
    )

}

# Save output
write.csv(all_papers, file = "content/process_papers/papers.csv", row.names = F)


# Read papers with category

papers_w_cat <- read.csv("content/process_papers/papers_w_category.csv", check.names=F)
paper_cols <- colnames(papers_w_cat)
paper_cols <- paper_cols[6:length(paper_cols)]

cat_match <- lapply(papers_w_cat[paper_cols], function(x) {match(x, "x")})
cat_lst <- list()

for (row in 1:nrow(papers_w_cat)) {

    cat_vec <- c()
    for (cat in paper_cols) {
        if (!is.na(cat_match[[cat]][row])) {
            cat_vec <- c(cat_vec, cat)
        }
    }

    cat_lst <- c(cat_lst, list(cat_vec))
}

paper_cats <- sapply(cat_lst, function(x) {paste(x, collapse = ", ")})

papers_w_cat <- papers_w_cat %>%
    mutate(
        yaml = sprintf('- path: "%s"\n  title: "%s"\n  author: "%s"\n  date: "%s"\n  categories: [%s]',
                       link, paper, authors, year, paper_cats)
        )

# Save output
write.csv(papers_w_cat, file = "content/process_papers/papers_w_yaml.csv", row.names = F)


# Save yaml output
cat(papers_w_cat$yaml, file="content/process_papers/papers.yaml", sep="\n\n")

