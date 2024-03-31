# R markdown code for the final project functions.

---
title: "Sequence Conversion Functions"
author: "VENKATESWAR REDDY PAMIREDDY GARI"
date: "2024-03-31"
output: html_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


# Function to convert DNA to RNA

```{r}
dna_to_rna <- function(dna_sequence) {
  # Convert DNA sequence to RNA sequence
  rna_sequence <- gsub("T", "U", dna_sequence)
  return(rna_sequence)
}
```

# Example of DNA to RNA converter

```{r}
dna_sequence <- "ATCGATCGATCGATCG"
rna_sequence <- dna_to_rna(dna_sequence)
print(rna_sequence)
```

# Function to convert RNA to Protein

```{r}
rna_to_protein <- function(rna_sequence) {
  # Define codon table to give the information on how to convert RNA to Protein.
  codon_table <- list(
    UUU = "F", UUC = "F", UUA = "L", UUG = "L",
    CUU = "L", CUC = "L", CUA = "L", CUG = "L",
    AUU = "I", AUC = "I", AUA = "I", AUG = "M",
    GUU = "V", GUC = "V", GUA = "V", GUG = "V",
    UCU = "S", UCC = "S", UCA = "S", UCG = "S",
    CCU = "P", CCC = "P", CCA = "P", CCG = "P",
    ACU = "T", ACC = "T", ACA = "T", ACG = "T",
    GCU = "A", GCC = "A", GCA = "A", GCG = "A",
    UAU = "Y", UAC = "Y", UAA = "*", UAG = "*",
    CAU = "H", CAC = "H", CAA = "Q", CAG = "Q",
    AAU = "N", AAC = "N", AAA = "K", AAG = "K",
    GAU = "D", GAC = "D", GAA = "E", GAG = "E",
    UGU = "C", UGC = "C", UGA = "*", UGG = "W",
    CGU = "R", CGC = "R", CGA = "R", CGG = "R",
    AGU = "S", AGC = "S", AGA = "R", AGG = "R",
    GGU = "G", GGC = "G", GGA = "G", GGG = "G"
  )
  
  # Convert RNA sequence to uppercase
  rna_sequence <- toupper(rna_sequence)
  
  # Check if the length of the RNA sequence is divisible by 3
  if (nchar(rna_sequence) %% 3 != 0) {
    stop("RNA sequence length must be divisible by 3.")
  }
  
  # Initialize an empty vector to store the protein sequence
  protein_sequence <- character()
  
  # Iterate over the RNA sequence by codons
  for (i in seq(1, nchar(rna_sequence), by = 3)) {
    codon <- substr(rna_sequence, i, i + 2)
    if (codon %in% names(codon_table)) {
      protein_sequence <- c(protein_sequence, codon_table[[codon]])
    } else {
      protein_sequence <- c(protein_sequence, "X")  # Unknown codon represented by 'X'
    }
  }
  
  # Combine the elements of the protein sequence vector into a single string
  protein_sequence <- paste(protein_sequence, collapse = "")
  
  return(protein_sequence)
}
```

# Example of RNA to Protein converter.

```{r}
rna_sequence <- "UGGAGUGCCUGGUGGCAAAGAUGGGGGGGGGCUGUGCGCCCCAACAGGCCCACUAAAUCCUGGGUGUCUAUCCCUAAAAAUCGAGGUCCCAUGUCAUGUUUCCGGGGUGCCCUGUGCUUGUGCAACAUGGGCGCGUGGUGCGAACCAACGUGCGUGGCACCCUAGGCCUCAGUGGUAGUGGCAGGACCUGUGCUCAGUUAGAGGGUUACAAGGUAGGCGCAUCCCGGCCGACUGAGUUAAGGCGCAGGUAGCUGGGUAGAGGGUUCUCACAGGGACCUACUUGCUAAGCAGGGGACAGUCCCCUGGAUCGGCUGGGGGGGGGGGGGGGAGCGCGCAGCCUCCACCUGCCUGAGCAGGCAGGGCGGGCUGUGAGGCACCUCGAAUAGCAGCCAGAUAGAUCUUACUAAGCGCCCCUAACCUCUGCUUUGCCUGGGAUCUCCUAGCAGCCUUUGGGGGGCCACCAGGUGCACCAUGAAGGGAAGGCUGGCUUGGCCACGCCAAAUCUGGGCCCCGCGUGGAGCCCCCGCACAAUGUCUGCGCCCUUCAGCAGAAAAAGAAGCAGGGCGGGAAACGUAGCUCAAACCAGACCUGUAAACCAGAGCUCACUGAAGUGCUUCUAAGCGGCGCGCCUUUGGGACCCCACCCAGUAUGAGCAGAUAACAGUUCCCUGUCCUCAGCACUUUGAAUUUGCUUAGUUGCAGGUUCGGCCCCAGGGGCCAAUGCGAUAAUUCCGGGCUAUUAAGAAACUCUCCCCGGGUGGUUUCAGAUAUAAGAAGCCAUUGUGAUUUUUACCAAGUCAAAAUCACUUCUAAACCGGUGCACCUUGUUUCCCUCUCCCGACCUCCGCACCUCUAGGCCUCUAAAGGGCCACUACUGGCGGAAACCAGUGCACCCGGGGCUAACCUCCCGGCCCGACCAUCAAUCACCGCGCCCGCGCUCUACACUCGCUUCCCCAAGCCCUCGACCUGUAGGGUUGGAGGCUCCAAGCCGCCCGAGCGCUCUGCCCUUCCCGCUUCCUGGCCGCCCAGGCGCUUUCCCUACCACCGCGUUGUUUGCAGAGUGCACACAAAGUGGCGCGCGUUUCGGAGCGCCCAGCCCAAAGGAGCGCUGCCGCCGAGGGAGAAGCCAGCAGCCCGCUCCCGCGCUACUCCGCAGCAGCCCUGAGGACAAACUGCGAGGUAGCCGGUGCCCCGCAGUGGAAGAGCUGGGGCACGAGGGAGCGCGCCACGGGGAGCUCACCACAAGGCCGAAGGUCGGGUCCGCUGGCGCAGGAGGAUCGGAGGAGAAGGGCGGGAGAGCGGAUGAAGUGGGAGCUGCUGAAGUGGACGCUGCUGAAGAGGUCUCUGGAGAAGCCGCUGACUCACACCCCGGAGAUGAACUCCUGGACUUUGCCAGGGUCCCCAGCAUGCUCUUCCUCAUGCCUGUGCUGGAUGCCUGCCACAGAGGACACACUCAACUGGGCUGCGGUCUUGGACACCACAUAGGGACUCCACACCAGGAAGACCCAAGCAAUGCUCUAUGCCAAUCUGCAAGCCUGGGAAGAAGUCACUACCUGAACCACCAACAUGAGGAUUGCACCAAACUGAUCAACACAACCCACACCAAAAAUUCUUUCCUCAUCUGGAACCCUGCCUUUCCCUCUGGAGCUGUCUGACCUCCCCCCUUCCCCAGCAUUUGCUACCUUUGCCCCUGGACUCCUGGAAGCAUUACAGAGACUGCUGCCUCGUGUCUGCUUGGCUUACACCCCUUGUUCCCAAGAAGUGACACCUGGAGCGCACCUGAAUCCCAGAUACUGUGACUGACUAGACUUCCUAGCCUUGGCUGGUUCCUUGGCUGAGUCUUGCAUUUGCAGGAGCUCUGAUGCUCCUGUGCUAGAGAUACCUCUGUUCUGCUGUAUACCUGAAGGACACUCACCUCUGAGGAAAGACUGGGGCUGGUGGGAGGUUUCUGAGGUUUCAUUCAGUUCUGGGAGCGUGGGGAUCUCCUGGGCCACAGGCCACAGCACUUGGGCACGGGGCUGGAGUCUCGGGAUGCUCUAGCCUGUUUCUUCUCAGGUUCCGGCUCAGGCUUCAGCUCGGGCUCAGGCUCAGGCUCAGGCUUCAGCUCAGGCCUCGGUGGCAGAGGCCUCGGCUCGGGCCUUGGUGGCAGAGGCCUCGACUCGGGCCUUGGUGGCAGAGGCCUCAGCUCGGGCCUUGGUGGCAGAGGCCUCAGCUCGGGCCUCGGUGGCACGGGCCUCGGCUCAGGCCUCAGUAGCAGAGGUCUCAGCUCAGGCCUUGGUGGCACGGGCCUCGGCUCAGACCUCAGUAGCAGAGGUCUCAGCUCAGGCCUCGGUGGCAGAGGCCUCGGCUCAGGCCUAGGUGGUACAGGCCUCGGCUCAGGCCUAGGUGGUACAGGCCUUGGCUCAGGCCUAGGUGGCACAGGCCUCGGCUCAGGCUUUGGUGGCUCAGGCCUCGGCUCAGGCUUUGGUGGCUCAGGCCUCGGCUCAGGCUUUGGUGGCUCAGGCCUCGGCUCAGGCUUAGGUGGCUCAGGCCUGGGCUCAGGCUUAGGUGGCUCAGGCCUCGGCUCAGGCCUUGGUGGCACAGGCCUGGACUUAGGCUUCUGCAGCUCUGGCUGGACGAGAAGUUUAGCCUUAGGAGACUCAGAACCCUGGGUUAGGGGCCUGCAGUCGUCGUAGUCGUUCUCCUCUUCCUCCUCCUCCUCUUGCACAGUGAGCUCUGUUUUAAUUACUCGUCCCCUGAGGAGGAGAAGCAAAAAUCCUUGAGAUUAUCUCAAAAACAAAUC"
# Convert RNA to protein
rna_length <- nchar(rna_sequence)
protein_sequence <- rna_to_protein(rna_sequence)
print(rna_length)
print(protein_sequence)
```

# Function to convert Protein to CDNA

```{r}
protein_to_cdna <- function(protein_seq) {
  # Define the standard genetic code
  genetic_code <- list(
    'A' = c('GCT', 'GCC', 'GCA', 'GCG'),
    'R' = c('CGT', 'CGC', 'CGA', 'CGG', 'AGA', 'AGG'),
    'N' = c('AAT', 'AAC'),
    'D' = c('GAT', 'GAC'),
    'C' = c('TGT', 'TGC'),
    'Q' = c('CAA', 'CAG'),
    'E' = c('GAA', 'GAG'),
    'G' = c('GGT', 'GGC', 'GGA', 'GGG'),
    'H' = c('CAT', 'CAC'),
    'I' = c('ATT', 'ATC', 'ATA'),
    'L' = c('TTA', 'TTG', 'CTT', 'CTC', 'CTA', 'CTG'),
    'K' = c('AAA', 'AAG'),
    'M' = c('ATG'),
    'F' = c('TTT', 'TTC'),
    'P' = c('CCT', 'CCC', 'CCA', 'CCG'),
    'S' = c('TCT', 'TCC', 'TCA', 'TCG', 'AGT', 'AGC'),
    'T' = c('ACT', 'ACC', 'ACA', 'ACG'),
    'W' = c('TGG'),
    'Y' = c('TAT', 'TAC'),
    'V' = c('GTT', 'GTC', 'GTA', 'GTG'),
    '*' = c('TAA', 'TAG', 'TGA')
  )
  
  # Initialize an empty vector to store codons
  codon_seq <- character()
  
  # Iterate through each amino acid in the protein sequence
  for (aa in strsplit(protein_seq, '')[[1]]) {
    # Find the corresponding codons for the amino acid
    codons <- genetic_code[[aa]]
    
    # Choose a random codon (if multiple exist for the same amino acid)
    selected_codon <- sample(codons, 1)
    
    # Append the selected codon to the codon sequence
    codon_seq <- c(codon_seq, selected_codon)
  }
  
  # Concatenate codons to form cDNA sequence
  cdna_seq <- paste(codon_seq, collapse = '')
  
  return(cdna_seq)
}

```

# Example of Protein To CDNA converter 

```{r}
protein_seq <- "MVLSPADKTNVKAAWGKVGAHAGEYGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHG"
cdna_seq <- protein_to_cdna(protein_seq)
print(cdna_seq)

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Explanation for the above Final project Functions in R markdown.

This R Markdown document begins with YAML metadata, which specifies the title, author, date, and output format of the document.
title: Specifies the title of the document.
author: Specifies the author's name.
date: Specifies the date of creation or last modification.
output: Specifies the desired output format. In this case, it's set to html_document, meaning the output will be an HTML document.

Following the metadata, the R Markdown document contains several code chunks and markdown content.

Introduction: This markdown section introduces R Markdown and provides a brief description of its usage.

Function to Convert DNA to RNA: This code chunk defines a function dna_to_rna that converts DNA sequences to RNA sequences.

Example of DNA to RNA Conversion: This code chunk demonstrates how to use the dna_to_rna function by converting a DNA sequence to an RNA sequence and printing the result.

Function to Convert RNA to Protein: This code chunk defines a function rna_to_protein that converts RNA sequences to protein sequences using a codon table.

Example of RNA to Protein Conversion: This code chunk demonstrates how to use the rna_to_protein function by converting an RNA sequence to a protein sequence and printing the result.

Function to Convert Protein to cDNA: This code chunk defines a function protein_to_cdna that converts protein sequences to cDNA sequences using the standard genetic code.

Example of Protein to cDNA Conversion: This code chunk demonstrates how to use the protein_to_cdna function by converting a protein sequence to a cDNA sequence and printing the result.

Each code chunk is followed by markdown content that explains the purpose of the function or provides an example of its usage. The echo = FALSE parameter is used in the last code chunk to prevent printing of the R code that generated the output.

When you knit this R Markdown document, it will produce an HTML document as specified in the YAML metadata.

#### For more information on the R markdown, refer to my blogger account (https://venkypintu.blogspot.com/2024/03/r-markdown.html)

