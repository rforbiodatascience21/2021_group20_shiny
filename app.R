# Our app
library(shiny)
library(shinythemes)




# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "Transcription and Translation",
                  tabPanel("DNA",
                           sidebarPanel(
                             tags$h3("Input:"),
                             sliderInput("n_bases_dna","Choose a number of bases", 0, 99, 12, step = 3)
                             
                           ),
                           mainPanel(
                             h1("Result"),
                             h3("Randomly generated bases"),
                             verbatimTextOutput("raw_bases_dna"),
                             h3("Complement"),
                             verbatimTextOutput("complement_dna"),
                             h3("Translation"),
                             verbatimTextOutput("translated_dna")
                           )
                           
                  ),
                  tabPanel("RNA", 
                           sidebarPanel(
                             tags$h3("Input:"),
                             sliderInput("n_bases_rna","Choose a number of bases", 0, 99, 12, step = 3)
                           ),
                           mainPanel(
                             h1("Result"),
                             h3("Randomly generated bases"),
                             verbatimTextOutput("raw_bases_rna"),
                             h3("Translation"),
                             verbatimTextOutput("translated_rna")
                           )
                  )
                ) # navbarPage
) # fluidPage


complement <- function(dna){
  lookup <- c("A" = "T", "T" = "A", "G" = "C", "C" = "G") # named vector, think key/value
  dna_split <- strsplit(dna, "")[[1]]
  dna_complement <- paste0(lookup[dna_split], collapse = "")
  return(dna_complement)
}

random_dna <- function(l){
  nucleotides <- sample(c("A", "T", "G", "C"), size = l, replace = TRUE)
  dna = paste0(nucleotides, collapse = "")
  return(dna)
}

random_rna <- function(l){
  nucleotides <- sample(c("A", "U", "G", "C"), size = l, replace = TRUE)
  dna = paste0(nucleotides, collapse = "")
  return(dna)
}

mk_codons <- function(dna, s = 1){
  l = nchar(dna)
  codons <- substring(dna,
                      first = seq(from = s, to = l-3+1, by = 3),
                      last = seq(from = 3+s-1, to = l, by = 3))
  return(codons)
}

dna_codons_to_aa <- function(codons){
  std_code_table <- c("TTT" = "F", "TCT" = "S", "TAT" = "Y", "TGT" = "C",
                      "TTC" = "F", "TCC" = "S", "TAC" = "Y", "TGC" = "C",
                      "TTA" = "L", "TCA" = "S", "TAA" = "*", "TGA" = "*",
                      "TTG" = "L", "TCG" = "S", "TAG" = "*", "TGG" = "W",
                      "CTT" = "L", "CCT" = "P", "CAT" = "H", "CGT" = "R",
                      "CTC" = "L", "CCC" = "P", "CAC" = "H", "CGC" = "R",
                      "CTA" = "L", "CCA" = "P", "CAA" = "Q", "CGA" = "R",
                      "CTG" = "L", "CCG" = "P", "CAG" = "Q", "CGG" = "R",
                      "ATT" = "I", "ACT" = "T", "AAT" = "N", "AGT" = "S",
                      "ATC" = "I", "ACC" = "T", "AAC" = "N", "AGC" = "S",
                      "ATA" = "I", "ACA" = "T", "AAA" = "K", "AGA" = "R",
                      "ATG" = "M", "ACG" = "T", "AAG" = "K", "AGG" = "R",
                      "GTT" = "V", "GCT" = "A", "GAT" = "D", "GGT" = "G",
                      "GTC" = "V", "GCC" = "A", "GAC" = "D", "GGC" = "G",
                      "GTA" = "V", "GCA" = "A", "GAA" = "E", "GGA" = "G",
                      "GTG" = "V", "GCG" = "A", "GAG" = "E", "GGG" = "G")
  aa <- paste0(std_code_table[codons], collapse = "")
  return(aa)
}

rna_codons_to_aa <- function(codons){ 
  std_code_table <- c("UUU" = "F", "UCU" = "S",
"UAU" = "Y", "UGU" = "C", "UUC" = "F", "UCC" = "S", "UAC" = "Y", "UGC" = "C",
"UUA" = "L", "UCA" = "S", "UAA" = "*", "UGA" = "*", "UUG" = "L", "UCG" = "S",
"UAG" = "*", "UGG" = "W", "CUU" = "L", "CCU" = "P", "CAU" = "H", "CGU" = "R", 
"CUC" = "L", "CCC" = "P", "CAC" = "H", "CGC" = "R", "CUA" = "L", "CCA" = "P",
"CAA" = "Q", "CGA" = "R", "CUG" = "L", "CCG" = "P", "CAG" = "Q", "CGG" = "R",
"AUU" = "I", "ACU" = "T", "AAU" = "N", "AGU" = "S", "AUC" = "I", "ACC" = "T",
"AAC" = "N", "AGC" = "S", "AUA" = "I", "ACA" = "T", "AAA" = "K", "AGA" = "R",
"AUG" = "M", "ACG" = "T", "AAG" = "K", "AGG" = "R", "GUU" = "V", "GCU" = "A",
"GAU" = "D", "GGU" = "G", "GUC" = "V", "GCC" = "A", "GAC" = "D", "GGC" = "G",
"GUA" = "V", "GCA" = "A", "GAA" = "E", "GGA" = "G", "GUG" = "V", "GCG" = "A",
"GAG" = "E", "GGG" = "G") 
  aa <- paste0(std_code_table[codons], collapse = "")
  return(aa)
}  

# Define server function  
server <- function(input, output) {
  
  dna <- reactive({random_dna(input$n_bases_dna)})
  
  output$raw_bases_dna <- renderText({
    print(dna())
    })

  comp <- reactive({complement(dna())})
  
  output$complement_dna <- renderText({
    print(comp())
  })
  
  output$translated_dna <- renderText({
    print(dna_codons_to_aa(mk_codons(comp())))
  })
  
  
  rna <- reactive({random_rna(input$n_bases_rna)})
  
  output$raw_bases_rna <- renderText({
    print(rna())
  })
  
  output$translated_rna <- renderText({
    print(rna_codons_to_aa(mk_codons(rna())))
  })
  

} # server


# Create Shiny object
shinyApp(ui = ui, server = server)

