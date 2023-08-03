
# Defines site header
argonHeader <- argonDash::argonDashHeader(
  gradient = TRUE,
  color = "default",
  separator = FALSE,
  separator_color = "secondary",

  h1("bemendel", class = "text-center", style = "color: #F5AA1C;
     font-size: 50px;"),

  h4("Practice Mendelian genetics for diploid species in R", class = "text-center", style = "color: #F5AA1C;
     font-size: 20px;")

#
#   # Add a help info drop down menu to page; positioned at top-right corner
#   argonR::argonRow(center = TRUE,
#   shinyWidgets::dropdownButton(
#     inputId = "help1",
#     label = "HELP!",
#     status = "info",
#     size = "default",
#     circle = FALSE,
#     inline = TRUE,
#     margin = "10%",
#     width = "200%",
#     icon = argonR::argonIcon(name="settings", color = "yellow")))

)


# Defines site sidebar
argonSidebar <- argonDash::argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "md",
  brand_logo = "images/bemendel_logo.png",
  side = "left",

  id = "my_sidebar",

  # CSS code to make logo bigger
  tags$head(
    tags$style(
      HTML("#my_sidebar img {
              transform: scale(4);
              object-fit: cover;
              margin: 30%;
              }"
      ))),



  argonDash::argonSidebarHeader(title = "Main Menu"),
  argonDash::argonSidebarMenu(
    argonDash::argonSidebarItem(
      tabName = "welcome",
      icon = argonR::argonIcon(name = "satisfied", color = "green"),
      "Welcome"
    ),

    argonDash::argonSidebarItem(
      tabName = "cross_tab",
      icon = argonR::argonIcon(name = "cloud-upload-96", color = "info"),
      "Hybridize parents"
    ),

    argonDash::argonSidebarItem(
      tabName = "pheno_tab",
      icon = argonR::argonIcon(name = "tag", color = "green"),
      "Phenotype"
    )

  ),

  argonDash::argonSidebarDivider()

)

# Defines site footer
argonFooter <- argonDash::argonDashFooter(
  copyrights = "Copyright © 2023 Alexander Kena | Geoffrey Morris",
  src = NULL,
  argonDash::argonFooterMenu(
    argonDash::argonFooterItem("Submit issues on this Shiny app on GitHub",
                    src = "https://github.com/awkena/bemendel")
  )
)



# Welcome page
pg_welcome <- argonDash::argonTabItem(
  tabName = "welcome",

# br(),
#
#   # Some intro text that may need to be summarized into two paragraphs or less
#   argonR::argonH1("Welcome!", display = 2),
#
#   p("Welcome to bemendel: a Shiny app for practicing Mendelian genetics in R.",
#     style = "text-align: justify; margin-top: 10px;"),

  br(),

  argonR::argonH1("How to use this Shiny app", display = 4),

tags$ol(type = '1',
        tags$li("Enter the genotypes of the maternal and paternal parents."),
        tags$li("Enter letters of the English alphabets only."),
        tags$li("Use a single letter to represent each allele at a locus."),
        tags$li("DO NOT mix letters with numbers to represent an allele at
                               any locus."),
        tags$li("Inputted parental genotypes must be in multiples of
                               two letters."),
        tags$li("Click on the Cross parents button to see the gametes of
                               the parents and Punnett square."),
        tags$li("View phenotypic predictions by clicking on the View phenotypes
                button in the Phenotype tab."))

)

# Hybridize parents tab setup
pg_cross_tab <- argonDash::argonTabItem(
  tabName = "cross_tab",

  argonR::argonCard(
    width = 12,
    title = "Make genetic crosses",
    src = NULL,
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonR::argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE,
    floating = FALSE,

      argonR::argonRow(
        argonR::argonColumn(width = 6,

          shiny::textInput("Geno_m", "Enter genotype of maternal parent:",
                           value = "AaBb"),

          shiny::textInput("Geno_p", "Enter genotype of paternal parent:",
                           value = "AaBb"),

          # shiny::actionButton("Cross", "Cross gametes", icon("sync"),
          #                     class = "btn btn-success"),

           shiny::actionButton("Cross", "Cross parents", icon("times"),
                               class = "btn btn-success")
      ),

      argonR::argonColumn(width = 6,
            p(strong("NB: Enter letters of the English alphabet only. Use a single letter for
               each allele. Genotypes must be in multiples of two letters.",
               class = "text-warning"))


      ))

  ),

  argonR::argonCard(
    width = 12,
    title = "Parental Gametes",
    src = NULL,
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonR::argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE,
    floating = FALSE,

    argonR::argonRow(
      argonR::argonColumn(width = 5, offset = 1,
                          shiny::htmlOutput("gamete_m")),


      argonR::argonColumn(width = 5,
                          shiny::htmlOutput("gamete_p"))

      )

  ),

  argonR::argonCard(
    width = 12,
    title = "Punnett square",
    src = NULL,
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonR::argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE,
    floating = FALSE,

    argonR::argonRow(
      argonR::argonColumn(width = 12,
                          reactable::reactableOutput("punnett"))),
  ),

  argonR::argonCard(
    width = 12,
    title = "Progeny summaries",
    src = NULL,
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonR::argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE,
    floating = FALSE,


  argonR::argonRow(
    argonR::argonColumn(width = 6,
                        reactable::reactableOutput("progeny")),

    argonR::argonColumn(width = 6,
                        shiny::htmlOutput("bub_plot"))
    ),

  hr(),
  argonR::argonRow(
    argonR::argonColumn(width = 12,
                        bubbles::bubblesOutput(outputId = "summary")))
  )
)

# Phenotypic predictions tab setup
pg_pheno_tab <- argonDash::argonTabItem(
  tabName = "pheno_tab",

  # Mimics a classic ArgonUI card
  argonR::argonCard(
    # Card configuration
    width = 12,
    title = "Gene interaction hypothesis",
    src = NULL,
    hover_lift = FALSE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonR::argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE,
    floating = FALSE,

    # Page items
    argonR::argonRow(
      argonR::argonColumn(width = 6,
                          shiny::selectInput(inputId = "type", label = "Select gene interaction",
                                             choices = c("Independent assortment" = "IndepA",
                                                         "Dominant epistasis" = "DE",
                                                         "Recessive epistasis" = "RE",
                                                         "Duplicate dominant epistasis" = "DDE",
                                                         "Duplicate recessive epistasis" = "DRE",
                                                         "Dominant and recessive epistasis" = "DnRE"),
                                             selected = "Independent assortment"),

                          shiny::numericInput(inputId = "text_size", label = "Set text size",
                                              min = 1, max = 30, value = 12),

                          shiny::actionButton("pheno", "View phenotypes", class = "btn btn-success")),

      argonR::argonColumn(width = 6,
                          tags$p(strong("For the best display for independent assortment, the number of
                         loci should not exceed 4.", class = "text-warning")),

                         tags$p(strong("For all digenic interactions, the number of loci = 2.",
                                class = "text-warning")),

                         tags$p(strong("Generate gametes and crosses first in the Hybridize parents tab.",
                                class = "text-warning"))
      )
    )
),

# Mimics a classic ArgonUI card
argonR::argonCard(
  # Card configuration
  width = 12,
  title = "Phenotypic predictions",
  src = NULL,
  hover_lift = FALSE,
  shadow = TRUE,
  shadow_size = NULL,
  hover_shadow = FALSE,
  border_level = 0,
  icon = argonR::argonIcon("atom"),
  status = "primary",
  background_color = NULL,
  gradient = FALSE,
  floating = FALSE,

  argonR::argonRow(
    argonR::argonColumn(width = 12,
                        shiny::htmlOutput("pheno_ratio"))),
  hr(),
  argonR::argonRow(
    argonR::argonColumn(width = 12,

                        shiny::plotOutput(outputId = "comp_plot")))
  )



)


# Piece everything together in UI
ui <- function(request) {
  argonDash::argonDashPage(
  title = "bemendel",
  author = "Alexander Wireko Kena, Ebenezer Ogoe, Geoffrey Preston Morris",
  description = "Genetic crosses in R",
  sidebar = argonSidebar,
  navbar = NULL,
  header = argonHeader,
  body = argonDash::argonDashBody(

    # tags$head(
    #   tags$style(
    #     HTML(".shiny-notification {
    #           height: 100px;
    #           width: 300px;
    #           position:fixed;
    #           top: calc(25% - 30px);;
    #           left: calc(80% - 200px);;
    #         }
    #        "
    #     ))),

    shinyjs::useShinyjs(),
    argonDash::argonTabItems(
      pg_welcome,
      pg_cross_tab,
      pg_pheno_tab
      )

  ),

  footer = argonFooter

)
}

# Server component of app
server <- function(input, output, session) {

    # stop app when app is closed
  session$onSessionEnded(function() {
    stopApp()

  })

  # Function to split inputted parental genotypes into alleles
  # Input data is a string object of parental genotypes with no white spaces
  # Output is a lsit object of split alleles at each locus
  split_geno <- function(x){

    geno <- gsub(" ", "", x) # Remove white spaces

    # Get the number of loci for diploid species
    if (nchar(geno) %% 2 == 0) {

      NLoci <- nchar(geno)/2

    } else {

      stop("Number of characters for inputted genotype must be a multiple of 2")

    }

    # Split inputted genotype into individual loci
    Loci <- strsplit(geno, "(?<=.{2})", perl = TRUE)

    # Split genotypes at individual loci into alleles
    Loci.ls <- strsplit(unlist(Loci), "")

    # Rename each locus using LETTERS
    names(Loci.ls) <- paste0("Locus_", 1:NLoci)

    out <- list(Loci = Loci.ls, NLoci = NLoci)

    return(out)

  }


  # Check whether locus will segregate using this function
  # Input data is list object of split alleles for each locus
  # Output is a string object

  # is.seg <- function(x){
  #
  #   if (x[1] == x[2]) {
  #     paste("Locus", toupper(x[1]), "is homozygous, hence would not segregate.")
  #
  #   } else (paste("Locus", toupper(x[1]), "is heterozygous, hence would segregate."))
  #
  # }

  # Function to extract one allele at all loci to use as locus names
  # It converts extracted allele to the dominant version
  # x A list object containing split alleles for each locus

  first_allele <- function(x) {

    # A character vector to hold first allele at each locus
    allele <- vector(mode = "character", length = length(x))

    # Loop over list object input
    for (i in seq_len(length(x))) {

      allele[[i]] <- toupper(x[[i]][1])

    }

    return(allele)

  }

  # Function to generate parental gametes
  # input data is a list object of split alleles for each locus
  # Output is a factor object of parental gametes
  gamete <- function(x){

    gg <- expand.grid(x) # Generates allele combinations

    # Find unique gametes; output is a factor object of unique gametes
    gam <- unique(interaction(gg[1:nrow(gg),], sep = ""))

    return(gam)
  }

  # Function to order allele combinations -- Dominant allele first
  # Input data is a list object; output is a vector of strings
  order_als <- function(x) {

    if (x[[1]] == x[[2]]) {

      paste0(x[[1]], x[[1]])

    } else if (x[[1]] != x[[2]] & toupper(x[[1]]) == toupper(x[[2]])) {

      paste0(toupper(x[[1]]), tolower(x[[1]]))

    } else if (x[[1]] != x[[2]] & toupper(x[[1]]) != toupper(x[[2]])) {

      paste0(x[[1]], x[[2]])}
  }

  # Function to generate Punnett square
  do_pun <- function(female.geno, male.geno) {

    Loci.fem <-  split_geno(female.geno)$Loci # Loci and alleles in female parent
    Loci.mal <-  split_geno(male.geno)$Loci   # Loci and alleles in male parent

    # NLoci <- split_geno("AaBb")$NLoci

    fem.gam <-  gamete(Loci.fem) # Cross female gametes
    mal.gam <- gamete(Loci.mal) # Cross male gametes

    # Create an empty matrix to hold genotypes of progenies after a cross
    pun1 <- matrix(NA, nrow = length(fem.gam), ncol = length(mal.gam))
    rownames(pun1) <- fem.gam # Assign female gametes
    colnames(pun1) <- mal.gam # Assign male gametes

    for (i in rownames(pun1)) {

      for (j in colnames(pun1)) {

        # Combine female and male gametes to generate progeny genotypes and organize
        # alleles occupying each locus
        Geno <- paste0(sort(unlist(strsplit(c(i, j), ""))),
                       collapse = "")

        # Split genotypes at each locus into alleles -- a list object output
        FF <- split_geno(Geno)$Loci

        # Re-order alleles at each locus and re-paste across all loci
        KK <- paste0(sapply(FF, order_als), collapse = "")

        pun1[i, j] <- KK

      }

    }

    # Summaries of progeny genotypes in punnett square
    # Convert genotype frequencies into fractions
    Geno.freq <- rev(table(pun1))

    tab1 <- as.data.frame(Geno.freq)

    if (dim(tab1)[1] == 1 && dim(tab1)[2] == 1) {

      tab1 <- cbind(progeny = rownames(tab1), tab1)

      tab1$pop_size <- sum(tab1[,2])

      tab1$freq <- tab1[,2]/tab1[,3]

      colnames(tab1)[1:2] <- c("progeny", "nobs")
      rownames(tab1) <- NULL

    } else {

      tab1$pop_size <- sum(tab1[,2])

      tab1$freq <- tab1[,2]/tab1[,3]

      colnames(tab1)[1:2] <- c("progeny", "nobs")

    }

    tab2 <- data.frame(pun1)

    tabs <- list(punnett_square = tab2, pun_summary = tab1)

    return(tabs)

  } # End of do_pun() function


  # Function to convert genotypes in punnett table to phenotypic
  # groups
  # Input data is a list object; output is a list object

  pun2df <- function(pun){

    # Convert punnett square to a vector object and split progeny genotypes
    # into loci
    x <- strsplit(unname(unlist(as.vector(pun))), "(?<=.{2})", perl = TRUE)

    NLoci <- length(x[[1]]) # Get the number of loci

    # Loop to find the phenotypic class of each genotype
    # Assuming complete dominance at each locus and independent assortment
    for (i in seq_len(length(x))) {

      for (j in seq_len(NLoci)) {

        if (x[[i]][j] == tolower(x[[i]][j])) {

          x[[i]][j] <- x[[i]][j]

        } else {

          substr(x[[i]][j], start = 2, stop = 2) <- '_'

        }
      }

      # Re-paste split and reformatted progeny genotypes together
      x[[i]] <- paste0(x[[i]], collapse = "")

    }

    # Melt punnett square into a long format data frame for plotting using
    # ggplot2
    testdf <- reshape2::melt(as.matrix(pun), value.name = "geno")

    # Add phenotypic group for each genotype to the melted data frame
    testdf$phenotype <- as.vector(unlist(x))

    return(testdf)

  }

  # Function to check if locus is heterozygous in diploid species

  is.het <- function(x){

    if (x[1] != x[2]) {

      het <- TRUE

    } else if (x[1] == x[2]) {

      het <- FALSE

    }

    return(het)

  }


  # Function to convert phenotypic data to digenic epistatic types
  # Input data is a data frame; output is a data frame

  epist <- function(female.geno,
                    male.geno,
                    type = c('DDE', 'DRE', 'DE', 'RE', 'DnRE')){

    # Check if loci are heterozygous
    mm <- all(sapply(split_geno(female.geno)$Loci, is.het))
    pp <- all(sapply(split_geno(female.geno)$Loci, is.het))

    # Number of gene loci
    NLoci.m <- split_geno(female.geno)$NLoci
    NLoci.p <- split_geno(male.geno)$NLoci

    if (NLoci.m != 2 || NLoci.m != 2) {

      stop("Number of loci for each parent = 2")

    }

    if (mm != TRUE || pp != TRUE) {


      stop("Both parents must be dihybrids.")

    }


    pun <- do_pun(female.geno = female.geno, male.geno = male.geno)$punnett_square

    x <- pun2df(pun = pun) # convert punnett to long format data frame

    # Extract unique phenotypes under independent assortment
    uu <- sort(unique(x$phenotype), decreasing = FALSE)

    # Re-code phenotypes based on the type of epistasis

    epi <-  switch(type,
                   DE = ifelse(x$phenotype == uu[1] | x$phenotype == uu[2],
                               paste(uu[1], uu[2], sep = '||'),
                               ifelse(x$phenotype == uu[3],
                                      paste(uu[3]), paste(uu[4]))),

                   RE = ifelse(x$phenotype == uu[1], paste(uu[1]),
                               ifelse(x$phenotype == uu[3] | x$phenotype == uu[4],
                                      paste(uu[3], uu[4], sep = '||'), paste(uu[2]))),

                   DDE = ifelse(x$phenotype == uu[1] | x$phenotype == uu[2] |
                                  x$phenotype == uu[3], paste(uu[1], uu[2], uu[3], sep = '||'),
                                paste(uu[4])),

                   DRE = ifelse(x$phenotype == uu[2] | x$phenotype == uu[3] |
                                  x$phenotype == uu[4], paste(uu[2], uu[3], uu[4], sep = '||'),
                                paste(uu[1])),

                   DnRE = ifelse(x$phenotype == uu[1] | x$phenotype == uu[2] |
                                   x$phenotype == uu[4], paste(uu[1], uu[2], uu[4], sep = '||'),
                                 paste(uu[3])),

                   stop("Invalid `epistasis type` value")

    )

    x$epistasis <- epi # Add epistatic phenotypes to melted data frame
    x$epi_type <- type # Add type of epistasis

    return(x)

  }

  # Function to show phenotypes in Punnett square
  # Input data is a data frame; output is a plot

  pun_plot <- function(pun2df, txt_size = 12, epistasis = FALSE) {

    Var1 <- Var2 <- geno <- phenotype <- NULL

    testdf <- pun2df

    NLoci <- nchar(testdf$geno[1])/2 # Number of loci

    # Get genotypic phenotypic ratio from punnett square
    if (epistasis == FALSE) {
      ratio <- table(testdf$phenotype)
      ratio <- paste(ratio, collapse = ":")

      # Plot title
      title <- paste('Independent assortment at all loci',
                     paste0('(', ratio, ')'))

    } else {

      testdf$phenotype <- testdf$epistasis
      ratio <- table(testdf$phenotype)
      ratio <- paste(ratio, collapse = ":")

      type <- testdf$epi_type[1]

      title <- switch(type,
                      DE = paste('Dominant epistasis', paste0('(', ratio, ')')),

                      RE = paste('Recessive epistasis', paste0('(', ratio, ')')),

                      DDE = paste('Duplicate dominant epistasis', paste0('(', ratio, ')')),

                      DRE = paste('Duplicate recessive epistasis', paste0('(', ratio, ')')),

                      DnRE = paste('Dominant and recessive epistasis', paste0('(', ratio, ')')),

                      stop("Invalid `epistasis type` value")

      )

    }

    # testdf <- testdf[order(testdf$phenotype, decreasing = TRUE),]

    plt <- ggplot2::ggplot(testdf, ggplot2::aes(x = Var1, y = Var2,
                                                label = geno, fill = phenotype)) +
      ggplot2::theme(axis.text = ggplot2::element_text(size  = txt_size),
                     axis.title = ggplot2::element_text(size = txt_size,
                                                        face = "bold")) +
      ggplot2::labs(x = 'Female gamete', y = 'Male gamete', fill = 'Phenotype',
                    title = title,
                    subtitle = paste('This cross involved', NLoci, 'gene loci.')) +
      ggplot2::theme_classic() +
      ggplot2::theme(plot.title = ggplot2::element_text(family = "serif",
                                                        color = "grey10",
                                                        size = txt_size,
                                                        hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0,
                                                           family = "serif",
                                                           face = "bold",
                                                           color = "grey20",
                                                           size = txt_size),
                     legend.title = ggplot2::element_text(color = "black", size = txt_size),
                     legend.text = ggplot2::element_text(size = txt_size, colour = "blue4"),
                     axis.line = ggplot2::element_blank()) +
      ggplot2::geom_text(colour = "black", size = txt_size/NLoci, fontface = 'bold',
                         hjust = 0.5) +
      ggplot2::geom_tile(alpha = 0.5)

    return(plt)

  }



  # Obtain number of loci from inputted maternal genotype
  NLoci <- eventReactive (input$Cross,{

    split_geno(input$Geno_m)$NLoci

  })

  # Generate Maternal gametes
  Loci_m <- eventReactive (input$Cross,{

    split_geno(input$Geno_m)$Loci
  })


  # Maternal gametes
  gamet_m <- eventReactive (input$Cross,{

    gamete(Loci_m())

  })

  # Output maternal gametes
  observeEvent(input$Cross,{

    #req(Loci_m(), gamet_m(), input$Geno_m, cancelOutput = TRUE)

    output$gamete_m <- renderUI({

      # Check if a locus is heterozygous or homozygous
      is_het_m <- isolate({unname(unlist(sapply(Loci_m(), is.het)))})

      first_als_m <- first_allele(Loci_m())

      if (all(is_het_m == FALSE)) {

        str3 <- paste("No segregating locus.")

      } else {

        msg1 <- "The following loci are segregating: "
        msg2 <- paste0(first_als_m[is_het_m], collapse = " | ")

        str3 <- paste(msg1, msg2)

      }

      str1 <- shiny::isolate({paste(gamet_m(), collapse = ", ")})

      str2 <- shiny::isolate({if (length(gamet_m()) == 1) {

        paste("There is only one maternal gamete:")

      } else if (length(gamet_m()) > 1) {

        paste("There are", length(gamet_m()), "maternal gametes:")
      }
        })

      str4 <- shiny::isolate({htmltools::HTML(paste("<strong>Maternal Parent:</strong>"))})

       shiny::isolate({htmltools::HTML(paste(str4, paste(str3, collapse = "; "),
                                             str2, str1, sep = "<br/> <br/>"))
       })
    })
  })


  # Generate Paternal gametes
  Loci_p <- eventReactive (input$Cross,{

    split_geno(input$Geno_p)$Loci
  })

  gamet_p <- eventReactive (input$Cross,{

    gamete(Loci_p())
  })

  # Output Paternal gametes
  shiny::observeEvent(input$Cross,{
    #shiny::req(Loci_p(), gamet_p(), input$Geno_p, cancelOutput = TRUE)

    output$gamete_p <- shiny::renderUI({

      # Check if a locus is heterozygous or homozygous
      is_het_p <- shiny::isolate({unname(unlist(sapply(Loci_p(), is.het)))})

      first_als_p <- first_allele(Loci_p())

      if (all(is_het_p == FALSE)) {

        str33 <- paste("No segregating locus.")

      } else {

        msg11 <- "The following loci are segregating: "
        msg22 <- paste0(first_als_p[is_het_p], collapse = " | ")

        str33 <- paste(msg11, msg22)

      }

      str11 <- shiny::isolate({paste(gamet_p(), collapse = ", ")})

      str22 <- shiny::isolate({if (length(gamet_p()) == 1) {

        paste("There is only one paternal gamete:")

      } else if (length(gamet_p()) > 1) {

        paste("There are", length(gamet_p()), "paternal gametes:")

      }

      })

      str44 <- shiny::isolate({htmltools::HTML(paste("<strong>Paternal Parent:</strong>"))})

      shiny::isolate({htmltools::HTML(paste(str44, paste(str33,  collapse = "; "),
                                            str22, str11, sep = "<br/> <br/>"))

      })
    })
  })

  ?HTML

  # Generate punnett square

  pun1 <- shiny::eventReactive(input$Cross,{

    do_pun(female.geno = input$Geno_m, male.geno = input$Geno_p)

  })

  pun2 <- reactive({

    df1 <- cbind(Gamete = rownames(pun1()$punnett_square),
                 pun1()$punnett_square)

    rownames(df1) <- NULL

    df1

  })

  # Output punnett square
  shiny::observeEvent(input$Cross,{

    #shiny::req(input$Geno_m, input$Geno_p, cancelOutput = TRUE)

    output$punnett <- reactable::renderReactable({
      sticky_style <- list(backgroundColor = "#f7f7f7")
      shiny::isolate({reactable::reactable(pun2(), fullWidth = TRUE, defaultPageSize = 8,
                                           columns = list(Gamete = reactable::colDef(sticky = "left",
                                                                                     style = sticky_style,
                                                                                     headerStyle = sticky_style)),
                                           sortable = TRUE, bordered = TRUE, highlight = TRUE, resizable = TRUE,
                                           wrap = FALSE)
      })

    })

  })

  # Output punnett square summary
  shiny::observeEvent(input$Cross,{

    shiny::req(pun1(), input$Geno_m, input$Geno_p, cancelOutput = TRUE)

    output$summary <- bubbles::renderBubbles({

      bubbles::bubbles(value = pun1()$pun_summary[,2],
                       label = pun1()$pun_summary[,2],
                       tooltip = pun1()$pun_summary[,1],
                       color = "purple",
                       textColor = "white"
                       )

    })
  })

  # Punnett square summaries

  # Progeny summary table
  pun_sum <- shiny::eventReactive(input$Cross,{

    df <- shiny::isolate({data.frame (nLoci = NLoci(),
                     nMaternal_gametes = length(gamet_m()),
                     nPaternal_gametes = length(gamet_p()),
                     nUnique_genotypes = nrow(pun1()$pun_summary),
                     pop_size = length(gamet_p()) * length(gamet_m())
                     )
  })

    df <- t(df)

    shiny::isolate({colnames(df)[1] <- "value"})

    df
  })

  shiny::observeEvent(input$Cross,{

    shiny::req(Loci_p(), gamet_p(), cancelOutput = TRUE)

      output$progeny <- reactable::renderReactable({

        shiny::isolate({reactable::reactable(pun_sum(), fullWidth = TRUE,
                                             defaultPageSize = 5, bordered = TRUE,
                                             highlight = TRUE, rownames = TRUE,
                                             resizable = TRUE)
        })
    })
  })

  # bubble plot title
  shiny::observeEvent(input$Cross,{

    output$bub_plot <- shiny::renderUI({

    bub_msg <- paste("<strong>The bubble plot below shows the frequencies of progeny
                     genotypes. Hover over each circle to see the genotype.</strong>")

     shiny::isolate({htmltools::HTML(paste(bub_msg))})

  })

})

  # Convert genotypes in Punnett square to phenotypic groups assuming complete dominance
  # Split genotypes in Punnett square into individual loci
  # Output is a data frame object
  long_df <- eventReactive(input$pheno,{

    input$pheno

    pun2df(pun1()$punnett_square)

  })

  tt <- shiny::eventReactive(input$pheno,{

    input$pheno

    if(input$type == 'DE'|input$type == 'RE'| input$type =='DDE'|
       input$type == 'DRE'| input$type == 'DnRE'){

      shiny::isolate({epist(female.geno = input$Geno_m, male.geno = input$Geno_p ,
                            type = input$type)
      })

    }


  })

  # Get phenotypic ratio when gene interaction is independent assortment
  pr_ida <- shiny::eventReactive(input$pheno,{

    input$pheno
    paste(table(long_df()$phenotype), collapse = ':')

  })

  # Get phenotypic ratio when gene interaction is epistasis
  pr_epis <- shiny::eventReactive(input$pheno,{

    input$pheno
    paste(table(tt()$epistasis), collapse = ':')

  })

  # Color genotypes in punnett square based on phenotype
  # Assuming complete dominance at each locus
  # Easiest way is to use the ggplot2 package

  shiny::observeEvent(input$pheno,{

    shiny::req(long_df(), cancelOutput = TRUE)

    output$comp_plot <- renderPlot({

      shiny::isolate({if (input$type == 'IndepA') {

        shiny::isolate({pun_plot(pun2df = long_df(), txt_size = input$text_size,
                                 epistasis = FALSE)})

      } else {

        shiny::isolate({pun_plot(pun2df = tt(), txt_size = input$text_size,
                                 epistasis = TRUE)})

      }

      })

    })

  })

  # Output plot summaries
  shiny::observeEvent(input$pheno,{

    shiny::req(long_df(), cancelOutput = TRUE)


    output$pheno_ratio <- shiny::renderUI({

      shiny::isolate({ if(input$type == 'IndepA'){

        str7 <- shiny::isolate({paste("Assuming complete dominance, there would be",
                                      length(unique(long_df()$phenotype)), "phenotypic classes.")})

        str8 <- shiny::isolate({paste("The phenotypic ratio is", pr_ida())})

        shiny::isolate({htmltools::HTML(paste(str7, str8,  sep = "<br/> <br/>"))})

      } else if(input$type == 'DE'|input$type == 'RE'| input$type =='DDE' |
                input$type == 'DRE'| input$type == 'DnRE'){

        str7 <- shiny::isolate({paste("There would be",
                                      length(unique(tt()$epistasis)), "phenotypic classes.")})

        str8 <- shiny::isolate({paste("The digenic epistatic ratio is", pr_epis())})

        shiny::isolate({HTML(paste(str7, str8,  sep = "<br/> <br/>"))})

      }
      })

    })

  })


}

shinyApp(ui, server)
