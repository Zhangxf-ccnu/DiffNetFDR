# ui.R
library(shiny)
library(shinyBS)
library(visNetwork)
library(DiffNetFDR)


shinyUI(navbarPage("DiffNetFDR-Shiny", id="master",
                   tabPanel("Job Setup",
                            fluidPage(
                              fluidRow(
                                column(4,
                                       fileInput("expr",
                                                 label = "Data matrix",
                                                 multiple = FALSE),
                                       bsTooltip("expr",
                                                 "Matrix of expression data, where rows correspond to genes, and columns correspond to samples",
                                                 placement = "bottom", trigger = "hover", options = NULL)
                                )),
                              
                              
                              fileInput("grouping",
                                        label = "Groups of samples",
                                        multiple = FALSE),
                              bsTooltip("grouping",
                                        "This file should contain a group classifier for each sample, in the same order as in the expression dataset",
                                        placement = "bottom", trigger = "hover", options = NULL),
                              
   
                              radioButtons("test.type", "Test type",
                                           c("partial correlation" = "pcor",
                                             "precision matrix" = "pmat"),
                                           selected = "pcor"),
                              
                              numericInput("alpha",
                                           label = "Desired FDR level (between 0 and 1)",
                                           value = 0.2, step = 0.1),
                              

                              radioButtons("parallel", "Run parallel",
                                           c("TRUE" = TRUE,
                                            "FALSE" = FALSE),
                                           selected = FALSE),
                              
                              
                              numericInput("numCore",
                                           label = "Number of Cores",
                                           value = 1, step = 1),
                              
                              actionButton("run",
                                           label = "Run DiffNetFDR",
                                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                            )),
                   
                   tabPanel("Visualize Results",
                            fluidPage(
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  numericInput("hubs",
                                               label = "Number of hubs for table",
                                               min=1, max=30, value=10),
                                  
                                  selectInput("layout", "Network layout",
                                              c("layout_with_fr" = "layout_with_fr",
                                                "layout_in_circle" = "layout_in_circle",
                                                "layout_as_star" = "layout_as_star",
                                                "layout_as_tree" = "layout_as_tree",
                                                "layout.auto" = "layout.auto",
                                                "layout_nicely" = "layout_nicely",
                                                "layout_with_dh" = "layout_with_dh",
                                                "layout_with_mds" = "layout_with_mds",
                                                "layout_on_sphere" = "layout_on_sphere",
                                                "layout_with_lgl" = "layout_with_lgl")),
                                  
                                  selectInput("nodecolor", "Node color",
                                              c("red" = "red",
                                                "yellow" = "yellow",
                                                "blue" = "blue",
                                                "black" = "black",
                                                "white" = "white",
                                                "green" = "green")),
                                  
                                  numericInput("maxnodesize",
                                               label = "Max node size",
                                               value = 10, step = 5),
                                  
                                  numericInput("minnodesize",
                                               label = "Min node size",
                                               value = 5, step = 5),
                                  
                                  numericInput("labelcex",
                                               label = "Node font size",
                                               value = 0.5, step = 0.1),
                                  
                                  selectInput("edgecolor", "Edge color",
                                              c("blue" = "blue",
                                                "red" = "red",
                                                "yellow" = "yellow",
                                                "black" = "black",
                                                "green" = "green")),
                                  
                                  numericInput("edgewidth",
                                               label = "Edge width",
                                               value = 1, step = 1),
                                  width = 2
                                ),
                                mainPanel(
                                  
                                  fluidRow(
                                    column(9,
                                           visNetworkOutput("network", width="100%",height="600px")
                                    ),
                                    
                                    column(3,
                                           textOutput("summary1"),
                                           textOutput("summary2"),
                                           br(),
                                           tableOutput("table")
                                    )
                                  )
                                )
                              )
                            )
                   ),
                   
                   navbarMenu("More",
                              tabPanel("Download Results",
                                       radioButtons("fileformat",
                                                    label = "Output File Format",
                                                    choices = c(".txt (tab-delimited text)" = "txt",
                                                                ".csv (comma-separated values)" = "csv"),
                                                    selected = "txt"),
                                       textInput("dlname",
                                                 label = "Output File Name (Do not include file extension)"),
                                       downloadButton("download",
                                                      label = "Download")
                              )
                              
                   )
)
)
