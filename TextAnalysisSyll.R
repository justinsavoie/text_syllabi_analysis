############################################## Code for Analysis of text analysis syllabi ######################################

##### 0 - Packages #####

library("tidyverse")
library("gmodels")
library("grid")
library("reshape2")

#### 0.2 - read data ####

DataClean <- read.csv("Data_Analysis_of_Text_Analysis_syllabi.csv")

#### 1- Dealing with Authors #### 

DataClean$author_all <- gsub(";", " & ", DataClean$author_all) # change the ; for &
DataClean$nAuthors <- unlist(lapply(strsplit(DataClean$author_all, " & "), function(x) length(x)))
DataClean$author_first <- unlist(lapply(strsplit(DataClean$author_all, " & "), `[[`, 1)) 
DataClean$author_second[DataClean$nAuthors >= 2] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 2], " & "), `[[`, 2))
DataClean$author_third[DataClean$nAuthors >= 3] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 3], " & "), `[[`, 3))
DataClean$author_forth[DataClean$nAuthors >= 4] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 4], " & "), `[[`, 4))
DataClean$author_fifth[DataClean$nAuthors >= 5] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 5], " & "), `[[`, 5))
DataClean$author_sixth[DataClean$nAuthors >= 6] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 6], " & "), `[[`, 6))
DataClean$author_seventh[DataClean$nAuthors >= 7] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 7], " & "), `[[`, 7))
DataClean$author_eighth[DataClean$nAuthors >= 8] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 8], " & "), `[[`, 8))
DataClean$author_nineth[DataClean$nAuthors >= 9] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 9], " & "), `[[`, 9))
DataClean$author_tenth[DataClean$nAuthors >= 10] <- unlist(lapply(strsplit(DataClean$author_all[DataClean$nAuthors >= 10], " & "), `[[`, 10))


#### 2 - CREATE THE CITATION ####
DataClean$citationAuthorYear[DataClean$nAuthors == 1] <- paste0("(",
                                                                unlist(lapply(strsplit(DataClean$author_first[DataClean$nAuthors == 1], ","), `[[`, 1)),
                                                                ", ",
                                                                DataClean$year[DataClean$nAuthors == 1],
                                                                ")") # originally with a - after # originally with a -
DataClean$citationAuthorYear[DataClean$nAuthors > 1] <- paste0("(",
                                                               unlist(lapply(strsplit(DataClean$author_first[DataClean$nAuthors > 1], ","), `[[`, 1)),
                                                               " et al., ",
                                                               DataClean$year[DataClean$nAuthors > 1],
                                                               ")") # originally with a - after # originally with a -

DataClean$citationAuthorYearTitle <- paste(DataClean$title, DataClean$citationAuthorYear)

#### 3 - Graph 3: Formula ####
DataClean$formula1 <- (as.numeric(scale(DataClean$nSyllabi, center = TRUE, scale = TRUE))^2) * 
  (as.numeric(scale(DataClean$nCites, center = TRUE, scale = TRUE)) +
     as.numeric(scale(DataClean$year, center = TRUE, scale = TRUE)))

DataClean$formula2 <- as.numeric(scale(DataClean$nSyllabi, center = TRUE, scale = TRUE)) + 
  as.numeric(scale(DataClean$nCites, center = TRUE, scale = TRUE)) +
  as.numeric(scale(DataClean$year, center = TRUE, scale = TRUE))

normalize0to1 <- function(x){
  y <- (x - min(x)) / (max(x) - min(x))
  return(y)
}    

DataClean$formula3 <- normalize0to1(as.numeric(scale(DataClean$nSyllabi, center = TRUE, scale = TRUE))) + 
  normalize0to1(as.numeric(scale(DataClean$nCites, center = TRUE, scale = TRUE))) +
  normalize0to1(as.numeric(scale(DataClean$year, center = TRUE, scale = TRUE)))



DataClean_Trim_three <- dplyr::filter(DataClean, DataClean$nSyllabi>=3)

## Eliminate doubles

DataClean_Trim_three <- DataClean_Trim_three[-c(65, 66), ]

## Rescale the original source index (from 0 to 2) to 0 to 1 to simplify its meaning 

DataClean_Trim_three$formula3_0_to_1 <- NA 
DataClean_Trim_three$formula3_0_to_1 <- DataClean_Trim_three$formula3/2

## Recode the category variable into a caracter variable

DataClean_Trim_three$category_new <- NA
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 1  ] <- "Theoretical questions"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 2  ] <- "Textual data"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 3  ] <- "Dictionary methods"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 4  ] <- "Supervised ideological scaling"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 5  ] <- "Supervised classification methods"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 6  ] <- "Unsupervised ideological scaling"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 7  ] <- "Unsupervised classification methods"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 8  ] <- "Statistical theory"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 9  ] <- "Deep learning / word embeddings"
DataClean_Trim_three$category_new[DataClean_Trim_three$category == 10 ] <- "Software"
table(DataClean_Trim_three$category_new)

#### 4 Graph (Of software, statistical theory, textual data and theoritical questions) with the facewrap category 1 to 6 ####

DataClean_category1_4_6_7 <- DataClean_Trim_three %>%
  filter(category %in% c(1,2,8,10))

#### 4.1 - Graph ####

DataClean_category1_4_6_7$citationAuthorYear[duplicated(DataClean_category1_4_6_7$citationAuthorYear)] <-
  paste0(DataClean_category1_4_6_7$citationAuthorYear[duplicated(DataClean_category1_4_6_7$citationAuthorYear)],"B")

DataClean_category1_4_6_7$citationAuthorYearTitle <- 
  str_wrap(DataClean_category1_4_6_7$citationAuthorYearTitle,70)

DataClean_category1_4_6_7$value <-
  scales::rescale(DataClean_category1_4_6_7$formula3_0_to_1,c(0,1))

ggplot(DataClean_category1_4_6_7, 
       aes(x=reorder(citationAuthorYear,value), y=as.numeric(value)), group=category_new) +
  geom_point(size=3,shape=1) +
  coord_flip() +
  theme_classic(base_size = 40) +
  theme_linedraw() + 
  facet_wrap(~category_new,scales = "free_y") +
  theme(axis.text.y=element_text(size = 6, face= "bold"), panel.grid = element_blank(),
        panel.grid.major.x = element_blank(), axis.title.x=element_blank(), 
        panel.grid.minor.x = element_blank(), axis.title.y=element_blank(), 
        axis.text.x=element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.background = element_rect(fill="transparent")) +
  labs(caption = "Note: The importance score scale goes from 0 (not important) to 1 (highly important).")

ggsave("F1.tiff",width=8,height=5, dpi = 300)

#### 5 Graph (On the methods in text analysis)####

DataClean_category1_4_6_7 <- DataClean_Trim_three %>%
  filter(!(category %in% c(1,2,8,10)))

DataClean_category1_4_6_7 <-
  DataClean_category1_4_6_7[-which((DataClean_category1_4_6_7$citationAuthorYear=="(Roberts et al., 2016)"))[2],]

DataClean_category1_4_6_7$citationAuthorYear[duplicated(DataClean_category1_4_6_7$citationAuthorYear)] <-
  paste0(DataClean_category1_4_6_7$citationAuthorYear[duplicated(DataClean_category1_4_6_7$citationAuthorYear)],"B")

DataClean_category1_4_6_7$citationAuthorYearTitle <- 
  str_wrap(DataClean_category1_4_6_7$citationAuthorYearTitle,70)

DataClean_category1_4_6_7$value <-
  scales::rescale(DataClean_category1_4_6_7$formula3_0_to_1,c(0,1))

DataClean_category1_4_6_7$category_new <- factor(DataClean_category1_4_6_7$category_new,
                                                 levels=c("Unsupervised ideological scaling", "Supervised ideological scaling", 
                                                          "Unsupervised classification methods", "Supervised classification methods", 
                                                          "Dictionary methods", "Deep learning / word embeddings"))
#### 6 - Graph ####

ggplot(DataClean_category1_4_6_7, 
       aes(x=reorder(citationAuthorYear,value), y=as.numeric(value)), group=category_new) +
  geom_point(size=3,shape=1) +
  coord_flip() +
  facet_wrap(~category_new,scales = "free_y", nrow=3) +
  theme_classic(base_size = 40) +
  theme_linedraw() +      
  theme(axis.text.y=element_text(size = 6, face= "bold"), panel.grid = element_blank(),
        panel.grid.major.x = element_blank(), axis.title.x=element_blank(), 
        panel.grid.minor.x = element_blank(), axis.title.y=element_blank(), 
        axis.text.x=element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        panel.background = element_rect(fill="transparent"))  +
  labs(caption = "Note: The importance score scale goes from 0 (not important) to 1 (highly important).")

ggsave("F2.tiff",width=8,height=(5*3)/2, dpi = 300)


#### 7 - Graph figure 3 (Citations - Year of publication) ####

### TIME
DataClean$citationAuthorYear_NoParenthesis <- gsub("(", "", DataClean$citationAuthorYear, fixed="TRUE")
DataClean$citationAuthorYear_NoParenthesis <- gsub(")", "", DataClean$citationAuthorYear_NoParenthesis, fixed="TRUE")

ggplot(dplyr::filter(DataClean, DataClean$nSyllabi>=2), 
       aes(y=sqrt(nCites), x=as.numeric(year))) +
  geom_text(aes(label=citationAuthorYear_NoParenthesis), check_overlap=T, size=2) +
  scale_x_continuous(name="\nYear of publication" ,limits=c(1950,2020)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text = element_text(size = 7.5)) +
  ylab("Number of citations (squared)\n")

ggsave("F3.tiff", width = 6, height = 3, dpi = 300)
