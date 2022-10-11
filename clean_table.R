## PLACENTA PROJECT - ANALYSIS OF ANNOTATION DATA FROM IHC 

###    1. GENERAL SETUP ----
#### 1.1. Library ----
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(tidyverse)
library(ggplot2)

#### functions ----
figure_dir <- "Output/"

save_fn <- function(name, h =15, w = 20){
  ggsave( paste(figure_dir, name, ".png", sep = ""), 
          height = h, width = w, limitsize = FALSE,
          dpi = 300, units = "cm", device = "png")
  ggsave( paste(figure_dir, name, ".pdf", sep = ""), 
          height = h, width = w, limitsize = FALSE,
          dpi = 300, units = "cm", device = "pdf")
}

#### 1.2 Work directory ----
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

#### 1.3 Input Data ----
# Annotation Raw Data
raw_text <- read.table("Label/Log.txt", sep = "\n") %>% as.data.frame() %>% 
  filter(V1 != "Coordinates: ")


### 2. Create the tables ----

#### Summarise table ----
sum_table <- raw_text %>% 
  filter(str_detect(V1, "ROI") |str_detect(V1, "Label")  )%>%
  separate(V1,c("Header", "Num"), sep = ":",remove = TRUE)  %>%
  mutate(Num = str_remove_all(Num, " "), 
         cellno = rep(1:(nrow(.)/2), each = 2, length.out = nrow(.))) %>% 
  spread(Header, Num) %>% 
  separate(Label, c("Ycor", "Xcor"), sep  = "-", remove = TRUE) %>% 
  mutate(Ycor = as.numeric(Ycor), Xcor = as.numeric(Xcor)) %>% 
  select(-`ROI Nr.`)
write.csv(sum_table, "Label/sum_table.txt", row.names = FALSE)

sum_table %>% 
  ggplot(aes(x = Xcor, y = Ycor)) + 
  #geom_point(alpha = .2)+ 
  theme_minimal() +
  geom_label(aes(x = Xcor, y = Ycor, label = cellno), size= 1) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) + scale_y_reverse()
save_fn("Mimic_plot", h = 20, w= 20)
#### Label table ----
text_vec <- raw_text %>% 
  filter(!str_detect(V1, "Label")) %>% 
  mutate(cellno = ifelse(str_detect(V1, "ROI"), 
                     str_remove(V1, "ROI Nr.:"), NA)) %>% 
  fill(cellno, .direction = "down") %>% 
  filter(!str_detect(V1, "ROI")) %>% 
  separate(V1, c("Ycor", "Xcor"), sep  = " ", remove = TRUE) %>% 
  mutate(Ycor = as.numeric(Ycor), Xcor = as.numeric(Xcor))



vect <- text_vec %>% 
  filter(Xcor >= 0, Xcor <= 2198,
         Ycor >= 0, Ycor <= 2198) %>% 
  spread(Ycor, cellno)

#  write.csv("Label/label.txt", row.names = FALSE)


text_vec %>% 
  count(Ycor, Xcor) %>% 
  mutate(n = factor(n)) %>%
  #filter(n != 1) %>% 
  ggplot(aes(x = Xcor, y = Ycor, fill =n))  +
  geom_tile() +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_reverse()
save_fn("Overlap_plot", h = 20, w = 20)  


text_vec %>% 
  count(cellno) %>% 
  ggplot(aes(x = n)) + 
  geom_histogram(bins = 100) + 
  theme_minimal() + 
  labs(x = "cell size (pixel)", y = "Count")
save_fn("Cell size dist")


text_vec %>% 
  count(cellno) %>%
  mutate(cellno = as.numeric(cellno)) %>%
  left_join(sum_table, by = "cellno") %>% 
  ggplot(aes(x = Xcor, y = Ycor, size =log(n)))  +
  geom_point(alpha = .2)  +
  scale_y_reverse() + theme_minimal()
save_fn("cell size overview", h = 20, w = 20)  
text_vec %>% 
  count(Ycor, Xcor) %>%
  write.csv("Label/pixel_count.txt", row.names = FALSE)
