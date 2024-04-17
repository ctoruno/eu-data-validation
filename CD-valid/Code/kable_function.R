kable_function<- function(data, type){
  if (nrow(data)>0){
      if (type == "gpp"){
        colnums<- c(6:ncol(data))
      } else if (type == "tps"){
        colnums<- c(9:ncol(data))
      }
    
    knitr::kable(data, caption = "Variable List and Flags", booktabs = F, escape = F, digits=3)%>%
      kable_classic(html_font = "Arial")%>%
      kable_styling(full_width = F)%>%
      row_spec(0, bold= TRUE)%>%
      row_spec(which(data$Flag == "Red"), background = "#FC7661", extra_css = "border-bottom: 1px solid;")%>%
      row_spec(which(data$Flag == "Green"), background = "#B6F161", extra_css = "border-bottom: 1px solid;")%>%
      row_spec(which(data$Flag == "Yellow"), background = "#FFFD69", extra_css = "border-bottom: 1px solid;")%>%
      column_spec(colnums, bold= TRUE)%>%
      remove_column(3)%>%
      scroll_box(width = "1000px", height = "500px", fixed_thead = T)
  } else {
    
    data<- data%>%
      select(-Flag)
    
    knitr::kable(data, caption = "Variable List and Flags", booktabs = F, escape = F, digits=3)%>%
      kable_classic(html_font = "Arial", )%>%
      kable_styling(bootstrap_options = c("striped"), full_width = T)%>%
      row_spec(0, bold= TRUE)%>%
      scroll_box(width = "1000px", height = "500px", fixed_thead = T)
  }
  
}
