library(shiny)
library(data.table)
server = function(input, output, session){
  
  sites_dico=readRDS("71_sites_dico.rds")
  ccx_cleaned_labels=readRDS("70_ccx_cleaned_labels.rds")
  ccx_labels=readRDS("70_ccx_labels.rds")
  ccx_id=readRDS("70_ccx_id.rds")
  RDS_base=readRDS("base.rds")
  crosstabdf=readRDS("74_sites_pairs_crosstab.rds")
  
  
  fun_random_wine=function(p){
    index_selected_cc=unlist(lapply(ccx_cleaned_labels,function(x){
      res=any(grepl(pattern = p,x = x,ignore.case = TRUE))
      return(res)
    }))
    if (sum(index_selected_cc)==0){
      index_selected_cc=unlist(lapply(ccx_cleaned_labels,function(x){
        res=any(grepl(pattern = "henriot",x = x,ignore.case = TRUE))
        return(res)
      }))
    }
    selected_cc_cleaned_labels=ccx_cleaned_labels[index_selected_cc]
    selected_cc_id=ccx_id[index_selected_cc]
    selected_cc_labels=ccx_labels[index_selected_cc]
    
    random=sample(seq(1,length(selected_cc_cleaned_labels)),size = 1,replace = FALSE)
    selected_ids=selected_cc_id[[random]]
    selected_labels=selected_cc_labels[[random]]
    selected_cleaned_labels=min(selected_cc_cleaned_labels[[random]])
    
    selected_sites=as.numeric(gsub(pattern ="\\|.*" ,replacement = "",x = selected_ids))
    selected_sites=sites_dico$site_name[selected_sites]
    
    selected_fiches=lapply(selected_ids,function(x){
      res=RDS_base[id_global==x,c("terme","valeur"),with=FALSE]
      setkeyv(x = res,c("terme","valeur"))
      res=unique(res)
      res=setDF(res)
      return(res)
    })
    
    res=list(selected_ids,selected_labels,selected_sites,selected_fiches,selected_cleaned_labels)
    return(res)
  }
  
  
# rajouter
    fun_random_wine_r_button=function(){

    selected_cc_cleaned_labels=ccx_cleaned_labels[seq_len(length(ccx_cleaned_labels))]
    selected_cc_id=ccx_id[seq_len(length(ccx_cleaned_labels))]
    selected_cc_labels=ccx_labels[seq_len(length(ccx_cleaned_labels))]

    
    random=sample(seq(1,length(selected_cc_cleaned_labels)),size = 1,replace = FALSE)
    selected_ids=selected_cc_id[[random]]
    selected_labels=selected_cc_labels[[random]]
    selected_cleaned_labels=min(selected_cc_cleaned_labels[[random]])
    
    selected_sites=as.numeric(gsub(pattern ="\\|.*" ,replacement = "",x = selected_ids))
    selected_sites=sites_dico$site_name[selected_sites]
    
    selected_fiches=lapply(selected_ids,function(x){
      res=RDS_base[id_global==x,c("terme","valeur"),with=FALSE]
      setkeyv(x = res,c("terme","valeur"))
      res=unique(res)
      res=setDF(res)
      return(res)
    })
    
    res=list(selected_ids,selected_labels,selected_sites,selected_fiches,selected_cleaned_labels)
    return(res)
  }
  
  
    output$crosstab<-renderTable({
    crosstabdf
    },include.rownames=TRUE,digits=0)

  
    random_wines<- eventReactive(input$go,{
    res=fun_random_wine(input$caption)
    res
    })
    
    
#rajouter
    random_wines<- eventReactive(input$rand,{
    res=fun_random_wine_r_button()
    res
    })

  
  size<-reactive({
    length(random_wines()[[4]])
  })
  
  output$thew <- renderText ({random_wines()[[5]]})
  
  observe({
    
    for (i in seq_len(size())){
      local({
        my_i <- i
        
        tablename <- paste("table_", my_i, sep="")
        original_label <-paste0("original_label_",my_i)
        ident <-paste0("ident_",my_i)
        output[[tablename]] <- renderTable({random_wines()[[4]][[my_i]]})
        output[[original_label]] <- renderText({random_wines()[[2]][[my_i]]})
        output[[ident]] <- renderText({random_wines()[[1]][[my_i]]})
      })
    }
    
  })
  
  
  output$mytabs = renderUI({
    nTabs = size()
    myTabs = lapply(seq_len(nTabs), function(x){
      tabPanel(random_wines()[[3]][x],
               HTML("<br/>"),
               h5(tags$b("Original Label : "),em(textOutput(paste0("original_label_",x)))),
               HTML("<br/>"),
               h5(tags$b("Identifiant : "),em(textOutput(paste0("ident_",x)))),
               HTML("<br/>"),
               tableOutput(paste0("table_",x)))
      
    })
    do.call(tabsetPanel, myTabs)
  })
  
}
