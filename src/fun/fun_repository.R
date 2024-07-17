# misc of repository functions


# datatable 

rep_datatable_row_hover<-function(rowCallback_id){
  # rowCollback < -rep_datatable_row_hover (id)
  out=JS(paste0('function(row, data) {
                                                         $(row).mouseenter(function(){
                                                             var hover_index = $(this)[0]._DT_RowIndex
                                                             /* console.log(hover_index); */
                                                             Shiny.onInputChange("',rowCallback_id,'", hover_index);
                                                        });
                                                    }')
         )
  return(out)
}


# links

rep_link_hmdb<-function(hmdb_id){
  out=lapply(hmdb_id, function(x){
    if(!is.na(x)){paste0(tags$a(x,target="_blank",href=paste0("http://www.hmdb.ca/metabolites/",x, sep="")))}
  else{NA}
    })
    out=out %>% unlist()
  return(out)
}

rep_link_kegg<-function(kegg_id){
  out=lapply(kegg_id, function(x){
    if(!is.na(x)){paste0(tags$a(x,target="_blank",href=paste0("http://www.genome.jp/dbget-bin/www_bget?cpd:",x, sep="")))}
    else{NA}
  })
  out=out %>% unlist()
  return(out)
}

rep_link_chebi<-function(chebi_id){
  out=lapply(chebi_id, function(x){
    if(!is.na(x)){
      split <- x %>% base::strsplit(split=", ") %>% unlist()
      lapply(split, function(yy) paste0(tags$a(yy,target="_blank",href=paste0("https://www.ebi.ac.uk/chebi/searchId.do?chebiId=",yy, sep="")))) %>% unlist() %>% paste0(collapse="")
      }
    else{NA}
  })
  out=out %>% unlist()
  return(out)
}
rep_link_pubchem<-function(pubchem_id){
  out=lapply(pubchem_id, function(x){
    if(!is.na(x)){
      split <- x %>% as.character()%>% base::strsplit(split=", ") %>% unlist()
      lapply(split, function(yy) paste0(tags$a(x,target="_blank",href=paste0("https://pubchem.ncbi.nlm.nih.gov/compound/",yy, sep="")))) %>% unlist() %>% paste0(collapse="")}
    else{NA}
  })
  out=out %>% unlist()
  return(out)
}

rep_link_rm<-function(x, type="ChEBI"){
  if(type=="ChEBI"){
    clean_id <- lapply(x, function(yy){
      matches <- gregexpr("CHEBI:([0-9]+)", yy)
      out <- regmatches(yy, matches)[[1]] %>% unique() %>% paste0(collapse=";")
      out <- ifelse(out=="", NA, out)
    }) %>% unlist() 

  }else if(type=="PubChem"){
    clean_id <- lapply(x, function(yy){
      matches <- gregexpr("compound/([0-9]+)", yy)
      out <- regmatches(yy, matches)[[1]] %>% unique() %>% paste0(collapse=";")
      out <- ifelse(out=="", NA,  gsub("compound/", "", x=out))
    }) %>% unlist() 
  }else{
    out=NA
  }
  return(out)
}
