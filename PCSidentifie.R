## Fonction identifie PCS
identifie<-function(prof) {
  prof<-Sansaccent(prof)
  li<-grep(prof, PCS2017$Profna, ignore.case=TRUE)
  i<-1
  while (length(li)==0) {
    li<-grep(substr(prof,1,length(li)-i), PCS2017$Profna, ignore.case=TRUE)
    print(li)
    i<-i+1
    if (length(prof)==i) {
      break
    }
  }
  if (length(li)==0) {
    print("Aucune correspondance trouvée, ni profession approchante.")
    prop<-999
    break
  } else {
    ## Choix de la PCS parmi les professions proposées
    for (j in li) {
      message("Profession demandée: ",prof," profession trouvée : ",PCS2017[j,2])
      rep<-readline(prompt="Validez-vous la proposition ? (o/n)")
      if (rep=="o"|rep=="O") {
        prop<-PCS2017[j,1]
        break
      } else {
        prop<-999
      }
    }
  }
  prop
}