# Edouard Bracquemond


#permet de générer un perceptron
Perceptron<-function(xn,wn,tr){
	
	xn<-c(1,xn)
	wn<-c(-tr,wn)
	x<-0
	x<-sum(xn*wn)
	print(x)
	if(x>0){
		o<-1
	}
	else{
		o<-0
	}

	return(o)
}
# Algorithme de calcul des poids par correction des erreurs
AlgoCorErrors<-function(s){
	NbLigne<-length(s[,1])
	NbCol<-length(s[1,])
	#génère des valeurs aléatoires pour wi..wn
	wn<-runif((NbCol),min=-1,max=1)
	
  k=1
	repeat{
	i<-1
	wnTemp=wn
	while(i<=NbLigne){
		# les x de la ligne
		xTemp<-c(1,s[i,1:(NbCol-1)])
		# la classe c de la ligne
		cTemp<-s[i,NbCol]
		sum<-0
		sum<-sum(xTemp*wn)

		if(sum>0){
		  o<-1
		}
		else{
		  o<-0
		}
		
		for(j in (1:NbCol)){
			wn[j]<-wn[j]+(cTemp-o)*xTemp[j]
		}
		
		i<-i+1
		
	}
  #k pour tester un nombre plus grand de répétitions avec le même jeu de données 
	if(k==100){
	  break
	}
	k=k+1
}
	
	return(wn)
}
#l'affichage ne prend pas en compte i>2 pouer xi // Le peu de points dans l'exemple utilisé ne permet pas toujours 
#de trouver une ligne avec une convergence optimale avec la ligne théorique, mais cette ligne sépare toujours bien les classes 0 et 1

Affichage<-function(s,wn){
  
  plot(s[1,1],s[1,2],type="p",col="blue",xlim=c(0,1),ylim=c(0,1))

  lines(s[2:4,1],s[2:4,2],type="p",col="red")
  #ligne théorique pour OR
  abline(0.5/1,-2/1,col="green",lty=2)
  #ligne obtenue
  abline(-wn[1]/wn[3],-wn[2]/wn[3],col="purple")
  
  
}
# matrice contenant les données pour le OR remplie par colonne
s=matrix(data=c(0,0,1,1,0,1,0,1,0,1,1,1),nrow=4,ncol=3)

wn=AlgoCorErrors(s)
Affichage(s,wn)

Perceptron(xn = c(0,0), wn=c(2,1), tr=0.5)	
Perceptron(xn = c(1,0), wn=c(2,1), tr=0.5)
Perceptron(xn = c(1,1), wn=c(2,1), tr=0.5)	
Perceptron(xn = c(0,1), wn=c(2,1), tr=0.5)	



