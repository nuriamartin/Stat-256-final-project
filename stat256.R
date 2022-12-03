
myprogram <- function() 
{    
	Football1<-read.csv('stats.csv',header=T, sep=';')
	attach(Football1)
	lm.fit<-lm(formula = wins ~ goals + clean_sheet + goals_conceded, data = Football1)	comparisontable=as.data.frame(cbind(as.character(Football1$team), lm.fit$fitted.values))
	var1 = readline("Enter 1st team ")
	marker1=0
	while(marker1==0) 
	{
		for(i in 1:length(comparisontable[,1])) 
      		if(var1== comparisontable[i,1]) 
      		{
      			marker1=1
      			varone=comparisontable[i,2]	
      		}
		if(marker1==0) print("This is not a team. Try again. ")
	}
	var2 = readline("Enter 2nd team ")
	marker2=0
	while(marker2==0) 
	{
		if(var1==var2) 
      		{
          		print("The two teams cannot be equal, try again")
		}
	else 
		{

			for(i in 1:length(comparisontable[,1])) 
      			if(var2== comparisontable[i,1]) 
      			{
      				marker2=1
      				vartwo=comparisontable[i,2]	
      			}
		}
      		if(marker2==0) 
      		{
      			print("This is not a team. Try again. ")
      		}

	}

	detach(Football1)

	if (as.numeric(as.character(varone))>as.numeric(as.character(vartwo))) 
	{
		print (paste(var1, "win"))
	}
	else if (as.numeric(as.character(varone))<as.numeric(as.character(vartwo))) 
	{
		print (paste(var2, "win"))
	} 
 	else 
	{
	print("Undetermined")
	}
}














