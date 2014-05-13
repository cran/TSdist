#include <R.h>
#include<math.h> 


//Función que calcula la distancia lcss
void lcss(int *tamx, int *tamy, int *sigma, double *costMatrix, double *subcost){

	int i,j,tam1,tam2,siggma, max;
	tam1=*tamx+1;
	tam2=*tamy+1;
	siggma=*sigma+2;
	
	//Vamos rellenando la matriz de costes

	//Rellenamos la posición 0,0
	costMatrix[0]=0.0;

	//Rellenamos las esquinas
	for(i=1;i<siggma;i++){
	costMatrix[i*(*tamy+1)]=0.0;}

	for(j=1;j<siggma;j++){
	costMatrix[j]=0.0;}

	//Fill all lines until i=sigma+2
	for(i=1;i<siggma;i++){
	max=fmin(i+*sigma+1,tam2);
		for(j=1;j<max;j++){
		  if(subcost[(i-1)*(*tamy)+(j-1)]==0.0){costMatrix[i*(*tamy+1)+j]=costMatrix[(i-1)*(*tamy+1)+(j-1)]+1.0;}
		  else{costMatrix[i*(*tamy+1)+j]=fmax(costMatrix[(i-1)*(*tamy+1)+j],costMatrix[i*(*tamy+1)+(j-1)]);}
		}
	}

	//Fill the rest of the matrix
	for(i=(siggma);i<(tam1);i++){
	  max=fmin(i+*sigma+1,tam2);
		for(j=(i-*sigma);j<max;j++){
		 if(subcost[(i-1)*(*tamy)+(j-1)]==0.0){costMatrix[i*(*tamy+1)+j]=costMatrix[(i-1)*(*tamy+1)+(j-1)]+1.0;}
	 	 else{costMatrix[i*(*tamy+1)+j]=fmax(costMatrix[(i-1)*(*tamy+1)+j],costMatrix[i*(*tamy+1)+(j-1)]);}
		}
	}
}


