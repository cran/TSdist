#include <R.h>
#include<math.h> 

//Function that calculates the cost matrix of the EDR distance.
void edr(int *tamx, int *tamy, int *sigma, double *costMatrix, double *subcost){

	int i,j,tam1,tam2,min,max,siggma;;
	tam1=*tamx+1;
	tam2=*tamy+1;
	siggma=*sigma+2;

	//The (0,0) position of the matrix is filled
	costMatrix[0]=0.0;

	//The edges of the matrix are filled (only sigma+2 positions).
	for(i=1;i<siggma;i++){
	costMatrix[i*(tam2)]=i;}

	for(j=1;j<siggma;j++){
	costMatrix[j]=j;}

	//Fill all lines until i=sigma+2
	for(i=1;i<siggma;i++){
	max=fmin(i+*sigma+1,tam2);
		for(j=1;j<max;j++){
		  costMatrix[i*(tam2)+j]=fmin(fmin(1+costMatrix[(i-1)*(tam2)+j],1+costMatrix[i*(tam2)+(j-1)]),subcost[(i-1)*(*tamy)+(j-1)]+costMatrix[(i-1)*(tam2)+(j-1)]);
		}
	}

	//Fill the rest of the matrix
	for(i=(siggma);i<(tam1);i++){
	  max=fmin(i+*sigma+1,tam2);
		for(j=(i-*sigma);j<max;j++){
		  costMatrix[i*(tam2)+j]=fmin(fmin(1+costMatrix[(i-1)*(tam2)+j],1+costMatrix[i*(tam2)+(j-1)]),subcost[(i-1)*(*tamy)+(j-1)]+costMatrix[(i-1)*(tam2)+(j-1)]);
		}
	}
}



    
