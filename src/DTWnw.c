
#include <R.h>
#include<math.h> 


//Function that calculates the DTW cost matrix with no temporal condition.
void dtwnw(int *tamx, int *tamy, double *costMatrix, double *distMatrix){

	int i,j,min,max;

	//Fill position (0,0) of cost matrix.
	costMatrix[0]=distMatrix[0];;

	//Fill the edges of the matrix.
	for(j=1;j<(*tamy);j++){
		costMatrix[j]=distMatrix[j]+costMatrix[j-1];
		}

	for(i=1;i<(*tamx);i++){
		costMatrix[i*(*tamy)]=distMatrix[i*(*tamy)]+costMatrix[(i-1)*(*tamy)];
		}


	//Fill the rest of the matrix.
	for(i=1;i<(*tamx);i++){
		for(j=1;j<(*tamy);j++){
		costMatrix[i*(*tamy)+j]=distMatrix[i*(*tamy)+j]+fmin(fmin(costMatrix[(i-1)*(*tamy)+j],costMatrix[i*(*tamy)+(j-1)]),costMatrix[(i-1)*(*tamy)+(j-1)]);
		}
	}
}




