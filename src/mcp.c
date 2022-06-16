#include<Rmath.h>
#include<R.h>
#include<stdlib.h>
#include<stdio.h>

int ind(int row, int col, int nrow){
	// returns index of a matrix stored
	return nrow*col+row;
}

int aind(int row, int col, int layer, int nrow){
	// returns index of an array
	return layer*nrow*nrow + nrow*col+row;
}


void graphproc(double *h, double *a, double *G,
		double *p, int *nH, double *G1,
		int *nGraphs, int *print, int *upscale){
	int grId,i,j,rej,dm,sumrej=0,nG;
	double asum,tmp;
	dm = *nH;
	nG = *nGraphs;

	while(1){

		// pick first significant test
		rej = 0;
		for(i = 0; i < dm; i++){
			asum = 0;
			for(grId = 0; grId < nG; grId++){
				asum += a[ind(grId,i,nG)];
			}
			rej = (p[i] < asum)*(i+1);
			if(rej){
				sumrej++;
				break;
			}
		}

		if(rej){
			rej = rej - 1; // recover index in C indexing
			if(*print){
				Rprintf("---------------------------------");
				Rprintf("---------------------------------\n");
				Rprintf("Reject hypothesis %i\n", rej+1);
			}
			h[rej] = 1; // mark hypothesis as rejected
			if(sumrej == dm){ // all hypotheses rejected
				if(*print){
					Rprintf("\nAll hypotheses rejected.\n\n");
				}
				break;
			}
			for(grId = 0; grId < nG; grId++){
				for(i = 0; i < dm; i++){
					a[ind(grId,i,nG)] += a[ind(grId,rej,nG)]*G[aind(rej,i,grId,dm)]; // update local alphas
					if(sumrej < dm-1){ // update graph weights (only when there are more than 2 hypotheses left)
						for(j = 0; j < dm; j++){
							if(i == j){
								G1[aind(i,j,grId,dm)] = 0;
							} else {
								tmp = (G[aind(i,j,grId,dm)] + G[aind(i,rej,grId,dm)]*G[aind(rej,j,grId,dm)]);
                if (G[aind(i,rej,grId,dm)]*G[aind(rej,i,grId,dm)]<1) {
								  G1[aind(i,j,grId,dm)] = tmp/(1 - G[aind(i,rej,grId,dm)]*G[aind(rej,i,grId,dm)]);
                } else {
                  G1[aind(i,j,grId,dm)] = 0;
                }
							}
						}
					}
				}

				if(sumrej < dm-1){
					for(i = dm*dm*grId; i < (grId+1)*dm*dm; i++){
						G[i] = G1[i];
					}
				} else { // no hypotheses left
					for(i = dm*dm*grId; i < (grId+1)*dm*dm; i++){
						G[i] = 0;
					}
				}
				for(i = 0; i < dm; i++){
					G[aind(rej,i,grId,dm)] = 0;
					G[aind(i,rej,grId,dm)] = 0;
				}
				a[ind(grId,rej,nG)] = 0;
			}
			if(*print){
				if(nG == 1)
					Rprintf("Updated alphas and graph:\n");
				else
					Rprintf("Updated alphas and graphs:\n");
				for(grId = 0; grId < nG; grId++){
					Rprintf("\n");
					for(i = 0; i < dm; i++){
						if(nG == 1)
							Rprintf("a%i: %1.3f ", i+1, a[ind(grId, i, nG)]);
						else
							Rprintf("a%i%i: %1.3f ", grId+1, i+1, a[ind(grId, i, nG)]);
					}
					Rprintf("\n\n");
					for(i = 0; i < dm; i++){
						for(j = 0; j < dm; j++){
							if(nG == 1)
								Rprintf("G%i%i: %1.3f ", i+1, j+1, G[aind(i,j,grId,dm)]);
							else
								Rprintf("G%i,%i%i: %1.3f ", grId+1, i+1, j+1, G[aind(i,j,grId,dm)]);
						}
						Rprintf("\n");
					}
				}
				Rprintf("\n");
			}
		} else {
			break;
		}
	}
}


void graphmult(double *hmat, double *hwork,
		double *a, double *awork,
		double *G, double *Gwork, double *G1work,
		double *pmat, double *pwork,
		int *nCount, int *nH,
		int *nGraphs, int *print, int *upscale){
	int i,count;

	for(count = 0; count < *nCount; count++){
		for(i = 0; i < *nH; i++){
			pwork[i] = pmat[ind(count, i, *nCount)];
			hwork[i] = 0;
		}
		for(i = 0; i < *nGraphs* *nH; i++){
			Gwork[i] = G[i];
			G1work[i] = 0;
			awork[i] = a[i];
		}
		for(i= *nGraphs**nH; i < *nGraphs**nH**nH; i++){
			Gwork[i] = G[i];
			G1work[i] = 0;
		}
		graphproc(hwork, awork, Gwork, pwork, nH,
				G1work, nGraphs, print, upscale);
		for(i = 0; i < *nH; i++){
			hmat[ind(count, i, *nCount)] = hwork[i];
		}
	}
}

