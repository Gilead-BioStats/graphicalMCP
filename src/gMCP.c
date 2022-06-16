#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP cgMCP(double *oldM, double *oldW, double *p, double *a, int *n, double *s, double *m, double *w) {
    /* Since declarations in 'for' loops are only allowed in C99 mode and we can
     * only expect an ISO C99 C compiler for R > 2.12, we now declare these variables
     * in the beginning.
     * See http://cran.r-project.org/doc/manuals/R-ints.html#R-coding-standards
     */
	int i=0,j=0,k=0,l=0;
	/* s is a vector initialized with 0 - later node i is rejected <=> s[i]==1 */
	for(i=0; i<*n; i++) {
		s[i] = 0;
	}

	/* We need a temporary matrix mtemp */
	double *mtemp = (double*) R_alloc(*n**n, sizeof(double));

	/* Copying oldM to m and oldW to w, since we do not want to change the parameters in R */
	for(i=0; i<*n; i++) {
		w[i] = oldW[i];
		for(j=0; j<*n; j++) {
			m[j+*n*i] = oldM[j+*n*i];
			mtemp[j+*n*i] = oldM[j+*n*i];
		}
	}

	while (1==1) {

		/* Searching for a node that can be rejected, e.g. p[i]<=w[i]*a[0] */
		j = -1;
		for(i=0; i<*n; i++) {
			if (p[i]<=w[i]*a[0] && s[i]==0) {
				j = i;
			}
		}

		/* If there is no node that can be rejected, return: */
		if (j==-1) return R_NilValue;


		/* Otherwise reject it: */
		s[j] = 1;
		for(l=0; l<*n; l++) {
			if(s[l]==0) {
				w[l] = w[l] + w[j]*m[j + *n*l];
				for(k=0; k<*n; k++) {
					if (s[k]==0) {
						if (l!=k && m[l + *n*j]*m[j + *n*l]<1) {
							mtemp[l + *n*k] = (m[l + *n*k]+m[l + *n*j]*m[j + *n*k])/(1-m[l + *n*j]*m[j + *n*l]);
						} else {
							mtemp[l + *n*k] = 0;
						}
					}
				}
			}
		}

		/* Remove all edges from and to node j: */
		for(l=0; l<*n; l++) {
			mtemp[l+*n*j] = 0;
			mtemp[j+*n*l] = 0;
		}
		w[j] = 0;

		/* Copy mtemp to m: */
		for(i=0; i<*n; i++) {
			for(j=0; j<*n; j++) {
				m[j+*n*i] = mtemp[j+*n*i];
			}
		}

	}

	/* We do not return anything, but the last two parameters m and w have been changed (and only these two).
	 * And actually this line is never reached...
	 */
	return R_NilValue;
}
