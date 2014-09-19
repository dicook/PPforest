#include <R.h> 
#include <stdio.h> 
#include <string.h> 
#include <stdlib.h> 
#include <math.h> 
#include <time.h>


void pda(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val,double *lambda); 
void zero(double *mem, int size); 
void zero_int(int *mem, int size); 
void ludcomp(double *a,int n,int *Pivot,double *val);

void pda(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val,double *lambda) 
{
  int i,j,k,right,left; 
  int n,p,g,*pivot,*group; 

  double *mean,*ovmean;
  double *det, *cov,*tempcov; 

  n=*fn; p=*fp; g=*groups;

  mean = (double *) R_alloc(g*p, sizeof(double)); 
  zero(mean, g*p); 
  ovmean = (double *) R_alloc(p, sizeof(double)); 
  zero(ovmean, p); 
  det = (double *) R_alloc(1, sizeof(double)); 
  right = n-1; 
  left = 0;

  group = (int *) R_alloc(n, sizeof(int)); 
  zero_int(group, n); 

  if(gname[(g-1)] !=g)
  { for(i=0; i<n; i++) 
     for(j=0; j<g; j++)
        if(groupraw[i] == gname[j]) group[i] = j+1;
  }
  else
    memcpy(group,groupraw,n*sizeof(int));   
  *val = 0;

   
  for (i=0; i<n; i++) 
  {  for (j=0; j<p; j++) 
     {  mean[j+p*(group[i]-1)] += fvals[j*n+i]/(double)ngroup[group[i]-1]; 
        ovmean[j] += fvals[j*n+i]/(double)n; 
     } 
   } 

cov = (double *) R_alloc(p*p, sizeof(double));
zero(cov, p*p); 
tempcov = (double *) R_alloc(p*p, sizeof(double));
zero(tempcov,p*p); 


 for (i=0; i<n; i++) 
  { for (j=0; j<p; j++) 
    { for(k=0; k<=j; k++) 
      { cov[k*p+j] +=(1-*lambda)*((fvals[j*n+i])-(mean[j+p*(group[i]-1)]))* 
                         ((fvals[k*n+i])-(mean[k+p*(group[i]-1)])); 
        cov[j*p+k] = cov[k*p+j];
      } 
    } 
  } 

 for(j=0; j<p; j++)
   cov[j*p+j] = cov[j*p+j] +(*lambda)*n;

 /* printf("%f\n",*var);*/
 memcpy(tempcov,cov,p*p*sizeof(double));
 pivot=(int *) R_alloc(p, sizeof(int)); 
 ludcomp(tempcov,p,pivot,det); 
 *val = *det; 

  for (j=0;j<p; j++) 
  { for(k=0; k<p; k++) 
    { for (i=0; i<g; i++) 
         cov[p*j+k] += (1-*lambda)*ngroup[i]*(mean[i*p+j]-ovmean[j])* 
                       (mean[i*p+k]-ovmean[k]); 
    } 

  } 

  memcpy(tempcov,cov,p*p*sizeof(double)); 
  ludcomp(tempcov,p,pivot,det);
  /*  printf("W :%f\n",*val);*/

  /* printf("S :%f\n",*det); */
 if((*det) < 1.0e-8) {*val = 0; REprintf("ZERO VARIANCE\n");}
 else  *val = 1-*val/(*det); 
 /* printf("I :%f\n",*val); */

/*   Free(cov);
   Free(tempcov);
   Free(mean);
   Free(ovmean);
   Free(pivot);
   Free(det);
   Free(group); */

} 

