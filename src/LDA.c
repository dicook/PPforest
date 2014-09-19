#include <R.h> 
#include <stdio.h> 
#include <string.h> 
#include <stdlib.h> 
#include <math.h> 
#include <time.h>


void ludcomp(double *a,int n,int *Pivot,double *val);
void zero(double *mem, int size); 
void zero_int(int *mem, int size); 

void discriminant1(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val); 
void discriminant2(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val); 
void discriminant3(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val,int *r); 

void sort_group(double *vals,int *group,int *fp,int *fn,int left, int right) ;
void swap_group(double *vals,int *group,int *fp,int *fn,int i, int j); 
void sort_data(double *x, int *index,int left, int right) ; 
void swap_data(double *x, int *index,int i, int j); 
void countgroup(int *group, int *groups, int n); 
void countngroup(int *group, int *groups, int *ngroup, int n); 

void ludcomp(double *a,int n,int *Pivot,double *val) 
{ 
    int i,j,k,ier;
    double *s,det,temp,c=0;
    det=1;
    s = (double *) R_alloc(n, sizeof(double)); 
    for(i=0;i<n; i++)
    {  s[i] = a[i*n+1];
       for(j=1; j<n; j++)
          if(s[i] < a[i*n+j]) s[i] = a[i*n+j];
    }
    for(k=0;k<n-1; k++)
    {  for(i=k; i<n; i++)
       {   temp = fabs(a[i*n+k]/s[i]);
           if(i==k) { c = temp; Pivot[k]=i;}
           else if(c <temp) {c = temp; Pivot[k]=i;}
       }  
        /* If all elements of a row(or column) of A are zero, |A| = 0 */
    /*       if(c==0) 
       {   det=0;
           return(det);
	   } */ 
        if(Pivot[k]!=k)
       {   det*=-1; 
           for(j=k; j<n; j++)
           {   temp =a[k*n+j]; 
               a[k*n+j]=a[Pivot[k]*n+j]; 
               a[Pivot[k]*n+j]=temp;
            }       
           temp = s[k];
           s[k] = s[Pivot[k]];   
           s[Pivot[k]]=temp;
       }
       for(i=k+1; i<n; i++)
       {   temp =a[i*n+k]/a[k*n+k];
           a[i*n+k] = temp;
           for(j=k+1; j<n; j++)
              a[i*n+j] -= temp*a[k*n+j];
       }
       det *= a[k*n+k];
    }
    k = n-1;
    det *= a[(n-1)*n+(n-1)];
    ier=0; 
    *val = det;
/*    Free(s);*/

}                               


void zero(double *mem, int size) 
{   int i; 
    for(i=0; i<size; i++) 
    mem[i] =0; 
} 

void zero_int(int *mem, int size) 
{  int i; 
   for(i=0; i<size; i++) 
   mem[i] =0; 
} 

void sort_group(double *vals,int *group,int *fp,int *fn,int left, int right) 
{  int i, last; 
   if(left >= right) return; 
   swap_group(vals,group,fp,fn,left,(left+right)/2); 
   last = left; 
   for(i=left+1;i<=right; i++) 
      if(group[i] < group[left]) 
          swap_group(vals,group,fp,fn,++last,i); 
   swap_group(vals,group,fp, fn,left,last); 
   sort_group(vals,group,fp, fn,left, last-1); 
   sort_group(vals,group,fp,fn,last+1,right); 
} 

void swap_group(double *vals,int *group,int *fp,int *fn,int i, int j) 
{  int temp1,k,p,n; double temp2;
 p= *fp; n=*fn;
   temp1 = group[i]; 
   group[i] =group[j]; 
   group[j] = temp1; 
   for(k=0;k<p; k++) 
   {  temp2 = vals[k*n+i]; 
      vals[k*n+i] = vals[k*n+j]; 
      vals[k*n+j] = temp2; 
   } 
} 


void sort_data(double *x, int *index,int left, int right) 
{ int i, last; 
  if(left >= right)  return; 
  swap_data(x,index,left,(left+right)/2); 
  last = left; 
  for(i=left+1; i<=right; i++) 
    if(x[i] < x[left]) 
       swap_data(x,index,++last,i); 
  swap_data(x,index, left,last); 
  sort_data(x,index, left, last-1); 
  sort_data(x,index,last+1,right); 
} 

void swap_data(double *x, int *index,int i, int j) 
{  int temp1; double temp2; 
   temp1 = index[i]; 
   index[i] = index[j]; 
   index[j] = temp1; 
   temp2 = x[i]; 
   x[i]= x[j]; 
   x[j] = temp2; 
} 




void countgroup(int *group, int *groups, int n) 
{  int temp,i; temp = group[0]; 
   *groups=1; 
   for(i=1; i<n; i++) 
   {  if (group[i] != temp) 
      {   (*groups)++; temp = group[i];} 
   } 
} 

void countngroup(int *group, int *groups, int *ngroup, int n) 
{  int temp,i,j; 
   temp= group[0]; 
   ngroup[0] = 1; 
   j=0; 
   for(i=1; i<n; i++) 
   {  if (group[i] != temp) 
      {  temp = group[i]; j++;} 
      (ngroup[j]) ++; 
   } 
} 

void discriminant1(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val) 
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
      { cov[k*p+j] +=((fvals[j*n+i])-(mean[j+p*(group[i]-1)]))* 
                         ((fvals[k*n+i])-(mean[k+p*(group[i]-1)])); 
        cov[j*p+k] = cov[k*p+j];
      } 
    } 
  } 

 

 memcpy(tempcov,cov,p*p*sizeof(double));
 pivot=(int *) R_alloc(p, sizeof(int));
 ludcomp(tempcov,p,pivot,det); 

 *val = *det; 

  for (j=0;j<p; j++) 
  { for(k=0; k<p; k++) 
    { for (i=0; i<g; i++) 
         cov[p*j+k] += ngroup[i]*(mean[i*p+j]-ovmean[j])* 
                       (mean[i*p+k]-ovmean[k]); 
    } 

  } 

    memcpy(tempcov,cov,p*p*sizeof(double)); 
  ludcomp(tempcov,p,pivot,det);

 if((*det) < 1.0e-8) {*val = 0; REprintf("ZERO VARIANCE\n");}
 else  *val = 1 - *val / (*det); 


/*   Free(cov);
   Free(tempcov);
   Free(mean);
   Free(ovmean);
   Free(pivot);
   Free(det);
   Free(group);  */

} 




void discriminant2(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val) 
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


   group = (int *) R_alloc(n, sizeof(int)); ; 
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
      { cov[k*p+j] +=((fvals[j*n+i])-(mean[j+p*(group[i]-1)]))* 
                         ((fvals[k*n+i])-(mean[k+p*(group[i]-1)]))/(double)ngroup[group[i]-1]; 
        cov[j*p+k] = cov[k*p+j];
      } 
    } 
  } 

 

 memcpy(tempcov,cov,p*p*sizeof(double));
 pivot=(int *) R_alloc(p, sizeof(int)); 
 ludcomp(tempcov,p,pivot,det); 

 *val = *det; 

  for (j=0;j<p; j++) 
  { for(k=0; k<p; k++) 
    { for (i=0; i<g; i++) 
         cov[p*j+k] += (mean[i*p+j]-ovmean[j])* 
                       (mean[i*p+k]-ovmean[k]); 
    } 

  } 

    memcpy(tempcov,cov,p*p*sizeof(double)); 
  ludcomp(tempcov,p,pivot,det);

 if((*det) < 1.0e-8) {*val = 0; REprintf("ZERO VARIANCE\n");}
 else  *val = 1-*val/(*det); 

/*   Free(cov);
   Free(tempcov);
   Free(mean);
   Free(ovmean);
   Free(pivot);
   Free(det);
   Free(group); */

} 



void discriminant3(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val,int *r) 
{
  int i,j,right,left,l; 
  int n,p,g,rr=*r,*group; 

  double *mean,*ovmean;
  double det; 


  n=*fn; p=*fp; g=*groups;

  mean = (double *) R_alloc(g*p, sizeof(double)); 
  zero(mean, g*p); 
  ovmean = (double *) R_alloc(p, sizeof(double)); 
  zero(ovmean, p); 
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


  for (i=0; i<n; i++) 
  {  for (j=0; j<p; j++) 
     {  mean[j+p*(group[i]-1)] += fvals[j*n+i]/(double)ngroup[group[i]-1]; 
        ovmean[j] += fvals[j*n+i]/(double)n; 
     } 
   } 
  det = 0;  
    for (l=0;l<g; l++) 
      for(j=0; j<p; j++) 
      det += pow(fabs(mean[l*p+j]-ovmean[j]),rr)*ngroup[l];
    *val = pow(det,(1/(double)rr));


  det = 0;
  
    for (i=0; i<n; i++) 
     for (j=0; j<p; j++) 
       det += pow(fabs(fvals[j*n+i]-mean[j+p*(group[i]-1)]),rr); 
  
  /*
    for (i=0; i<n; i++) 
     for (j=0; j<p; j++) 
       det += pow(fabs(fvals[j*n+i]-ovmean[j]),rr); 
  */

 if((det) < 1.0e-8) {*val = 0; REprintf("ZERO VARIANCE\n");}
 else
  *val = *val / pow(det,(1/(double)rr)); 

 
/*   Free(mean);
   Free(ovmean);
   Free(group); */

} 




