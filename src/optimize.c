#include <stdio.h> 
#include <string.h> 
#include <stdlib.h> 
#include <math.h> 
#include <R.h>
#include <Rmath.h>

void zero(double *mem, int size); 
void zero_int(int *mem, int size); 

void discriminant1(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val); 
void discriminant2(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val); 
void discriminant3(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val,int *r); 
void pda(int *fn, int *fp, int *groups,double *fvals,int *groupraw,int *gname, int *ngroup,double *val,double *lambda); 
void cartgini(int *fn, int *fp, int *groups,double *fvals,int *group, int *ngroup,double *val); 
void cartentropy(int *fn, int *fp, int *groups,double *fvals,int *group, int *ngroup,double *val); 

void optimize1(int *fn, int *fp, int *groups, double *fvals, int *groupraw,int *gname,
	     int *ngroup, int *method, double *cooling, double *temp_start,
	      int *projdim, double *val, double *proj,int *r,double *lambda);
void optimize2(int *fn, int *fp, int *groups, double *fvals, int *groupraw,int *gname,
	     int *ngroup, int *method, double *cooling, double *temp_start,
	      int *projdim, double *val, double *proj,double *energy, int *r,double *lambda);

void optimize3(int *fn, int *fp, int *groups, double *fvals, int *groupraw, int *gname,
	     int *ngroup, int *method, double *cooling, double *temp_start,
	      int *projdim, double *val, double *proj, int *r,double *lambda);
void iszero (int *fn, int *fp,double *vals,double *sum);

void normal_fill1 (int *fn, int *fp,double *datavals, double delta, double *basevals);
void orthonormal (double *projvals, int *fn, int *fp);
void initrandom(float start);
float uniformrandom();
float normalrandom();
void normal_fill (int *fn, int *fp,double *fvals, double delta, double *base);
int randomval=0;
int start=0;
int nset=0;
int nrand=0;

void initrandom(float start)
{ 
  randomval = floor (fmod (fabs(start), 62748517.0));
  nset   = 0;
}

float uniformrandom()
{ 
  randomval = fmod (27132.0 * randomval + 7.0, 62748517.0);
  return (randomval / 62748517.0);
}

float normalrandom()
{ 
  float x, y, r;
  if (nset) { nset = 0; return(nrand); }
  do
    { x = 2.0*uniformrandom()-1.0;
    y = 2.0*uniformrandom()-1.0; 
    r = x*x+y*y;
  }
  while (r>=1.0);
  r = sqrt(-2.0*log(r)/r);
  nrand = x*r;
  nset  = 1;
  return(y*r);
}

void normal_fill (int *fn, int *fp,double *fvals, double delta, double *base)
{ 
  int i, j;
  int n,p;
  n= *fn; p=*fp;
  for (i=0; i<n; i++)
  { for (j=0; j<p; j++)
      fvals[j*n+i] = base[j*n+i]+delta*normalrandom();
  }
}


void iszero (int *fn, int *fp,double *vals,double *sum)
{ 
  int i, j,n,p;
  n=*fn; p=*fp;
  *sum = 0;
  for (i=0; i<n; i++)
  { for (j=0; j<p; j++)
     *sum += fabs(vals[j*n+i]);
   }
}




void normal_fill1 (int *fn, int *fp,double *datavals, double delta, double *basevals)
{ 
  int i, j,n,p;
  n= *fn; p=*fp;
  for (i=0;i<n; i++)
  { for (j=0; j<p; j++)
       datavals[j*n+i] = basevals[j*n+i]+delta*norm_rand();
  }

}


void orthonormal (double *projvals, int *fn, int *fp)
{ 
  int i,j, k,p,n;
  float *ip, norm;
  p=*fp; n=*fn;
  ip = (float *) R_alloc(p, sizeof(float)); 
  /*  
  for (i=0; i<p; i++)
  { 
  */
    i=0;
    norm = 0.0;
    for (k=0; k<n; k++)
      norm += (projvals[i*n+k]*projvals[i*n+k]);
    norm = sqrt(norm);
    for (k=0; k<n; k++)
      projvals[i*n+k] /= norm;
    /*
  }
    */

  for (i=1; i<p;i++)
  { 

    for (j=0; j<i; j++)
    { ip[j] = 0;
      for(k=0; k<n; k++)
        ip[j] +=projvals[j*n+k]*projvals[i*n+k];
    }

    for (j=0; j<i; j++)
    { for (k=0;k<n; k++)
        projvals[i*n+k] -=ip[j]*projvals[j*n+k];
    }

    norm = 0.0;
    for (k=0; k<n; k++)
      norm += (projvals[i*n+k]*projvals[i*n+k]);   
    norm = sqrt(norm);
    for (k=0; k<n; k++)     
       projvals[i*n+k] /= norm;
  }
/*  Free(ip);*/
}

/*

void orthonormal (double *projvals, int *fn, int *fp)
{ 
  int i,j, k,p,n;
  float *ip, norm;
  p=*fp; n=*fn;
  ip = (float *) R_alloc(p, sizeof(float)); 
  for (i=0; i<p;i++)
  { 
    for (j=0; j<i; j++)
      { ip[j] = 0; 
      for(k=0; k<n; k++)
         ip[j] +=projvals[j*n+k]*projvals[i*n+k];
      for (k=0;k<n; k++)
        projvals[i*n+k] -=ip[j]*projvals[j*n+k];
      }
    norm = 0.0;
    for (k=0; k<n; k++)
      norm += (projvals[i*n+k]*projvals[i*n+k]);   
   for (k=0; k<n; k++)     
     projvals[i*n+k] /= sqrt(norm);
  }
  Free(ip);
}

*/

/* Optimization in GGobi */

void optimize1(int *fn, int *fp, int *groups, double *fvals, int *groupraw,int *gname,
	     int *ngroup, int *method, double *cooling, double *temp_start,
             int *projdim, double *val, double *proj,int *r,double *lambda)
{

  int n,p,g,projn,projp,maxproj,i,j,m,kt,*group;
  double *proj_best,*proj_work, *projdata,*index_work, *index_best;
  double temp_end,temp,cool,*sum;

  n = *fn; p = *fp; g = *groups;
  projn = p;
  projp = *projdim;
  temp = *temp_start;
  cool = *cooling;
   temp_end = 0.001; 
  maxproj=1000;



   group = (int *) R_alloc(n, sizeof(int)); 
  zero_int(group, n); 

  if(gname[(g-1)] !=g)
  { for(i=0; i<n; i++)
      for(j=0; j<g; j++)
        if(groupraw[i] == gname[j]) group[i] = j+1;
  }
  else
    memcpy(group,groupraw,n*sizeof(int));

  /********************************/
  
  proj_best = (double *) R_alloc(projp*projn, sizeof(double)); 
  zero(proj_best,projp*projn);
  proj_work = (double *) R_alloc(projp*projn, sizeof(double)); 
  zero(proj_work,projp*projn);
  projdata = (double *) R_alloc(projp*n, sizeof(double)); 
   zero(projdata,projp*n);
  index_best = (double *) R_alloc(1, sizeof(double)); 
  index_work = (double *) R_alloc(1, sizeof(double)); 
  sum = (double *) R_alloc(1, sizeof(double)); 
  /*  initrandom(start); */
  GetRNGstate();
  iszero(fp,projdim,proj_best,sum);
  if (!(*sum))
  { normal_fill1 (fp,projdim,proj_best, 1.0,proj_best);          
    orthonormal (proj_best,fp,projdim);
  }
 
  for(i=0; i<projp; i++)
  {  for (j=0;j<n; j++)
    {    projdata[i*n+j] = 0;
         for (m=0; m<p; m++)
         projdata[i*n+j] += proj_best[i*p+m]*fvals[m*n+j];
     }
  }
  switch(*method)
  {
     case 1:
       discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_best);
      break;
     case 2:
       discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_best);
      break;
     case 3:
       discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_best,r); 
      break;  
     case 4:
       cartgini(fn,projdim,groups,projdata,group,ngroup,index_best); 
      break;
     case 5:
       cartentropy(fn,projdim,groups,projdata,group,ngroup,index_best); 
      break;
     case 6:
       pda(fn,projdim,groups,projdata,group,gname,ngroup,index_best,lambda); 
      break;  
   }

  kt = 0;
 
  for(i=0; i<projp; i++)
    for (m=0; m<projn; m++)
         proj_work[i*p+m] = proj_best[i*p+m];
    
 
 
    while (temp> temp_end && kt < maxproj) 
    { 

      normal_fill1 (fp,projdim,proj_work,temp,proj_best);
      orthonormal (proj_work,fp,projdim);                          
      temp *= cool;

      zero(projdata,projp*n);
      for(i=0; i<projp; i++)
      {  for (j=0;j<n; j++)
         { for (m=0; m<p; m++)
             projdata[i*n+j] += proj_work[i*p+m]*fvals[m*n+j];
         }
      }
      switch(*method)
      {
         case 1:
           discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_work); 
          break;
         case 2:
           discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_work); 
          break;
         case 3:
           discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_work,r); 
          break;
         case 4:
           cartgini(fn,projdim,groups,projdata,group,ngroup,index_work); 
          break;
         case 5:
           cartentropy(fn,projdim,groups,projdata,group,ngroup,index_work); 
          break;
         case 6:
           pda(fn,projdim,groups,projdata,group,gname,ngroup,index_best,lambda); 
          break;  
      }
      if(*index_work > *index_best)
      { 
        for(i=0;i<projp; i++)
           for(j=0; j<projn; j++)
              proj_best[i*projn+j] =proj_work[i*projn+j];
        *index_best = *index_work;
       }
      kt++; 
    }
    /*  printf("kt = %d\n",kt);*/

     for(i=0;i<projp; i++)
       for(j=0; j<projn; j++)
          proj[i*projn+j] =proj_best[i*projn+j];  
    *val = *index_best;
    PutRNGstate();
/*    Free(proj_best);
    Free(proj_work);
    Free(projdata);
    Free(index_work);
    Free(index_best);
    Free(sum);
    Free(group); */

}


/* Simulated Annealing */

void optimize2(int *fn, int *fp, int *groups, double *fvals, int *groupraw, int *gname,
	     int *ngroup, int *method, double *cooling, double *temp_start,
             int *projdim, double *val, double *proj, double *energy,int *r,double *lambda)
{

  int n,p,g,projn,projp,maxproj,i,j,m,kt,*group;
  double *proj_best,*proj_work, *projdata,*index_work, *index_best;
  double temp_end,temp,cool,tempp,*sum,prob,e,diff;

  n = *fn; p = *fp; g = *groups;
  projn = p;
  projp = *projdim;
  temp = *temp_start;
  cool = *cooling;
  temp_end = 0.001;
  maxproj=1000;

   group = (int *) R_alloc(n, sizeof(int));
  zero_int(group, n); 

  if(gname[(g-1)] !=g)
  { for(i=0; i<n; i++)
      for(j=0; j<g; j++)
        if(groupraw[i] == gname[j]) group[i] = j+1;
  }
  else
    memcpy(group,groupraw,n*sizeof(int));
  

  /*  printf("in Optimize %f\n",*var);*/
  /********************************/
  
  proj_best = (double *) R_alloc(projp*projn, sizeof(double)); 
  zero(proj_best,projp*projn);
  proj_work = (double *) R_alloc(projp*projn, sizeof(double)); 
  zero(proj_work,projp*projn);
  projdata = (double *) R_alloc(projp*n, sizeof(double)); 
  zero(projdata,projp*n);
  index_best = (double *) R_alloc(1, sizeof(double)); 
  index_work = (double *) R_alloc(1, sizeof(double)); 
  sum = (double *) R_alloc(1, sizeof(double)); 
  /*  initrandom(start); */
  GetRNGstate();
  iszero(fp,projdim,proj_best,sum);
  if (!(*sum))
  { normal_fill1 (fp,projdim,proj_best, 1.0,proj_best);          
    orthonormal (proj_best,fp,projdim);
  }
 
  for(i=0; i<projp; i++)
  {  for (j=0;j<n; j++)
    {    projdata[i*n+j] = 0;
         for (m=0; m<p; m++)
         projdata[i*n+j] += proj_best[i*p+m]*fvals[m*n+j];
     }
  }
 
  switch(*method)
  {
     case 1:
       discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_best);
       break;
     case 2:
       discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_best);
       break;
     case 3:
       discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_best,r); 
      break;  
     case 4:
       cartgini(fn,projdim,groups,projdata,group,ngroup,index_best); 
      break;
     case 5:
       cartentropy(fn,projdim,groups,projdata,group,ngroup,index_best); 
      break;
     case 6:
       pda(fn,projdim,groups,projdata,group,gname,ngroup,index_best,lambda);
       break;

  }

  kt = 1;

 
  for(i=0; i<projp; i++)
    for (m=0; m<projn; m++)
         proj_work[i*p+m] = proj_best[i*p+m];
    
 
 
  diff=100;
  while ((temp >0.001 || fabs(diff)>(*energy/1000000)) && kt <=500000)
    { 
      tempp = *energy/log(kt+1)/10000;
      temp = temp*cool;
      normal_fill1 (fp,projdim,proj_work,temp,proj_best);
      orthonormal (proj_work,fp,projdim);                          

      zero(projdata,projp*n);
      for(i=0; i<projp; i++)
      {  for (j=0;j<n; j++)
         { for (m=0; m<p; m++)
             projdata[i*n+j] += proj_work[i*p+m]*fvals[m*n+j];
         }
      }
      switch(*method)
      {
         case 1:
           discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_work); 
          break;
         case 2:
           discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_work);
          break;
         case 3:
           discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_work,r); 
          break;  
         case 4:
           cartgini(fn,projdim,groups,projdata,group,ngroup,index_work); 
          break;
         case 5:
           cartentropy(fn,projdim,groups,projdata,group,ngroup,index_work); 
          break;
         case 6:
           pda(fn,projdim,groups,projdata,group,gname,ngroup,index_work,lambda);
          break;
      }
      prob=uniformrandom();
      diff=*index_work-*index_best;
      e=exp(diff/tempp);
      
      if( prob<e)
      { 
        for(i=0;i<projp; i++)
           for(j=0; j<projn; j++)
              proj_best[i*projn+j] =proj_work[i*projn+j];
        *index_best = *index_work;
      }
       kt++;
      
    }

     for(i=0;i<projp; i++)
       for(j=0; j<projn; j++)
          proj[i*projn+j] =proj_best[i*projn+j];  
    *val = *index_best;
    /*   printf("iteration = %d\n",kt);*/
    PutRNGstate();
/*    Free(proj_best);
    Free(proj_work);
    Free(projdata);
    Free(index_work);
    Free(index_best);
    Free(sum);
    Free(group); */

}


/* Algorithm from Huber's tech report */

void optimize3(int *fn, int *fp, int *groups, double *fvals, int *groupraw,int *gname,
	     int *ngroup, int *method, double *cooling, double *temp_start,
             int *projdim, double *val, double *proj,int *r,double *lambda)
{

  int n,p,g,projn,projp,maxproj,i,j,m,kt,count,*group;
  double *proj_best, *projdata, *index_best,*proj_temp,*index_temp;
  double temp_end,temp,cool,*sum,diff,radius;
  double *index_work,*proj_work;

  n = *fn; p = *fp; g = *groups;
  projn = p;
  projp = *projdim;
  temp = *temp_start;
  cool = *cooling;
  temp_end = 0.001;
  maxproj=1000;


   group = (int *) R_alloc(n, sizeof(int)); 
  zero_int(group, n); 

  if(gname[(g-1)] !=g)
  { for(i=0; i<n; i++)
      for(j=0; j<g; j++)
        if(groupraw[i] == gname[j]) group[i] = j+1;
  }
  else
    memcpy(group,groupraw,n*sizeof(int));
  

  /********************************/
  
  proj_best = (double *) R_alloc(projp*projn, sizeof(double)); 
  zero(proj_best,projp*projn);
  proj_work = (double *) R_alloc(projp*projn, sizeof(double)); 
  zero(proj_work,projp*projn);
  proj_temp = (double *) R_alloc(projp*projn, sizeof(double)); 
  zero(proj_temp,projp*projn);

  projdata = (double *) R_alloc(projp*n, sizeof(double)); 
   zero(projdata,projp*n);
  index_best = (double *) R_alloc(1, sizeof(double)); 
  index_work = (double *) R_alloc(1, sizeof(double)); 
  index_temp = (double *) R_alloc(1, sizeof(double)); 

  sum = (double *) R_alloc(1, sizeof(double)); 
  /*  initrandom(start); */
  GetRNGstate();
  iszero(fp,projdim,proj_best,sum);
  if (!(*sum))
  { normal_fill1 (fp,projdim,proj_best, 1.0,proj_best);          
    orthonormal (proj_best,fp,projdim);
  }
 
  for(i=0; i<projp; i++)
  {  for (j=0;j<n; j++)
    {    projdata[i*n+j] = 0;
         for (m=0; m<p; m++)
         projdata[i*n+j] += proj_best[i*p+m]*fvals[m*n+j];
     }
  }
 
  switch(*method)
  {
     case 1:
       discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_best); 
      break;
     case 2:
       discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_best);
       break;
     case 3:
       discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_best,r); 
      break;  
     case 4:
       cartgini(fn,projdim,groups,projdata,group,ngroup,index_best); 
      break;
     case 5:
       cartentropy(fn,projdim,groups,projdata,group,ngroup,index_best); 
      break;
     case 6:
       pda(fn,projdim,groups,projdata,group,gname,ngroup,index_work,lambda);
      break;

  }

  kt = 1;

 
  for(i=0; i<projp; i++)
    for (m=0; m<projn; m++)
         proj_work[i*p+m] = proj_best[i*p+m];
    
 
  temp = 1.5/sqrt(projn+10);
  count = 0;
  radius = 89;
  while (radius>0.6)
  { 
    /* Step 1 */
      normal_fill1 (fp,projdim,proj_temp,(0.2/log(projn+5)),proj_best);
      orthonormal (proj_temp,fp,projdim);                          

      zero(projdata,projp*n);
      for(i=0; i<projp; i++)
      {  for (j=0;j<n; j++)
         { for (m=0; m<p; m++)
             projdata[i*n+j] += proj_temp[i*p+m]*fvals[m*n+j];
         }
      }
      switch(*method)
      {
         case 1:
           discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_temp); 
          break;
         case 2:
           discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_temp);
          break;
         case 3:
           discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_temp,r); 
          break;  
         case 4:
           cartgini(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 5:
           cartentropy(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 6:
           pda(fn,projdim,groups,projdata,group,gname,ngroup,index_work,lambda);
          break;
      }
      diff=*index_temp-*index_best;
      if(diff <=0)
	 count++;
      else
	{  *index_work=*index_temp;
           for(i=0;i<projp; i++)
             for(j=0; j<projn; j++)
               proj_work[i*projn+j] =proj_temp[i*projn+j];
       }


    /* Step 2 */
      normal_fill1 (fp,projdim,proj_temp,temp,proj_best);
      orthonormal (proj_temp,fp,projdim);                          

      zero(projdata,projp*n);
      for(i=0; i<projp; i++)
      {  for (j=0;j<n; j++)
         { for (m=0; m<p; m++)
             projdata[i*n+j] += proj_temp[i*p+m]*fvals[m*n+j];
         }
      }
      switch(*method)
      {
         case 1:
           discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_temp); 
          break;
         case 2:
           discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_temp);
          break;
         case 3:
           discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_temp,r); 
          break;  
         case 4:
           cartgini(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 5:
           cartentropy(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 6:
           pda(fn,projdim,groups,projdata,group,gname,ngroup,index_work,lambda);
          break;
      }
      diff=*index_temp-*index_best;
      if(diff <=0)
	 count++;
      else if(*index_temp > *index_work)
	{  *index_work=*index_temp;
           for(i=0;i<projp; i++)
             for(j=0; j<projn; j++)
               proj_work[i*projn+j] =proj_temp[i*projn+j];
       }

    /* Step 3 */

      normal_fill1 (fp,projdim,proj_temp,temp,proj_best);
      orthonormal (proj_temp,fp,projdim);                          

      zero(projdata,projp*n);
      for(i=0; i<projp; i++)
      {  for (j=0;j<n; j++)
         { for (m=0; m<p; m++)
             projdata[i*n+j] += proj_temp[i*p+m]*fvals[m*n+j];
         }
      }
      switch(*method)
      {
         case 1:
           discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_temp); 
          break;
         case 2:
           discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_temp);
          break;
         case 3:
           discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_temp,r); 
          break;  
         case 4:
           cartgini(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 5:
           cartentropy(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 6:
           pda(fn,projdim,groups,projdata,group,gname,ngroup,index_work,lambda);
          break;
      }
      diff=*index_temp-*index_best;
      if(diff <=0)
	 count++;
      else if(*index_temp > *index_work)
	{  *index_work=*index_temp;
           for(i=0;i<projp; i++)
             for(j=0; j<projn; j++)
               proj_work[i*projn+j] =proj_temp[i*projn+j];
       }

    /* Step 4 */

      normal_fill1 (fp,projdim,proj_temp,temp,proj_best);
      orthonormal (proj_temp,fp,projdim);                          

      zero(projdata,projp*n);
      for(i=0; i<projp; i++)
      {  for (j=0;j<n; j++)
         { for (m=0; m<p; m++)
             projdata[i*n+j] += proj_temp[i*p+m]*fvals[m*n+j];
         }
      }
      switch(*method)
      {
         case 1:
           discriminant1(fn,projdim,groups,projdata,group,gname,ngroup,index_temp); 
          break;
         case 2:
           discriminant2(fn,projdim,groups,projdata,group,gname,ngroup,index_temp);
          break;
         case 3:
           discriminant3(fn,projdim,groups,projdata,group,gname,ngroup,index_temp,r); 
          break;  
         case 4:
           cartgini(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 5:
           cartentropy(fn,projdim,groups,projdata,group,ngroup,index_temp); 
          break;
         case 6:
           pda(fn,projdim,groups,projdata,group,gname,ngroup,index_work,lambda);
          break;
      }
      diff=*index_temp-*index_best;
      if(diff <=0)
	 count++;
      else if(*index_temp > *index_work)
	{  *index_work=*index_temp;
           for(i=0;i<projp; i++)
             for(j=0; j<projn; j++)
               proj_work[i*projn+j] =proj_temp[i*projn+j];
        }

      /* final step */
      if(count > projn*8) 
	{ radius = radius/2; temp= temp/2;
        count = 0; 
      }
      for(i=0;i<projp; i++)
         for(j=0; j<projn; j++)
            proj_best[i*projn+j] =proj_work[i*projn+j];
      *index_best = *index_work;
      kt++;
  }

     for(i=0;i<projp; i++)
       for(j=0; j<projn; j++)
          proj[i*projn+j] =proj_best[i*projn+j];  
    *val = *index_best;

      /*    printf("%d\n",kt);*/
    PutRNGstate();
/*    Free(proj_best);
    Free(proj_work);
    Free(proj_temp);
    Free(projdata);
    Free(index_work);
    Free(index_best);
    Free(index_temp);
    Free(sum);
    Free(group); */

}

