#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <fftw3.h>
#include <omp.h>
char *Wisfile = NULL;
char *Wistemplate = "%s/.fftwis";
#define WISLEN 8

void set_wisfile(void)
{
	char *home;

	if (Wisfile) return;
	home = getenv("HOME");
	Wisfile = (char *)malloc(strlen(home) + WISLEN + 1);
	sprintf(Wisfile, Wistemplate, home);
}

/* Convert frequencies in Hz into rows of the ST, given sampling rate and
length. */

int st_freq(double f, int len, double srate)
{
	return floor(f * len / srate + .5);
}

static double gauss(int n, int m);

/* Stockwell transform of the real array data. The len argument is the
number of time points, and it need not be a power of two. The lo and hi
arguments specify the range of frequencies to return, in Hz. If they are
both zero, they default to lo = 0 and hi = len / 2. The result is
returned in the complex array result, which must be preallocated, with
n rows and len columns, where n is hi - lo + 1. For the default values of
lo and hi, n is len / 2 + 1. */

void Strans(int len, int lo, int hi, double df, double *data, double *result)
{
	int i, k, n, l2,num;
	double s,mean;
	FILE *wisdom,*fp;
	static int planlen = 0;
	static double *p;
	static fftw_plan p1, p2;
	// fftw_plan p1, p2;
	static fftw_complex *h, *H, *G;
	//static fftw_complex h[16384], *H, G[16384];
       
	fftw_complex *cG;
	double *g,pi2,frq,scale;
		
//	fp=fopen("Fourier.SP1","w");
	/* Check for frequency defaults. */
//	num=2*(hi-lo+1)*len;
//	p=(double*)malloc(sizeof(double)*num);

	if (lo == 0 && hi == 0) {
		hi = len / 2;
	}
//		printf("len=%d, lo=%d, hi=%d, data:%d, results:%d\n",len,lo,hi,&data[0],&result[0]);
	//	for(i=0;i<100;i++)
	//	printf("%7.5lf\n",data[i]);
	//	getchar();
	/* Keep the arrays and plans around from last time, since this
	is a very common case. Reallocate them if they change. */
	
	if (len != planlen && planlen > 0) {
		fftw_destroy_plan(p1);
		fftw_destroy_plan(p2);
		fftw_free(h);
		fftw_free(H);
		fftw_free(G);
	//	free(g);
		planlen = 0;
	}
	if (planlen == 0) {
		planlen = len;
		h = fftw_malloc(sizeof(fftw_complex) * len);
		H = fftw_malloc(sizeof(fftw_complex) * len);
		G = fftw_malloc(sizeof(fftw_complex) * len);
	//	g = (double *)malloc(sizeof(double) * len);

		/* Get any accumulated wisdom. */

		set_wisfile();
		wisdom = fopen(Wisfile, "r");
		if (wisdom) {
			fftw_import_wisdom_from_file(wisdom);
			fclose(wisdom);
		}
		/* Set up the fftw plans. */

		p1 = fftw_plan_dft_1d(len, h, H, FFTW_FORWARD, FFTW_MEASURE);
		p2 = fftw_plan_dft_1d(len, G, h, FFTW_BACKWARD, FFTW_MEASURE);

		/* Save the wisdom. */

		wisdom = fopen(Wisfile, "w");
		if (wisdom) {
			fftw_export_wisdom_to_file(wisdom);
			fclose(wisdom);
		}
	}
	/* Convert the input to complex. Also compute the mean. */

	s = 0.;
	memset(h, 0, sizeof(fftw_complex) * len);
	for (i = 0; i < len; i++) {
		h[i][0] = data[i];
		s += data[i];
	//	printf("i=%d,,,,,data=%f\n",i,data[i]);
	}
	s /= len;
	mean=s;
	/* FFT. */

	fftw_execute(p1); /* h -> H */

	/* Hilbert transform. The upper half-circle gets multiplied by
	two, and the lower half-circle gets set to zero.  The real axis
	is left alone. */

	l2 = (len + 1) / 2;
	for (i = 1; i < l2; i++) {
		H[i][0] *= 2.;
		H[i][1] *= 2.;
	}
	l2 = len / 2 + 1;
	for (i = l2; i < len; i++) {
		H[i][0] = 0.;
		H[i][1] = 0.;
	}
//	for(i=0;i<l2;i++)
//		fprintf(fp,"%d  %f\n",i,H[i][0],  H[i][1]);
	/* Fill in rows of the result. */

//	p = result;
	/* The row for lo == 0 contains the mean. */
	/* Subsequent rows contain the inverse FFT of the spectrum
	multiplied with the FFT of scaled gaussians. */

	pi2=-2.0*M_PI*M_PI;
	#pragma omp parallel for lastprivate(n,i,s,k,cG,g) num_threads(4)	
	for(n=lo;n<=hi;n++)
	{
		/* Scale the FFT of the gaussian. Negative frequencies
		wrap around. */

		/* The row for lo == 0 contains the mean. */
		/* Subsequent rows contain the inverse FFT of the spectrum
 		multiplied with the FFT of scaled gaussians. */
		frq=n*df;
		/*
		if( frq > 0.025 ) scale=4.0;
		if( frq <=0.025 && frq>=0.02) scale=4.0-0.3*(1./frq-40);
		if( frq < 0.02 && frq>=0.01 )  scale=1.0-(1.0/frq-50)*0.01;
		if ( frq <0.01 ) scale=0.5;
		//printf("%d  %f  %f  %f\n",n,df, 1./frq, scale);
		*/

		/*Parameter scale controls the time-frequency resolution. Bigger value can help to increase the frequenct
 		  resolution but decrease the time resolution, and vice versa
		*/ 
		scale=1.0;
		g=(double *)malloc(sizeof(double)*len);
		cG=(fftw_complex*)malloc(sizeof(fftw_complex)*len);
		if(n==0)
		{
			for(i=0;i<len;i++)
			{
				result[(n-lo)*len+2*i]= mean;	
				result[(n-lo)*len+2*i+1]=0.;
			}
		}
		else
		{	
			g[0] = exp( pi2 * 0 * 0 / (n * n));
			l2 = len / 2 + 1;
			for (i = 1; i < l2; i++) 
			{
				g[i] = g[len - i] = exp( pi2 * i * i * scale/ (n * n));
				//g[i] = g[len - i] = 1.0/(1+((i*i)/n)*((i*i)/n));
			}
			for (i = 0; i < len; i++) 
			{
				s = g[i];
				k = n + i;
				if (k >= len) k -= len;
				cG[i][0] = H[k][0] * s;
				cG[i][1] = H[k][1] * s;
			//	printf("%d  %d  %f  %f  %f\n",i,n,s ,G[i][0] ,G[i][1] );
			}

			/* Inverse FFT the result to get the next row. */
			#pragma omp critical (setction1)
			{
				for(i = 0; i < len; i++)
				{
					G[i][0]=cG[i][0];
					G[i][1]=cG[i][1];
				}
				fftw_execute(p2); /* G -> h */
			for (i = 0; i < len; i++) 
			{
				//	*p++ = h[i][0] / len;
				/*
				p[(n-lo)*len+2*i]=h[i][0] / len;
				fprintf(fp,"n=%d,i=%d,  p=%7.5lf  %d\n",n,i,p[(n-lo)*len+2*i], len);
				p[(n-lo)*len+2*i+1] = h[i][1] / len;
				fprintf(fp,"n=%d,i=%d,  p=%7.5lf  %d\n",n,i, p[(n-lo)*len+2*i+1] , len);
				*/
				*(result+(2*(n-lo)*len+2*i))=h[i][0]/len;
				//fprintf(fp,"lo=%3d hi=%3d, n=%3d,i=%3d,  p=%7.5lf  %d\n",lo,hi,n-lo,i,*(result+(2*(n-lo)*len+2*i)), len);
				*(result+(2*(n-lo)*len+2*i+1))=h[i][1]/len;
				//fprintf(fp,"lo=%3d hi=%3d, n=%3d,i=%3d,  p=%7.5lf  %d\n",lo,hi,n-lo,i,*(result+(2*(n-lo)*len+2*i+1)), len);
			}
			}
			/* Go to the next row. */
		}
		free(g); fftw_free(cG);
	}
//	fclose(fp);
//		printf("len=%d, lo=%d, hi=%d, data:%d, results:%d\n",len,lo,hi,&data[0],result);
//	printf("Sucessfully!\n");
}

/* This is the Fourier Transform of a Gaussian. */

static double gauss(int n, int m)
{
	return exp(-2.0 * M_PI * M_PI * m * m / (n * n));
}

/* Inverse Stockwell transform, this inverse algorithm is based on Stockwell et al. 1996 */

void iStrans(int len, int lo, int hi, double *data, double *result)
{
	int i, n, l2;
	double *p;
	FILE *wisdom;
	static int planlen = 0;
	static fftw_plan p2;
	static fftw_complex *h, *H;
	FILE *fp;
//	fp=fopen("Fourier.SP2","w");
	#ifdef debuge
	FILE *fp,*fp1;

	if((fp=fopen("new_SP.dat","w"))==NULL)
	{
		printf("new_SP.dat cannot be writen!\n");
		exit(0);
	}
	if((fp1=fopen("signal.dat","w"))==NULL)
        {
                printf("signal.dat cannot be writen!\n");
                exit(0);
        }
	#endif
	/* Check for frequency defaults. */

	if (lo == 0 && hi == 0) {
		hi = len / 2;
	}

//	printf("len=%d, lo=%d, hi=%d\n",len,lo,hi);
	/* Keep the arrays and plans around from last time, since this
	is a very common case. Reallocate them if they change. */

	if (len != planlen && planlen > 0) {
		fftw_destroy_plan(p2);
		fftw_free(h);
		fftw_free(H);
		planlen = 0;
	}

	if (planlen == 0) {
		planlen = len;
		h = fftw_malloc(sizeof(fftw_complex) * len);
		H = fftw_malloc(sizeof(fftw_complex) * len);

		/* Get any accumulated wisdom. */

		set_wisfile();
		wisdom = fopen(Wisfile, "r");
		if (wisdom) {
			fftw_import_wisdom_from_file(wisdom);
			fclose(wisdom);
		}

		/* Set up the fftw plans. */

		p2 = fftw_plan_dft_1d(len, H, h, FFTW_BACKWARD, FFTW_MEASURE);

		/* Save the wisdom. */

		wisdom = fopen(Wisfile, "w");
		if (wisdom) {
			fftw_export_wisdom_to_file(wisdom);
			fclose(wisdom);
		}
	}

	/* Sum the complex array across time. */

	memset(H, 0, sizeof(fftw_complex) * len);
	p = data;
	for (n = lo; n <= hi; n++) 
	{
		for (i = 0; i < len; i++) 
		{
			#ifdef debuge
			fprintf(fp,"%8.5f ",*p);
			#endif
			H[n][0] += *p++;
			#ifdef debuge
			fprintf(fp,"%8.5f ",*p);
			#endif
			H[n][1] += *p++;
		}
	//	fprintf(fp,"%d  %f\n",n,H[n][0],H[n][1]);
		#ifdef debuge
		fprintf(fp,"\n");
		#endif
	}

	/* Invert the Hilbert transform. */
/*
	l2 = (len + 1) / 2;
	for (i = 1; i < l2; i++) {
		H[i][0] /= 2.;
		H[i][1] /= 2.;
	}
	l2 = len / 2 + 1;
	for (i = l2; i < len; i++) {
		H[i][0] = H[len - i][0];
		H[i][1] = -H[len - i][1];
	}
	for(i=0;i<hi;i++)
		printf("%d %7.5lf  %7.5lf %7.5lf  %7.5lf\n",i, H[i][0],H[i][1] ,H[len-1-i][0],H[len-1-i][1]);
	getchar();
*/
	/* Inverse FFT. */

	fftw_execute(p2); /* H -> h */
	fftw_destroy_plan(p2);
	p = result;
	for (i = 0; i < len; i++) {
		*p++ = h[i][0] / len;
		#ifdef debuge
		fprintf(fp1,"%7.5lf ",h[i][0] / len);
		#endif
	}
	#ifdef debuge
	fclose(fp);
	fclose(fp1);
	#endif
//	fclose(fp);
}

/* This does just the Hilbert transform. */

void hilbert(int len, double *data, double *result)
{
	int i, l2;
	double *p;
	FILE *wisdom;
	static int planlen = 0;
	static fftw_plan p1, p2;
	static fftw_complex *h, *H;

	/* Keep the arrays and plans around from last time, since this
	is a very common case. Reallocate them if they change. */

	if (len != planlen && planlen > 0) {
		fftw_destroy_plan(p1);
		fftw_destroy_plan(p2);
		fftw_free(h);
		fftw_free(H);
		planlen = 0;
	}

	if (planlen == 0) {
		planlen = len;
		h = fftw_malloc(sizeof(fftw_complex) * len);
		H = fftw_malloc(sizeof(fftw_complex) * len);

		/* Get any accumulated wisdom. */

		set_wisfile();
		wisdom = fopen(Wisfile, "r");
		if (wisdom) {
			fftw_import_wisdom_from_file(wisdom);
			fclose(wisdom);
		}

		/* Set up the fftw plans. */

		p1 = fftw_plan_dft_1d(len, h, H, FFTW_FORWARD, FFTW_MEASURE);
		p2 = fftw_plan_dft_1d(len, H, h, FFTW_BACKWARD, FFTW_MEASURE);

		/* Save the wisdom. */

		wisdom = fopen(Wisfile, "w");
		if (wisdom) {
			fftw_export_wisdom_to_file(wisdom);
			fclose(wisdom);
		}
	}

	/* Convert the input to complex. */

	memset(h, 0, sizeof(fftw_complex) * len);
	for (i = 0; i < len; i++) {
		h[i][0] = data[i];
	}

	/* FFT. */

	fftw_execute(p1); /* h -> H */

	/* Hilbert transform. The upper half-circle gets multiplied by
	two, and the lower half-circle gets set to zero.  The real axis
	is left alone. */

	l2 = (len + 1) / 2;
	for (i = 1; i < l2; i++) {
		H[i][0] *= 2.;
		H[i][1] *= 2.;
	}
	l2 = len / 2 + 1;
	for (i = l2; i < len; i++) {
		H[i][0] = 0.;
		H[i][1] = 0.;
	}

	/* Inverse FFT. */

	fftw_execute(p2); /* H -> h */

	/* Fill in the rows of the result. */

	p = result;
	for (i = 0; i < len; i++) {
		*p++ = h[i][0] / len;
		*p++ = h[i][1] / len;
	}
}


/* Inverse Stockwell transform, this inverse algorithm is based on Schimmel et al. 2005, and this
 * inverse algorihm is only used for testing */

void iStrans2(int len, int lo, int hi,double df, double *data, double *result)
{
	int i, n, l2;
	double *p,scale,out,mean;
	FILE *wisdom;
	static int planlen = 0;
	static fftw_plan p2;
	static fftw_complex *h, *H;
	
        /*
 	FILE *fp;	
	if((fp=fopen("new_SP.dat","w"))==NULL)
        {
                printf("new_SP.dat cannot be writen!\n");
                exit(0);
        }
	*/
	#ifdef debuge
	FILE *fp,*fp1;

	if((fp=fopen("new_SP.dat","w"))==NULL)
	{
		printf("new_SP.dat cannot be writen!\n");
		exit(0);
	}
	if((fp1=fopen("signal.dat","w"))==NULL)
        {
                printf("signal.dat cannot be writen!\n");
                exit(0);
        }
	#endif
	/* Check for frequency defaults. */

	if (lo == 0 && hi == 0) {
		hi = len / 2;
	}

//	printf("len=%d, lo=%d, hi=%d\n",len,lo,hi);
	/* Keep the arrays and plans around from last time, since this
	is a very common case. Reallocate them if they change. */

	if (len != planlen && planlen > 0) {
		fftw_destroy_plan(p2);
		fftw_free(h);
		fftw_free(H);
		planlen = 0;
	}

	if (planlen == 0) {
		planlen = len;
		h = fftw_malloc(sizeof(fftw_complex) * len);
		H = fftw_malloc(sizeof(fftw_complex) * len);

		/* Get any accumulated wisdom. */

		set_wisfile();
		wisdom = fopen(Wisfile, "r");
		if (wisdom) {
			fftw_import_wisdom_from_file(wisdom);
			fclose(wisdom);
		}

		/* Set up the fftw plans. */

		p2 = fftw_plan_dft_1d(len, H, h, FFTW_BACKWARD, FFTW_MEASURE);

		/* Save the wisdom. */

		wisdom = fopen(Wisfile, "w");
		if (wisdom) {
			fftw_export_wisdom_to_file(wisdom);
			fclose(wisdom);
		}
	}

	/* Sum the complex array across time. */
	scale=1.0*sqrt(2*M_PI);
	memset(H, 0, sizeof(fftw_complex) * len);
	p = data;

	for(i=0; i<len;i++)
	{
		for(n=lo;n<=hi;n++)
		{
			H[n][0]=*(p+2*i+2*(n-lo)*len);
			H[n][1]=*(p+2*i+1+2*(n-lo)*len);
			if(n==0)
			{	
				mean=H[n][0];
				H[n][0]=0;
				H[n][1]=0;
			//	printf("%d  %f  %f\n",n,H[n][0],H[n][1]);
			}
			if( n>0 )
			{
				H[n][0] = H[n][0]*scale/((n)*df);
				H[n][1] = H[n][1]*scale/((n)*df);
			}
		}
//			getchar();
		fftw_execute(p2); /* H -> h */
		out=0.0;
		for(l2=0;l2<len;l2++)
		{
			out=out+h[l2][0];
		//	printf("h: %d  %d  %f  %f\n",i,l2,h[l2][0],h[l2][1]);
		}
		/*
		if( i%40==10 )
		for(n=0;n<len;n++)
		{
			fprintf(fp,"%d  %d  %f\n",i,n,h[n][0]);
		}
		*/
		//getchar();
		//*result++=out/len*2.0/len;
		*result++=h[i][0]/len+mean;
		//printf("%d  %f\n",i,h[i][0]*2.0/len);
	}
	/* Invert the Hilbert transform. */
/*
	l2 = (len + 1) / 2;
	for (i = 1; i < l2; i++) {
		H[i][0] /= 2.;
		H[i][1] /= 2.;
	}
	l2 = len / 2 + 1;
	for (i = l2; i < len; i++) {
		H[i][0] = H[len - i][0];
		H[i][1] = -H[len - i][1];
	}
	for(i=0;i<hi;i++)
		printf("%d %7.5lf  %7.5lf %7.5lf  %7.5lf\n",i, H[i][0],H[i][1] ,H[len-1-i][0],H[len-1-i][1]);
	getchar();
*/
	/* Inverse FFT. */

	fftw_destroy_plan(p2);
	#ifdef debuge
	fclose(fp);
	fclose(fp1);
	#endif
//	fclose(fp);
}

