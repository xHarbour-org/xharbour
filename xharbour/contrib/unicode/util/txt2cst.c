/*
 * $Id: txt2cst.c,v 1.1 2004/01/14 13:59:52 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    TXT2CST.EXE
 *
 * Copyright 2004 Dmitry V. Korzhov <dk@april26.spb.ru>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEADS 10
#define MAX_CHAR 0x100
#define MAX_2CHAR 0x200
#define MAX_WCHAR 0x10000
#define MAX_2WCHAR 0x20000

char defchar='?',maxchar='\001';
char cleads[MAX_CHAR];
char leads[MAX_CHAR];
char tuni[MAX_2CHAR];
char tchar[MAX_2WCHAR];
char *aleads=NULL;

void Usage(char *programName)
{
	printf("%s usage:\n\n"
		"%s file.txt -> file.cst\n\n"
		"Converts Unicode group charset description text file\n"
		"to Harbour charset support library file\n"
		"file lines format:\n"
		"[0x??	0x????][#comment]",
		programName,programName);
}

void wsconv(char *ins)
{
	int i;
	for (i=0;ins[i];i++) if (ins[i]<' ') ins[i]=' ';
	return;
}

int alltrim(char *ins)
{
	int i,j,k;
	j=strlen(ins)-1;
	for (i=0;i<j;i++) {
		if (ins[i]==' ') continue;
		else break;
	}
	for (;i<j;j--) {
		if (ins[j]==' ') continue;
		else break;
	}
	for (k=0;k<=j-i;k++) ins[k]=ins[k+i];
	ins[k]='\0';
	return (j-i);
}

int main(int argc,char *argv[])
{
	FILE *hin;
	FILE *hout;
	char *fout;
	char *csname;
	char *fnew;
	int i,j,k,n;
	char s1[2],s2[2];
	if (argc == 1) {
		Usage(argv[0]);
		return 1;
	}
	if ((argv[1][0] == '/')||(argv[1][0] == '-')) {
		Usage(argv[0]);
		return 1;
	}
	hin=fopen(argv[1],"r");
	if (hin==NULL) {
		printf("%s open error\n",argv[1]);
		exit(1);
	}
	fout=(char *) malloc(strlen(argv[1])+5);
	csname=(char *) malloc(strlen(argv[1]));
	k=strlen(argv[1])-1;
	strcpy(csname,argv[1]);
	if (csname[k--]=='.') csname[k+1]=='\0';
	else if (csname[k--]=='.') csname[k+1]='\0';
	else if (csname[k--]=='.') csname[k+1]='\0';
	else if (csname[k--]=='.') csname[k+1]='\0';
	strcpy(fout,csname);
	strcat(fout,".cst");
	hout=fopen(fout,"wb");
	if (hout==NULL) {
		printf("%s create error\n",fout);
		exit(1);
	}
	fout=(char *)realloc(fout,MAX_2CHAR);
	for (i=0;i<MAX_WCHAR;i++) {
		tchar[i]=defchar;
	}
	for (i=0;i<MAX_2CHAR;i++) {
		if (i&1) tuni[i]='\0';
		else tuni[i]=defchar;
	}
	for (i=0;i<MAX_CHAR;i++) {
		cleads[i]='\0';
		leads[i]='\0';
	}
	while (fgets(fout,MAX_2CHAR,hin)!=NULL) {
		fnew=strchr(fout,'#');
		if (fnew!=NULL) {
			fout[fnew-fout]='\0';
		}
		wsconv(fout);
		k=alltrim(fout);
		if (k<=0) continue;
		if (k>4) sscanf(fout,"0x%X 0x%X",&i,&j);
		else continue;
		if (maxchar==1) {
			if (i>MAX_CHAR) {
				maxchar=(char) 2;
				for (k=MAX_WCHAR;k;k--) {
					tchar[2*k-2]=tchar[k-1];
					tchar[2*k-1]='\0';
				}
			}
			else {
				tuni[2*i]=(char) (j&(MAX_CHAR-1));
				tuni[2*i+1]=(char) ((j>>8)&(MAX_CHAR-1));
				tchar[j]=(char) i;
			}
		}
		if (maxchar==2) {
			if (i<MAX_CHAR) {
				tchar[2*j]=(char) (i&(MAX_CHAR-1));
				tchar[2*j+1]='\0';
			}
			else {
				tchar[2*j]=(char) ((i>>8)&(MAX_CHAR-1));
				tchar[2*j+1]=(char) (i&(MAX_CHAR-1));
			}
			if ((i>>8)&(MAX_CHAR-1)) {
				if (leads[(i>>8)&(MAX_CHAR-1)]) {
					fnew=strchr(cleads,(i>>8)&(MAX_CHAR-1));
					k=(fnew-cleads);
				}
				else {
					leads[(i>>8)&(MAX_CHAR-1)]=(char) 1;
					k=strlen(cleads);
					cleads[k]=(char) ((i>>8)&(MAX_CHAR-1));
					aleads=(char *) realloc(aleads,(k+1)*(MAX_2CHAR));
					for (n=0;n<MAX_CHAR;n++) {
						aleads[k*MAX_2CHAR+2*n]=defchar;
						aleads[k*MAX_2CHAR+2*n+1]='\0';
					}
				}
				aleads[k*MAX_2CHAR+2*(i&(MAX_CHAR-1))]=(char) (j&(MAX_CHAR-1));
				aleads[k*MAX_2CHAR+2*(i&(MAX_CHAR-1))+1]=(char) ((j>>8)&(MAX_CHAR-1));
			}
			else {
				tuni[2*i]=(char) (j&(MAX_CHAR-1));
				tuni[2*i+1]=(char) ((j>>8)&(MAX_CHAR-1));
			}
		}
	}
	free(fout);
	fclose(hin);
	fout=(char *) malloc(MAX_LEADS+1);
	for (i=0;i<=MAX_LEADS;i++) fout[i]='\0';
	j=0;
	for (i=0;i<MAX_CHAR;i++) {
		if (leads[i]) {
			if ((j&1)==0) {
				fout[j]=(char) i;
				j++;
			}
			if (leads[i+1])
				continue;
			else {
				fout[j]=(char) i;
				j++;
			}
		}
		if (j==MAX_LEADS) break;
	}
	for (i=0;i<strlen(csname);i++) fputc(csname[i],hout);
	for (;i<48;i++) fputc(0,hout);
	fputc(maxchar,hout);
	fputc(defchar,hout);
	fputc(0,hout);
	fputc(0,hout);
	for (i=0;i<MAX_LEADS;i++) fputc(fout[i],hout);
	fputc(0,hout);
	fputc(strlen(cleads)+1,hout);
	for (i=0;i<MAX_2CHAR;i++) fputc(tuni[i],hout);
	for (k=0;k<strlen(cleads);k++)
		for (i=0;i<MAX_2CHAR;i++) fputc(aleads[k*MAX_2CHAR+i],hout);
	for (i=0;i<((int)maxchar)*MAX_WCHAR;i++) fputc(tchar[i],hout);
	fclose(hout);
	free(aleads);
	return 0;
}
