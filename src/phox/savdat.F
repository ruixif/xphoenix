*
**************************************************************
*
*  PHOENIX Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of PHOENIX-2.1.4.
*
*  PHOENIX-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation
*
*  PHOENIX-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with PHOENIX-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
      Subroutine SAVDAT(XF,NERR)
*
* purpose :  save various data derived from input file
*             to file '<XOUT>_<suffix>.<ext>'.
*            FORTRAN unit <LUNOUT> is used.
* date    :  November 27, 1997
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XF    = save action specifier
*    XOUT  = output file prefix
*    KPD1  = start index of phonon DOS range
*    KD    = stop index of phonon DOS range
*
* export variables :
*    NERR  = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      Character*(*) XF
*
      Character*4   XEXT
      Character*72  XFILE,XF2,XF3
*
*:  set error code
*
      NERR = 0
*
      If(Index(XOPT,',').eq.0) Then
       XF2  = XFORM2
       XF3  = XFORM3
       XEXT = '.dat'
      Else
       XF2  = XFORM2C
       XF3  = XFORM3C
       XEXT = '.csv'
      Endif
*
*:  the specifier XF initiates the following save action
*
*:   a  =  thermal asymmetry: (S(E)-S(-E))/(S(E)+(-E)) vs. energy
*:   d  =  phonon density of states, (1/eV/atom), normalized to 3
*:   n  =  data, elastic peak stripped, normalized
*:   r  =  fitted resolution function
*:   s  =  data, elastic peak stripped
*:   1  =  one-phonon part of data, (1/eV)
*:   2  =  two-phonon part of data, (1/eV)
*:   3  =  N>2-phonon part of data, (1/eV)
*
      B = .false.
      XFILE = XOUT
*
      IF(XF(1:1).EQ.'a') THEN
       CALL XMERGE(XFILE,'_asy'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .true.
       Call SAVHDR(LUNOUT,B,'energy (meV)','thermal asymmetry')
*
       DO 10  K = 1,KDBL
        WRITE(LUNOUT,XF3,IOSTAT=IERR,ERR=999)
     +            DDBX(K)/(0.5D0*DBETA),DDBY(K),DDBDY(K)
   10  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
*
       XFILE = XOUT
       CALL XMERGE(XFILE,'_asq'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .false.
       Call SAVHDR(LUNOUT,B,'energy (meV)','thermal asymmetry')
*
       DO 11  K = 1,KDBL
        DH = Dtanh(DDBX(K)*(1.D0-DEC1) + DEC2*DDBX(K)*DDBX(K)*DQFCT(K))
        WRITE(LUNOUT,XF2,IOSTAT=IERR,ERR=999)
     +            DDBX(K)/(0.5D0*DBETA),DH
   11  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
*
       XFILE = XOUT
       CALL XMERGE(XFILE,'_asl'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .false.
       Call SAVHDR(LUNOUT,B,'energy (meV)','thermal asymmetry')
*
       DO 12  K = 1,KDBL
        DH = Dtanh(DDBX(K)*(1.D0-DEC1))
        WRITE(LUNOUT,XF2,IOSTAT=IERR,ERR=999)
     +            DDBX(K)/(0.5D0*DBETA),DH
   12  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
*
       XFILE = XOUT
       CALL XMERGE(XFILE,'_eco'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .true.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                       'energy scale correction (meV)')
*
       DDE = DSX(2) - DSX(1)
       KE  = 2*Idnint(1.D2/DDE)
       DO 13  K = 0,KE
        DE = -1.D2 + Dfloat(K)*DDE
        WRITE(LUNOUT,XF3,IOSTAT=IERR,ERR=999)
     +            DE,-DEC1*DE-DEC2*DBETA*DE**2,
     +   Dabs(DE)*Dsqrt(DVEC1 + DVEC2*(DBETA*DE)**2 
     +               + 2.D0*DVEC12*DBETA*DE)
   13  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      IF(XF(1:1).EQ.'d') THEN
       CALL XMERGE(XFILE,'_dos'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .true.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                       'partial phonon DOS (1/eV)')
*
       DO 1  K = 1,KD-KPD1+1
        WRITE(LUNOUT,XF3,IOSTAT=IERR,ERR=999)
     +            DSX(K+KPD1-1),DCY(K),DCDY(K)
    1  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      IF(XF(1:1).EQ.'n') THEN
       CALL XMERGE(XFILE,'_psn'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .true.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                        'normalized data (1/eV)')
*
       DN = 1.D3/(DSX(2) - DSX(1))
*       WRITE(LUNOUT,2000,IOSTAT=IERR,ERR=999) DN*DSCLE
* 2000  FORMAT('### Scale =',1PD13.6)
       DO 2  K = 1,KD
        WRITE(LUNOUT,XF3,IOSTAT=IERR,ERR=999)
     +            DSX(K),DSY(K)*DN,DSQRT(DSDY(K))*DN
    2  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      IF(XF(1:1).EQ.'r') THEN
       CALL XMERGE(XFILE,'_rfc'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .false.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                        'fitted resolution function')
*
       DO 3  K = 1,KD
        WRITE(LUNOUT,XF2,IOSTAT=IERR,ERR=999) DSX(K),DRY(K)
    3  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      IF(XF(1:1).EQ.'s') THEN
       CALL XMERGE(XFILE,'_pst'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .true.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                        'data without elastic peak')
*
       DO 4  K = 1,KD
        WRITE(LUNOUT,XF3,IOSTAT=IERR,ERR=999)
     +            DSX(K),DSY(K),DSQRT(DMDY(K))
    4  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      IF(XF(1:1).EQ.'1') THEN
       CALL XMERGE(XFILE,'_1ph'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .false.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                        'one-phonon contribution (1/eV)')
*
       DN = 1.D3*DFFAC/(DSX(2) - DSX(1))
       DO 5  K = 1,KD
        WRITE(LUNOUT,XF2,IOSTAT=IERR,ERR=999)
     +            DSX(K),DCNY(K,1)*DN
    5  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      IF(XF(1:1).EQ.'2') THEN
       CALL XMERGE(XFILE,'_2ph'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .false.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                        'two-phonon contribution (1/eV)')
*
       DN = 1.D3*DFFAC/(DSX(2) - DSX(1))
       DO 6  K = 1,KD
        WRITE(LUNOUT,XF2,IOSTAT=IERR,ERR=999)
     +            DSX(K),DCNY(K,2)*DN
    6  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      IF(XF(1:1).EQ.'3') THEN
       CALL XMERGE(XFILE,'_3ph'//XEXT)
       OPEN(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       B = .false.
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                   'three and higher phonon contribution (1/eV)')
*
       DN = 1.D3*DFFAC/(DSX(2) - DSX(1))
       DO 7  K = 1,KD
        WRITE(LUNOUT,XF2,IOSTAT=IERR,ERR=999)
     +            DSX(K),DCNY(K,3)*DN
    7  CONTINUE
*
       CLOSE(UNIT=LUNOUT)
       RETURN
      ENDIF
*
******************************
*
      RETURN
*
*:  IO error exits
*
  998 CONTINUE
      NERR  = 5701
      NCOM1 = IERR
      XMSG  = XFILE
      RETURN
*
  999 CONTINUE
      NERR  = 5702
      NCOM1 = IERR
      XMSG  = XFILE
*
*
      END
