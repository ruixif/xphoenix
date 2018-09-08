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
      Subroutine SAVDAT(NERR)
*
* purpose :  save various data derived from input DOS
*              to file '<XOUT>_<suffix>.<ext>'
* date    :  February 18, 2011
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XOPT       = save action specifier
*    XOUT       = output file prefix
*    NENG       = number of energy points
*    DSX(*)     = energy values (meV)
*    DSY(*,0)   = phonon excitation probability (1/eV)
*    DSY(*,N)   = N-phonon excitation probability (1/eV)
*    DSYP(*)    = DSY(*,0) with elastic peak (1/eV)
*    DSYT(*,*)  = DSYP(*) for various temperatures (1/eV)
*    DGFX(*)    = time values (ps)
*    DGFY(*)    = time-dep. displacement (pm)
*    DENG1      = start energy (meV)
*    DENG2      = end energy (meV)
*    DTMP1      = start temperature (K)
*    DTMP2      = end temperature (K)
*    NTMP       = number of temperature points
*    DETR(*)    = entropy (k_B/atom)
*    DDETR(*)   = error of entropy (k_B/atom)
*    DFLM(*)    = Lamb-Moessbauer factor
*    DDFLM(*)   = error of Lamb-Moessbauer factor
*    DFRE(*)    = Helmholtz free energy (meV/atom)
*    DDFRE(*)   = error of Helmholtz free energy (meV/atom)
*    DSPH(*)    = specific heat (k_B/atom)
*    DDSPH(*)   = error of specific heat (k_B/atom)
*    DKIN(*)    = kinetic energy (meV/atom)
*    DDKIN(*)   = error of kinetic energy (meV/atom)
*
* export variables :
*    NERR       = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psth.h>
*
      Character*4   XEXT
      Character*72  XFILE,XF2,XF2N,XF3
*
*
*:  set error code
*
      NERR = 0
*
      If(Index(XOPT,',').eq.0) Then
       XF2  = XFORM2
       XF2N = '(1PE13.5,I10)'
       XF3  = XFORM3
       XEXT = '.dat'
      Else
       XF2  = XFORM2C
       XF2N = '(1PE13.5,'','',I10)'
       XF3  = XFORM3C
       XEXT = '.csv'
      Endif
      B = .false.
*
*:  the specifier XOPT initiates the following save action
*
*:   e  =  entropy versus temperature, (k_B/atom)
*:         free energy versus temperature, (meV/atom)
*:         kinetic energy versus temperature, (meV/atom)
*:   f  =  recoilless fraction versus temperature
*:   h  =  specific heat c_V versus temperature, (k_B/atom)
*:   s  =  phonon excitation probabilities, (1/eV)
*:   t  =  time-dep. displacement after momentum transfer DGAMMA/c (pm)
*
*
      If(Index(XOPT,'S').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_psn'//XEXT)
       Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                 'phonon excitation prob. dens. (1/eV)')
*
       Do  K = 1,NENG
        Write(LUNOUT,XF2,IOSTAT=IERR,ERR=999) DSX(K),DSY(K,0)
       EndDo
       Close(UNIT=LUNOUT)
*
       L = LENX(XFILE,' ')
       Write(LUNPTL,2000) XFILE(1:L)
 2000  Format(3X,'Phonon excitation prob. dens. : ',A)
*
       If(Index(XOPT,'X').ne.0) Then
        XFILE = XOUT
        Call XMERGE(XFILE,'_mpc'//XEXT)
        Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
        Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                 'n-phonon excit. prob. dens. (1/eV)')
*
        Do  J = 1,JMAX
         Do  K = 1,NENG
          Write(LUNOUT,XF2,IOSTAT=IERR,ERR=999) DSX(K),DSY(K,J)
         EndDo
         If(J.ne.JMAX) Write(LUNOUT,*)
        EndDo
        Close(UNIT=LUNOUT)
*
        L = LENX(XFILE,' ')
        Write(LUNPTL,2001) XFILE(1:L)
 2001   Format(3X,'n-Phonon excit. prob. dens.   : ',A)
       Endif
*
       If(DELPST.gt.0.D0 .and. DFWHM.gt.0.01D0) Then
        XFILE = XOUT
        Call XMERGE(XFILE,'_snp'//XEXT)
        Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
        Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                 'phonon excitation prob. dens. (1/eV)')
*
        Do  K = 1,NENG
         Write(LUNOUT,XF2,IOSTAT=IERR,ERR=999) DSX(K),DSYP(K)
        EndDo
        Close(UNIT=LUNOUT)
*
        L = LENX(XFILE,' ')
        Write(LUNPTL,2002) XFILE(1:L)
 2002   Format(3X,'Phonon excit. prob. w/ peak   : ',A)
*
        If(NTMP.gt.1) Then
         XFILE = XOUT
         Call XMERGE(XFILE,'.bin')
         Open(UNIT=LUNOUT,FILE=XFILE,Form='unformatted',
     +         IOSTAT=IERR,ERR=998)
*
         Write(LUNOUT) DENG1,DENG2,NENG,DTMP1,DTMP2,NTMP
         Write(LUNOUT) (DSX(K),K=1,NENG)
         Write(LUNOUT) (DSYP(K),K=1,NENG)
         Do  N = 1,NTMP-1
          Write(LUNOUT) (DSYT(K,N),K=1,NENG)
         EndDo
         Close(UNIT=LUNOUT)
*
         L = LENX(XFILE,' ')
         Write(LUNPTL,2003) XFILE(1:L)
 2003    Format(3X,'S(E,T) binary file            : ',A)
        Endif
*
        If(DINTCT.ne.0.D0) Then
         XFILE = XOUT
         Call XMERGE(XFILE,'_spc'//XEXT)
         Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
         Call SAVHDR(LUNOUT,B,'energy (meV)',
     +                 'simulated NRIXS spectrum')
*
*:  seed random number generator
*
         D = D_RAND(0)
*
*:  create and save artificial spectrum
*
         DFAC = (DINTCT - Dfloat(NENG)*DBGRD)/Dnorm1(DSYP,NENG,N,4)
         Do  K = 1,NENG
          DV = DFAC*DSYP(K) + DBGRD
          DX = D_RAND(0)
          NY = Idnint(DV + Dsqrt(2.D0*DV)*Diverf(DX))
          NY = Max0(0,NY)
          Write(LUNOUT,XF2N,IOSTAT=IERR,ERR=999) DSX(K),NY
         EndDo
         Close(UNIT=LUNOUT)
*
         L = LENX(XFILE,' ')
         Write(LUNPTL,2004) XFILE(1:L)
 2004    Format(3X,'Artificial NRIXS spectrum     : ',A)
        Endif
       Endif
      Endif
*
******************************
*
      B = .true.
      If(Index(XOPT,'T').ne.0) THEN
       XFILE = XOUT
       Call XMERGE(XFILE,'_dyn'//XEXT)
       Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNOUT,B,'time (ps)','impact displacement (pm)')
*
       Do  K = 1,KDG
        Write(LUNOUT,XF3,IOSTAT=IERR,ERR=999)
     +            DGFX(K),DGFY(K),DGFDY(K)
       EndDo
       Close(UNIT=LUNOUT)
*
       L = LENX(XFILE,' ')
       Write(LUNPTL,2005) XFILE(1:L)
 2005  Format(3X,'Time-dependent displacement   : ',A)
      Endif
*
******************************
*
      If(NTMP.eq.1) Return
*
******************************
*
*:  entropy
*:  Helmholtz free energy
*:  kinetic energy
*
      If(Index(XOPT,'E').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_etp'//XEXT)
       Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNOUT,B,'temperature (K)',
     +                    'vibrational entropy (k_B/atom)')
*
       L = LENX(XFILE,' ')
       Write(LUNPTL,2006) XFILE(1:L)
 2006  Format(3X,'Vibrational entropy           : ',A)
*
       XFILE = XOUT
       Call XMERGE(XFILE,'_fre'//XEXT)
       Open(UNIT=LUNTMP,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNTMP,B,'temperature (K)',
     +                    'vibrational free energy (meV/atom)')
       L = LENX(XFILE,' ')
       Write(LUNPTL,2007) XFILE(1:L)
 2007  Format(3X,'Vibrational free energy       : ',A)
*
       XFILE = XOUT
       Call XMERGE(XFILE,'_kin'//XEXT)
       Open(UNIT=LUNTMP+1,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNTMP+1,B,'temperature (K)',
     +                    'vibrational kinetic energy (meV/atom)')
       L = LENX(XFILE,' ')
       Write(LUNPTL,2008) XFILE(1:L)
 2008  Format(3X,'Vibrational kinetic energy    : ',A)
*
*
       DDTMP = (DTMP2-DTMP1)/Dfloat(NTMP-1)
       Do  N = 1,NTMP
        DTMP = DTMP1 + Dfloat(N-1)*DDTMP
        Write(LUNOUT,XF3,IOSTAT=IERR,ERR=999) DTMP,DETR(N),DDETR(N)
        Write(LUNTMP,XF3,IOSTAT=IERR,ERR=999) DTMP,DFRE(N),DDFRE(N)
        Write(LUNTMP+1,XF3,IOSTAT=IERR,ERR=999) DTMP,DKIN(N),DDKIN(N)
       EndDo
       Close(UNIT=LUNOUT)
       Close(UNIT=LUNTMP)
       Close(UNIT=LUNTMP+1)
*
      Endif
*
******************************
*
*:  recoilless fraction
*
      If(Index(XOPT,'F').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_flm'//XEXT)
       Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNOUT,B,'temperature (K)','Lamb-Moessbauer factor')
*
       DDTMP = (DTMP2-DTMP1)/Dfloat(NTMP-1)
       Do  N = 1,NTMP
        DTMP = DTMP1 + Dfloat(N-1)*DDTMP
        Write(LUNOUT,XF3,IOSTAT=IERR,ERR=999) DTMP,DFLM(N),DDFLM(N)
       EndDo
       Close(UNIT=LUNOUT)
*
       L = LENX(XFILE,' ')
       Write(LUNPTL,2009) XFILE(1:L)
 2009  Format(3X,'Lamb-Moessbauer factor        : ',A)
      Endif
*
******************************
*
*:  specific heat
*
      If(Index(XOPT,'H').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_cvt'//XEXT)
       Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNOUT,B,'temperature (K)',
     +                    'vibrational specific heat (k_B/atom)')
*
       DDTMP = (DTMP2-DTMP1)/Dfloat(NTMP-1)
       Do  N = 1,NTMP
        DTMP = DTMP1 + Dfloat(N-1)*DDTMP
        Write(LUNOUT,XF3,IOSTAT=IERR,ERR=999) DTMP,DSPH(N),DDSPH(N)
       EndDo
       Close(UNIT=LUNOUT)
*
       L = LENX(XFILE,' ')
       Write(LUNPTL,2010) XFILE(1:L)
 2010  Format(3X,'Vibrational specific heat     : ',A)
      Endif
*
******************************
*
*:  isotope fractionation, i.e., ln(Beta)*M/DeltaM
*
      If(Index(XOPT,'I').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_iso'//XEXT)
       Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNOUT,B,'temperature (K)',
     +                    'isotopic fractionation (perMille/%)')
*
       DDTMP = (DTMP2-DTMP1)/Dfloat(NTMP-1)
       Do  N = 1,NTMP
        DTMP = DTMP1 + Dfloat(N-1)*DDTMP
        DLNB  = 3.D1*(DKIN(N)/(DBLTZ*DTMP) - 0.5D0)
        DDLNB = 3.D1*DDKIN(N)/(DBLTZ*DTMP)
        Write(LUNOUT,XF3,IOSTAT=IERR,ERR=999) DTMP,DLNB,DDLNB
       EndDo
       Close(UNIT=LUNOUT)
*
       L = LENX(XFILE,' ')
       Write(LUNPTL,2011) XFILE(1:L)
 2011  Format(3X,'Isotopic fractionation        : ',A)
*
       XFILE = XOUT
       Call XMERGE(XFILE,'_isf'//XEXT)
       Open(UNIT=LUNOUT,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
       Call SAVHDR(LUNOUT,B,'temperature (K)',
     +                    'isotopic fractionation (perMille/%)')
*
       Call DTAFC(DMX,DMY,DMDY,KD,DGAMMA,DECOIL,DAFC,DDAFC)
       DLNBN = 0.25D-2*DECOIL/DELCH*(DHQUER*DCLIC/(DBLTZ*DGAMMA))**2
       Do  N = 1,NTMP
        DTMP  = DTMP1 + Dfloat(N-1)*DDTMP
        DLNB  = DLNBN*DAFC/DTMP**2
        DDLNB = DLNBN*DDAFC/DTMP**2
        Write(LUNOUT,XF3,IOSTAT=IERR,ERR=999) DTMP,DLNB,DDLNB
       EndDo
       Close(UNIT=LUNOUT)
*
       L = LENX(XFILE,' ')
       Write(LUNPTL,2012) XFILE(1:L)
 2012  Format(33X,': ',A)
      Endif
*
      Return
*
*:  IO error exits
*
  998 Continue
      NERR  = 4801
      NCOM1 = IERR
      XMSG1 = XFILE
      Return
*
  999 Continue
      NERR  = 4802
      NCOM1 = IERR
      XMSG1 = XFILE
*
*
      End
