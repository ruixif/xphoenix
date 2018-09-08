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
*   Program PHOENIX module PSTH
*
* purpose :  To calculate the phonon excitation probability
*            from a given PDOS assuming quasi-harmonic lattice
*            vibrations.
* date    :  December 20, 2010
* author  :  Wolfgang Sturhahn                                          
*                                                                       
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*                                                                       
#include<phoenix.h>
#include<psth.h>
#include<xterm.h>
*
      Character*1  X
      Character*4  XEXT
      Character*10 XD,XT,XZ
      Character*72 XFILE
*
      Dimension RT(2),II(8)
*
*
*:  set version number
*
#include<version.h>
*
*:  set error code
*
      NERR = 0
*
*:  start global timer
*
      CALL TIMEC(RT,1,1,-1)
*
*:  print copyright message
*
      CALL COPYRD(6,XVERSION)
*
*:  obtain line arguments
*
      CALL INSOPT(B,BGRAPH,BPRT,X,XFILE)
*
*:  date and time
*
      Call DATE_AND_TIME(XD,XT,XZ,II)
      XDATE = XD(5:6)//'/'//XD(7:8)//'/'//XD(3:4)
      XTIME = XT(1:2)//':'//XT(3:4)//':'//XT(5:6)
      XZONE = XZ(1:3)//':'//XZ(4:5)
*
*:  open input file
*
      Open(UNIT=LUNINP,FILE=XFILE,STATUS='OLD',ERR=998)
*
*:  read the module specific input file
*
      Call LESPST(NERR)
      Call ERRCHK(NERR,*999)
*
*:  close input file
*
      Close(LUNINP)
*
*:  read data
*
      Call LESDOS(NERR)
      Call ERRCHK(NERR,*999)
*
*:  negative energies were eliminated already,
*:  now force DMX(1)=0 and DMY(1)=0
*
      If(DMX(1).eq.0.D0) Then
       DMY(1)  = 0.D0
       DMDY(1) = 0.D0
      Else
       Do K = Min0(KD+1,PDATA),2,-1
        DMX(K)  = DMX(K-1)
        DMY(K)  = DMY(K-1)
        DMDY(K) = DMDY(K-1)
       EndDo
       DMX(1)  = 0.D0
       DMY(1)  = 0.D0
       DMDY(1) = 0.D0
       KD = Min0(KD+1,PDATA)
      Endif
*
*:  linearize x-scale of data
*
      Call BINEQ1(DMX,DMY,DSX,DSY,KD,KD,K,1)
      Call BINEQ1(DMX,DMDY,DSX,DSDY,KD,KD,K,5)
      Call DCOPY1(DSX,DMX,KD)
      Call DCOPY1(DSY,DMY,KD)
      Call DCOPY1(DSDY,DMDY,KD)
*
*:  open protocol file and write header
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_psth_ptl.txt')
      Open(UNIT=LUNPTL,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
      Call HEADER(LUNPTL,XVERSION,'psth ')
*
*:  print info
*
      Write(6,2001) XT_MAG,XT_DEF,DGAMMA,DECOIL
      Write(LUNPTL,2001) '','',DGAMMA,DECOIL
 2001 Format(/2X,A,'Input parameters -->',A/
     +        4X,'Nuclear transition energy   :',F9.3,' keV'/
     +        4X,'Recoil energy of free atom  :',F9.3,' meV')
*
      If(NTMP.eq.1) Then
       Write(6,2002) DTMP1
       Write(LUNPTL,2002) DTMP1
      Else
       Write(6,2003) DTMP1,DTMP2,NTMP
       Write(LUNPTL,2003) DTMP1,DTMP2,NTMP
      Endif
 2002 Format(4X,'Temperature                 :',F8.2,' K')
 2003 Format(4X,'Temperature range           :',F8.2,' ->',F8.2,' K'/
     +       4X,'Number of temp. points      :',I5)
*
      If(Index(XOPT,'S').ne.0) Then
       Write(6,2004) DENG1,DENG2,NENG,DELPST
       Write(LUNPTL,2004) DENG1,DENG2,NENG,DELPST
       If(DFWHM.gt.0.01D0) Then
        Write(6,2005) DFWHM,DSHP,DASM
        Write(LUNPTL,2005) DFWHM,DSHP,DASM
       Endif
      Endif
 2004 Format(4X,'Energy range in output      :',F8.2,' to',F8.2,' meV'/
     +       4X,'Number of energy points     :',I5//
     +       3X,'Resolution function and elastic peak:'/
     +        5X,'Function type               : Gaussian-like'/
     +        5X,'Elastic peak strength       :',F6.2)
 2005 Format(5X,'Full width at half maximum  :',F6.2,' meV'/
     +       5X,'Shape coefficient           :',F6.2/
     +       5X,'Left/right asymmetry        :',F6.2)
*
*:  normalize PDOS
*
      DFAC  = DNORM1(DMY,KD,N,4)
      DFAC2 = DFAC*DFAC
      Do  K = 1,KD
       DMY(K)  = DMY(K)/DFAC
       DMDY(K) = DMDY(K)/DFAC2
      EndDo
*
*:  production criteria
*
      If(NTMP.eq.1) Then
       BE = .true.
       BF = .true.
       BH = .true.
       BK = .true.
      Else
       BE = Index(XOPT,'E').ne.0
       BF = Index(XOPT,'F').ne.0 .or. Index(XOPT,'S').ne.0
       BH = Index(XOPT,'H').ne.0
       BK = Index(XOPT,'E').ne.0 .or. Index(XOPT,'I').ne.0
      Endif
*
*:  temperature loop
*
      If(NTMP.eq.1) Then
       DDTMP = 0.D0
      Else
       DDTMP = (DTMP2-DTMP1)/Dfloat(NTMP-1)
      Endif
      Do  N = 1,NTMP
       DTMP  = DTMP1 + Dfloat(N-1)*DDTMP
       DBETA = 1.D0/(DBLTZ*DTMP)
*
*:  vibrational entropy
*:  Helmholtz free energy
*
       If(BE) Then
        Call DTETR(DMX,DMY,DMDY,KD,DBETA,DETR(N),DDETR(N))
        Call DTFRE(DMX,DMY,DMDY,KD,DBETA,DFRE(N),DDFRE(N))
       Endif
*
*:  Lamb-Moessbauer factor
*
       If(BF) Then
        Call DTFLM(DMX,DMY,DMDY,KD,DBETA,DECOIL,DFLM(N),DDFLM(N))
       Endif
*
*:  specific heat
*
       If(BH) Then
        Call DTSPH(DMX,DMY,DMDY,KD,DBETA,DSPH(N),DDSPH(N))
       Endif
*
*:  kinetic energy
*
       If(BK) Then
        Call DTKIN(DMX,DMY,DMDY,KD,DBETA,DKIN(N),DDKIN(N))
       Endif
      EndDo
*
*:  excitation probability
*
      If(Index(XOPT,'S').ne.0) Then
       DBETA = 1.D0/(DBLTZ*DTMP1)
       Call SOFE(DENG1L,DENG2L,NENGL)
*
       If(NTMP.gt.1) Then
        Call SOFET(DENG1L,DENG2L,NENGL)
       Endif
*
       DENG1 = DENG1L
       DENG2 = DENG2L
       NENG  = NENGL
*
*:  send data to pipe
*
       If(BGRAPH) Then
        Do  J = 1,JMAX+1
         Call TOPIPE(J,NERR)
         Call ERRCHK(NERR,*200)
  200    Continue
        EndDo
        Call TOPIPE(-1,NERR)
       Endif
      Endif
*
*:  Greens function
*
      Call DOSFTS
*
*:  print data
*
      Call DTLMT(DMX,DMY,DMDY,KD,DECOIL,DLMT,DDLMT)
      Call DTAFC(DMX,DMY,DMDY,KD,DGAMMA,DECOIL,DAFC,DDAFC)
*
      DIAFC  = 3D-3*(DHQUER*DCLIC/DGAMMA)**2
     +              /(DELCH*DBLTZ*DLMT)
      DIAFC  = 1.D0/DIAFC
      DDIAFC = DIAFC*DDLMT/DLMT
*
      DLNBN = 0.25D-2*DECOIL/DELCH*(DHQUER*DCLIC/(DBLTZ*DGAMMA))**2
      If(NTMP.eq.1) Then
       DRMSD  = Dsqrt(-Dlog(DFLM(1)))
       DDRMSD = 0.5D0*DDFLM(1)/(DFLM(1)*DRMSD)
       DRMSD  = DRMSD*1.D9*DHQUER*DCLIC/DGAMMA
       DDRMSD = DDRMSD*1.D9*DHQUER*DCLIC/DGAMMA
       DLNB   = 3.D1*(DKIN(1)*DBETA - 0.5D0)
       DDLNB  = 3.D1*DDKIN(1)*DBETA
       DLNBH  = DLNBN*DAFC/DTMP1**2
       DDLNBH = DLNBN*DDAFC/DTMP1**2
       Write(LUNPTL,2006) '','',DFLM(1),DDFLM(1),DKIN(1),DDKIN(1),
     +               DAFC,DDAFC,DSPH(1),DDSPH(1),DETR(1),DDETR(1),
     +               DFRE(1),DDFRE(1),DIAFC,DDIAFC,DLMT,DDLMT,
     +               DLNB,DDLNB,DLNBH,DDLNBH,DRMSD,DDRMSD,
     +               DGFY(1),DGFDY(1)
       Write(6,2006) XT_MAG,XT_DEF,DFLM(1),DDFLM(1),DKIN(1),DDKIN(1),
     +               DAFC,DDAFC,DSPH(1),DDSPH(1),DETR(1),DDETR(1),
     +               DFRE(1),DDFRE(1),DIAFC,DDIAFC,DLMT,DDLMT,
     +               DLNB,DDLNB,DLNBH,DDLNBH,DRMSD,DDRMSD,
     +               DGFY(1),DGFDY(1)
      Else
       DLNBH  = 1.D-4*DLNBN*DAFC/9.D0
       DDLNBH = 1.D-4*DLNBN*DDAFC/9.D0
       Write(LUNPTL,2007) '','',DAFC,DDAFC,DIAFC,DDIAFC,
     +               DLMT,DDLMT,DLNBH,DDLNBH,DGFY(1),DGFDY(1)
       Write(6,2007) XT_MAG,XT_DEF,DAFC,DDAFC,DIAFC,DDIAFC,DLMT,DDLMT,
     +               DLNBH,DDLNBH,DGFY(1),DGFDY(1)
      Endif
*
 2006  Format(/2X,A,'Quantities calculated with the partial DOS -->',A/
     + 4X,'Lamb-Moessbauer factor',6X,':',F10.4,' +-',F9.4/
     + 4X,'kinetic energy',14X,':',F10.4,' +-',F9.4,' meV/atom'/
     + 4X,'mean force constant',9X,':',F10.4,' +-',F9.4,' N/m'/
     + 4X,'specific heat',15X,':',F10.4,' +-',F9.4,' k_B/atom'/
     + 4X,'entropy',21X,':',F10.4,' +-',F9.4,' k_B/atom'/
     + 4X,'free energy',17X,':',F10.4,' +-',F9.4,' meV/atom'/
     + 4X,'resilience',18X,':',F10.4,' +-',F9.4,' N/m'/
     + 4X,'Lamb-Moessbauer temperature :',F10.4,' +-',F9.4,' K'/
     + 4X,'isotope fractionation',7X,':',F10.4,' +-',F9.4,' perMille/%'/
     + 4X,'high T isotope frac.',8X,':',F10.4,' +-',F9.4,' perMille/%'/
     + 4X,'rms displacement            :',F10.4,' +-',F9.4,' pm'/
     + 4X,'impact displacement         :',F10.4,' +-',F9.4,' pm')
*
 2007  Format(/2X,A,'Quantities calculated with the partial DOS -->',A/
     + 4X,'mean force constant',9X,':',F10.4,' +-',F9.4,' N/m'/
     + 4X,'resilience',18X,':',F10.4,' +-',F9.4,' N/m'/
     + 4X,'Lamb-Moessbauer temperature :',F10.4,' +-',F9.4,' K'/
     + 4X,'high T isotope frac. coeff. :',F10.4,' +-',F9.4,
     +            ' (300K)^2*perMille/%'/
     + 4X,'impact displacement         :',F10.4,' +-',F9.4,' pm')
*
*:  list of created files
*
      If(Index(XOPT,',').eq.0) Then
       XEXT = '.dat'
      Else
       XEXT = '.csv'
      Endif
      L = LENX(XOUT,' ')
      Write(LUNPTL,2008) XOUT(1:L)//'_psth_ptl.txt'
 2008 Format(//2X,'Created files -->'/
     +       3X,'Protocol of calculations      : ',A)
*
*:  save data
*
      Call SAVDAT(NERR)
      Call ERRCHK(NERR,*999)
*
*:  close protocol file
*
      Close(UNIT=LUNPTL)
*
      GoTo 999
*
*:  I/O error
*
  998 Continue
      NERR  = 901
      NCOM1 = IERR
      XMSG1 = XFILE
      Call ERRCHK(NERR,*999)
*
*:  print elapsed time
*
  999 Continue
      Call TIMEC(RT,1,1,0)
      Write(6,2011) XT_CYN,RT,XT_DEF
 2011 Format(
     +  /A,' -- CPU time :  user ',F6.2,' s  system ',F6.2,' s'/
     +   ' -- PHOENIX module PSTH finished',A/)
*
*
      End
