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
*   Program PHOENIX module PHOX
*
* purpose :  To calculate the PDOS and related physical
*            parameters from a NRIXS data set.
*            The elastic peak from data of inelastic
*            nuclear resonance scattering by fitting a modified
*            Gaussian with linear background to the central
*            region of the data.
*            The f-factor and the normalization are calculated
*            from the moments of the purely inelastic data.
*            The 1,2, and 3-phonon contributions and the PDOS
*            are derived assuming quasi-harmonic lattice
*            vibrations.
* date    :  January 7, 1995
* author  :  Wolfgang Sturhahn                                          
*                                                                       
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*                                                                       
#include<phoenix.h>
#include<phox.h>
#include<xterm.h>
*
      Character*1  X
      Character*4  XEXT
      Character*10 XD,XT,XZ
      Character*72 XFILE
*
      Dimension RT(2),II(8),DH(PDATA),DW(-PDATA/2:PDATA/2)
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
*:  start timer
*
      Call TIMEC(RT,1,1,-1)
*
*:  print copyright message
*
      Call COPYRD(6,XVERSION)
*
      Write(6,2000) XT_CYN,XT_DEF
 2000 Format(A,' -- PHOENIX module PHOX execution starting..',A/)
*
*:  obtain line arguments
*
      Call INSOPT(B,BGRAPH,BPRT,X,XFILE)
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
      Open(UNIT=LUNINP, FILE=XFILE, STATUS='OLD', ERR=998)
*
*:  read the module specific input file
*
      Call LESPHX(NERR)
      Call ERRCHK(NERR,*999)
      
      Write(6, 5000) DTEMPEND, DTEMPSTEP, XMES, XDBG
 5000 Format('----new parameters-----',F9.3,F9.3,A,A)

*:  close input file
*
      Close(LUNINP)
*
*:  read data
*
      Call LESDAT(NERR)
      Call ERRCHK(NERR,*999)
*
*:  linearize x-scale of data
*
      Call BINEQ1(DMX,DMY,DSX,DSY,KD,KD,K,1)
      Call BINEQ1(DMX,DMDY,DSX,DSDY,KD,KD,K,5)
      Call DCOPY1(DSX,DMX,KD)
      Call DCOPY1(DSY,DMY,KD)
      Call DCOPY1(DSDY,DMDY,KD)
*
*:  subtract background
*
      Call BGCORR(NERR)
      Call ERRCHK(NERR,*999)
*
*:  read external resolution function
*:  modify external resolution function
*
      Call LESXRF(NERR)
      Call ERRCHK(NERR,*999)
*
*:  open protocol file and write header
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_phox_ptl.txt')
      Open(UNIT=LUNPTL,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
      Call HEADER(LUNPTL,XVERSION,'phox ')
*
*:  print info
*
      Write(6,2001) XT_MAG,XT_DEF,
     +           DGAMMA,DECOIL,DTEMP,DBGRD,DFUDGE,DENSCL,DENQC
      Write(LUNPTL,2001) '','',
     +            DGAMMA,DECOIL,DTEMP,DBGRD,DFUDGE,DENSCL,DENQC
*
 2001 Format(/2X,A,'Input parameters -->',A/
     +     4X,'Nuclear transition energy    :',F9.3,' keV'/
     +     4X,'Recoil energy of free atom   :',F9.3,' meV'/
     +     4X,'Temperature of the material  :',F8.2,'  K'/
     +     4X,'Constant background          :',F8.2,'  cnts/channel'/
     +     4X,'Normalization correction     :',F8.2,'  %/100meV'/
     +     4X,'Linear E-scale correction    :',F8.2,'  0.1%'/
     +     4X,'Quadratic E-scale correction :',F8.2,'  1E-6/meV'/)
*
*:  start parameters for fit procedure
*
      DP(3) = DRNGE0
      DP(4) = DBGR
      DP(6) = DASM
      DP(7) = DPCF
*
      If(DP(4).LT.0.D0) Then
       NFIT = 4
      Else
       NFIT = 3
      Endif
*
      DBETA = 1.D0/(DBLTZ*DTEMP)
*
*:  fit resolution function to the elastic peak
*
      Call GFIT(NFIT,NERR)
      Call ERRCHK(NERR,*999)
*
*:  reproduce the resolution function
*:  remove the elastic peak from the data
*:  shift x-scale
*:  calculate ratio of elastic peak to inelastic part;
*:   provide limits at DBLIM for calculation of
*:   detailed balance and sound velocity
*
      KDBN = 0
      KDBP = 0
      BSET = .false.
      Do  K = 1,KD
       DRY(K) = DTH(-1,DMX(K),DBETA,DP,DRRX,DRRY,KRD,BREXT)
       DSY(K) = DMY(K) - DRY(K)
       DSX(K) = DMX(K) - DP(3)
       If(DSY(K).le.0.D0) Cycle
*
       If(DSX(K).lt.0.D0) Then
        If(DRY(K)/DSY(K).lt.DBLIM*0.01D0) KDBN = K
       ElseIf(.not.BSET) Then
         If(DRY(K)/DSY(K).ge.DBLIM*0.01D0) KDBP = K
         BSET = .true.
       Endif
      EndDo
      KDBP = KDBP + 1
*
      KDBN = Max0(KDBN,KMAX+5*(K1-KMAX))
      KDBP = Min0(KDBP,KMAX+5*(K2-KMAX))
*
*:  convolute elastic peak region with resolution function
*
      If(DCRNGE.ne.0.D0) Then
       KCR = Idnint(DCRNGE/(DSX(2)-DSX(1)))
       KCN = Max0(KDBN-KDBP,-KMAX+1,-KCR)
       KCP = Min0(KDBP-KDBN,KD-KMAX,KCR)
       Write(6,2002) DSX(KMAX+KCN),DSX(KMAX+KCP)
       Write(LUNPTL,2002) DSX(KMAX+KCN),DSX(KMAX+KCP)
 2002  Format(4X,'smoothing range (meV) : ',F7.3,' to',F7.3/)
*
       DWN = 0.D0
       Do  K = KCN,KCP
        DW(K) = DRY(K+KMAX)
        DWN   = DWN + DW(K)
       EndDo
*
       Do  K = KMAX+KCN, KMAX+KCP
        DH(K) = 0.D0
        Do  J = KCN,KCP
         DH(K) = DH(K) + DW(J)*DSY(K+J)
        EndDo
       EndDo
*
       Do  K = KMAX+KCN, KMAX+KCP
        DSY(K) = DH(K)/DWN
       EndDo
      Endif
*
*:  save resolution function
*
      If(Index(XOPT,'R').ne.0) Then
       Call SAVDAT('r',NERR)
       Call ERRCHK(NERR,*200)
  200  Continue
      Endif
*
*:  save data, elastic peak stripped
*
      If(Index(XOPT,'S').NE.0) Then
       Call SAVDAT('s',NERR)
       Call ERRCHK(NERR,*201)
  201  Continue
      Endif
*
*:  open fifo pipe and write data to it
*
      If(BGRAPH) Then
       Call TOPIPE(1,NERR)
       Call ERRCHK(NERR,*202)
  202  Continue
      Endif
*
*:  calculate the moments of data, resolution function, and
*:  derive the moments of the deconvoluted data
*
      Call MOMENT(3)
*
*:  reconstruct the area of the elastic peak
*
      DSCLE = DECOIL/DMS(1)
      DPEAK = 1.D0/DSCLE - DMS(0)
*
*:  relative peak defect, should be positive
*
      DEFECT = 1.D0 - DSCLE*DMD(0)
*
      If(DEFECT.lt.0.D0) Then
       NERR = 701
       Write(XMSG,'(1P,E10.3)') -1.D2*DEFECT
       Call ERRCHK(NERR,*203)
  203  Continue
      Endif
*
*:  calculate the f-factor
*
      DFFAC = DPEAK*DSCLE
*
*:  kinetic energy per MB-atom
*
      DKN  = 0.25/DECOIL
      DKIN = DMS(2) - DMS(1)*DECOIL
      DKIN = DKN*DKIN*DSCLE
*
*:  mean force constant
*
      DEN   = 1.D0/DECOIL
      DMPHE = DMS(3) - 3.0*DMS(2)*DECOIL + 2.0*DMS(1)*DECOIL**2
      DMPHE = DEN*DMPHE*DSCLE
*
*:  propagate uncertainty in zero energy
*
      DDFFAC = DDP(3)*(1.D0-DFFAC)**2/DECOIL
      DDKIN  = DDP(3)*(0.5D0 - 0.25D0*DMS(2)*DMS(0)/DMS(1)**2)
      DDMPHE = DDP(3)*(((3.D0*DECOIL*DMS(2)-DMS(3))*DMS(0)/DMS(1)+
     +                   3.D0*DMS(2))/DMS(1) - 6.D0*DECOIL)
*
*:  propagate the statistical errors
*
      DS1 = 0.0
      DS2 = 0.0
      DS3 = 0.0
      Do  K = 1,KD
       DA  = (DSX(K)-DMR(1))/DECOIL - 1.0
       DS1 = DS1 + DMDY(K)*(DA/DPEAK - (DA+1.0)*DSCLE)**2
       DS2 = DS2 + DMDY(K)*((DSX(K)-DECOIL)**2 -  DKIN/DKN)**2
       DS3 = DS3 + DMDY(K)*((DSX(K)-DECOIL)**3 - DMPHE/DEN)**2
      EndDo
*
*:  combine errors
*
      DDFFAC = Dsqrt(DS1*DFFAC**2 + DDFFAC**2)
      DDKIN  = Dsqrt(DS2*(DSCLE*DKN)**2 + DDKIN**2)
      DDMPHE = Dsqrt(DS3*(DSCLE*DEN)**2 + DDMPHE**2)
*
*:  units of mean force constant are N/m
*
      DEN    = 5.D2*DELCH*(DGAMMA/(DHQUER*DCLIC))**2/DECOIL
      DMPHE  = DEN*DMPHE
      DDMPHE = DEN*DDMPHE
*
      Write(LUNPTL,2003) '','',DFFAC,DDFFAC,DKIN,DDKIN,DMPHE,DDMPHE
      Write(6,2003) XT_MAG,XT_DEF,DFFAC,DDFFAC,DKIN,DDKIN,DMPHE,DDMPHE
*
 2003 Format(2X,A,'Quantities derived directly from the data -->',A/
     + 4X,'Lamb-Moessbauer factor : ',F8.4,'  +-',F8.4/
     + 4X,'kinetic energy / atom  : ',F8.4,'  +-',F8.4,' meV'/
     + 4X,'mean force constant    :', F9.4,'  +-',F8.4,' N/m'/)
*
*:  normalize data and errors of data
*
      DSCLE2 = DSCLE**2
      Do  K = 1,KD
       DSY(K)  = DSCLE*DSY(K)
       DSDY(K) = DSCLE2*DMDY(K)
      EndDo
*
*:  save data, elastic peak stripped, normalized
*
      If(Index(XOPT,'N').NE.0) Then
       Call SAVDAT('n',NERR)
       Call ERRCHK(NERR,*204)
  204  Continue
      Endif
*
      If(BDCNV) Then
       Write(6,2004) XT_MAG,XT_DEF,DDIFL,NF,XT_RED,XT_DEF
       Write(LUNPTL,2004) '','',DIFL,NF,'',''
      Else
       Write(6,2005) XT_MAG,XT_DEF,DIFL
       Write(LUNPTL,2005) '','',DIFL
      Endif
*
 2004 Format(2X,A,'Decomposition into n-phonon contributions ',
     +          '+ deconvolution -->',A/
     +  4X,'FFT high frequency cut off value :',F6.2/
     +  4X,'power coefficient for FFT-filter :',I3/
     +  4X,A,'WARNING :  error estimates will be unreliable.',A/)
 2005 Format(2X,A,'Decomposition into n-phonon contributions -->',A/
     +       4X,'FFT-filter smoothing :',F6.2,' meV'/)
*
*:  calculate 1,2,3-phonon contributions and DOS
*:  optionally deconvolute the data
*
      Call DCNV
*
*:  send data to pipe
*
      If(BGRAPH) Then
       Call TOPIPE(2,NERR)
       Call ERRCHK(NERR,*205)
  205  Continue
       Call TOPIPE(3,NERR)
       Call ERRCHK(NERR,*206)
  206  Continue
       Call TOPIPE(-1,NERR)
      Endif
*
*:  print and save detailed balance information
*
      If(Index(XOPT,'A').NE.0) Then
       Write(LUNPTL,2006) '','',-DEC0*1.D3,Dsqrt(DVEC0)*1.D3,DCHI0,
     +       -DEC1*1.D3,Dsqrt(DVEC1)*1.D3,-DEC2*DBETA*1.D6,
     +                       Dsqrt(DVEC2)*DBETA*1.D6,DCHI2
       Write(6,2006) XT_MAG,XT_DEF,-DEC0*1.D3,Dsqrt(DVEC0)*1.D3,DCHI0,
     +       -DEC1*1.D3,Dsqrt(DVEC1)*1.D3,-DEC2*DBETA*1.D6,
     +                       Dsqrt(DVEC2)*DBETA*1.D6,DCHI2
*
       Call SAVDAT('a',NERR)
       Call ERRCHK(NERR,*207)
  207  Continue
      Endif
*
 2006 Format(2X,A,'Corrections to the energy scale -->',A/
     +  4X,'polynomial :  C1*[energy/1000] + C2*[energy/1000]^2'/
     +  6X,'type of fit',8X,'C1',10X,'C2/(meV^-1)',5X,'Chi^2'/
     +  6X,'  linear  ',F8.2,' +-',F5.2,10X,'0',10X,F5.2/
     +  6X,' quadratic',F8.2,' +-',F5.2,F9.2,' +-',F5.2,4X,F5.2/)
*
*:  list of created files
*
      If(Index(XOPT,',').eq.0) Then
       XEXT = '.dat'
      Else
       XEXT = '.csv'
      Endif
      L = LENX(XOUT,' ')
      Write(LUNPTL,2007) XOUT(1:L)//'_phox_ptl.txt'
 2007 Format(/2X,'Created files -->'//
     +       3X,'Protocol of calculations      : ',A)
*
      L = L + 8
      If(Index(XOPT,'R').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_rfc'//XEXT)
       Write(LUNPTL,2008) XFILE(1:L)
 2008  Format(3X,'Fitted resolution function    : ',A)
      Endif
*
      If(Index(XOPT,'S').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_pst'//XEXT)
       Write(LUNPTL,2009) XFILE(1:L)
 2009  Format(3X,'Data without elastic peak     : ',A)
      Endif
*
      If(Index(XOPT,'N').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_psn'//XEXT)
       Write(LUNPTL,2010) XFILE(1:L)
 2010  Format(3X,'Norm. data w/out elastic peak : ',A)
      Endif
*
      If(Index(XOPT,'A').ne.0) Then
       XFILE = XOUT
       Call XMERGE(XFILE,'_asy'//XEXT)
       Write(LUNPTL,2011) XFILE(1:L)
 2011  Format(3X,'Thermal asymmetry             : ',A)
       XFILE = XOUT
       Call XMERGE(XFILE,'_asl'//XEXT)
       Write(LUNPTL,2012) XFILE(1:L)
 2012  Format(3X,'Thermal asymmetry, linear fit : ',A)
       XFILE = XOUT
       Call XMERGE(XFILE,'_asq'//XEXT)
       Write(LUNPTL,2013) XFILE(1:L)
 2013  Format(3X,'Thermal asymmetry, quadr. fit : ',A)
       XFILE = XOUT
       Call XMERGE(XFILE,'_eco'//XEXT)
       Write(LUNPTL,2014) XFILE(1:L)
 2014  Format(3X,'Correction to energy scale    : ',A)
      Endif
*
*:  save phonon contributions
*
      If(Index(XOPT,'1').NE.0) Then
       Call SAVDAT('1',NERR)
       Call ERRCHK(NERR,*208)
  208  Continue
       XFILE = XOUT
       Call XMERGE(XFILE,'_1ph'//XEXT)
       Write(LUNPTL,2015) XFILE(1:L)
 2015  Format(3X,'one-phonon contribution       : ',A)
      Endif
*
      If(Index(XOPT,'2').NE.0) Then
       Call SAVDAT('2',NERR)
       Call ERRCHK(NERR,*209)
  209  Continue
       XFILE = XOUT
       Call XMERGE(XFILE,'_2ph'//XEXT)
       Write(LUNPTL,2016) XFILE(1:L)
 2016  Format(3X,'two-phonon contribution       : ',A)
      Endif
*
      If(Index(XOPT,'3').NE.0) Then
       Call SAVDAT('3',NERR)
       Call ERRCHK(NERR,*210)
  210  Continue
       XFILE = XOUT
       Call XMERGE(XFILE,'_3ph'//XEXT)
       Write(LUNPTL,2017) XFILE(1:L)
 2017  Format(3X,'3+ phonon contribution        : ',A)
      Endif
*
*:  save phonon density of states
*
      Call SAVDAT('d',NERR)
      Call ERRCHK(NERR,*211)
  211 Continue
      XFILE = XOUT
      Call XMERGE(XFILE,'_dos'//XEXT)
      Write(LUNPTL,2018) XFILE(1:L)
 2018 Format(3X,'partial phonon DOS            : ',A)
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
      XMSG  = XFILE
      Call ERRCHK(NERR,*999)
*
*:  print elapsed time
*
  999 Continue
      Call TIMEC(RT,1,1,0)
      Write(6,2020) XT_CYN,RT,XT_DEF
 2020 Format(A,
     +   ' -- CPU time :  user ',F6.2,' s  system ',F6.2,' s'/
     +   ' -- PHOENIX module PHOX finished',A/)
*
*
      End
