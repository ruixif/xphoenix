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
*   Program PHOENIX module PADD
*
* purpose :  To add a collection of data sets of inelastic
*            nuclear resonance scattering
*            The central peak of each data set is fitted with
*            a modified Gaussian with linear background.
*            The angle scales are matched and converted to
*            an energy scale.
* date    :  April 8, 1996
* author  :  Wolfgang Sturhahn
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
#include<xterm.h>
*
      Character*10 XD,XT,XZ
      Character*72 XFILE,XDEF
*
      Dimension  RT(2),II(8),DCX(PDATA),DCY(PDATA),DCYL(PDATA),
     +           DCZ(PDATA),DCZL(PDATA),DCDY(PDATA),DCDYL(PDATA),
     +           DCDZ(PDATA),DCDZL(PDATA)
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
 2000 Format(A,' -- PHOENIX module PADD execution starting..',A/)
*
*:  obtain line arguments
*
      Call INSOPT(B,BGRAPH,BPRT,XDIR,XFILE)
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
      Open(UNIT=LUNINP,FILE=XFILE,STATUS='OLD', ERR=999)
*
*:  read the module specific input file
*
      Call LESPAD(NERR)
      Call ERRCHK(NERR,*999)
*
*:  close input file
*
      Close(LUNINP)
*
*:  read external resolution function
*
      If(BPEAK) Then
       Call LESXRF(NERR)
       Call ERRCHK(NERR,*999)
      Endif
*
*:  open protocol file and write header
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_padd_ptl.txt')
      Open(UNIT=LUNPTL,FILE=XFILE,IOSTAT=IERR,ERR=998)
*
      Call HEADER(LUNPTL,XVERSION,'padd ')
*
*:  print info
*
      Select Case (KINTPL)
       Case(0)
        XDEF = 'quadratic'
        L    = 9
       Case(1)
        XDEF = 'linear'
        L    = 6
       Case(2)
        XDEF = 'binning'
        L    = 7
       Case Default
        XDEF = ' '
        L    = 1
      End Select
*
      Write(6,2001) XT_MAG,XT_DEF,DOPENG,DTEMP,DENSHF,
     +               1.D3*(DENSCL-1.D0),DENQC,XDEF(1:L)
      Write(LUNPTL,2001) '','',DOPENG,DTEMP,DENSHF,
     +               1.D3*(DENSCL-1.D0),DENQC,XDEF(1:L)
*
 2001 Format(/2X,A,'Input parameters -->',A/
     +     4X,'Operating energy             :',F9.3,' keV'/
     +     4X,'Temperature of the material  :',F8.2,' K'/
     +     4X,'Energy shift, output data    :',F8.2,' meV'/
     +     4X,'Linear E-scale correction    :',F8.2,' 0.1%'/
     +     4X,'Quadratic E-scale correction :',F8.2,' 1E-6/meV'/
     +     4X,'Interpolation method         :',2X,A/)
*
*
*:  loop over all data files
*
      BFIRST = .true.
      BNORM  = .false.
      DTOTAL(0) = 0.D0
      DMSIDE(0) = 0.D0
      Do  I = 1,NCL
*
*:  evaluate input options
*
*:   ICE  =  energy
*:   ICD  =  data
*:   ICA1 =  angle of 1. crystal
*:   ICA2 =  angle of 2. crystal
*:   ICC1 =  angle calibration of 1. crystal
*:   ICC2 =  angle calibration of 2. crystal
*:   ICT1 =  temperature of 1. crystal
*:   ICT2 =  temperature of 2. crystal
*:   ICT3 =  temperature of analyzer crystal
*:   ICN  =  normalization
*:   ICM  =  monitor counts
*:   ICP  =  data for elastic peak
*
*:   ICS  =  scan number, for some input formats only
*
*:   DBGRD =  constant data background
*
       ICM8 = ICM
       Call CEVAL(XMES(I),ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +                  ICT1,ICT2,ICT3,ICN,ICM,ICP,ICS,DBGRD)
*
       NICS(I) = ICS
       If(BFIRST) Then
        XFILE = XMES(I)
        IICC1 = ICC1
        IICC2 = ICC2
       Endif
*
*:  read data
*
       If(XINTYP(1:5).eq.'ASCII') Then
        Call FMTRD(I,ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +               ICT1,ICT2,ICT3,ICN,ICM,ICP,NERR)
       Else
        Call API(I,ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +             ICT1,ICT2,ICT3,ICN,ICM,ICP,ICS,NERR)
       Endif
*
       If(NERR.ne.0) Then
        Call ERRCHK(NERR,*100)
  100   Continue
        BDS(I) = .false.
        Cycle
       Else
        BDS(I) = .true.
       Endif
*
*:  monitor-count option has changed
*
       If(.not.BFIRST .and. ICM.ne.ICM8) Then
        NERR  = 801
        XMSG1 = XMES(I)
        If(ICS.ne.0) Then
         L  = LENX(XMES(I),' ')
         LL = Len(XMSG1)
         If(L+5.le.LL) Then
          Write(XMSG1(L+1:L+5),'(''#'',I4.4)') ICS
         Endif
        Endif
        Call ERRCHK(NERR,*999)
       Endif
*
       If(ICN+ICM.ne.0)  BNORM = .true.
*
*:  process the data, remove noise spikes
*
       Call PRODAT(I,ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +               ICT1,ICT2,ICT3,ICN,ICM,ICP,ICS)
*
*:  perform calibration of angles
*
       Call ANGCAL(I,ICC1,ICC2)
*
*:  perform normalization
*
       Call NRMDAT(I,ICN)
*
*:  create energy axis from angles
*:  perform temperature corrections
*:  apply energy scale corrections
*
       Call DTCORR(ICE,ICT1,ICT2,ICT3,NERR)
       Call ERRCHK(NERR,*999)
*
*:  write normalization function
*
       If(ICN.gt.0) Then
        Call SAVNRM(NERR)
        Call ERRCHK(NERR,*101)
  101   Continue
       Endif
*
*:  sort energy values, increasing values
*:  create equal binning of data
*
       MODE = 1
       If(KINTPL.eq.1) MODE = 5
       If(KINTPL.eq.2) MODE = 9
       Call DCOPY1(DMX,DCX,KD)
       Call DCOPY1(DMY,DCY,KD)
       Call DCOPY1(DMZ,DCZ,KD)
       Call DCOPY1(DMDY,DCDY,KD)
       Call DCOPY1(DMDZ,DCDZ,KD)
       Call BINEQ1(DCX,DCY,DMX,DMY,KD,KD,K,MODE)
       Call BINEQ1(DCX,DCZ,DMX,DMZ,KD,KD,K,MODE)
       Call BINEQ1(DCX,DCDY,DMX,DMDY,KD,KD,K,MODE)
       Call BINEQ1(DCX,DCDZ,DMX,DMDZ,KD,KD,K,MODE)
*
*:  quadratic interpolation may produce negative values
*:   (instead of zeroes) for counts
*
       If(KINTPL.eq.0) Then
        Call BINEQ1(DCX,DCY,DMX,DCYL,KD,KD,K,5)
        Call BINEQ1(DCX,DCZ,DMX,DCZL,KD,KD,K,5)
        Call BINEQ1(DCX,DCDY,DMX,DCDYL,KD,KD,K,5)
        Call BINEQ1(DCX,DCDZ,DMX,DCDZL,KD,KD,K,5)
*
        Do  K = 1,KD
         If(DMY(K).lt.0.D0)  DMY(K) = DCYL(K)
         If(DMZ(K).lt.0.D0)  DMZ(K) = DCZL(K)
         If(DMDY(K).lt.0.D0) DMDY(K) = DCDYL(K)
         If(DMDZ(K).lt.0.D0) DMDZ(K) = DCDZL(K)
        EndDo
       Endif
*
*:  handle monitor normalization
*
       If(ICM.ne.0) Then
        Call DCOPY1(DMM,DCDY,KD)
        Call BINEQ1(DCX,DCDY,DMX,DMM,KD,KD,K,MODE)
*
        DMONAV(I) = DNORM1(DMM,KD,K,4)/Dfloat(KD)
*
        If(DMM(1).eq.0.D0) Then
         DMY(1)  = DMY(1)/DMONAV(I)
         DMZ(1)  = DMZ(1)/DMONAV(I)
         DMDY(1) = DMDY(1)/DMONAV(I)**2
         DMDZ(1) = DMDZ(1)/DMONAV(I)**2
        Else
         DMY(1)  = DMY(1)/DMM(1)
         DMZ(1)  = DMZ(1)/DMM(1)
         DMDY(1) = DMDY(1)/DMM(1)**2
         DMDZ(1) = DMDZ(1)/DMM(1)**2
        Endif
*
        DFLC(I) = (DMM(1)/DMONAV(I) - 1.D0)**2
        Do  K = 2,KD
         If(DMM(K).eq.0.D0) Then
          DMY(K)  = DMY(K-1)
          DMZ(K)  = DMZ(K-1)
          DMDY(K) = DMDY(K-1)
          DMDZ(K) = DMDZ(K-1)
         Else
          DMY(K)  = DMY(K)/DMM(K)
          DMZ(K)  = DMZ(K)/DMM(K)
          DMDY(K) = DMDY(K)/DMM(K)**2
          DMDZ(K) = DMDZ(K)/DMM(K)**2
         Endif
         DFLC(I) = DFLC(I) + (DMM(K)/DMONAV(I) - 1.D0)**2
        EndDo
        DFLC(I) = 1.D2*Dsqrt(DFLC(I)/Dfloat(KD))
       Else
        DMONAV(I) = 1.D0
       Endif
*
       If(BPEAK) Then
*
*:  define control parameters for fit function
*
        DP(3) = DCRNGE
        DP(4) = DBGR
        DP(6) = DASM
        DP(7) = DPCF
*
        If(DP(4).lt.0.D0) Then
         NFIT = 4
        Else
         NFIT = 3
        Endif
*
*:  fit Gaussian to the elastic peak that determines
*:   zero energy, data field DMY(*)
*
        Call GFIT(I,NFIT,NERR)
        Call ERRCHK(NERR,*999)
*
        If(NERR.ne.0) Then
         Call ERRCHK(NERR,*102)
  102    Continue
         BDS(I) = .false.
         Cycle
        Endif
*
*:  retrace angle positions and temperatures of crystals
*:   for zero energy in first valid data set
*
*:  determine index of zero energy, KE0, from
*:   original energy scale (stored in DCX)
*
*:  fitted peak lies between DCX(KE0) and DCX(KE01)
*
        If(BFIRST) Then
         If(DCX(KD).gt.DCX(1)) Then
          KE0 = 1
          Do While (DCX(KE0).lt.DP(3) .and. KE0.le.KD)
           KE0 = KE0 + 1
          EndDo
*
          KE01 = KE0-1
          DW0  = (DP(3)-DCX(KE01))/(DCX(KE0)-DCX(KE01))
          DW1  = (DCX(KE0)-DP(3))/(DCX(KE0)-DCX(KE01))
         Else
          KE0 = KD
          Do While (DCX(KE0).lt.DP(3) .and. KE0.gt.0)
           KE0 = KE0 - 1
          EndDo
*
          KE01 = KE0+1
          DW0  = (DCX(KE01)-DP(3))/(DCX(KE01)-DCX(KE0))
          DW1  = (DP(3)-DCX(KE0))/(DCX(KE01)-DCX(KE0))
         Endif
*
         If(ICA1.ne.0) Then
          DA1E0 = DW0*DMA1(KE0) + DW1*DMA1(KE01)
         Else
          DA1E0 = 1.D66
         Endif
*
         If(ICA2.ne.0) Then
          DA2E0 = DW0*DMA2(KE0) + DW1*DMA2(KE01)
         Else
          DA2E0 = 1.D66
         Endif
*
         If(ICC1.ne.0) Then
          DC1E0 = DW0*DMC1(KE0) + DW1*DMC1(KE01)
         Endif
*
         If(ICC2.ne.0) Then
          DC2E0 = DW0*DMC2(KE0) + DW1*DMC2(KE01)
         Endif
*
         If(ICT1.ne.0) Then
          DT1E0 = DW0*DMT1(KE0) + DW1*DMT1(KE01)
         Else
          DT1E0 = 1.D66
         Endif
*
         If(ICT2.ne.0) Then
          DT2E0 = DW0*DMT2(KE0) + DW1*DMT2(KE01)
         Else
          DT2E0 = 1.D66
         Endif
*
         If(ICT3.ne.0) Then
          DT3E0 = DW0*DMT3(KE0) + DW1*DMT3(KE01)
         Else
          DT3E0 = 1.D66
         Endif
        Endif
*
        DENSHG = DENSHF - DP(3)
       Else
        DENSHG   = DENSHF
        XINFO(I) = XCC//
     +     '                     no fit to peak position'
       Endif
*
*:  store info, will be written to output file
*
       Call XMERGE(XINFO(I),'   '//XMES(I))
       If(ICS.ne.0) Then
        L = LENX(XINFO(I),' ')
        L = Min0(L,Len(XINFO(1))-6)
        Write(XINFO(I),'(A,'' #'',I4.4)') XINFO(I)(1:L),ICS
       Endif
*
*:  shift x-scale; store boundary values
*
       Do  K = 1,KD
        DMX(K) = DMX(K) + DENSHG
       EndDo
*
       DEVON(I) = DMX(1)
       DEBIS(I) = DMX(KD)
*
*:  analyze shifted spectrum
*
       Call ANASHF(I)
*
*:  write monitor counts
*
       If(ICM.gt.0) Then
        Call SAVMON(NERR)
        Call ERRCHK(NERR,*103)
  103   Continue
       Endif
*
*:  write shifted data
*
       Call SAVSHF(I,NERR)
       Call ERRCHK(NERR,*104)
  104  Continue
*
*:  create or update energy scale
*
       If(BFIRST) Then
        KDS = KD
        Do  K = 1,KDS
         DSX(K)  = DMX(K)
         DSY(K)  = DMZ(K)
         DSDY(K) = DMDZ(K)
        EndDo
       Else
*
*:  the energy scale of the recent data set may cover
*:   a smaller range than the prior permanent energy scale
*:  in this case the permanent energy scale may be reduced to overlap
*
        Call NEWXSC(ICM)
*
*:  interpolate the recent data at positions
*:   given by the permanent energy scale
*:  add the interpolated values to the existing data
*
        Call INTADD
       Endif
*
       BFIRST = .false.
      EndDo
*
*:  no valid data sets
*
      If(BFIRST) GoTo 999
*
*:  close file units
*
      Close(UNIT=LUNTMP+1)
      Close(UNIT=LUNTMP+2)
      Close(UNIT=LUNTMP+3)
*
*
*:  normalize added data (monitor mode only)
*
      If(ICM.ne.0) Then
       Do  K = 1,KDS
        N = 0
        Do  J = 1,NCL
         If(BDS(J) .and. DSX(K).ge.DEVON(J) .and.
     +                          DSX(K).le.DEBIS(J)) N = N + 1
        EndDo
        If(N.eq.0) Cycle
        DSY(K)  = DSY(K)/Dfloat(N)
        DSDY(K) = DSDY(K)/Dfloat(N)**2
       EndDo
*
       DMONSM = 0.D0
       Do  I = 1,NCL
        If(BDS(I))  DMONSM = DMONSM + DMONAV(I)
       EndDo
       Do  K = 1,KDS
        DSY(K)  = DSY(K)*DMONSM
        DSDY(K) = DSDY(K)*DMONSM**2
       EndDo
      Endif
*
*:  save the result of the summation
*
      Call SAVSUM(NERR)
      Call ERRCHK(NERR,*999)
*
*:  print results
*
      Call RESOUT(XFILE,IICC1,IICC2,ICM,BNORM)
*
*:  open fifo pipe and write data to it
*
      If(BGRAPH) Then
       Call TOPIPE(1,NERR)
       Call ERRCHK(NERR,*105)
       Call TOPIPE(-1,NERR)
  105  Continue
      Endif
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
      Write(6,2010) XT_CYN,RT,XT_DEF
 2010 Format(
     +  /A,' -- CPU time :  user ',F5.2,' s  system ',F4.2,' s'/
     +   ' -- PHOENIX module PADD finished',A/)
*
*
      End
