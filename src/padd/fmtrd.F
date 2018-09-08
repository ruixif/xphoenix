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
      Subroutine FMTRD(IDS,ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +                     ICT1,ICT2,ICT3,ICN,ICM,ICP,NERR)
*
* Purpose :  Read columns of data from a formatted file.
*            The input file is assumed to consist of a set of columns.
*            The meaning of the columns was defined for each
*            input file.
* Date    :  November 6, 1997
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    IDS        = file number
*    ICE        = col. for energy
*    ICD        = col. for counts
*    ICA1       = col. for angle position of first crystal
*    ICA2       = col. for angle position of second crystal
*    ICC1       = col. for angle calibration of first crystal
*    ICC2       = col. for angle calibration of second crystal
*    ICT1       = col. for temperature of first crystal
*    ICT2       = col. for temperature of second crystal
*    ICT3       = col. for temperature of analyzer crystal
*    ICN        = col. to normalize counts
*    ICM        = col. for count monitor
*    ICP        = col. for elastic peak spectrum
*    XMES(IDS)  = name of input file no. <IDS>
*
*
* export variables :
*    NERR       = error code
*    DMZ(*)     = counts
*    DMX(*)     = energy, if ICE>0
*    DMA1(*)    = angle of 1. xtal, if ICA1>0
*    DMA2(*)    = angle of 2. xtal, if ICA2>0
*    DMC1(*)    = calibrated angle of 1. xtal, if ICC1>0
*    DMC2(*)    = calibrated angle of 2. xtal, if ICC2>0
*    DMT1(*)    = temperature of 1. xtal, if ICT1>0
*    DMT2(*)    = temperature of 2. xtal, if ICT2>0
*    DMT3(*)    = temperature of analyzer xtal, if ICT3>0
*    DMM(*)     = monitor, if ICM>0
*    DMN(*)     = normalization, if ICN>0
*    DMY(*)     = counts of elastic peak spectrum
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
*
      Character*1  XTAB
      Character*72 XP(PCMAX+1)
      Character*(PCLEN) XLINE
*
*
*:  error conditions
*
      NERR  = 0
      NCOM1 = IDS
      If(ICE.gt.PCMAX) Then
       NERR = 2501
       ICE  = 0
      Endif
*
      If(ICD.gt.PCMAX .or. ICD.eq.0) Then
       NERR = 2702
       Return
      Endif
*
      If(ICA1.gt.PCMAX) Then
       NERR = 2503
       ICA1 = 0
      Endif
*
      If(ICA2.gt.PCMAX) Then
       NERR = 2504
       ICA2 = 0
      Endif
*
      If(ICT1.gt.PCMAX) Then
       NERR = 2505
       ICT1 = 0
      Endif
*
      If(ICT2.gt.PCMAX) Then
       NERR = 2506
       ICT2 = 0
      Endif
*
      If(ICN.gt.PCMAX) Then
       NERR = 2507
       ICN  = 0
      Endif
*
      If(ICC1.gt.PCMAX) Then
       NERR = 2508
       NCOM1   = IDS
       Call PADERR(6,NERR)
       ICC1 = 0
      Endif
*
      If(ICC2.gt.PCMAX) Then
       NERR = 2509
       ICC2 = 0
      Endif
*
      If(ICP.gt.PCMAX) Then
       NERR = 2510
       NCOM1   = IDS
       Call PADERR(6,NERR)
       ICP = 0
      Endif
*
      If(ICT3.gt.PCMAX) Then
       NERR = 2511
       ICT3 = 0
      Endif
*
      If(ICM.gt.PCMAX) Then
       NERR = 2512
       ICM = 0
      Endif
*
      If(NERR.ne.0)  Call PADERR(6,NERR)
*
      If(ICE+ICA1+ICA2.eq.0) Then
       NERR  = 2703
       NCOM1 = IDS
       Return
      Endif
      ICC = Max0(ICE,ICD,ICA1,ICA2,ICC1,ICC2,ICT1,ICT2,ICT3,ICN,ICM,ICP)
*
*:  open data file
*
      Open(UNIT=LUNMES,FILE=XMES(IDS),STATUS='OLD',IOSTAT=IERR,ERR=999)
*
*:  read data
*
      KL = Len(XLINE)
      KD = 0
      XTAB = Char(9)
*
  200 Continue
       Read(LUNMES,'(A)',ERR=201,END=201) XLINE
       Call XADJ(XLINE,' ')
       Call XADJ(XLINE,XTAB)
       Call XADJ(XLINE,',')
*
*:  skip invalid lines
*
       If(XLINE(1:1).eq.' ') GoTo 200
       If(Index('+-0123456789.',XLINE(1:1)).eq.0) GoTo 200
*
*:  csv files
*
       If(Index(XLINE,',').ne.0) Then
        Do  K = 1,KL-1
         If(XLINE(K:K+1).eq.',,') Then
          XLINE(K+1:KL) = '0'//XLINE(K+1:KL-1)
         Endif
        EndDo
        If(XLINE(1:1).eq.',') Then
         XLINE = '0'//XLINE(1:KL-1)
        Endif
        Call XPARSE(XLINE,',',ICC+1,XP)
        GoTo 202
       Endif
*
*:  tab-separated-value files
*
       If(Index(XLINE,XTAB).ne.0) Then
        Call XPARSE(XLINE,XTAB,ICC+1,XP)
        GoTo 202
       Endif
*
*:  space-separated-value files; this is the default
*
       Call XPARSE(XLINE,' ',ICC+1,XP)
*
  202  Continue
       KD = KD + 1
       Read(XP(ICD),*,ERR=201,END=201) DMZ(KD)
*
       If(ICE .ne.0) Then
        Read(XP(ICE),*,ERR=201,END=201) DR
        DMX(KD) = DESCF*DR
       Endif
*
       If(ICA1.ne.0) Then
        Read(XP(ICA1),*,ERR=201,END=201) DR
        DMA1(KD) = DROTD1*DASCF1*DR
       Else
        DMA1(KD) = 0.0D0
       Endif
*
       If(ICA2.ne.0) Then
        Read(XP(ICA2),*,ERR=201,END=201) DR
        DMA2(KD) = DROTD2*DASCF2*DR
       Else
        DMA2(KD) = 0.0D0
       Endif
*
       If(ICC1.ne.0) Then
        Read(XP(ICC1),*,ERR=201,END=201) DR
        DMC1(KD) = DCSCF*DR
       Else
        DMC1(KD) = 0.0D0
       Endif
*
       If(ICC2.ne.0) Then
        Read(XP(ICC2),*,ERR=201,END=201) DR
        DMC2(KD) = DCSCF*DR
       Else
        DMC2(KD) = 0.0D0
       Endif
*
       If(ICT1.ne.0) Then
        Read(XP(ICT1),*,ERR=201,END=201) DR
        DMT1(KD) = DTSCF*DR
       Endif
*
       If(ICT2.ne.0) Then
        Read(XP(ICT2),*,ERR=201,END=201) DR
        DMT2(KD) = DTSCF*DR
       Endif
*
       If(ICT3.ne.0) Then
        Read(XP(ICT3),*,ERR=201,END=201) DR
        DMT3(KD) = DTSCF*DR
       Endif
*
       If(ICN.ne.0) Read(XP(ICN),*,ERR=201,END=201) DMN(KD)
       If(ICM.ne.0) Read(XP(ICM),*,ERR=201,END=201) DMM(KD)
*
       If(ICP.ne.0) Then
        Read(XP(ICP),*,ERR=201,END=201) DMY(KD)
       Else
        DMY(KD) = DMZ(KD)
       Endif
*
       If(KD.eq.PDATA) GoTo 201
      GOTO 200
*
  201 Continue
      Close(LUNMES)
*
      If(KD.lt.10) Then
       NERR  = 2704
       NCOM1 = IDS
      Endif
*
      Return
*
*:  IO error exit
*
  999 Continue
      NERR  = 2701
      NCOM1 = IDS
      NCOM2 = IERR
*
*
      End
