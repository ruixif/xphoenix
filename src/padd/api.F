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
      Subroutine API(IDS,ICE,ICD,ICA1,ICA2,ICC1,ICC2,
     +               ICT1,ICT2,ICT3,ICN,ICM,ICP,ICS,NERR)
*
* Purpose :  Read columns of data using a plugin.
*            The plugin must provide an ASCII formatted file
*            consisting of a set of data value columns.
*            The meaning of the columns was defined for each
*            input file.
* Date    :  March 29, 2012
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
*    ICS        = scan number
*    XPLGIN     = name of the plugin
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
      Character*72  X,XTMPF
      Character*500 XCMD
*
      Dimension DR(PDATA,PCMAX),IC(PCMAX),ICR(PCMAX),IX(PCMAX)
*
*
*:  reset errors
*
      NERR = 0
*
*:  maintain order of col definitions
*
      IC(1)  = ICE
      IC(2)  = ICD
      IC(3)  = ICA1
      IC(4)  = ICA2
      IC(5)  = ICC1
      IC(6)  = ICC2
      IC(7)  = ICT1
      IC(8)  = ICT2
      IC(9)  = ICT3
      IC(10) = ICN
      IC(11) = ICM
      IC(12) = ICP
      NCMAX  = 12
*
*:  error conditions
*
      NCOM1 = IDS
      NCOM2 = ICS
      If(ICD.gt.PCMAX .or. ICD.eq.0) Then
       NERR  = 3702
       Return
      Endif
*
      Do  N = 1,NCMAX
       If(IC(N).gt.PCMAX) Then
        NERR  = 3500 + N
        CALL PADERR(6,NERR)
        IC(N) = 0
       Endif
      EndDo
*
      If(ICE+ICA1+ICA2.eq.0) Then
       NERR = 3703
       Return
      Endif
*
*
*: create command to execute plugin
*
      XCMD = XPLGIN
      Call XMERGE(XCMD,' '//XMES(IDS))
*
*:  name of output file
*
      Write(XTMPF,'(''tmp'',I8.8,''-'',I3.3,''.dat'')') GETPID(),IDS
      Call XMERGE(XCMD,' '//XTMPF)
*
*:  scan number for plugins that need it
*
      Write(X,*) ICS
      Call XADJ(X,' ')
      Call XMERGE(XCMD,' '//X)
*
*:  requested data column numbers
*
      NCOLS = 0
      Do  N = 1,NCMAX
       If(IC(N).ne.0) Then
        Write(X,*) IC(N)
        Call XADJ(X,' ')
        Call XMERGE(XCMD,' '//X)
        NCOLS = NCOLS + 1
       Endif
      EndDo
*
*:  call plugin
*
      Call SYSTEM(XCMD)
*
*:  open converted data file
*
      Open(UNIT=LUNMES,FILE=XTMPF,STATUS='OLD',IOSTAT=IERR,ERR=999)
*
*:  read first line;
*:  unavailable col numbers are returned as zero
*
      Do  K = 1,NCMAX
       ICR(K) = 0
      EndDo
*
      Read(LUNMES,*) (ICR(K),K=1,NCOLS)
*
*:  report requested cols that were not returned
*
      NCRQ  = 0
      NCR   = 0
      Do  N = 1,NCMAX
       If(IC(N).eq.0)  Cycle
*
       NCRQ = NCRQ + 1
       If(ICR(NCRQ).ne.0)  Then
        NCR = NCR + 1
        IX(NCR) = N
       Endif
*
       If(IC(N).ne.0 .and. ICR(NCRQ).eq.0) Then
        IC(N) = 0
        If(N.eq.2) Then
         NERR = 3704
         Return
        Else
         NERR = 3512 + N
         Call PADERR(6,NERR)
        Endif
       Endif
      EndDo
*
*:  read data
*
      KD = 0
  200 Continue
       KD = KD + 1
       If(KD.gt.PDATA) GoTo 201
       Read(LUNMES,*,ERR=201,END=201) (DR(KD,J),J=1,NCR)
      GoTo 200
*
  201 Continue
      KD = KD - 1
      Close(LUNMES,Status='delete')
*
      If(KD.lt.10) Then
       NERR = 3705
       Return
      Endif
*
*:  assign data to appropriate arrays
*
      Do  K = 1,KD
       DMA1(K) = 0.0D0
       DMA2(K) = 0.0D0
       DMC1(K) = 0.0D0
       DMC2(K) = 0.0D0
      EndDo
*
      Do  J = 1,NCR
       If(IX(J).eq.1)  Then
        Do  K = 1,KD
         DMX(K) = 1.D-6*DESCF*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.2)  Then
        Do  K = 1,KD
         DMZ(K) = DR(K,J)
         DMY(K) = DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.3)  Then
        Do  K = 1,KD
         DMA1(K) = DROTD1*DASCF1*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.4)  Then
        Do  K = 1,KD
         DMA2(K) = DROTD2*DASCF2*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.5)  Then
        Do  K = 1,KD
         DMC1(K) = DCSCF*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.6)  Then
        Do  K = 1,KD
         DMC2(K) = DCSCF*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.7)  Then
        Do  K = 1,KD
         DMT1(K) = DTSCF*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.8)  Then
        Do  K = 1,KD
         DMT2(K) = DTSCF*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.9)  Then
        Do  K = 1,KD
         DMT3(K) = DTSCF*DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.10)  Then
        Do  K = 1,KD
         DMN(K) = DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.11)  Then
        Do  K = 1,KD
         DMM(K) = DR(K,J)
        EndDo
       Endif
*
       If(IX(J).eq.12)  Then
        Do  K = 1,KD
         DMY(K) = DR(K,J)
        EndDo
       Endif
      EndDo
*
*:  update col definitions
*
      ICE  = IC(1)
      ICD  = IC(2)
      ICA1 = IC(3)
      ICA2 = IC(4)
      ICC1 = IC(5)
      ICC2 = IC(6)
      ICT1 = IC(7)
      ICT2 = IC(8)
      ICT3 = IC(9)
      ICN  = IC(10)
      ICM  = IC(11)
      ICP  = IC(12)
*
*
      Return
*
*:  IO error exit
*
  999 Continue
      NERR = 3701
*
*
      End
