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
      Subroutine ANGCAL(IDS,ICC1,ICC2)
*
* Purpose :  Calibrate angle scales
* Date    :  October 28, 2016
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    IDS        = sequence number
*    ICC1       = col. for angle calibration of first crystal
*    ICC2       = col. for angle calibration of second crystal
*    DMA1(*)    = angle of 1. xtal, if ICA1>0
*    DMA2(*)    = angle of 2. xtal, if ICA2>0
*    DMC1(*)    = calibrated angle of 1. xtal, if ICC1>0
*    DMC2(*)    = calibrated angle of 2. xtal, if ICC2>0
*
* export variables :
*    DMA1(*)    = calibrated angle of 1. xtal, if ICA1>0
*    DMA2(*)    = calibrated angle of 2. xtal, if ICA2>0
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
*
*:  calibrate angle of 1.xtal
*
      If(ICC1.ne.0) Then
       DAO = DMA1(KD/2)
       DCO = DMC1(KD/2)
       DS1 = 0.D0
       DS2 = 0.D0
       DS3 = 0.D0
       DS4 = 0.D0
       Do  K = 1,KD
        DXX = DMA1(K) - DAO
        DYY = DMC1(K) - DCO
        DS1 = DS1 + DXX*DXX
        DS2 = DS2 + DXX*DYY
        DS3 = DS3 + DXX
        DS4 = DS4 + DYY
       EndDo
       DCAL = Dabs( (DS2 - DS3*DS4/Dfloat(KD))/
     +              (DS1 - DS3*DS3/Dfloat(KD)) )
*
       If(Dabs(DCAL-1.D0).gt.0.2D0) Then
        NERR  = 11701
        NCOM1 = IDS
        Write(XMSG1,'(1PD10.3)') DCAL
        CALL ERRCHK(NERR,*100)
  100   Continue
       Else
*
        Do  K = 1,KD
         DMA1(K) = DCAL*DMA1(K)
        EndDo
       Endif
      Endif
*
*:  calibrate angle of 2.xtal
*
      If(ICC2.ne.0) Then
       DAO = DMA2(KD/2)
       DCO = DMC2(KD/2)
       DS1 = 0.D0
       DS2 = 0.D0
       DS3 = 0.D0
       DS4 = 0.D0
       Do  K = 1,KD
        DXX = DMA2(K) - DAO
        DYY = DMC2(K) - DCO
        DS1 = DS1 + DXX*DXX
        DS2 = DS2 + DXX*DYY
        DS3 = DS3 + DXX
        DS4 = DS4 + DYY
       EndDo
       DCAL = Dabs( (DS2 - DS3*DS4/Dfloat(KD))/
     +              (DS1 - DS3*DS3/Dfloat(KD)) )
*
       If(Dabs(DCAL-1.D0).gt.0.2D0) Then
        NERR  = 11702
        NCOM1 = IDS
        Write(XMSG1,'(1PD10.3)') DCAL
        CALL ERRCHK(NERR,*101)
  101   Continue
       Else
*
        Do  K = 1,KD
         DMA2(K) = DCAL*DMA2(K)
        EndDo
       Endif
      Endif
*
*
      End
