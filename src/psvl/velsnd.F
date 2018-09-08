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
      Subroutine VELSND(DSV,DDSV,DCHI,DQVAL,BCONV)
*
*
* purpose :  calculate Debye sound velocities
* date    :  November 10, 2005
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    K1      = index, beginning of fit range
*    K2      = index, end of fit range
*    DSX(*)  = energy scale
*    DSY(*)  = Debye sound velocities
*    DSDY(*) = error**2 of Debye sound velocities
*    BGRAPH  = set to .FALSE. if "--NOGRAPH"  option present
*
* export variables :
*    DSV   = fitted Debye sound velocity
*    DDSV  = error of velocity
*    DCHI  = chi^2 value of fit
*    BCONV = .TRUE. if fit has converged
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psvl.h>
*
      Dimension BVALID(PDATA),DP8(PFIT)
*
      Data BFIRST /.true./
*
      Save DP8
*
*
      DSUM  = 0.D0
      DDSUM = 0.D0
      Do  K = 1,KD
       IF(K.ge.K1 .and. K.le.K2) Then
        If(DSY(K).gt.0.D0) Then
         BVALID(K) = .true.
         DSUM  = DSUM  + DSY(K)**2
         DDSUM = DDSUM + DSDY(K)
        Else
         BVALID(K) = .false.
        Endif
       Else
        BVALID(K) = .false.
       Endif
      EndDo
*
*:  statistical quality of fitted data range
*
      DQVAL = Dsqrt(DDSUM/DSUM/Dfloat(K2-K1+1))
*
*:  start values and bounds for iteration
*
      If(BFIRST) Then
       DP(1) = DSY(K1)
      Else
       DP(1) = DP8(1)
      Endif
      BFIT(1)   = .true.
      DPMIN(1)  = 0.3D0*DP(1)
      DPMAX(1)  = 2.0D0*DP(1)
      DDPMAX(1) = 0.1D0
*
      Read(XOPT1,*) DP(2)
      If(Index(XOPT1,'FIT').eq.0) Then
       BFIT(2)   = .false.
       DPMIN(2)  = 0.D0
       DPMAX(2)  = 0.D0
       DDPMAX(2) = 0.D0
      Else
       If(.not.BFIRST) Then
        DP(2) = DP8(2)
       Endif
       BFIT(2)   = .true.
       DPMIN(2)  = 1.D-4
       DPMAX(2)  = 1.D2
       DDPMAX(2) = 0.2D0
      EndIf
*
      Read(XOPT2,*) DP(3)
      If(Index(XOPT2,'FIT').eq.0) Then
       BFIT(3)   = .false.
       DPMIN(3)  = 0.D0
       DPMAX(3)  = 0.D0
       DDPMAX(3) = 0.D0
      Else
       If(.not.BFIRST) Then
        DP(3) = DP8(3)
       Endif
       BFIT(3)   = .true.
       DPMIN(3)  = 2.D-1
       DPMAX(3)  = 2.D1
       DDPMAX(3) = 0.2D0
      EndIf
*
      Call VFIT(PDATA,KD,DSX,DSY,DSDY,BVALID,
     +           0,PFIT,DP,DPMIN,DPMAX,DDPMAX,DDP,BFIT,
     +           100,1.D-3,LUNINP,DCHI,NERR)
*
      DSV  = DP(1)
      DDSV = DDP(1)
      If(NERR.ne.0) Then
       BCONV = .false.
      Else
       BCONV  = .true.
       DP8(1) = DP(1)
       DP8(2) = DP(2)
       DP8(3) = DP(3)
      Endif
*
      BFIRST = .false.
*
*
      End
