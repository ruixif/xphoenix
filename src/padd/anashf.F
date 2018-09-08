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
      Subroutine ANASHF(IDS)
*
* Purpose :  Calculate the detailed balance for data set;
*            first the energy scale will shifted such that E=0
*            amounts to a grid point; then the weighted
*            left/right difference will be calculated.
* Date    :  March 22, 2001
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DMONAV  = monitor normalization
*    KD      = number of data points
*    DMX(*)  = x-value of data
*    DMZ(*)  = y-value of data
*    DMDZ(*) = y-value errors
*    K1,K2   = indices defining the central range;
*               these values originate from the peak-fit
*               procedure and are undefined if BPEAK=.FALSE.
*
* export variables :
*    DTBH(IDS)  = detailed balance of data
*    DDTBH(IDS) = error of detailed balance
*    DNTBH(IDS) = normalization factor
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Dimension  DMXR(PDATA),DMYR(PDATA),DMDYR(PDATA),
     +           DPP(PFIT),DTT(PDATA)
*
*
*:  produce zero-energy on energy scale, i.e., DMXR(K0)=0
*
      MODE = 3
      If(KINTPL.eq.1) MODE = 7
      If(KINTPL.eq.2) MODE = 11
      Call BINEQ1(DMX,DMZ,DMXR,DMYR,KD,KD,K0,MODE)
      Call BINEQ1(DMX,DMDZ,DMXR,DMDYR,KD,KD,K0,MODE)
*
*:  zero-energy out of range
*
      If(K0.lt.1 .or. K0.gt.KD) Then
       DTBH(IDS)  = 0.D0
       DDTBH(IDS) = 0.D0
       DNTBH(IDS) = 0.D0
       GoTo 100
      Endif
*
*:  background correction
*
      Do  I = 1,KD
       DMYR(I) = DMYR(I) - DBGRD/DMONAV(IDS)
      EndDo
*
*:  produce estimates for central range
*
      If(.not.BPEAK) Then
       K1 = K0 - Idnint(4.D0/(DMXR(2)-DMXR(1)))
       K2 = K0 + Idnint(4.D0/(DMXR(2)-DMXR(1)))
*
*:  insufficient range
*
       If(K1.lt.1 .or. K1.gt.KD .or. K2.lt.1 .or. K2.gt.KD) Then
        DTBH(IDS)  = 0.D0
        DDTBH(IDS) = 0.D0
        DNTBH(IDS) = 0.D0
        GoTo 100
       Endif
      Endif
*
*:  detailed balance of the inelastic part of the data
*:   excluding the central part
*
      DBETA = 1.D0/(DBLTZ*DTEMP)
      If(DBETA*Dmax1(Dabs(DMXR(1)),DMXR(KD)).gt.15.D0) Then
       KB  = KD
      Else
       KB  = Min0(KD,2*K0-1)
      Endif
      DS1 = 0.D0
      DS2 = 0.D0
      DS3 = 0.D0
      DS4 = 0.D0
      DS5 = 0.D0
      DS6 = 0.D0
*
      Do  K = Max0(K0-K1,K2-K0),KB-K0
       KP = K0 + K
       KM = K0 - K
       If(DBETA*DMXR(KP).gt.5.D0) Exit
       DW   = DEXP(DBETA*DMXR(KP))
       DPLS = DMYR(KP)
       DMNS = DMYR(KM)
       D1   = DPLS - DW*DMNS
       D2   = DPLS + DW*DMNS
*
       DS1 = DS1 + D1
       DS2 = DS2 + D2
       DS3 = DS3 + DW*DMNS
       DS4 = DS4 + DMDYR(KP)
       DS5 = DS5 + DPLS
       DS6 = DS6 + DMDYR(KM)*DW**2
      EndDo
*
*:  obtain correction due to finite energy resolution
*
      If(BPEAK) Then
*
*:  1.moment of the normalized resolution function
*
       DPP(1) = 1.0
       DPP(2) = DP(2)
       DPP(3) = 0.0
       DPP(6) = DP(6)
       DPP(7) = DP(7)
*
       DMR0 = 0.0
       Do  K = 1,KD
        DTT(K) = DTH(-1,DMX(K),DBETA,DPP,DRRX,DRRY,KRD,BREXT)
        DMR0 = DMR0 + DTT(K)
       EndDo
*
       DMR1  = 0.0
       Do  K = 1,KD
        DMR1 = DMR1 + DTT(K)*DMX(K)
       EndDo
       DMR1 = DMR1/DMR0
*
       DS0 = DTH(-2,0.D0,DBETA,DP,DRRX,DRRY,KRD,BREXT)/(DMX(2)-DMX(1))
       DS1 = DS1 - DMR1*DS0
      Endif
*
      If(DS2.eq.0.D0) Then
       DTB  = 0.D0
       DDTB = 0.9999D0
      ELSE
       DTB  = DS1/DS2
       DDTB = 2.D0*Dsqrt(DS3**2*DS4 + DS5**2*DS6)/DS2**2
      Endif
*
      DTBH(IDS)  = DTB
      DDTBH(IDS) = DDTB
      DNTBH(IDS) = DS2
*
*
*:  total and max. inelastic counts; exclude peak region
*
  100 Continue
      KK = 0
      DTOTAL(IDS) =  0.D0
      DMSIDE(IDS) = -1.D10
      Do  K = 1,KD
       If(DMX(K).lt.-DRANGE .or. DMX(K).gt.DRANGE) Then
        KK = KK + 1
        DTOTAL(IDS) = DTOTAL(IDS) + DMZ(K)
        DMSIDE(IDS) = Dmax1(DMSIDE(IDS),DMZ(K))
       Endif
      EndDo
      DTOTAL(0) = DTOTAL(0) + Dfloat(KK)*DBGRD/DMONAV(IDS)
      DMSIDE(0) = DMSIDE(0) + DBGRD/DMONAV(IDS)
*
*
      End
