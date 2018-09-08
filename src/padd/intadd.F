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
      Subroutine INTADD
*
* Purpose :  interpolate y-values for a given x-scale
*            add newly interpolated values to old ones
* Date    :  April 9, 1996
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DSX(*)   = old x-scale, establishes the reference scale
*    DSY(*)   = old y-values
*    DSDY(*)  = old y-errors
*    DMX(*)   = x-scale of recent data
*    DMZ(*)   = y-values of recent data
*    DMDZ(*)  = y-errors of recent data
*    KD       = number of data points on recent scale
*    KDS      = number of data points on ref.scale
*    KINTPL      = interpolation scheme used for adding data sets
*                   0 :  quadratic interpolation (default)
*                   1 :  linear interpolation
*                   2 :  binning, bin size given by first data set
*
* export variables :
*    DSY(*)   = sum of old y-values and newly interpolated values
*    DSDY(*)  = sum of old y-errors and recent y-errors, no interpl.
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
*
      Select Case (KINTPL)
       Case (0)
*
*:  quadratic interpolation of DMY-values on the DSX-scale
*
        Do  K = 1,KDS
         If(DSX(K).LT.DMX(1) .OR. DSX(K).GT.DMX(KD)) Cycle
*
*:  evaluate index on DMX-scale corresponding to x-value on DSX-scale
*
         KE = 1
         Do  J = 1,KD
          If(DMX(J).ge.DSX(K)) Then
           KE = J
           Exit
          Endif
*
          If(DMX(KD-J+1).le.DSX(K)) Then
           KE = KD - J + 2
           Exit
          Endif
         EndDo
*
*:  exceptions, interpolate linearly
*
         If(KE.lt.2) Then
          DMYI = DSPLL(DSX(K),DMX(1),DMZ(1),DMX(2),DMZ(2))
         ElseIf(KE.gt.KD-1) Then
           DMYI = DSPLL(DSX(K),DMX(KD-1),DMZ(KD-1),DMX(KD),DMZ(KD))
          Else
*
*:  quadratic interpolation, forward and backward looking
*
           DMYI = DSPLQ( DSX(K),DMX(KE-1),DMZ(KE-1),
     +                          DMX(KE),  DMZ(KE),
     +                          DMX(KE+1),DMZ(KE+1) )
*
           If(KE.gt.2) Then
            DMYI = 0.5D0*(DMYI + DSPLQ( DSX(K),DMX(KE-2),DMZ(KE-2),
     +                                         DMX(KE-1),DMZ(KE-1),
     +                                         DMX(KE),DMZ(KE) ))
           Endif
*
*:  negative results may occur for low statistical quality of data
*:  in such cases resort to linear interpolation
*
           If(DMYI.lt.0.D0) Then
            DMYI = DSPLL(DSX(K),DMX(KE-1),DMZ(KE-1),DMX(KE),DMZ(KE))
           Endif
         Endif
*
*:  add interpolated value to old y-value
*
         DSY(K)  = DSY(K)  + DMYI
         DSDY(K) = DSDY(K) + DMDZ(KE)
        EndDo
*
*:  linear interpolation
*
       Case (1)
        Do  K = 1,KDS
         If(DSX(K).lt.DMX(1) .or. DSX(K).gt.DMX(KD)) Cycle
*
*:  evaluate index on DMX-scale corresponding to x-value on DSX-scale
*
         KE = 1
         Do  J = 1,KD
          If(DMX(J).ge.DSX(K)) Then
           KE = J
           Exit
          Endif
*
          If(DMX(KD-J+1).le.DSX(K)) Then
           KE = KD - J + 2
           Exit
          Endif
         EndDo
*
*:  interpolate
*
         If(KE.lt.2) Then
          DMYI = DSPLL(DSX(K),DMX(1),DMZ(1),DMX(2),DMZ(2))
         Else
          DMYI = DSPLL(DSX(K),DMX(KE-1),DMZ(KE-1),DMX(KE),DMZ(KE))
         Endif
*
*:  add interpolated value to old y-value
*
         DSY(K)  = DSY(K) + DMYI
         DSDY(K) = DSDY(K) + DMDZ(KE)
        EndDo
*
*:  binning
*
       Case (2)
        DX = (DSX(KDS) - DSX(1))/DFLOAT(KDS-1)
        Do  K = 1,KDS
*
*:  bins are symmetric around values on DSX-scale
*
         DMYI = 0.D0
         Do  J = 1,KD
          DX1 = DSX(K) - 0.5D0*Dabs(DX)
          DX2 = DSX(K) + 0.5D0*Dabs(DX)
          If(DMX(J).ge.DX1 .and. DMX(J).lt.DX2) Then
           DSY(K)  = DSY(K)  + DMY(J)
           DSDY(K) = DSDY(K) + DMDY(J)
          Endif
         EndDo
        EndDo
*
      End Select
*
*
      End
