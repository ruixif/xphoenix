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
      SUBROUTINE PUTDSP(NW,NERR)
*
*
* purpose :  write data to fifo pipe for pdsp
* date    :  August 16, 2009
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NW      = determines what to write
*    KD      = index, data range
*    KPD1    = index for E=0
*    DSX(*)  = energy scale
*    DSY(*)  = measured data, peak subtracted
*    DSDY(*) = error**2 of measured data
*    DCY(*)  = DOS
*    DCDY(*) = error of DOS
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
*
      NERR = 0
*
      If(NW.lt.1) Return
*
      If(NW.eq.1) Then
       DXMIN = -(DSX(KD)-DSX(1))/6.D0
       DXMAX =  (DSX(KD)-DSX(1))/6.D0
       KK    = 0
       Do 1  K = 1,KD
        If(DSX(K).ge.DXMIN .and. DSX(K).le.DXMAX)  KK = KK + 1
    1  Continue
*
*:  write measured data
*
       Write(LUNPIP,'(A4)',ERR=998) 'phox'
       Write(LUNPIP,'(A4)') 'xydy'
       Write(LUNPIP,'(2I5)') KK,KK
*
       Do 2  K = 1,KD
        If(DSX(K).lt.DXMIN .or. DSX(K).gt.DXMAX) GoTo 2
        Write(LUNPIP,'(1P,3E13.5)') DSX(K),DSY(K),Dsqrt(DSDY(K))
    2  Continue
      Endif
*
*
      If(NW.eq.2) Then
*
*:  write DOS
*
       KM = KD-KPD1+1
       Write(LUNPIP,'(A4)') 'xydy'
       Write(LUNPIP,'(2I5)') KM,KM
*
       Do 3  K = 1,KM
        Write(LUNPIP,'(1P,3E13.5)') DSX(K+KPD1-1),DCY(K),DCDY(K)
    3  Continue
      Endif
*
*
      If(NW.eq.3) Then
*
*:  write integrated DOS
*
       KM = KD-KPD1+1
       Write(LUNPIP,'(A4)') 'xydy'
       Write(LUNPIP,'(2I5)') KM,KM
*
       DOSUM  = 0.D0
       DDOSUM = 0.D0
       Do 4  K = 1,KM
        Write(LUNPIP,'(1P,3E13.5)') DSX(K+KPD1-1),
     +                DOSUM,Dsqrt(DDOSUM)
        DOSUM  = DOSUM + DCY(K)
        DDOSUM = DDOSUM + DCDY(K)**2
    4  Continue
      Endif
*
*:  display
*
      Write(LUNPIP,'(A4)') 'show'
      Write(LUNPIP,'(I4)') NW
      Return
*
  998 Continue
      NERR = 1
*
*
      END
