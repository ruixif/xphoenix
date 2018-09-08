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
*    NW        = determines what to write
*    KDS       = index, data range
*    DSX(*)    = energy scale
*    DSY(*)    = added data
*    DSDY(*)   = error**2 of added data
*    DRANGE    = energy range for central peak
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
*
      NERR = 0
*
      If(NW.lt.1) Return
*
      If(NW.eq.1) Then
       K1 = 0
       Do 1  K = 1,KDS
        If(DSX(K).ge.DRANGE) Then
         K1 = K
         GoTo 100
        Endif
    1  Continue
  100  Continue
*
*:  write added data
*
       Write(LUNPIP,'(A4)',ERR=998) 'padd'
       Write(LUNPIP,'(A4)') 'xydy'
       Write(LUNPIP,'(2I5)') KDS,K1
*
       Do 2  K = 1,KDS
        Write(LUNPIP,'(1P,3E13.5)') DSX(K),DSY(K),Dsqrt(DSDY(K))
    2  Continue
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
