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
*    KVEL    = index, data range
*    KTVEL1  = index, beginning of fit range
*    KTVEL2  = index, end of fit range
*    DSX(*)  = energy scale
*    DSY(*)  = Debye sound velocities
*    DSDY(*) = error**2 of Debye sound velocities
*    DTHR(*) = theory function
*    DSY(PDATA-2) = error of fitted Debye sound velocity
*    DSY(PDATA-1) = chi^2 value of fit
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psvl.h>
*
*
      NERR = 0
*
      If(NW.lt.1) Return
*
      If(NW.eq.1) Then
*
*:  write measured data
*
       Write(LUNPIP,'(A4)',ERR=998) 'psvl'
       Write(LUNPIP,'(A4)') 'xydy'
       Write(LUNPIP,'(2I5)') KVEL,K1
*
       Do 1  K = 1,KVEL
        Write(LUNPIP,'(1P,3E13.5)') DSX(K),DSY(K),Dsqrt(DSDY(K))
    1  Continue
      Else
*
*:  write theory data
*
       Write(LUNPIP,'(A4)') 't---'
       Write(LUNPIP,'(3I5)') KIX(NW-1)+2,K1,NWW
*
       Do  K = 1,KIX(NW-1)
        Write(LUNPIP,'(1P,E13.5)') DTTY(K,NW-1)
       EndDo
       Write(LUNPIP,'(1P,E13.5)') DDSVT(NW-1),DCHIT(NW-1)
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
