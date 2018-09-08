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
*    NW       = determines what to write
*    NENG     = index, energy range
*    DSX(*)   = energy scale
*    DSY(*,0) = phonon excitation probability
*    DSY(*,N) = N-phonon excitation probability
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<psth.h>
*
*
      NERR = 0
*
      If(NW.lt.1) Return
*
*:  write data
*
      Write(LUNPIP,'(A4)',ERR=998) 'psth'
      If(NW.eq.1) Then
       Write(LUNPIP,'(A4)') 'xy--'
       Write(LUNPIP,'(I5)') NENG
*
       Do 1  K = 1,NENG
        Write(LUNPIP,'(1P,2E13.5)') DSX(K),DSY(K,NW-1)
    1  Continue
      Else
       Write(LUNPIP,'(A4)') 't---'
       Write(LUNPIP,'(3I5)') NENG,JMAX,N
*
       Do 2  K = 1,NENG
        Write(LUNPIP,'(1PE13.5)') DSY(K,NW-1)
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
