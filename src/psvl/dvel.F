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
      SUBROUTINE DVEL(DVB,DVD,DVP,DVS)
*
* purpose :  calculate shear and pressure wave velocties from
*             bulk sound velocity and Debye sound velocity
* date    :  June 12, 2003
* author  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      PARAMETER(D23 = 2.D0/3.D0, D43 = 4.D0/3.D0, D53 = 5.D0/3.D0,
     +          NIT = 20, DEPS = 1.D-10)
*
*
      DVP  = 0.D0
      DVS  = 0.D0
*
      IF(DVB.LE.0.D0 .OR. DVD.LE.0.D0) RETURN
*
      DETA0 = (DVB/DVD)**2
*
*:  start values
*
      DXI = Dmax1((0.952D0 - 0.041D0*DVB/DVD)**2,D23**D23) + 1.D-3
      DF  = 2.D0*DEPS
      I   = 0
*
*:  start iteration
*
      Do While (I.LE.NIT .AND. DABS(DF).GT.DEPS)
       I   = I+1
       DH  = DABS(DXI)**1.5D0
       DF  = D43*DXI + DETA0 - 1.D0/(DABS(3.D0 - 2.D0/DH))**D23
*
       DHH = 3.D0*DH - 2.D0
       DDF = D43 + 2.D0/Dsign(DABS(DHH)**D53,DHH)
*
       DXI = DXI - DF/DDF
      EndDo
*
*:  results
*
      IF(DXI.LT.0.D0) THEN
       DVS = 0.D0
       DVP = 0.D0
      ELSE
       DVS = DVD*DSQRT(DXI)
       DVP = DVD*DSQRT(D43*DXI + DETA0)
      ENDIF
*
*
      END
