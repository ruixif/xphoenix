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
      FUNCTION DIVERF(DX)
*
* Purpose :  calculate the inverse error function;
* Date    :  October 21, 2005
* Author  :  Wolfgang Sturhahn
*
* import variables :
*     DX  = argument with abs(DX)<1
*
* export variables :
*     DIVERF  = value of inverse error function
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Parameter(DA1 = 0.62146D0, DA2 = 0.7734D0, DFAC = 1.1283792D0,
     +           DEPS = 1.D-8, NIT = 20)
*
*
      DIVERF = Dsign(6.D0,DX)
      DAX    = Dabs(DX)
*
*:  values of DAX very close to unity
*
      If(DAX.GE.ERF(5.99D0)) Return
*
*:  start value
*
      DZ = Dmin1(DA1*(-Dlog(1.D0-DAX))**DA2,3.D0)
*
*:  refine value by iteration
*
      DF = 2.D0*DEPS
      I  = 0
      Do While (I.LE.NIT .AND. Dabs(DF).GT.DEPS .AND. DZ.LT.6.D0)
       I   = I+1
       DF  = ERF(DZ) - DAX
       DDF = DFAC*Dexp(-DZ**2)
       DZ  = DZ - DF/DDF
       DZ  = Dmin1(6.D0,Dabs(DZ))
      EndDo
*
      DIVERF = Dsign(DZ,DX)
*
*
      END
