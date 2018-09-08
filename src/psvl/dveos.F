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
      FUNCTION DVEOS(DP,DKP,MTYPE)
*
* purpose :  calculate volume from pressure using
*             various equations of state;
* date    :  December 2, 2004
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DP        = pressure divided by bulk modulus at zero pressure
*    MTYPE     = type of EoS to be used
*                  0 :  third-order Birch-Murnaghan
*                  1 :  logarithmic
*                  2 :  Vinet
*    DKP       = pressure derivative of bulk modulus
*
* export variables :
*    DVEOS     = volume divided by volume at zero pressure
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      PARAMETER(NIT = 20, DEPS = 1.D-12, DH = 1.D-5)
*
      DVEOS = 1.D0
      IF(DP.LE.1.D-5 .OR. DKP.LE.0.D0) RETURN
*
*:  start values
*
      DV =  0.8D0
      DF = 2.D0*DEPS
      I  = 0
*
*:  iteration
*
      Do While (I.LE.NIT .AND. DABS(DF).GT.DEPS)
       I   = I+1
       DD  = DPEOS(DV,DKP,MTYPE)
       DF  = DD - DP
       DDF = (DPEOS(DV+DH,DKP,MTYPE) - DD)/DH
*
       If(DF/DDF.GE.DV) Then
        DV = 0.8D0*DV
       Else
        DV  = DV - DF/DDF
       Endif
      EndDo
*
*:  result
*
      DVEOS = DV
*
*
      END
