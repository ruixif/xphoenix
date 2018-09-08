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
      SUBROUTINE TICKS(DVMIN,DVMAX,MAXTCK,DTICK,DTICKM,MTICK,NPREC)
*
* purpose :  calculate major and minor ticks for axes scaling
* date    :  October 11, 2009
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DVMIN    = minmum data value
*    DVMAX    = maximum data value
*    MAXTCK   = maximum number of major ticks
*
* export variables :
*    DTICK    = major tick separation
*    DTICKM   = minor tick separation
*    MTICK    = number of minor ticks
*    NPREC    = precision of decimal
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
      DH1 = (DVMAX-DVMIN)/Dfloat(MAXTCK)
      NHL = Idint(Dlog10(Dabs(DH1)))
      If(DH1.le.1.D0)  NHL = NHL-1
      DH1 = 10.D0**Dmod(Dlog10(Dabs(DH1)),1.D0)
      If(NHL.lt.0)  DH1 = DH1*10.D0
*
      DTICK = 1.D0
      NPREC = Max0(0,-NHL)
      MTICK = 1
*
      If(DH1.ge.1.D0) Then
       DTICK = 2.D0
       MTICK = 3
      Endif
*
      If(DH1.ge.2.D0) Then
       DTICK = 5.D0
       MTICK = 4
      Endif
*
      If(DH1.ge.5.D0) Then
       DTICK = 10.D0
       NPREC = Max0(0,NPREC - 1)
       MTICK = 1
      Endif
*
      If(NHL.lt.0) Then
       DTICK = DTICK/10**Iabs(NHL)
      Else
       DTICK = DTICK*10**NHL
      Endif
*
      DTICKM = DTICK/Dfloat(MTICK+1)
*
*
      END
