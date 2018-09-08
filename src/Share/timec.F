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
      SUBROUTINE TIMEC(RR,N,K,M)
*
* AUFGABE :  Verwaltung von max. 5 Uhren
* DATUM   :  09. Februar 1992
* PERSON  :  WOLFGANG STURHAHN
*
* VARIABLEN : n    = Anzahl der Uhren (max.5)
*             k    = Auszulesende Uhr
*             m    = 0  kein restart, >0 restart Uhr <k>,
*                    <0 restart aller Uhren
*             r(2) = 1.El. vergangene Nutzerzeit
*                    2.El. vergangene Systemzeit
*
#include<implici.h>
*
      PARAMETER(PUHR = 5)
*
      DIMENSION RT(2,PUHR),RR(*),R(2)
      SAVE RT
*
      r(1) = 0.0
      r(2) = 0.0
#ifndef no_U77
#ifdef GNUcomp
#ifdef GNUold
      CALL DTIME(RSUM,R)
#else
      CALL DTIME(R,RSUM)
#endif
#elif IBMcomp
      R(1) = 0.01*FLOAT(MCLOCK())
#else
      CALL DTIME(R)
#endif
#endif
      rr(1) = r(1)
      rr(2) = r(2)
*
      nn = min0(n,puhr)
      nn = max0(nn,1)
      kk = min0(k,puhr)
      kk = max0(kk,1)
*
      if(m.lt.0) then
       rr(1) = 0.0
       rr(2) = 0.0
*
*:  reset clocks
*
       do 1 i=1,nn
        rt(1,i) = 0.0
        rt(2,i) = 0.0
    1  continue
      else
*
*:  update clocks
*
       do 2 i=1,nn
#ifdef IBMcomp
        rt(1,i) = rr(1)
        rt(2,i) = rr(2)
#else
        rt(1,i) = rt(1,i) + rr(1)
        rt(2,i) = rt(2,i) + rr(2)
#endif
    2  continue
       rr(1) = rt(1,kk)
       rr(2) = rt(2,kk)
*
*:  reset clock <kk>
*
       if(m.gt.0) then
        rt(1,kk) = 0.0
        rt(2,kk) = 0.0
       endif
      endif
*
*
      END
