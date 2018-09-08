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
      SUBROUTINE XFILL(X,L1,L2,XP)
*
* purpose :  fill string X with pattern XP
* date    :  05. march 1992
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    X     = character string to be filled
*    L1,L2 = lower,upper bound for filling, L2 >= L1
*    XP    = character string pattern
*
* export variables :
*    X    = filled with pattern XP
*
*
#include<implici.h>
*
      CHARACTER*(*) X,XP
*
      LT = MIN0(L2,LEN(X))
      LI = LT-L1+1
      LP = LEN(XP)
*
      IF(L1.LE.0 .OR. LT.LE.0) RETURN
      IF(LP.EQ.0 .OR. LI.LE.0) RETURN
*
*:  length of pattern too large
*
      IF(LP.GE.LI) THEN
       X(L1:LT) = XP(1:LI)
       RETURN
      ENDIF
*
*:  fill string
*
      LIP = LI/LP
      LIR = LIP*LP
      DO 1  L = 0,LIP-1
       LL = L*LP + L1
       X(LL:LL+LP-1) = XP
    1 CONTINUE
      IF(LIR+L1-1.EQ.LT) RETURN
      X(LIR+L1:LT) = XP(1:LI-LIR)
*
*
      END
