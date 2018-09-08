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
      SUBROUTINE XADJ(X,XP)
*
* purpose :  remove leading string XP in string X
* date    :  March 22, 1992
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    X  = character string
*    XP = string which will be removed from X
*
* export variables :
*    X = leading strings XP removed
*
*
#include<implici.h>
*
      CHARACTER*(*) X,XP
*
      LI = LEN(X)
      LP = LEN(XP)
*
      IF(LI.LE.0 .OR. LP.LE.0) RETURN
      IF(LP.GT.LI)             RETURN
*
*:  number of matches for XP in X
*
      LM = 0
      DO 1  L = 1,LI-LP+1,LP
       IF(X(L:L+LP-1).EQ.XP) THEN
        LM = LM+1
       ELSE
        GOTO 100
       ENDIF
    1 CONTINUE
  100 CONTINUE
      IF(LM.EQ.0) RETURN
*
*:  length of adjusted input string
*
      LA = LI-LM*LP
*
*:  fill output string properly, pad with blanks
*
      IF(LA.GT.0) X(1:LA) = X(1+LM*LP:LI)
*
      CALL XFILL(X,LA+1,LI,' ')
*
*
      END
