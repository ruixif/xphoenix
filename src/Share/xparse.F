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
      SUBROUTINE XPARSE(X,XS,NA,XA)
*
* purpose :  parse string X to XA(*) using separator XS
* date    :  05. march 1992
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    X     = string to be parsed
*    XS    = string to be used as separator
*    NA    = number of strings in output array
*
* export variables :
*    XA(*) = parts of X, originally separated by XS
*
*
#include<implici.h>
*
      CHARACTER*(*) X,XS,XA(*)
      CHARACTER*1   X0,XB
*
      LI = LEN(X)
      LS = LEN(XS)
      LA = LEN(XA(1))
*
      IF(LI.LE.0 .OR. LS.LE.0) RETURN
      IF(NA.LE.0 .OR. LA.LE.0) RETURN
*
      LL = MIN0(LI,LA)
      BF = LL.LT.LA
      X0 = CHAR(0)
      XB = ' '
*
*:  remove leading separators
*
      CALL XADJ(X,XS)
*
*:  default output
*
      XA(1)(1:LL) = X(1:LL)
      IF(BF) CALL XFILL(XA(1),LL+1,LA,XB)
      DO 1  N = 2,NA
       CALL XFILL(XA(N),1,LA,XB)
    1 CONTINUE
*
*:  parse input string
*:  process only NA-1 output strings
*
      IF(INDEX(X,XS).NE.0) THEN
       DO 2  N = 1,NA-1
        LSP = INDEX(X,XS)
*
*:  first char in X is still a separator
*
        IF(LSP.EQ.1 .AND. N.GT.1) THEN
         XA(N)(1:LL) = X(1:LL)
         IF(BF) CALL XFILL(XA(N),LL+1,LA,XB)
         RETURN
        ENDIF
*
*:  if no more separators found put remainder
*:   in output and exit
*
        IF(LSP.EQ.0) THEN
         XA(N)(1:LL) = X(1:LL)
         IF(BF) CALL XFILL(XA(N),LL+1,LA,XB)
         RETURN
        ENDIF
*
*:  if there are separators found but no more
*:   output strings available put remainder
*:   in output and exit loop
*
*
*:  analyze leading part of X
*
        LAP = MIN0(LSP-1,LA)
        XA(N)(1:LAP) = X(1:LAP)
        CALL XFILL(XA(N),LAP+1,LA,XB)
        CALL XFILL(X,1,LSP-1,X0)
*
*:  strip leading part of X
*:  strip leading separators
*
        CALL XADJ(X,X0)
        CALL XADJ(X,XS)
    2  CONTINUE
*
*:  process the last output strings
*:  if we get here it has to be the remainder
*
       XA(NA)(1:LL) = X(1:LL)
       IF(BF) CALL XFILL(XA(NA),LL+1,LA,XB)
      ENDIF
*
*
      END
