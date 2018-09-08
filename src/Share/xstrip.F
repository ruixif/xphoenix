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
       SUBROUTINE XSTRIP(NL,NV,XC,X)
*
* purpose :  remove those elements from string array X
*             which begin with the comment string <XC>
* date    :  08. march 1992
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    X(*) = string array
*    NL   = total nr. of elements in X(*)
*    XC   = comment string
*
* export variables :
*    X(*) = string array, comment lines stripped
*    NV   =  nr. of valid elements in X(*)
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      CHARACTER*(*) XC,X(*)
*
*
      LI = LEN(X(1))
      LC = LEN(XC)
*
      IF(NL.LE.0 .OR. LI.LE.0 .OR. LC.LE.0 .OR. LI.LT.LC) RETURN
*
*:  strip comment lines
*
      NV = 0
      DO 1 N = 1,NL
       IF(X(N)(1:LC).EQ.XC) GOTO 1
       NV    = NV + 1
       X(NV) = X(N)
    1 CONTINUE
*
*
      END
