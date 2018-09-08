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
       SUBROUTINE XRMEL(NL,NV,XC,X)
*
* purpose :  remove leading characters XC from all
*            elements of string array X.
*            Elements that are filled with XCs only will
*            get removed.
* date    :  June 11, 1995
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    X(*) = string array
*    XC   = adjust character
*    NL   = total nr. of elements in X(*)
*
* export variables :
*    X(*) = string array, adjusted, empty lines stripped
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
*:  adjust lines
*:  remove empty lines
*
      NV = 0
      Do 1 N = 1,NL
       X(NL+1) = X(N)
       Call XADJ(X(NL+1),XC)
       If(X(NL+1)(1:1).ne.' ') THEN
        NV    = NV + 1
        X(NV) = X(N)
       Endif
    1 Continue
*
*
      END
