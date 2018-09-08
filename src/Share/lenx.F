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
      FUNCTION LENX(X,XT)
*
* purpose :  determines length of string, an arbitrary
*            number of trailing strings XT is ignored
* date    :  January 4, 1996
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    X    = string to be evaluated
*    XT   = string to be ignored
*
* export variables :
*    LENX = length of X
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      CHARACTER*(*) X,XT
*
*
      LI = LEN(X)
      LT = LEN(XT)
*
      IF(LT.GT.LI) THEN
       LENX = LI
      ELSE
       LENX = 0
       DO 1  I = LI,1,-LT
        IF(X(I-LT+1:I).NE.XT) THEN
          LENX = I
          RETURN
        ENDIF
    1  CONTINUE
      ENDIF
*
*
      END
