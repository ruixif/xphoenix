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
      SUBROUTINE XTOUP(X)
*
* purpose :  convert letters of string X to upper case
* date    :  05. march 1992
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    X     = input character string to be filled
*
* export variables :
*    X     = like import but letters converted to upper case
*
*
#include<implici.h>
*
      CHARACTER*(*) X
*
      LI = LEN(X)
*
      IF(LI.LE.0) RETURN
*
*:  scan X and convert to upper case
*
      DO 1  L = 1,LI
       IF(LLE('a',X(L:L)) .AND. LGE('z',X(L:L))) THEN
        X(L:L) = CHAR(ICHAR(X(L:L)) - 32)
       ENDIF
    1 CONTINUE
*
*
      END
