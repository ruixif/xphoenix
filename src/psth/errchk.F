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
      SUBROUTINE ERRCHK(NERR,*)
*
*
* Purpose :  decide on program termination due to an error
* Date    :  November 6, 1997
* Author  :  Wolfgang Sturhahn
*
* export variables:
*    NERR       error code created as created by
*                the subroutine
*                the following coding convention is used:
*                  ...xxnnn (each char. symbolizes a figure)
*                 xx  = codes the subroutine name
*                 nnn = error number
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
*:  no error
*
      IF(NERR.EQ.0) RETURN
*
*:  error analysis
*
      CALL PSTERR(6,NERR)
*
*:  terminate program if error was not recovered
*
      IF(NERR.EQ.0) THEN
       RETURN 1
      ELSE
       Call Exit(1)
       Stop
      ENDIF
*
*
      END
