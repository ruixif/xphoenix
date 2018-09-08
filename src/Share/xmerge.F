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
      SUBROUTINE XMERGE(XM,XS)
*
* purpose :  merge strings XS and XM,
*            trailing blanks are removed
* date    :  December 28, 1995
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XS,XM = character strings to be merged
*
* export variables :
*    XM   = merged input strings, blanks removed
*
*
#include<implici.h>
*
      CHARACTER*(*) XM,XS
*
      LM = LEN(XM)
*
      IF(LM.EQ.0) RETURN
*
*:  length of string ignoring trailing blanks
*
      LMB = LENX(XM,' ')
      LSB = LENX(XS,' ')
*
      IF(LMB+LSB.GT.LM) RETURN
*
*:  add string
*
      XM(LMB+1:LMB+LSB) = XS(1:LSB)
*
*
      END
