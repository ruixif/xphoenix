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
       Subroutine COPYRD(LUN,XVERSION)
*
* purpose :  print the copyright message
* date    :  January 7, 1995
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    LUN       = logical unit number for print out
*    XVERSION  = version number
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
#include<xterm.h>
*
      Character*(*) XVERSION
*
      External LENX
*
      L = LENX(XVERSION,' ')
      Write(LUN,2000) XT_CYN,XVERSION(1:L),XT_DEF
*
 2000 FORMAT(/A,
     +   1X,'++ PHOENIX-',A,' Copyright (C) 2016 Wolfgang Sturhahn'/
     +   1X,'++ This program comes with ABSOLUTELY NO WARRANTY.'/
     +   1X,'++ This is free software.'/
     +   1X,'++ You may redistribute it under certain conditions.'/
     +   1X,'++ For details see <http://www.gnu.org/licenses/>.',A/)
*
*
      End
