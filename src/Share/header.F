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
      Subroutine HEADER(LUN,XVERSION,XMODULE)
*
* Purpose :  Print PHOENIX logo
* Date    :  March 18, 1997
* Author  :  Wolfgang Sturhahn                                          
*
*
* import variables :
*    LUN      = logical unit number for print out
*    XVERSION = PHOENIX version
*    XMODULE  = PHOENIX module
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Character*(*) XVERSION,XMODULE
*
      WRITE(LUN,2000) XMODULE(1:(LENX(XMODULE,' '))),
     +                XVERSION(1:(LENX(XVERSION,' ')))
 2000 FORMAT(/6X,
     + '      #####   ##   ## #####  ###### ##   ## ##    ##   ##'
     + /6X,
     + '     ##   ## ##   ## ##   ## ##     ###  ##  ##    ##  ##'
     + /6X,
     + '    ##   ##  ##   ## ##   ## ##      ###  ## ##     ####'
     + /6X,
     + '   ######   ####### ##   ##  #####   ## # ##  ##     ##'
     + /6X,
     + '  ##       ##   ##  ##   ##  ##      ##  ###   ##    ###'
     + /6X,
     + ' ##        ##   ##  ##   ##  ##      ##   ##   ##   ## ##'
     + /6X,
     + '###       ##   ##    #####   ######   ##   ##   ## ##   ###'
     + //3X,'PHOnon Excitation by Nuclear resonant Inelastic ',
     +                'X-ray scattering'
     + /18X,'module ',A,' - version ',A/)
*
*
      END
