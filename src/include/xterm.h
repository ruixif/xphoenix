*
**************************************************************
*
*  CONUSS Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of CONUSS-2.1.4.
*
*  CONUSS-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation.
*
*  CONUSS-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with CONUSS-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
* purpose :  define xterm control sequences
* date    :  December 22, 2013
* author  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
      Character*2  XT_CSI
      Character*4  XT_BLD,XT_REV,XT_RST
      Character*5  XT_NRV,XT_BLK,XT_RED,XT_GRN,XT_YLW,XT_BLU,
     +             XT_MAG,XT_CYN,XT_WHT,XT_DEF
*
      Parameter( XT_CSI = Char(27)//'[',
     +           XT_REV = XT_CSI//'7m', XT_NRV = XT_CSI//'27m',
     +           XT_BLD = XT_CSI//'1m', XT_RST = XT_CSI//'0m',
     +           XT_BLK = XT_CSI//'30m', XT_RED = XT_CSI//'31m',
     +           XT_GRN = XT_CSI//'32m', XT_YLW = XT_CSI//'33m',
     +           XT_BLU = XT_CSI//'34m', XT_MAG = XT_CSI//'35m',
     +           XT_CYN = XT_CSI//'36m', XT_WHT = XT_CSI//'37m',
     +           XT_DEF = XT_CSI//'39m' )
*
